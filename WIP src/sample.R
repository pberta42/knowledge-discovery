# At firste we prepare our data into various tables

library(text2vec)
library(data.table)
library(magrittr)
data_raw = read.csv(file.path("job-salary-prediction", "Train_rev1.csv"), nrows=200)
data_test = read.csv(file.path("job-salary-prediction", "Test_rev1.csv"), nrows=200)
data_result = read.csv(file.path("job-salary-prediction", "random_forest_benchmark_test_rev1.csv"), nrows=200)
train_vars <- c("Id", "FullDescription", "SalaryNormalized")
test_vars <- c("Id", "FullDescription")

data_train <- data_raw[train_vars]
data_test <- data_test[test_vars]
setDT(data_train)
setDT(data_test)
setkey(data_train, Id)
setkey(data_test, Id)
set.seed(2017L)

# Now we start to work on our model
prep_fun = tolower
tok_fun = word_tokenizer

library(tm)
data_train$CleanDescription <- sapply(data_train$FullDescription, function(x)
{
  x = tolower(x)
  x = gsub("[[:digit:]]", "", x)
  x = gsub("[[:punct:]]", "", x)
  x = removeWords(x, stopwords("en"))
  x = gsub("[ \t]{2,}", " ", x)
  x = gsub("^\\s+|\\s+$", "", x)
  x
})

data_test$CleanDescription <- sapply(data_train$FullDescription, function(x)
{
  x = tolower(x)
  x = gsub("[[:digit:]]", "", x)
  x = gsub("[[:punct:]]", "", x)
  x = removeWords(x, stopwords("en"))
  x = gsub("[ \t]{2,}", " ", x)
  x = gsub("^\\s+|\\s+$", "", x)
  x
})

help(itoken)
it_train = itoken(as.character(data_train$CleanDescription),
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  ids = data_train$Id,
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)
vocab


vectorizer = vocab_vectorizer(vocab)
# t1 = Sys.time()
dtm_train = create_dtm(it_train, vectorizer)
# print(difftime(Sys.time(), t1, units = 'sec'))

library(glmnet)
NFOLDS = 4
#t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = dtm_train, y = data_train[['SalaryNormalized']], 
                              family = 'gaussian', 
                              # L1 penalty
                              alpha = 1,
                              # interested in the area under ROC curve
                              type.measure = "mse", # pozri nieco ine
                              # 5-fold cross-validation
                              nfolds = NFOLDS,
                              # high value is less accurate, but has faster training
                              thresh = 1e-3,
                              # again lower number of iterations for faster training
                              maxit = 1e3)
#print(difftime(Sys.time(), t1, units = 'sec'))
plot(glmnet_classifier)



it_test = data_test$CleanDescription %>% 
  prep_fun %>% tok_fun %>% 
  # turn off progressbar because it won't look nice in rmd
  itoken(ids = data_test$Id, progressbar = FALSE)


dtm_test = create_dtm(it_test, vectorizer)

preds = predict(glmnet_classifier, dtm_test, type = 'response')[,1]
# glmnet:::mse(data_result$SalaryNormalized, preds)
plot(preds)
plot(data_result$SalaryNormalized)
plot(x = preds, y = data_result$SalaryNormalized)
summary(lm(data_result$SalaryNormalized~preds))

# vycistit slova
# pozriet co kolega spravil
# vyhodnotenie

# tvorba ngramov
library(dplyr)
library(tidytext)
train_ngrams <- data_train %>% 
  unnest_tokens(Bigram, CleanDescription, token = "ngrams", n = 2)

train_ngrams %>%
  count(Bigram, sort = TRUE)
