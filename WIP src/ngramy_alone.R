## OZNAL project by Peter Berta

# Read training data
data_raw = read.csv(file.path("job-salary-prediction", "Train_rev1.csv"),nrows=200)
data_test = read.csv(file.path("job-salary-prediction", "Test_rev1.csv"),nrows=200)

# All attributes
# Id,Title,FullDescription,LocationRaw,LocationNormalized,ContractType,ContractTime,Company,Category,SalaryRaw,SalaryNormalized,SourceName
train_vars <- c("FullDescription", "SalaryNormalized")
test_vars <- c("FullDescription")


data_train <- data_raw[train_vars]
data_test <- data_test[test_vars]
# Show data summary
summary(data_train)
summary(data_test)
head(data_train)
head(data_test)

# Prepare sample text for processing
library(tm)
test <- data_train

# Text cleaning function
test$CleanDescription <- sapply(test$FullDescription, function(x)
  {
    x = tolower(x)
    x = gsub("[[:digit:]]", "", x)
    x = gsub("[[:punct:]]", "", x)
    x = removeWords(x, stopwords("en"))
    x = gsub("[ \t]{2,}", " ", x)
    x = gsub("^\\s+|\\s+$", "", x)
    x
  })


# Lets create the ngrams
library(dplyr)
library(tidytext)
funky_ngrams <- test %>% 
  unnest_tokens(Bigram, CleanDescription, token = "ngrams", n = 2)

funky_ngrams %>%
  count(Bigram, sort = TRUE)

library(quanteda)
dfm(funky_ngrams, tolower = FALSE, stem = FALSE, select = NULL, remove = NULL,
    dictionary = NULL, thesaurus = NULL, valuetype = "fixed", 
    groups = NULL)
is.corpus(funky_ngrams)
# Create some ngrams
library(ngram)
the_thing <- ngram(as.character(test), n=2)
print(the_thing, output = "full")


test_corpus <- corpus(test$CleanDescription)
is.corpus(test_corpus)
why_is_that <- dfm(test_corpus)
what_is_this <- convert()
featnames(dfm(test_corpus))
why_is_that


