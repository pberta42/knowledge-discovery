## OZNAL project by Peter Berta

# Read training data

data_raw = read.csv("job-salary-prediction/Train_rev1.csv",nrows=200)
data_test = read.csv("job-salary-prediction/Test_rev1.csv",nrows=200)

# Specify possible good attributes
train_vars <- c("LocationNormalized", "ContractType", "ContractTime", "Company", "Category", "SourceName", "SalaryNormalized")
test_vars <- c("LocationNormalized", "ContractType", "ContractTime", "Company", "Category", "SourceName")


data_train <- data_raw[train_vars]
data_test <- data_test[test_vars]
# Show data summary
summary(data_train)
summary(data_test)
head(data_train)
head(data_test)

# data[data == ""] <- NA

library(dplyr)
valLoc <- function(dataset){
  result <- recode(dataset$LocationNormalized,
                   "UK"="1",
                   "London"="2",
                   "Surrey"="3", 
                   "Avon"="4",
                   "Berkshire"="5",
                   "Hampshire"="6", 
                   .default = "0")
  return(result)
}

valConTyp <- function(dataset){
  result <- recode(dataset$ContractType,
                   "full_time"="1", 
                   .default = "0")
  return(result)
}

valConTim <- function(dataset){
  result <- recode(dataset$ContractTime, 
                   "contract"="1", 
                   "permanent"="2", 
                   .default = "0")
  return(result)
}

valCom <- function(dataset){
  result <- recode(dataset$Company,
                   "Clear Selection"="2", 
                   "Gregory Martin International"="3", 
                   "Chef Results"="4", 
                   "Indigo 21 Ltd"="5",
                   "MatchBox Recruiting Ltd"="6",
                   .default = "0")
  return(result)
}

valCat <- function(dataset){
  result <- recode(dataset$Category, 
                   "Hospitality & Catering Jobs"="1", 
                   "Engineering Jobs"="2", 
                   "IT Jobs"="3", 
                   "Healthcare & Nursing Jobs"="4", 
                   "Travel Jobs"="5", 
                   "Sales Jobs"="6", 
                   .default = "0")
  return(result)
}

valSou <- function(dataset){
  result <- recode(dataset$SourceName, 
                   "cv-library.co.uk"="1", 
                   "caterer.com"="2", 
                   "jobs.catererandhotelkeeper.com"="3", 
                   "careworx.co.uk"="4", 
                   "leisurejobs.com"="5", 
                   "hays.co.uk"="6", 
                   .default = "0")
  return(result)
}

data_train$Location_val <- valLoc(data_train)
data_train$ContractType_val <- valConTyp(data_train)
data_train$ContractTime_val <- valConTim(data_train)
data_train$Company_val <- valCom(data_train)
data_train$Category_val <- valCat(data_train)
data_train$SourceName_val <- valSou(data_train)

data_test$Location_val <- valLoc(data_test)
data_test$ContractType_val <- valConTyp(data_test)
data_test$ContractTime_val <- valConTim(data_test)
data_test$Company_val <- valCom(data_test)
data_test$Category_val <- valCat(data_test)
data_test$SourceName_val <- valSou(data_test)

summary(data_train)
summary(data_test)
head(data_train)
head(data_test)


# Nice graphs
library(ggplot2)
plot(data_train$SalaryNormalized, type = "l")
ggplot(data = data_train, aes(x=SalaryNormalized))+geom_histogram()

ggplot(data = data_train, aes(y=SalaryNormalized, x=Location_val))+geom_point()+labs(x="Location", y="Salary", title="Location by Salary")
ggplot(data = data_train, aes(y=SalaryNormalized, x=Category_val))+geom_point()+labs(x="Category", y="Salary", title="Category by Salary")
ggplot(data = data_train, aes(y=SalaryNormalized, x=Company_val))+geom_point()+labs(x="Company", y="Salary", title="Company by Salary")


# Try to learn
model <- lm(SalaryNormalized ~ Location_val + ContractTime_val + ContractType_val + Company_val + Category_val + SourceName_val, data = data_train)
model <- lm(SalaryNormalized ~ LocationNormalized + ContractTime + ContractType + Company + Category + SourceName, data = data_train)

plot(model)
predictions <- predict(model, data_test)
summary(predictions)
summary(model)
fitted(model)
vcov(model)

library(ngram)
test <- data_raw$FullDescription
the_thing <- ngram(as.character(test), n=2)
print(the_thing, output = "full")



