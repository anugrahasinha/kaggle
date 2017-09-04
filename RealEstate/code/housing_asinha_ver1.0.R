library(dplyr)
library(ggplot2)


# Setup #
setwd("C:/Users/anugraha.sinha/OneDrive/Documents/Kaggle_work_git_repo/kaggle/RealEstate/data")


## Data Sourcing ##
housing_train <- read.csv("housing_train.csv",stringsAsFactors = FALSE)
housing_test <- read.csv("housing_test.csv",stringsAsFactors = FALSE)

# Business Understanding #
# We need predict the house prices, based on the data sets given.#

# Data Understanding #
# Lets keep the price column of train data set separately and do a merge of this later on #
housing_train_price <- housing_train$Price

housing_train <- housing_train %>% select(-Price)

# Lets add a column called "DataType" = Train and "DataType" = Test for both the train
# and test data set and merge the 2 data frames

housing_train$DataType <- "Train"
housing_test$DataType <- "Test"


housing_all <- rbind(housing_train,housing_test)

View(housing_all)

# NA Analysis #
sapply(housing_all, function(x) sum(is.na(x)))


housing_cor_check <- housing_all %>% 
  select(Rooms,Distance,Bedroom2,Bathroom,Car,Landsize,BuildingArea,YearBuilt) %>% 
  filter(!is.na(Bedroom2),!is.na(Bathroom),!is.na(Car),!is.na(Landsize),
         !is.na(BuildingArea),!is.na(YearBuilt))

cor(housing_cor_check[,-8])

# Data Preparation #
# Model Preparation #
# Model Evaluation #
# Model Deployement #

# Data Understanding #
