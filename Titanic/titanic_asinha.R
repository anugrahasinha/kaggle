library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(car)
library(stringr)

titanic_train_base <- read.csv("train.csv",stringsAsFactors = F)




data_preparation <- function(titanic_work_df) {

  # Lets drop 1st make some columns factors
  # Pclass
  titanic_work_df$Pclass <- as.factor(titanic_work_df$Pclass)

  #Sex
  titanic_work_df$Sex <- as.factor(titanic_work_df$Sex)
  
  #Survived
  titanic_work_df$Survived <- as.factor(titanic_work_df$Survived)

  # Lets just drop the Name, Ticket columns #
  titanic_work_df <- titanic_work_df %>% dplyr::select(-c(Name,Ticket))

  # Number of NAs in the dataset
  sapply(titanic_work_df,function(x) sum(is.na(x)))
  # PassengerId    Survived      Pclass         Sex         Age       SibSp       Parch        Fare       Cabin    Embarked 
  #           0           0           0           0         177           0           0           0           0           0 

  # There are 177 NAs in Age
  # Lets see the boxplot of age by Sex
  ggplot(titanic_work_df %>% filter(!is.na(Age))) + geom_boxplot(aes(x=Sex,y=Age))
  titanic_work_df %>% group_by(Sex) %>% summarize(medianAge = median(Age,na.rm=TRUE),meanAge=mean(Age,na.rm=TRUE))

  # For men lets impute Age = NA as -> mean(Age) of men
  # For Women lets impute Age = NA -> mean(Age) of women
  # we do not see a NA pattern here

  titanic_work_df$Age[which(is.na(titanic_work_df$Age) & titanic_work_df$Sex == "male")] <- 
    (titanic_work_df %>% group_by(Sex) %>% summarize(medianAge = median(Age,na.rm=TRUE),meanAge=mean(Age,na.rm=TRUE)) %>% 
    filter(Sex=="male"))$meanAge

  titanic_work_df$Age[which(is.na(titanic_work_df$Age) & titanic_work_df$Sex == "female")] <- 
    (titanic_work_df %>% group_by(Sex) %>% summarize(medianAge = median(Age,na.rm=TRUE),meanAge=mean(Age,na.rm=TRUE)) %>% 
    filter(Sex=="female"))$meanAge

  sapply(titanic_work_df,function(x) sum(is.na(x)))

  levels(as.factor(titanic_work_df$Cabin))
  # There are Cabins where multiple entries also exist, lets first modify it and have only 1st cabin name for considerations
  # There are 4 rows as "F G73", like. Assuming them to only G73 type. Lets first change them
  cabin_F_space_idx <- which(grepl("F +",titanic_work_df$Cabin))
  titanic_work_df$Cabin[cabin_F_space_idx] <- sapply(titanic_work_df$Cabin[cabin_F_space_idx],function(x) str_split(x,pattern = " ")[[1]][2])

  # There are multiple entries in some cabins, we will take the first entry
  titanic_work_df$Cabin <- sapply(titanic_work_df$Cabin,function(x) str_split(x,pattern = " ",simplify = TRUE)[1])

  # Lets separate the cabin class and cabin number, and reject the numbers as such
  titanic_work_df$Cabin <- sapply(titanic_work_df$Cabin,function(x) str_split(x,pattern="[0-1000]*")[[1]][2])

  # Cabin = blank -> specify as "No Cabin" #
  titanic_work_df[which(is.na(titanic_work_df$Cabin)),'Cabin'] <- "No Cabin"
  return(titanic_work_df)
}


titanic_train <- titanic_train_base
titanic_train_2 <- data_preparation(titanic_train)

# Extrapolatory Data Analysis #
G01 <- ggplot(titanic_train_2) + geom_bar(aes(x=Sex,fill=Survived))

G02 <- ggplot(titanic_train_2) + geom_bar(aes(x=Pclass,fill=Survived))

G03 <- ggplot(titanic_train_2) + geom_histogram(aes(x=Age,fill=Survived),binwidth = 1) +
  scale_x_continuous(breaks = seq(0,100,2))





