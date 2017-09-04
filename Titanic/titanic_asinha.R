library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(car)
library(stringr)

titanic_train_base <- read.csv("train.csv",stringsAsFactors = F)
titanic_test_base <- read.csv("test.csv",stringsAsFactors = F)


## Data Understanding and Data Preparation - Train Data ##
  titanic_train <- titanic_train_base
 
  # Lets drop 1st make some columns factors
  # Pclass
  titanic_train$Pclass <- as.factor(titanic_train$Pclass)

  #Sex
  titanic_train$Sex <- as.factor(titanic_train$Sex)
  
  #Survived
  titanic_train$Survived <- as.factor(titanic_train$Survived)

  # Lets just drop the Name, Ticket columns #
  titanic_train <- titanic_train %>% dplyr::select(-c(Name,Ticket))

  # Number of NAs in the dataset
  sapply(titanic_train,function(x) sum(is.na(x)))
  # PassengerId    Survived      Pclass         Sex         Age       SibSp       Parch        Fare       Cabin    Embarked 
  #           0           0           0           0         177           0           0           0           0           0 

  # There are 177 NAs in Age
  # Lets see the boxplot of age by Sex
  ggplot(titanic_train %>% filter(!is.na(Age))) + geom_boxplot(aes(x=Sex,y=Age))
  titanic_train %>% group_by(Sex) %>% summarize(medianAge = median(Age,na.rm=TRUE),meanAge=mean(Age,na.rm=TRUE))

  # Tring to see if WOE has some inference for Age #
  
  woe.df <- woe(Data=titanic_temp_train,
                Independent = "Age",
                Continuous = TRUE,
                Dependent = "Survived",
                Good=1,
                Bad=0,
                C_Bin=3)
  ggplot(woe.df) + geom_line(aes(x=BIN,y=WOE))
  woe.df
  sum(woe.df$IV)
  
  #The monotonous nature of WOE is only available in 2 bins, with WOE as -5 & +5. Not very interesting.
  #Not going with WOE analysis#
  
  # Lets see the mean & median age of Men and Women
  titanic_train %>% 
    filter(!is.na(Age)) %>% 
    group_by(Sex) %>% 
    summarize(cnt=length(PassengerId),avg=mean(Age),med=median(Age)) %>% 
    mutate(perc=cnt/sum(cnt))
  
  # Percentage distribution between male and female for NAs of Age #
  titanic_train %>% 
    filter(is.na(Age)) %>% 
    group_by(Sex) %>% 
    summarize(cnt=length(PassengerId)) %>% 
    mutate(perc=cnt/sum(cnt))
  
  # For men lets impute Age = NA as -> median(Age) of men
  # For Women lets impute Age = NA -> median(Age) of women
  # we do not see a NA pattern here

  titanic_train$Age[which(is.na(titanic_train$Age) & titanic_train$Sex == "male")] <- 
    (titanic_train %>% group_by(Sex) %>% summarize(medianAge = median(Age,na.rm=TRUE),meanAge=mean(Age,na.rm=TRUE)) %>% 
    filter(Sex=="male"))$medianAge

  titanic_train$Age[which(is.na(titanic_train$Age) & titanic_train$Sex == "female")] <- 
    (titanic_train %>% group_by(Sex) %>% summarize(medianAge = median(Age,na.rm=TRUE),meanAge=mean(Age,na.rm=TRUE)) %>% 
    filter(Sex=="female"))$medianAge

  # Now lets see the distribution once again #
  titanic_train %>% 
    filter(!is.na(Age)) %>% 
    group_by(Sex) %>% 
    summarize(cnt=length(PassengerId),avg=mean(Age),med=median(Age)) %>% 
    mutate(perc=cnt/sum(cnt))
  
  # The distribution looks now pretty similar as to what it was earlier, so we are good here!
  
  sapply(titanic_train,function(x) sum(is.na(x)))
  ## No more NAs now ##

  levels(as.factor(titanic_train$Cabin))
  # There are Cabins where multiple entries also exist, lets first modify it and have only 1st cabin name for considerations
  # There are 4 rows as "F G73", like. Assuming them to only G73 type. Lets first change them
  cabin_F_space_idx <- which(grepl("F +",titanic_train$Cabin))
  titanic_train$Cabin[cabin_F_space_idx] <- sapply(titanic_train$Cabin[cabin_F_space_idx],function(x) str_split(x,pattern = " ")[[1]][2])

  # There are multiple entries in some cabins, we will take the first entry
  titanic_train$Cabin <- sapply(titanic_train$Cabin,function(x) str_split(x,pattern = " ",simplify = TRUE)[1])

  # Lets separate the cabin class and cabin number, and reject the numbers as such
  titanic_train$Cabin <- sapply(titanic_train$Cabin,function(x) str_split(x,pattern="[0-1000]*")[[1]][2])

  # Cabin = blank -> specify as "No Cabin" #
  titanic_train[which(is.na(titanic_train$Cabin)),'Cabin'] <- "No Cabin"
  
  # Convert Cabin as Factors
  titanic_train$Cabin <- as.factor(titanic_train$Cabin)
  
  # Convert Embarked as Factors
  titanic_train$Embarked <- as.factor(titanic_train$Embarked)
  
  

## Data Exploration - EDA ##
  # Lets see the survival rate by different parameters we have ##  
  G01 <- ggplot(titanic_train) + 
    geom_bar(aes(x=Pclass,fill=Survived),position=position_dodge(width=0.5),alpha=0.7) +
    scale_y_continuous(breaks=seq(0,400,20)) +
    labs(title="Pclass Vs Survived",xlab="PClass",ylab="Count",fill="Survived\n1=Yes\n0=No")
  
  G02 <- ggplot(titanic_train) + 
    geom_bar(aes(x=Sex,fill=Survived),position=position_dodge(width=0.5),alpha=0.7) +
    scale_y_continuous(breaks=seq(0,500,20)) +
    labs(title="Sex Vs Survived",xlab="Sex",ylab="Count",fill="Survived\n1=Yes\n0=No")
  
  G03 <- ggplot(titanic_train) + 
    geom_histogram(aes(x=Age,fill=Survived),binwidth=1,position=position_dodge(width=0.5),alpha=0.7) +
    scale_y_continuous(breaks=seq(0,500,20)) +
    labs(title="Sex Vs Survived",xlab="Sex",ylab="Count",fill="Survived\n1=Yes\n0=No")
  
  
  
# We have do chi-square analysis here and annova analysis for categorical variable co-relation with
# continuous variables. Lets do that a bit later, lets just move ahead as of now.
# Also we need to complete the EDA analysis here.
  
#  Logistic Regression data preparation #
  
  titanic_train_reg <- titanic_train
  
  str(titanic_train_reg)
  
  # 1) For PClass, we will keep the values as it is, as PClass = 1 is higher and Pclass =3 is lower, and
  #    these levels express the valid understanding.
  
  # 2) Sex - Male/Female needs to be modified as 0 and 1, 
  #    Lets keep Male = 0 and Female = 1
  titanic_train_reg$Sex <- ifelse(titanic_train_reg$Sex == "male",0,1)
  
  # 3) Cabin - Use model.matrix for this
  dummy_cabin <- model.matrix(~Cabin,data=titanic_train_reg)
  titanic_train_reg <- cbind(titanic_train_reg %>% dplyr::select(-Cabin),dummy_cabin[,-1])
  
  # 4) Embarked - Use model.matrix for this
  dummy_embarked <- model.matrix(~Embarked,data=titanic_train_reg)
  titanic_train_reg <- cbind(titanic_train_reg %>% dplyr::select(-Embarked),dummy_embarked[,-1])
  