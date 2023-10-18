###################################
#Horse_heath_outcome Project
#27092023
#CLIFFORD ODUOR,PhD 
####################################
#SUPERVISED LEARNING###
#CLASSICATION PROBLEM

rm(list=ls())
require(ggplot2)
require(dplyr)
require(reshape)
require(foreign)
require(haven)
require(magrittr)

train_data=read.csv("C:/DATA SCIENCE/Data_science_COMPETITIONS/kaggle/project1/horse_heath_outcome/train.csv",header = T,sep=",", stringsAsFactors = F,na.strings = c(""))
test_data=read.csv("C:/DATA SCIENCE/Data_science_COMPETITIONS/kaggle/project1/horse_heath_outcome/test.csv",header = T,sep=",", stringsAsFactors = F,na.strings = c(""))

names(train_data)
table(train_data$outcome,useNA = "always")
str(train_data)
str(test_data)

###CLEANING DATASET####
#CHECKING FOR MISSING VALUES
#install.packages("Amelia")
#install.packages("Rcpp")
library(Amelia)
library(Rcpp)
require(dplyr)
missmap(train_data, main = "Missing values vs observed")
#CHECKING FOR DUPLICATES
sapply(train_data, function(x) length(unique(x)))
is.factor(train_data$outcome)
#contrasts(train_data$outcome)
train_data$outcome.numeric=recode(train_data$outcome,lived=1, died=2,euthanized=3)
train_data1=subset(train_data,select=-c(hospital_number))
###SUMMARY######
table(train_data1$peristalsis)

train_data2 <-train_data1%>% mutate_if(is.character, as.factor)


#######ML ALGORITHIM1:NEURAL NETWORKS###########################################################################################

#install.packages('nnet')
library(nnet)
#MODEL FIT 1
str(train_data1)
fit1 <- multinom(outcome.numeric~surgery+age+respiratory_rate+packed_cell_volume,data+peristalsis, data= train_data2,
                 family = multinomial)
summary(fit1)

predicted_results<-predict(fit1, data = test_data, type = "class")

table(train_data1$outcome.numeric, predicted_results)

confusion_matrix <- table(train_data1$outcome.numeric, predicted_results,dnn=c("Data", "Predictions"))

(accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix))














