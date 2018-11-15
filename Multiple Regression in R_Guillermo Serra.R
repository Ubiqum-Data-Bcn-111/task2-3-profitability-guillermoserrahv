setwd("/Users/guillermoserrahv/Desktop/Ubiqum/GitHub_Ubiqum/ubiqum1/task2-3-profitability-guillermoserrahv")
getwd()
library(caret)
library(readxl)
library(rpart)
#library(dplyr)
#library(XML)

# Upload the Existing Product Attributes file
MReg <- read.csv("existingproductattributes2017.2.csv")
MReg <- as.data.frame(MReg)

# Dummify the data
MRegDum <- dummyVars(" ~ .", data=MReg)
MRD <- data.frame(predict(MRegDum, newdata=MReg))

# Checking the datatypes in the dataframe
str(MReg)
str(MRegDum)
str(MRD)

# Remove the missing value column, which is "Best Sellers Rank"
summary(MRD)
is.na(MRD)
sum(is.na(MRD$BestSellersRank))
MRD$BestSellersRank <- NULL
sum(is.na(MRD))





#qqplots and histograms and for each numeric variables
for(i in 1:(ncol(Survey_Responses_Complete))) {    #for every column
  if (is.numeric(Survey_Responses_Complete[,i])){  #if the variable is numeric
    qqnorm(Survey_Responses_Complete[,i],main=paste("Test",colnames(Survey_Responses_Complete)[i])) #plot qqnorm
    qqline(Survey_Responses_Complete[,i],col="red") #add qqnormal line in red
    hist(Survey_Responses_Complete[,i],main=paste("Histogram of",colnames(Survey_Responses_Complete)[i]), 
         xlab=colnames(Survey_Responses_Complete)[i])
  }
}

