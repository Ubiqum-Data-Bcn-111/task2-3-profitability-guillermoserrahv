setwd("/Users/guillermoserrahv/Desktop/Ubiqum/GitHub_Ubiqum/ubiqum1/task2-3-profitability-guillermoserrahv")
getwd()

##############################-INSTALLING LIBRARIES & DATA PREPARATION-##########################################
# Install appropiate libraries here 
library(readr)
library(caret)
library(rpart)
library(rpart.plot)
library(data.table)
library(plyr)
library(ggplot2)
library(ggcorrplot)
#library(corrplot)
library(reshape2)
library(RColorBrewer)
library(rpart.plot)

# Upload the Existing Product Attributes file
MReg <- read.csv("existingproductattributes2017.2.csv")
MReg <- as.data.frame(MReg)

# Data exploration
attributes(MReg)   # Attibutes list
summary(MReg)      # Summary of the values in the data set
str(MReg)          # Displays the structure of the data set
names(MReg)        # Names your attributes

# Dummify the data
MRegDum <- dummyVars(" ~ .", data=MReg)
MRD <- data.frame(predict(MRegDum, newdata=MReg))

# Checking the datatypes in the dataframe
str(MReg)
str(MRegDum)
str(MRD)

# Remove the missing value column, which is "Best Sellers Rank" based on the missing value test
summary(MRD)
is.na(MRD)
sum(is.na(MRD$BestSellersRank))
MRD$BestSellersRank <- NULL
sum(is.na(MRD))

# QQplots and Histograms (before correlation)
for(i in 1:(ncol(MRD))) {    #for every column
  if (is.numeric(MRD[,i])){  #if the variable is numeric
    qqnorm(MRD[,i],main=paste("Test",colnames(MRD)[i])) #plot qqnorm
    qqline(MRD[,i],col="blue") #add qqnormal line in red
    hist(MRD[,i],main=paste("Histogram of",colnames(MRD)[i]), 
         xlab=colnames(MRD)[i])
  }
}

# Correlation Matrix for 28 variables
corrMRD <- cor(MRD[,13:length(MRD)]) 
corrMRD

#Choose columns from 13 onwards (Basically remove the dummies)(only for visualization in the correlation matrix)
MRD[,13:length(MRD)]

# Remove x5Starreviews from the dataset
MRDNO5<-MRD[,c(-15)]

###################################-VISUALISATION OF CORRELATION MATRIX-######################################

# Visualize the Correlation Matrix with the ggplot package
ggplot(melt(corrMRD), aes(Var1, Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red") +
  coord_equal()

# Visualize the Correlation Matrix with the ggcorrplot package 
ggcorrplot(corrMRD, type = "upper", hc.order = TRUE, colors=brewer.pal(n = 3, name = "RdYlBu"))
#COLOUR CHOICES: RdYlBu or Spectral or YlGnBu

# Visualize the Correlation Matrix with the corrplot package (same as the Plan of Attack)
#corrplot(corrMRD)

#################################################-SIMPLE DESCISION TREE-#######################################

# Simple Decision Tree 
set.seed(123)
DTcorrMRD <- rpart(
  Volume~ ., data=MRDNO5) #predict volume using all variables
rpart.plot(DTcorrMRD) #plot the decision tree

# Remove x5Starreviews from the dataset
MRDNO5<-MRD[,c(-15)]

#############################################-BELOW: FOR UPDATING ABOVE-##################################


# Create a variable for Volume using Depth*Width*Height   (NEEDS TO BE PROPERLY PLACED)
MRD$Product.Volume<-MRD$ProductDepth*MRD$ProductWidth*MRD$ProductHeight
summary(Product.Volume)
Product.Volume

# Choosing some variables for the correlation visual test, but not all (NEEDS TO BE PROPERLY PLACED)(SAMPLE SET)

MRD[,c(1, 2, 4, 5, 6)]


#CHI SQUARED TESTING ()

#################################################-ALGORITHMS-############################################

#SVM models

#GBM models

