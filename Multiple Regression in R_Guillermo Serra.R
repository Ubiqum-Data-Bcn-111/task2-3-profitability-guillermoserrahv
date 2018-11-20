setwd("/Users/guillermoserrahv/Desktop/Ubiqum/GitHub_Ubiqum/ubiqum1/task2-3-profitability-guillermoserrahv")
getwd()

###-Appendix of data names and included values-
#MReg - Original, unedited file
#MRegDum - Isolated dummy variables extracted from Product Type (total: 12 dummies)
#MRD - Data including the dummies, but minus BestSellersRank (total: 28 variables)
#corrMRD - MRD ran under the correlation matrix with ALL variables (total: 28 variables)
#MRDNO5 - MRD excluding 5 Star Reviews, 1 Star Reviews, Negative Service Reviews, and adding Product Volume
#         replacing Product Width, Product Lenght, and Product Depth. The dummies are kept (total: 23 variables)
#VIMRD -For visual purposes, the dummies from MRDNO5 are removed (total: 11 variables, no dummies)
#corrVIMRD - VIMRD ran under the correlation matrix with selected variables (total: 11 variables, no dummies)
#DTcorrMRD - A simple Decision Tree based on the VIMRD (total: 11 variables)
#VIMRD5 - Only Customer Review -2,3,4 Stars-, PositiveServiceReviews and Volume (total: 5 variables)
#DTcorrMRD5 - VIMRD5 ran under the correlation matrix with selected variables (total: 5 variables, no dummies)
#res.an - ANOVA of "ProductType" (categorial) and "Volume" (numerical) from MReg
#res.man - MANOVA of "ProductType" (categorical) and "Volume", "4StarReviews" and "PositiveServiceReviews" (numerical) from MReg
#INTraining - Data partition using MRD
#MRDtraining - 80% of samples from MRD as a training set
#MRDtesting - 20% of samples from MRD as a testing set
#LM4SMRD - Linear model of volumes and 4 Star Reviews using MRD (total: 2 variables)
#LMPSMRD - Linear model of volumes and PositiveServiceReviews using MRD (total: 2 variables)


##############################-INSTALLING LIBRARIES & DATA PREPARATION-#######################################
# Install appropiate libraries for this exercise
library(readr)
library(caret)
library(rpart)
library(rpart.plot)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(reshape2)
library(RColorBrewer)
library(rpart.plot)
library(xgboost)

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

# Remove the missing value column, which is "Best Sellers Rank" based on the missing value test
summary(MRD)
is.na(MRD)
sum(is.na(MRD$BestSellersRank))
MRD$BestSellersRank <- NULL
sum(is.na(MRD))

# Checking the datatypes in the dataframe
str(MReg)
str(MRegDum)
str(MRD)

# QQplots and Histograms (before correlation)
set.seed(123)
for(i in 1:(ncol(MRD))) {    #for every column
  if (is.numeric(MRD[,i])){  #if the variable is numeric
    qqnorm(MRD[,i],main=paste("Test",colnames(MRD)[i])) #plot qqnorm
    qqline(MRD[,i],col="blue") #add qqnormal line in red
    hist(MRD[,i],main=paste("Histogram of",colnames(MRD)[i]), 
         xlab=colnames(MRD)[i])
  }
}

# Correlation Matrix without the dummies
corrMRD <- cor(MRD[,13:length(MRD)]) 
corrMRD

# Remove x5StarReviews, x1StarReviews and NegativeServiceReview from the dataset based on the correlation matrix
MRDNO5<-MRD[,c(-15, -19, -21)]

# Create a variable for Volume using Depth*Width*Height = Product Volume
MRDNO5$Product.Volume<-MRDNO5$ProductDepth*MRDNO5$ProductWidth*MRDNO5$ProductHeight
summary(MRDNO5$Product.Volume)
MRDNO5$Product.Volume

# Remove the Product Depth, Product Width, and Product Height, instead keep only Product Volume
MRDNO5<-MRDNO5[,c(-21, -22, -23)]

# Choose columns from 13 onwards (Basically remove the dummies)(only for visualization in the correlation matrix)
VIMRD<-MRDNO5[,13:length(MRDNO5)]

# Correlation Matrix for 11 relevant variables, excluding dummies
corrMRD5 <- cor(MRDNO5[,13:length(MRDNO5)]) 
corrVIMRD <- cor(VIMRD[,]) 
corrVIMRD

###################################-VISUALISATION OF CORRELATION MATRIX-######################################
# Visualize the Correlation Matrix with the ggplot package
ggplot(melt(corrVIMRD), aes(Var1, Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red") +
  coord_equal()

# Visualize the Correlation Matrix with the ggcorrplot package 
ggcorrplot(corrVIMRD, type = "upper", hc.order = TRUE, colors=brewer.pal(n = 3, name = "RdYlBu"))
#COLOUR CHOICES: RdYlBu or Spectral or YlGnBu

#######################################-SIMPLE DESCISION TREE (11 & 5 variables)-#################################
# Simple Decision Tree (11 variables, no dummies)
set.seed(123)
DTcorrMRD <- rpart(
  Volume~ ., data=VIMRD, cp=0.0001) # Predict "Volume" using all remaining variables
rpart.plot(DTcorrMRD) # Plot the simple Decision Tree

# Simple Decision Tree (5 variables, isolating Customer Review -2,3,4 Stars- and Positive Service)
VIMRD5<-VIMRD[,c(3:6, 10)]
set.seed(123)
DTcorrMRD5 <- rpart(
  Volume~ ., data=VIMRD5, cp=0.0001) # Predict "Volume" using all remaining variables
rpart.plot(DTcorrMRD5) # Plot the simple Decision Tree

#####################################-ANOVA & MANOVA-##########################################
# ANOVA of ProductType (categorial) and Volume (numerical) from MReg
res.an <- aov(Volume ~ ProductType  , data = MReg)
summary.aov(res.an) #P number 0.0961

# MANOVA of ProductType (categorical) and Volume, 4StarReviews and PositiveServiceReviews (numerical) from MReg
res.man <- manova(cbind(Volume, x4StarReviews, PositiveServiceReview) ~ ProductType, data = MReg)
summary(res.man, test="Pillai")
summary.manova(res.man)

#####################################-DATA SPLITTING-#############################################
set.seed(123)
# Set a 80/20 split
INTraining <- createDataPartition(MRD$Volume, p = .8, list = FALSE)
MRDtraining <- MRD[INTraining,] # Training set (80%)
MRDtesting <- MRD[-INTraining,] # Testing set (20%)

##############################################-LINEAR REGRESSION MODELS-##############################################
# Linear model of Volume and 4 Star Reviews
LM4SMRD<-lm(Volume~ x4StarReviews, MRD)
summary(LM4SMRD)
plot(LM4SMRD)

# Linear model of Volume and PositiveServiceReview
LMPSMRD<-lm(Volume~ PositiveServiceReview, MRD)
summary(LMPSMRD)
plot(LMPSMRD)

#################################################-SVM MODELS-#########################################
set.seed(123)


#######################################-GBM MODELS-###############################################
set.seed(123)
ctrl<-trainControl(method="repeatedcv", number=10, repeats=3, savePred=T)

# With Positive Service Reviews only
GBMPS<-train(
  Volume ~ PositiveServiceReview,
  data=MRDtraining,
  method="xgbTree", 
  preProc=c("center","scale"),
  tune.lenght=100,
  trControl=ctrl)
GBMPS
predictors(GBMPS)
summary(GBMPS)

# With 4 Star Reviews only
GBM4S<-train(
  Volume ~ x4StarReviews,
  data=MRDtraining,
  method="xgbTree", 
  preProc=c("center","scale"),
  tune.lenght=100,
  trControl=ctrl)
GBM4S
predictors(GBM4S)
summary(GBM4S)

# With 4 Star Reviews and Positive Service Review
GBM4SPS<-train(
  Volume ~ x4StarReviews+PositiveServiceReview,
  data=MRDtraining,
  method="xgbTree", 
  preProc=c("center","scale"),
  tune.lenght=100,
  trControl=ctrl)
GBM4SPS
predictors(GBM4SPS)
summary(GBM4SPS)

# With 4 Star Reviews, Positive Service Reviews and Product Game Console
GBM4SPSGC<-train(
    Volume ~ x4StarReviews+PositiveServiceReview+ProductType.GameConsole,
    data=MRDtraining,
    method="xgbTree", 
    preProc=c("center","scale"),
    tune.lenght=100,
    trControl=ctrl)
GBM4SPSGC
predictors(GBM4SPSGC)
summary(GBM4SPSGC)

##############################-APPLY MODELS TO TESTING DATA-##################################

# Apply Training set (80%) to Test set (20%)



# Apply the SVM model into the Test set

# Apply the GBM model into the Test set



#total brand preferences
all_table <- table(survey_inc$predictions)+table(survey$brand)

# Use the test set to apply into the new product attributes folder

output <- newproductattributes 

output$predictions <- finalPred

################################-SAVE THE FILE-##########################################

#Savig the file
rite.csv(output, file="C2.T3output.csv", row.names = TRUE)

#MAKE SURE THAT THE INFORMATION IS RELEVANT TO THE 4 PRODUCTS - PC, Laptops, Netbooks and Smartphones