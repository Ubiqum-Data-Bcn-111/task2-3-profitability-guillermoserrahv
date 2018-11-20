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

# Upload the Existing Product Attributes file & New Products file
MReg <- read.csv("existingproductattributes2017.2.csv")
MReg <- as.data.frame(MReg)
NPA<-read.csv("newproductattributes2017.2.csv")
NPA <- as.data.frame(NPA)

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

#######REMOVE ALL WARRANTIES, THEY ARE LITTER FOR THIS TEST#######

# MANOVA of ProductType (categorical) and Volume, 4StarReviews and PositiveServiceReviews (numerical) from MReg
res.man <- manova(cbind(Volume, x4StarReviews, PositiveServiceReview) ~ ProductType, data = MReg)
summary(res.man, test="Pillai")
summary.manova(res.man) #P number: 2.084e-07 (because is so small (less than 0.05) the chances of relationship are high)

#############################-REMOVE OUTLIERS FROM VOLUME-#########################################################
# Finding the outlier values
outlier_values <- boxplot.stats(VIMRD5$Volume)$out
outlier_values
# Visual graphic of the outliers from Volume
par(mfrow = c(1, 1))
boxplot(VIMRD5$Volume, main="Outliers in Sales Volume", boxwex=0.2)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
# Outliers filtered
O5VO<-filter(VIMRD5, Volume!=7036 & Volume!=11204)

#####################################-DATA SPLITTING-#############################################
# Choose only the values relevant to this exercise: 4StarReview, PositiveServiceReview and Volume
O3V<-O5VO[,c(1,4,5)]
# Set a 80/20 split
set.seed(123)
INTraining <- createDataPartition(O3V$Volume, p = .8, list = FALSE)
MRDtraining <- O3V[INTraining,] # Training set (80%)
MRDtesting <- O3V[-INTraining,] # Testing set (20%)

##############################################-LINEAR REGRESSION MODELS-#####################################
# Linear model of Volume and 4 Star Reviews
set.seed(123)
LM4SMRD<-lm(Volume~ x4StarReviews, O3V)
summary(LM4SMRD)
#plot(LM4SMRD)
test_LM4SMRD<-predict(LM4SMRD, newdata=MRDtesting, metric="RMSE")
test_LM4SMRD
postResample(test_LM4SMRD, MRDtesting$Volume)
#       RMSE    Rsquared         MAE 
# 446.5131104   0.3526014 334.4223700 

# Linear model of Volume and PositiveServiceReview
set.seed(123)
LMPSMRD<-lm(Volume~ PositiveServiceReview, O3V)
summary(LMPSMRD)
#plot(LMPSMRD)
test_LMPSMRD<-predict(LMPSMRD, newdata=MRDtesting, metric="RMSE")
test_LMPSMRD
postResample(test_LMPSMRD, MRDtesting$Volume)
#       RMSE    Rsquared         MAE 
#397.6473042   0.4781089 302.9595108 

#################################################-SVM MODELS-#########################################
# Using all variables - number=10, repeats=3 (LINEAR)
set.seed(123)
ctrlSVM10 <- trainControl(method = "repeatedcv", number = 10, repeats = 3, savePred=T)
SVMALL <- train(Volume ~., data=MRDtraining, method="svmLinear",
                       trControl=ctrlSVM10,
                       preProcess=c("center", "scale"),
                       tuneLength=10)
SVMALL
predictors(SVMALL)
summary(SVMALL)
test_SVMALL<-predict(SVMALL, newdata=MRDtesting, metric="RMSE")
test_SVMALL
postResample(test_SVMALL, MRDtesting$Volume)
#       RMSE    Rsquared         MAE 
#302.6583255   0.7252705 133.8805233  

# Using all variables - number=20, repeats=6 (RADIAL)
set.seed(123)
ctrlSVM20 <- trainControl(method = "repeatedcv", number = 20, repeats = 6, savePred=T)
SVMALL2 <- train(Volume ~., data = MRDtraining, method = "svmRadial",
                       trControl=ctrlSVM20,importance = TRUE,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
SVMALL2
predictions(SVMALL2)
summary(SVMALL2)
test_SVMALL2<-predict(SVMALL2, newdata=MRDtesting, metric="RMSE")
test_SVMALL2
postResample(test_SVMALL2, MRDtesting$Volume)
#      RMSE   Rsquared        MAE 
#114.075640   0.966004  78.787039 

#######################################-GBM MODELS-###############################################
set.seed(123)
ctrlGBM<-trainControl(method="repeatedcv", number=10, repeats=3, savePred=T)

# With Positive Service Reviews only
GBMPS<-train(
  Volume ~ PositiveServiceReview,
  data=MRDtraining,
  method="xgbTree", 
  preProc=c("center","scale"),
  tune.lenght=100,
  trControl=ctrlGBM)
GBMPS
predictors(GBMPS)
summary(GBMPS)
test_GBMPS<-predict(GBMPS, newdata=MRDtesting, metric="RMSE")
test_GBMPS
postResample(test_GBMPS, MRDtesting$Volume)
#      RMSE   Rsquared        MAE 
#98.1457942  0.9728496 49.5076163

# With 4 Star Reviews only
GBM4S<-train(
  Volume ~ x4StarReviews,
  data=MRDtraining,
  method="xgbTree", 
  preProc=c("center","scale"),
  tune.lenght=100,
  trControl=ctrlGBM)
GBM4S
predictors(GBM4S)
summary(GBM4S)
test_GBM4S<-predict(GBM4S, newdata=MRDtesting, metric="RMSE")
test_GBM4S
postResample(test_GBM4S, MRDtesting$Volume)
#       RMSE    Rsquared         MAE 
#380.9054297   0.5505641 204.5372620

# With 4 Star Reviews and Positive Service Review
GBM4SPS<-train(
  Volume ~ x4StarReviews+PositiveServiceReview,
  data=MRDtraining,
  method="xgbTree", 
  preProc=c("center","scale"),
  tune.lenght=100,
  trControl=ctrlGBM)
GBM4SPS
predictors(GBM4SPS)
summary(GBM4SPS)
test_GBM4SPS<-predict(GBM4SPS, newdata=MRDtesting, metric="RMSE")
test_GBM4SPS
postResample(test_GBM4SPS, MRDtesting$Volume)
#       RMSE    Rsquared         MAE 
#414.7201538   0.4836321 208.9047484 

##############################-APPLY MODELS TO TESTING DATA-##################################
# Training the final chosen models
#LM1 <- train(Volume ~ ., data=data.frame(O3V), method="lm", trainControl=ctrlGBM, importance=TRUE, preProcess=c("center","scale"), tuneLenght=5)
SVM1 <- train(Volume ~ ., data=data.frame(O3V), method="svmRadial", trainControl=ctrlGBM, importance=TRUE, preProcess=c("center","scale"), tuneLenght=5)  
GBM1 <- train(Volume ~ ., data=data.frame(O3V), method="xgbTree", trainControl=ctrlGBM, importance=TRUE, preProcess=c("center","scale"), tuneLenght=5)
# Make predictions
preSVM <- predict(SVM1,NPA)
preGBM <- predict(GBM1,NPA)
# Attirbutes of the prediction models
preSVM
summary(preSVM)
preGBM
summary(preGBM)
# New prediction list
newpredictionlist <- c()
newpredictionlist <- cbind(data.frame(NPA$ProductType),data.frame(NPA$ProductNum),preSVM,preGBM)
newpredictionlist
summary(newpredictionlist)
# Write over new document
write.csv(newpredictionlist, file="final.csv", row.names = TRUE)

#################################-THE END IS THE BEGGINING IS THE END-#####################################