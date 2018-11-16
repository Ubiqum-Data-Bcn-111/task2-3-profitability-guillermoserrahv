setwd("/Users/guillermoserrahv/Desktop/Ubiqum/GitHub_Ubiqum/ubiqum1/task2-3-profitability-guillermoserrahv")
getwd()

# Appendix of data names and included values

#MReg - Original, unedited file
#MRegDum - Isolated dummy variables extracted from Product Type
#MRD - Data including the dummies, but minus BestSellersRank
#corrMRD - MRD ran under the correlation matrix with ALL variables
#MRD05 - MRD excluding 5 Star Reviews
#DTcorrMRD - Decision Tree on the correlation matrix data
#CHI2 - A reduced sample including only the Volume, Service Reviews and Customer Reviews
#CHI2T - Chi squared ran over CHI2.

##############################-INSTALLING LIBRARIES & DATA PREPARATION-##########################################
# Install appropiate libraries here 
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


# CHECK THIS PART, MAKE SURE IS CORRECT!! INCLUDING THE ONE BELOW  (REMOVE IT?????)
MRD$ProductNum <- MReg$ProductNum
head(MRD)

#merging the two datasets readyData and existingprod (REMOVE ITTTTTTTTTT?????? HURRRY UP!!!!!!!)
MRDum <- merge(MRD,MReg,by="ProductNum")
head(MRDum)
summary(MRDum)
MRDum


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

# Neural model (ANN) (ASK ABDEL ON THIS ONE)(MAKE SURE TO MENTIONED THE "BENEFITS" OVER SVM FROM BAYES BOOK)
for (i in 1:(ncol(CHI2))) {
  predictions = compute(get(paste("neuralmodel_t", i, sep = "")), MRDNO5[, 1:4])
  qqnorm(CHI2[,i],main=paste("Test",colnames(CHI2)[i])) #plot qqnorm
  qqline(CHI2[,i],col="blue") #add qqnormal line in red
  hist(CHI2[,i],main=paste("Histogram of",colnames(CHI2)[i]), 
       xlab=colnames(CHI2)[i])
}

?neuralmodel_t

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

tree<- rpart(Volume~., data=existingtot, cp=0.001)
rpart.plot(tree)

#############################################-BELOW: FOR UPDATING ABOVE-##################################


# Create a variable for Volume using Depth*Width*Height   (NEEDS TO BE PROPERLY PLACED)
MRD$Product.Volume<-MRD$ProductDepth*MRD$ProductWidth*MRD$ProductHeight
summary(Product.Volume)
Product.Volume

# Choosing some variables for the correlation visual test, but not all (NEEDS TO BE PROPERLY PLACED)(SAMPLE SET)

MRD[,c(1, 2, 4, 5, 6)]


chisq.test(MRD)

######################################-VARIABLE ISOLATED CHI SQUARED#####################################
# Only the Product Type, Volume, Service Reviews and Customer Reviews for CHI Squared
CHI2<-MReg[,c(5:10, 18)]

# Expected values
chisq.test(CHI2)$expected
#Normal Chi squared test
chisq.test(CHI2)
# Dec tree of isolated columns 
set.seed(123)
CHI2T <- rpart(
  Volume~ ., data=CHI2) #predict volume using all variables
rpart.plot(CHI2T) #plot the decision tree


?plot

#CHI SQUARED TESTING (DONE!)


#####################################-MANOVA#######################################################

# Compare volume against a metric from the service and one from the shipping (4 star and positive reviews)

# 2x2 Factorial MANOVA with 3 Dependent Variables. 
Y <- cbind(y1,y2,y3)
fit <- manova(Y ~ A*B)
summary(fit, test="Pillai")

sepl <- iris$Sepal.Length
petl <- iris$Petal.Length
# MANOVA test
res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man)


# Look to see which differ
summary.aov(res.man)

#####################################-DATA SPLITTING-#############################################

set.seed(123)
# Set a 80/20 split
INTraining <- createDataPartition(survey_cat$brand, p = .8, list = FALSE) # add volume as a variable
#creates a vector with the rows to use for training
MRDtraining <- survey_cat[inTraining,] #subset training set
MRDtesting <- survey_cat[-inTraining,] #subset testing set

#################################################-ALGORITHMS-############################################

#SVM models

#GBM models

calibrate.plot(y, p, distribution = "bernoulli", replace = TRUE,
               line.par = list(col = "black"), shade.col = "lightyellow",
               shade.density = NULL, rug.par = list(side = 1),
               xlab = "Predicted value", ylab = "Observed average", xlim = NULL,
               ylim = NULL, knots = NULL, df = 6, ...)


##########################-Bayesian Linear Regression-############################

#Ask Abdel for help on this, should be in the Bayes Statistical methods book





##############################-APPLY MODELS TO TESTING DATA-##################################

#COPY FROM PREVIOUS TASK



output <- newproductattributes 

output$predictions <- finalPred


################################-SAVE THE FILE-##########################################


rite.csv(output, file="C2.T3output.csv", row.names = TRUE)


#MAKE SURE THAT THE INFORMATION IS RELEVANT TO THE 4 PRODUCTS THAT BLACKWELL WANTS

