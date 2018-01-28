# Homework 10 - Submitted by Manan Shah on November 15, 2017.
# Portions of this code came from Introduction to Data Science.
# But the comments are all original.

# Name: Manan Shah
# SUID: 205981068
# IST 687 - Lab Section 4
# Due Date: November 16,2017
# Submission Date: November 15, 2017

#Installing the required packages.
install.packages("e1071")
library("e1071")
install.packages("kernlab")
library("kernlab")
install.packages("gridExtra")
library(gridExtra)
install.packages("ggplot2")
library("ggplot2")

##################################################
# Step 1: Load the Data
##################################################

#Copying the dataset.
air<-airquality

#Removing NAs.
for(i in 1:ncol(air)){
  air[is.na(air[,i]),i] <- mean(air[,i], na.rm = TRUE)
}

air

##################################################
# Step 2: Creating Tran and Test Data Sets
##################################################

#Creating random index and cut point at 2/3rd of the data set.
randIndex <- sample(1:dim(air)[1])
cutPoint2_3 <-floor(2*dim(air)[1]/3)

#Creating training and testing data set.
trainData <- air[randIndex[1:cutPoint2_3],]
testData <- air[randIndex[(cutPoint2_3+1):dim(air)[1]],]
head(trainData)

#################################################################
# Step 3: Build a Model Using KSVM and Visualize the Results
#################################################################

#Using KSVM to build a model.
model.ksvm <- ksvm(Ozone ~ . , data=trainData)
PredictY<- predict(model.ksvm, testData)

#Computing RMSE.
error <- testData$Ozone - PredictY
sqrt(mean(error^2))

#Plotting results using a scatter plot.
g1 <- ggplot(testData, aes(x=Temp, y=Wind)) + geom_point(aes(size = error, color=error)) 
g1

#Using SVM to build a model.
model.svm <- svm(Ozone ~ Solar.R+Wind , data=trainData)
PredictY2<- predict(model.svm, testData)

#Computing RMSE.
error1 <- testData$Ozone - PredictY2
sqrt(mean(error1^2))

#Plotting results using a scatter plot.
g2 <- ggplot(testData, aes(x=Temp, y=Wind)) + geom_point(aes(size = error1, color=error1)) 
g2

#Using lm to build a model.
model.lm <- lm(formula = Ozone~Wind+Temp, data = trainData)
PredictY3 <- predict(model.lm, testData, type="response")
error2 <- testData$Ozone - PredictY3

#Computing RMSE.
sqrt(mean(error2^2))

#Plotting results using a scatter plot.
g3 = ggplot(data = testData, aes(x=Temp,y=Wind)) + geom_point(aes(size=error2, color = error2)) 
g3

#Showing all the charts using grid.arrange function.
grid.arrange(g1,g2,g3)

##################################################
# Step 4: Creating a 'goodOzone' Variable
##################################################

#Calculating mean Ozone.
meanOzone <- mean(air$Ozone)
meanOzone 

#Adding a column for the above variable in the air quality data set.
for ( i in 1:length(air$Ozone))
{
  if( air[i,1] < meanOzone )
  {
    air[i,7] = '0'
  }
  else
  {
    air[i,7] = '1'
  }
}

##################################################
# Step 5: Predicting Good and Bad Days
##################################################

#Performing step 1 to step 3 again.

#Step 1
newair=air

#Step 2
randIndex1 <- sample(1:dim(newair)[1])
CutPoint2_3 <-floor(2*dim(newair)[1]/3)

trainData2 <- newair[randIndex1[1:cutPoint2_3],]
testData2 <- newair[randIndex[(CutPoint2_3+1):dim(newair)[1]],]
head(trainData2)

#Using KSVM Model
model.ksvm2 <- ksvm(V7 ~ .,trainData2)
predictGO <- predict(model.ksvm2, testData2)
summary(predictGO)
str(predictGO)

errorGO <- as.numeric(testData2$V7)-as.numeric(predictGO)
sqrt(mean(errorGO^2))
predictGO

g4 <- ggplot(data = testData2, aes(x=Temp,y=Wind)) + geom_point(aes(size=errorGO,shape = predictGO, color = testData2$V7))
g4

#Calculating the accuracy of prediction of KSVM.
results <- table(predictGO, testData2$V7)
print(results)
percentCorrect <- (results[1,1]+results[2,2])/(results[1,1]+results[1,2]+results[2,1]+results[2,2])*100
percentCorrect

#Using SVM Model.
model.svm2 <- svm(V7 ~ ., trainData2, type = "C-classification")
predictGO1 <- predict(model.svm2, testData2)
summary(predictGO1)

errorGO1 <- as.numeric(testData2$V7) - as.numeric(predictGO1)
sqrt(mean(errorGO1^2))

g5 = ggplot(data = testData2, aes(x=Temp,y=Wind)) + geom_point(aes(size=errorGO1,shape = predictGO1, color = testData2$V7))
g5

#Calculating the accuracy of prediction of SVM.
results2 <- table(predictGO1, testData2$V7)
print(results2)
percentCorrect2 <- (results2[1,1]+results2[2,2])/(results2[1,1]+results2[1,2]+results2[2,1]+results2[2,2])*100
percentCorrect2

#Using Naive Bayes Model
model.nb <- naiveBayes(as.factor(V7) ~ ., trainData2)
predictNB <- predict(model.nb, testData2)
str(predictNB)
summary(predictNB)

errorNB <- as.numeric(testData2$V7) - as.numeric(predictNB)
sqrt(mean(errorNB^2))

#Calculating the accuracy of prediction of Naive Bayes.
results3 <- table(predictNB, testData2$V7)
print(results3)
percentCorrect3 <- (results3[1,1]+results3[2,2])/(results3[1,1]+results3[1,2]+results3[2,1]+results3[2,2])*100
percentCorrect3

#Showing all three results in one window.
g6 = ggplot(data = testData2, aes(x=Temp,y=Wind)) + geom_point(aes(size=errorNB,shape = predictNB, col = testData2$V7))
grid.arrange(g4,g5,g6)

##################################################
# Step 6: Best Model
##################################################

#Naive Bayes seems like the best model, according to me.
#Naive Bayes gave 1.009 as RMSE, KSVM and SVM gave more RMSE.
#Therefore, the error rate of Naive Bayes is comparitively less.
#This makes Naive Bayes better than other algorithms in this example.