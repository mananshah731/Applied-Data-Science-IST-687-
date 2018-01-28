# Homework 8 - Submitted by Manan Shah on November 1, 2017.
# Portions of this code came from Introduction to Data Science.
# But the comments are all original.

# Name: Manan Shah
# SUID: 205981068
# IST 687 - Lab Section 4
# Due Date: November 2,2017
# Submission Date: November 1, 2017

install.packages("ggplot2")
install.packages("RCurl")
install.packages("gdata")
library("RCurl")
library("gdata")
library("ggplot2")

##################################################
# Step 1: Reading Data from URL
##################################################

url <- "http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls"
dataAn <- read.xls(url, perl="C:/Strawberry/perl/bin/perl.exe")

##################################################
# Step 2: Method of Reading Data
##################################################

#The data has been directly loaded from the given URL.

##################################################
# Step 3: Inspecting Data
##################################################

#Inspecting data using str() command.
str(dataAn)

#Changing column names for easy use.
names(dataAn)[]<-c('Fawns','AdultAntelope','Precipitation','Winter')
str(dataAn)

##################################################
# Step 4: Creating Bivariate Plots
##################################################

#Creating bivariate plot for number of baby fawns versus adult antelope population.
x <- ggplot(dataAn, aes(x=AdultAntelope, y=Fawns, color="red")) + geom_point()
x + labs(x="Adult Antelope Population", y="Number of Baby Fawns")

#Creating bivariate plot for number of baby fawns versus precipitation that year.
y <- ggplot(dataAn, aes(x=Precipitation, y=Fawns, color="red")) + geom_point()
y + labs(x="Precipitation that Year", y="Number of Baby Fawns")

#Creating bivariate plot for number of baby fawns versus severity of winter.
z <- ggplot(dataAn, aes(x=Winter, y=Fawns, color="red")) + geom_point()
z + labs(x="Severity of Winter", y="Number of Baby Fawns")

#Since Number of Fawns is the outcome, it must be represented on the Y-axis.

##################################################
# Step 5: Creating Regression Models
##################################################

#Creating three different models and predicting twice for each.

#Model 1: Predicting number of fawns from severity of winter.
model1<-lm(formula=Fawns~Winter, data=dataAn)
summary(model1)
predict(model1,data.frame(Winter=1),type = "response")
predict(model1,data.frame(Winter=5),type = "response")

#Model 2A: Predicting number of fawns from two variables, severity of winter and annual precipitation.
model2a<-lm(formula=Fawns~Winter+Precipitation, data=dataAn)
summary(model2a)
predict(model2a,data.frame(Winter=1,Precipitation=14.1),type = "response")
predict(model2a,data.frame(Winter=5,Precipitation=10.6),type = "response")

#Model 2B: Predicting number of fawns from two variables, severity of winter and adult antelope population.
model2b<-lm(formula=Fawns~Winter+AdultAntelope, data=dataAn)
summary(model2b)
predict(model2b,data.frame(Winter=1,AdultAntelope=9.7),type = "response")
predict(model2b,data.frame(Winter=5,AdultAntelope=6.8),type = "response")

#Model 3: Predicting number of fawns from three other variables.
model3<-lm(formula=Fawns~Winter+AdultAntelope+Precipitation, data=dataAn)
summary(model3)
predict(model3,data.frame(Winter=1,AdultAntelope=9.7,Precipitation=14.1),type = "response")
predict(model3,data.frame(Winter=5,AdultAntelope=6.8,Precipitation=10.6),type = "response")

#According to me, Model 3 gives the best results.

#After performing the following two types of predictions:
#1. Combination of low severity of winter, high adult antelope population and annual precipitation.
#2. Combination of high severity of winter, low adult antelope population and annual precipitation.

#All three independent variables can predict the dependent variable efficiently.

#Best model with two predictors is the one using adult antelope population and annual precipitation.

#Best model with one predictor is the one using adult antelope population.

#End of Assignment