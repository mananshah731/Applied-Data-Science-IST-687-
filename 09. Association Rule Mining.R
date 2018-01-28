# Homework 9 - Submitted by Manan Shah on November 8, 2017.
# Portions of this code came from Introduction to Data Science.
# But the comments are all original.

# Name: Manan Shah
# SUID: 205981068
# IST 687 - Lab Section 4
# Due Date: November 9,2017
# Submission Date: November 8, 2017

#Loading the Titanic data set.
load("titanic.raw.rdata")
View(titanic.raw)
summary(titanic.raw)
titanic <- titanic.raw

##################################################
# Step 1: Descriptive Statistics
##################################################

#Compute the percentage of people that survived.
n<-length(titanic$Survived[titanic$Survived=='Yes'])
n
m <- length(titanic$Survived)
m
surpercent <- (n/m)*100
surpercent

#Compute the percentage of people that were children.
NoOfChild <- length(titanic$Age[titanic$Age=='Child'])
NoOfChild
o <- length(titanic$Age)
o
ChildSurvivedPer <- (NoOfChild/o)*100
ChildSurvivedPer

#Compute the percentage of people that were female.
NoOfFemale <- length(titanic$Sex[titanic$Sex=='Female'])
NoOfFemale
FemalePer <- (NoOfFemale/o)*100
FemalePer

#Compute the percentage of people that were in first class.
NoOf1st <- length(titanic$Class[titanic$Class=='1st'])
NoOf1st
stper <- (NoOf1st/o)*100
stper

##################################################
# Step 2: Descriptive Statistics
##################################################

#What percentage of children survived?
NoOfChildSurvived <- length(titanic$Age[(titanic$Age=='Child')& (titanic$Survived=='Yes')])
NoOfChildSurvived                            
ChildrenSurvivedPer <- (NoOfChildSurvived/NoOfChild)*100                            
ChildrenSurvivedPer

#What percentage of female survived?
NoOfFemaleSurvived <- length(titanic$Sex[(titanic$Sex=='Female')& (titanic$Survived=='Yes')])
NoOfFemaleSurvived 
FemaleSurvivedPer <- (NoOfFemaleSurvived/NoOfFemale)*100                            
FemaleSurvivedPer

#What percentage of first class passengers survived?
NoOf1stSurvived <- length(titanic$Class[(titanic$Class=='1st')& (titanic$Survived=='Yes')])
NoOf1stSurvived
stSurvivedPer <- (NoOf1stSurvived/NoOf1st)*100 
stSurvivedPer

#What percentage of third class people survived?
NoOf3rd <- length(titanic$Class[titanic$Class=='3rd'])
NoOf3rd
NoOf3rdSurvived <- length(titanic$Class[(titanic$Class=='3rd')& (titanic$Survived=='Yes')])
NoOf3rdSurvived
rdSurvivedPer <- (NoOf3rdSurvived/NoOf3rd)*100 
rdSurvivedPer

##################################################
# Step 3: Writing a Function
##################################################

#Function that returns new data frame of people that satisfy the specified criteria of sex, age, class and survived as parameters.
selectValue<-function(a,b,c,d)
{
  index<-which(titanic$Sex==a & titanic$Age==b & titanic$Class==c & titanic$Survived==d)
  return(titanic[index,])
}
selectValue("Male","Adult","1st","No")

#Function that calculates percentages, using the previous function, of people who live or die.
percentSelectValue<-function(a,b,c)
{
  firstValue<-nrow(selectValue(a,b,c,"Yes"))
  secondValue<-nrow(selectValue(a,b,c,"No"))
  percentWhoLives<-firstValue*100/(firstValue+secondValue)
  percentWhoDies<-secondValue*100/(firstValue+secondValue)
  return(c("Percentage of people who lived = ", percentWhoLives, "Percentage of people who die = ", percentWhoDies))
}

#Use the above function to compare age and third class male survivor rates.
percentSelectValue("Male","Adult","3rd")
percentSelectValue("Male","Child","3rd")

#Use the above function to compare age and first class female survivor rates.
percentSelectValue("Female","Adult","1st")
percentSelectValue("Female","Child","1st")

##################################################
# Step 4: Using aRules
##################################################

#Installing the required packages.
#install.packages("arules")
library("arules")
#install.packages("arulesViz")
library("arulesViz")

#Calculating some association rules using apriori() command.
summary(titanic)
rules<-apriori(titanic,parameter=list(support=0.01,confidence=0.5),appearance = list(default="lhs", rhs=("Survived=Yes")))
summary(rules)             
inspect(rules)

#Visulaizing the results.
plot(rules)

#Finding the best rules.

#Sorting the list of previously created rules by putting lift > 3.0 to find the best rules.
bRules <- rules[quality(rules)$lift > 3.0]
bRules
inspect(bRules)

#Three most interesting rules, on the basis of value of lift, are:
#    lhs                                 rhs            support    confidence lift     count
#[1] {Class=2nd,Age=Child}            => {Survived=Yes} 0.01090413 1.0000000  3.095640  24  
#[2] {Class=1st,Sex=Female}           => {Survived=Yes} 0.06406179 0.9724138  3.010243 141  
#[3] {Class=1st,Sex=Female,Age=Adult} => {Survived=Yes} 0.06360745 0.9722222  3.009650 140 

#Comparing the above results with descriptive analysis done earlier.
percentSelectValue("Female","Child","2nd")
percentSelectValue("Female","Child","1st")
percentSelectValue("Female","Adult","1st")

#As seen above, the association rules generated above are in line with the analysis done earlier.

#End of code.