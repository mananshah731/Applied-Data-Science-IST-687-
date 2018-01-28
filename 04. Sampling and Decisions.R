# Homework 4 - Submitted by Manan Shah on September 27, 2017.
# Portions of this code came from Introduction to Data Science.
# But the comments are all original.

# Name: Manan Shah
# SUID: 205981068
# IST 687 - Lab Section 4
# Lab 4
# Due Date: September 28,2017
# Submission Date: September 27, 2017

######################################################################
# Step 1: Summarizing Function to Understand Distribution of a Vector
######################################################################

#Install 'moments' package for skewness,
install.packages("moments")

#Import 'moments' package.
library(moments)

#1. Create function that takes vector as input.
printVecInfo <- function(a)
{
  #2. Printing information:
  cat(" Mean: ",mean(a),"\n",
      "Median: ",median(a),"\n",
      "Min: ",min(a),"\n",
      "Max: ",max(a),"\n",
      "Standard Deviation: ",sd(a),"\n",
      "Quantile (0.05 - 0.95): ",quantile(a,c(0.05,0.95)),"\n",
      "Skewness: ",skewness(a),"\n")
}

#3. Create a test function with vector (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 50).
vtest <- c(1,2,3,4,5,6,7,8,9,10,50)

#Test the function.
printVecInfo(vtest)

##################################################
# Step 2: Creating Samples in a Jar
##################################################

#4. Create varaible 'jar' with 50 Red and 50 Blue marbles.
redm <- "Red Marble"
bluem <- "Blue Marble"

v.redm <- replicate(50,redm)
v.bluem <- replicate(50,bluem)

jar <- replicate(1,c(v.redm,v.bluem))

#5. Confirm that there are 50 Reds by summing the samples that are Red.
print(paste("Number of Red Marbles is: ",sum(jar == "Red Marble")))

#6. Sample 10 'marbles' to find number of Reds. 
table(sample(jar,10,replace = TRUE))["Red Marble"]

#Find the percentage of Red Marbles.
table(sample(jar,10,replace = TRUE))["Red Marble"]/10

#7. Perform sampling 20 times using the replicate command. Each sample should be of size 10.
samplingVector <- replicate(20,mean(table(sample(jar,10,replace = TRUE))["Red Marble"]))
#This generates a list of 20 numbers that are means of Reds in 10 samples.

#Generating histogram of the samples.
hist(samplingVector)

#Using printVecInfor function on the samples.
printVecInfo(samplingVector)

#8. Perform sampling 100 times using the replicate command. Each sample should be of size 10.
samplingVector1 <- replicate(100,mean(table(sample(jar,10,replace = TRUE))["Red Marble"]))
#This generates a list of 100 numbers that are means of Reds in 10 samples.

#Generating histogram of the samples.
hist(samplingVector1)

#Using printVecInfor function on the samples.
printVecInfo(samplingVector1)

#9. Perform sampling 100 times using the replicate command. Each sample should be of size 10.
samplingVector2 <- replicate(1000,mean(table(sample(jar,10,replace = TRUE))["Red Marble"]))
#This generates a list of 1000 numbers that are means of Reds in 10 samples.

#Generating histogram of the samples.
hist(samplingVector2)

#Using printVecInfor function on the samples.
printVecInfo(samplingVector2)

##################################################
# Step 3: Explore the 'airquality' Dataset
##################################################

#10. Store 'airquality' dataset into temporary variable.
airQuality <- airquality

#11. Clean the dataset, that is, remove NAs.
airQuality <- na.omit(airQuality)

#12. Explore Ozone, Wind and Temperature by using printVecInfo and generating histogram for each.

#Exploring Ozone:
printVecInfo(airQuality$Ozone)
hist(airQuality$Ozone)

#Exploring Wind:
printVecInfo(airQuality$Wind)
hist(airQuality$Wind)

#Exploring Temperature
printVecInfo(airQuality$Temp)
hist(airQuality$Temp)