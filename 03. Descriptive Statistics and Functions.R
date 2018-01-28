# Homework 3 - Submitted by Manan Shah on September 20, 2017.
# Portions of this code came from Introduction to Data Science.
# But the comments are all original.

# Name: Manan Shah
# SUID: 205981068
# IST 687
# Lab 3
# Due Date: September 20,2017
# Submission Date: September 20, 2017

##################################################
# Step 1: Function to Read CSV File
##################################################

readStates <- function(csvFile)
{
  dataset <- read.csv(url(csvFile))
  return(dataset)
}

States <- readStates("http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv")

##################################################
# Step 2: Clean the Data Frame
##################################################

#Note the issue that needs to be cleaned:
#Removing all rows and columns except rows 9 through 59 and columns 1 through 5

cleanStates <- States[(9:59),(1:5)]

#Changing column names:

colnames(cleanStates) <- c("stateName", "Census", "Estimates", "Pop2010", "Pop2011")
str(cleanStates)

#Cleaning the data:
#Removing '.' from first column.

cleanStates$stateName <- gsub("\\.","",cleanStates$stateName)

#Removing ',' from remaining columns and converting all data to numeric form.

cleanStates$Census <- as.numeric(gsub(",", "", cleanStates$Census))
cleanStates$Estimates <- as.numeric(gsub(",", "", cleanStates$Estimates))
cleanStates$Pop2010 <- as.numeric(gsub(",", "", cleanStates$Pop2010))
cleanStates$Pop2011 <- as.numeric(gsub(",", "", cleanStates$Pop2011))

#Display clean dataset.

str(cleanStates)
cleanStates

##################################################
# Step 3: Store and Explore the Data Set
##################################################

#Store clean data set into data frame named dfStates.

dfStates <- cleanStates

#Test data frame by calculating mean for 2011 data

mean(dfStates$Pop2011)

##################################################
# Step 4: Find the State with Highest Population
##################################################

#Find highest population in 2011.

max(dfStates$Pop2011)

#State with maximum population is:

dfStates[which.max(dfStates$Pop2011), 1]

#Sort data in increasing order based on July 2011 data.

sortedDf <- dfStates[order(dfStates$Pop2011),]
sortedDf

##################################################
# Step 5: Explore the Distribution of States
##################################################

#Write a function that takes vector and number as two parameters.

# METHOD 1

Distribution <- function(vector,number)
{
  #Store only eligible elements, that is, numbers within the vector that are less than number given in the function.
  
  ctr <- length(vector[vector < number])
  
  #Calculate percentage.
  
  return(ctr/length(vector))
}

#Test the function with the vector as (1, 2, 3, 4, 5) and number as 2.

Distribution(c(1,2,3,4,5),2)

#Test the function with the vector 'dfStates$Pop2011', and the mean of dfStates$Pop2011'.

Distribution(dfStates$Pop2011,mean(dfStates$Pop2011))

# METHOD 2

distributionFor <- function(newVector,newValue)
{
  #Compare each number of vector with value using a FOR loop, and increase count for eligible elements.
  #Divide count by length of vector to obtain percentage.
  
  count = 0
  len = length(newVector)
  for(i in 1:len)
  {
    if(newVector[i]<newValue){count = count+1}
  }
  percentValue = count/len
  return(percentValue)
}

#Test the function with the vector as (1, 2, 3, 4, 5) and number as 2.

distributionFor(c(1,2,3,4,5),2)

#Test the function with the vector 'dfStates$Pop2011', and the mean of dfStates$Pop2011'.

distributionFor(dfStates$Pop2011,mean(dfStates$Pop2011))

#Which method is better?
#Method 1 and Method 2, both give accurate results for the given parameters.
#Though, Method 1 is better because it is more time efficient.

#End of File




             
             
             



