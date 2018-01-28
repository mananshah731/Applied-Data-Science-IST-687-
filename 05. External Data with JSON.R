# Homework 5 - Submitted by Manan Shah on October 5, 2017.
# Portions of this code came from Introduction to Data Science.
# But the comments are all original.

# Name: Manan Shah
# SUID: 205981068
# IST 687 - Lab Section 4
# Due Date: October 5,2017
# Submission Date: October 5, 2017

######################################################################
# Step 1: Load the Data
######################################################################

#Install packages.
install.packages("RJSONIO")
library(RJSONIO)
install.packages("RCurl")
library(RCurl)

#Import data from URL.
results <- fromJSON("https://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD", nullValue = NA)
length(results)
accident <- results[[2]]

######################################################################
# Step 2: Clean the Data
######################################################################

numRows <- length(accident)

#Convert data into table format.
dfResults <- data.frame(matrix(unlist(accident), nrow = numRows, byrow = T), stringsAsFactors = FALSE)

#Remove first 8 columns.
dfResults <- dfResults[,-1:-8]

#Change column names.
nameList <- c("CASE_NUMBER",
              "BARRACK",
              "ACC_DATE",
              "ACC_TIME",
              "ACC_TIME_CODE",
              "DAY_OF_WEEK",
              "ROAD",
              "INTERSECT_ROAD",
              "DIST_FROM_INTERSECT",
              "DIST_DIRECTION",
              "CITY_NAME",
              "COUNTY_CODE",
              "COUNTY_NAME",
              "VEHICLE_COUNT",
              "PROP_DEST",
              "INJURY",
              "COLLISION_WITH_1",
              "COLLISION_WITH_2")
names(dfResults) <- nameList
dfResults <- na.omit(dfResults)
dfResults

######################################################################
# Step 3: Understand the Data Using SQL
######################################################################

#Import SQLDF
install.packages('sqldf')
library(sqldf)

#How many accidents happen on Sundays?
sqldf("SELECT COUNT(DAY_OF_WEEK) AS NUMBER_OF_ACCIDENTS_HAPPENED_ON_SUNDAY
      FROM dfResults
      WHERE DAY_OF_WEEK LIKE '%SUNDAY%'
      ")

#How many accidents had injuries?
sqldf("SELECT COUNT(INJURY) AS NUMBER_OF_INJURY
      FROM dfResults
      WHERE INJURY = 'YES'
      ")

#List injuries by day.
sqldf("SELECT 
      DAY_OF_WEEK,
      COUNT(INJURY) AS NUMBER_OF_INJURY
      FROM dfResults
      WHERE INJURY = 'YES'
      GROUP BY DAY_OF_WEEK
      ")

######################################################################
# Step 4: Understand the Data Using Tapply
######################################################################

#How many accidents happen on Sundays?
accidentSunday <- tapply(dfResults$DAY_OF_WEEK, dfResults$DAY_OF_WEEK, length)
accidentSunday[4]

#How many accidents had injuries?
accidentInjuries <- tapply(dfResults$INJURY, dfResults$INJURY, length)
accidentInjuries[2]

#List injuries by day.
tapply(dfResults$INJURY, dfResults$DAY_OF_WEEK,function(x) length(which(x == "YES")))