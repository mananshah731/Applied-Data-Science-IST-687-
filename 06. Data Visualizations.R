# Homework 6 - Submitted by Manan Shah on October 12, 2017.
# Portions of this code came from Introduction to Data Science.
# But the comments are all original.

# Name: Manan Shah
# SUID: 205981068
# IST 687 - Lab Section 4
# Due Date: October 12,2017
# Submission Date: October 12, 2017

#Installing ggplot2 package.
install.packages("ggplot2")
library("ggplot2")
install.packages("reshape2")
library(reshape2)

#############################################
# Step 1: Load the Data
#############################################

#Copying airquality dataset already available in R into temporary dataset.
aq <- airquality
aq

#############################################
# Step 2: Clean the Data
#############################################

#Replace NAs in Ozone and Solar.R columns.
aq$Ozone[is.na(aq$Ozone)] <- mean(aq$Ozone, na.rm=TRUE) 
aq$Solar.R[is.na(aq$Solar.R)] <- mean(aq$Solar.R, na.rm=TRUE) 

#############################################
# Step 3: Understand the Data Distribution
#############################################

#Creating a histogram for Ozone
temp <- ggplot(aq,aes(x=Ozone))
temp <- temp + geom_histogram(binwidth=10,color="red",fill="green",na.rm=TRUE)
temp

#Creating a histogram for Solar.R
temp <- ggplot(aq,aes(x=Solar.R))
temp <- temp + geom_histogram(binwidth=10,color="red",fill="yellow",na.rm=TRUE)
temp

#Creating a histogram for Wind
temp <- ggplot(aq,aes(x=Wind))
temp <- temp + geom_histogram(binwidth=1,color="red",fill="lightblue",na.rm=TRUE)
temp

#Creating a histogram for Temp
temp <- ggplot(aq,aes(x=Temp))
temp <- temp + geom_histogram(binwidth=1,color="red",fill="pink",na.rm=TRUE)
temp

#Boxplot for Ozone.
ggplot(aq, aes(x = factor(0), y = Ozone)) + geom_boxplot()

#Boxplot for different values of wind.
ggplot(aq, aes(x = factor(0), y = Wind, group = Wind)) + geom_boxplot(color = "blue", size=2)

##################################################
# Step 4: Explore How the Data Changes Over Time
##################################################

#Create appropriate dates.
aq$Date <- as.Date(paste("1973", airquality$Month, airquality$Day, sep = "-"))

#Create line charts for each variable.
ggplot(aq, aes(x = Date, y = Ozone)) + geom_line(color = "blue", size = 2)
ggplot(aq, aes(x = Date, y = Temp)) + geom_line(color = "red", size = 2)
ggplot(aq, aes(x = Date, y = Wind)) + geom_line(color = "green", size = 2)
ggplot(aq, aes(x = Date, y = Solar.R)) + geom_line(color = "yellow", size = 2)

#Create a scale function for drawing four lines.
scale <- function(v){
  min <- min(v)
  max <- max(v)
  v1 <- (v-min)/(max-min)
  return (v1)
}

#Scale first four columns into a vector.
vec <- sapply(aq[,1:4], scale)

#Unlist Date column.
date <- unlist(aq$Date)

#Combine vec and date into a new dataframe.
vecdf <- data.frame(vec, date)

#Melt the data.
datamelt <- melt(vecdf, id=c("date"))
datamelt

#Create one linechart for all 4 parameters.
ggplot(datamelt, aes(x=date, y=value, color=variable, group=variable)) + geom_line(size = 2) + ggtitle('Line Chart for AirQuality')

##################################################
# Step 5: Look at the Data via a Heatmap
##################################################

# Heatmap with each day along the x-axis, and ozone, temp, wind and solar.r along the y-axis, and days as rows along the y-axis.
ggplot(datamelt, aes(x=date, y=variable)) + geom_tile(aes(fill=datamelt$value)) + scale_fill_gradient(low='white',high='red') + ggtitle('Heatmap for AirQuality')

#Heatmap showing relative change equally across all variables.
ggplot(datamelt, aes(x=variable, y=date)) + geom_tile(aes(fill=datamelt$value)) + scale_fill_gradient(low='white',high='red') + ggtitle('Heatmap for AirQuality')

##################################################
# Step 6: Look at the Data via a Scatter Chart
##################################################

#Create a scatter chart with:
#X-axis representing the wind.
#Y-axis representing the temperature.
#Size of each dot representing the ozone and the color representing solar.R
ggplot(vecdf, aes(x=Wind, y=Temp)) + geom_point(aes(size=Ozone, color=Solar.R))

##################################################
# Step 7: Final Analysis
##################################################

#Do you see any pattern(s) after exploring the data?
#Yes, visualizations help identifying patterns in data with the help of colors and graphics.

#What was the most useful visualization?
#In my opinion, the Scatter Chart works the best.