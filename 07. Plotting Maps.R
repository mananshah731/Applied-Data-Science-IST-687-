# Homework 7 - Submitted by Manan Shah on October 18, 2017.
# Portions of this code came from Introduction to Data Science.
# But the comments are all original.

# Name: Manan Shah
# SUID: 205981068
# IST 687 - Lab Section 4
# Due Date: October 19,2017
# Submission Date: October 18, 2017

#install.packages('gdata')
library("gdata")
#install.packages("zipcode")
library("zipcode")
#install.packages("ggplot2")
library("ggplot2")
#install.packages("ggmap")
library("ggmap")
#install.packages("xlsx")
#library("xlsx")

#######################################################
# Step 1: Load the Data
#######################################################

#Read the data.
incomeDf <- read.xls("C:/Users/Manan Shah/Desktop/MedianZIP.xlsx",sheet=1,header=TRUE,perl="C:/Strawberry/perl/bin/perl.exe")

#Updating column names.
colnames(incomeDf) <- c("zip","median","mean","population")

#Loading the zipcode package.
data("zipcode")

#Removing Hawaii and Alaska.
newZip <- subset(zipcode,zipcode$state != "AK")
finalZip <- subset(newZip,newZip$state != "HI")

#Merge the zip code information from the two data frames (merge into one dataframe).

#Combining the data frames incomeDf and finalZip using the common attribute zip between them.
mergedDf <- merge(x=incomeDf,y=finalZip,by="zip")

#Sorting the state abbreviations in mergedDf to put into the final dataframe
stateAbb <- sort(unique(mergedDf$state))

#######################################################
# Step 2: Show the Income and Population Per State
#######################################################

#Finding out the average median income and sum of population in mergedDf.
avgmedianDf <- tapply(as.numeric(mergedDf$median),mergedDf$state,mean)
sumPop <- tapply(as.numeric(mergedDf$population),mergedDf$state,sum)

#Creating a simpler data frame, with just the average median income and the population for each state.
custDf <- data.frame(avgmedianDf,sumPop,stateAbb)

#Adding names of states in the final data frame.
custDf$stateNames <- state.name[match(custDf$stateAbb,state.abb)]

#Creating a simple map.
us_map <- map_data("state")
map.simple	<- ggplot()		
map.simple	<- map.simple +	geom_map(data=us_map,aes(x=us_map$long,y=us_map$lat,map_id=region),map=us_map,fill="white",	color="black")	
map.simple

#Map showing color representing the average median income of each state.
custDf$stateNames <- tolower(custDf$stateNames)
map.income	<- map.simple + geom_map(data=custDf,map=us_map,aes(fill=avgmedianDf,map_id=stateNames),color="black",na.rm=TRUE)
map.income

#Map showing color representing the population of each state.
map.pop <- map.simple + geom_map(data=custDf,map=us_map,aes(fill=sumPop,map_id=stateNames),color="black",na.rm=TRUE)
map.pop

#######################################################
# Step 3: Show the Income Per Zip Code
#######################################################

#Map showing dots of different colors representing median income.
map.zip <- map.simple + geom_point(data=mergedDf,aes(x=mergedDf$latitude,y=mergedDf$longitude,colour=median),na.rm=TRUE)
map.zip <- map.zip + ggtitle("Income Per Zip Code")
map.zip

#######################################################
# Step 4: Show Zip Code Density
#######################################################

#Map showing density distribution of zip codes (using 'stat_density2d' function).
map.density <- map.simple + stat_density2d(aes(x=mergedDf$longitude, y=mergedDf$latitude), data=mergedDf, geom="polygon") +
  scale_fill_gradient(low="black",high="green")+
  scale_alpha(range=c(0.00,0.25))+
  ggtitle("Density for all Zip codes in USA")+
  theme (plot.title=element_text(lineheight=3.5,face="bold"))
map.density

#######################################################
# Step 5: Zoom into the Region Around NYC
#######################################################

zoomGeo <- geocode("New York, ny")
zoomAmount <- 6

#Finding coordinates for the region.
centerx <- zoomGeo$lon
centery <- zoomGeo$lat
ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount, centerx+zoomAmount)

#Zooming in using coord_cartesian.

#Repeat Step 3 for NY Region.
map.zip + coord_cartesian(x = xlimit, y = ylimit)

#Repeat Step 4 for NY region.
map.density + coord_cartesian(x = xlimit, y = ylimit)