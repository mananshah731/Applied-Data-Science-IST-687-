# Homework 2 - Submitted by Manan Shah on September 13, 2017.
# Portions of this code came from Introduction to Data Science.
# But the comments are all original.

# Name: Manan Shah
# SUID: 205981068
# IST 687
# Lab 2
# Due Date: September 13,2017
# Submission Date: September 13, 2017

###########################################################
# Making a Copy of the mtcars Dataset
###########################################################

mtCars <- mtcars

###########################################################
# Step 1 - Car with the Best Horse Power (HP)
###########################################################

# What is better, higher or lower HP?
# Higher HP is better.

# Which car has the best HP?
bestHP <- mtCars[which.max(mtCars$hp),]
print(bestHP)

###########################################################
# Step 2 - Exploring Miles per Gallon (MPG)
###########################################################

# What is the highest MPG?
highestMPG <- mtCars[which.max(mtCars$mpg),1]
print(highestMPG)

# Which car has the highest MPG
highestMPGCar <- mtCars[which.max(mtCars$mpg),]
print(highestMPGCar)

#Creating a sorted data fram based on MPG
sorted = mtCars[order(mtCars$mpg),]
print(sorted)

###########################################################
# Step 3 - Best Combination of MPG and HP
###########################################################

# Logic: 
# Find the ratio between MPG and HP for each car in the data set.
# Print record for the car with the highest value of the ratio.

mpgVsHp = mtCars$mpg/mtCars$hp
mtCars = cbind(mtCars,mpgVsHp)
mpgVsHpCar <- mtCars[which.max(mtCars$mpgVsHp),]
print(mpgVsHpCar)

###########################################################
# Step 4 - Best Combination of Equal Weights of MPG and HP
###########################################################

# Logic: 
# Find standard-deviated values for HP and MPG.
# Take the mean of standard-deviated values for each car.
# Best car is the one with the highest mean.

stdMpg = (mtCars$mpg-min(mtCars$mpg))/(max(mtCars$mpg)-min(mtCars$mpg))
mtCars = cbind(mtCars,stdMpg)
stdHp = (mtCars$hp-min(mtCars$hp))/(max(mtCars$hp)-min(mtCars$hp))
mtCars = cbind(mtCars,stdHp)

avg = (stdMpg+stdHp)/2
mtCars = cbind(mtCars,avg)

bestcar = which.max(avg)
bestCar=mtCars[bestcar,]
print(bestCar)