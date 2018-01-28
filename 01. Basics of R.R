# Homework 1 - Submitted by Manan Shah on September 2, 2017.
# Portions of this code came from Introduction to Data Science.
# But the comments are all original.

# Name: Manan Shah
# SUID: 205981068
# IST 687
# Lab 1 - Introduction
# Due Date: September 06,2017
# Submission Date: September 02, 2017
# 1

############################################
# Defining Data
############################################

# Defining Vectors
height <- c(59,60,61,58,67,72,70)
weight <- c(150,140,180,220,160,140,130)

# Defining a Variable
a <- 150

############################################
# Step 1: Calculating the Mean
############################################

# Mean of Height
mean(height)

# Mean of Weight
mean(weight)

# Length of Height
length(height)

# Length of Weight
length(weight)

# Sum of Heights
sum(height)

# Average of Height and Weight
avgHeight <- sum(height)/length(height)
avgHeight

avgWeight <- sum(weight)/length(weight)
avgWeight

if (avgHeight == mean(height)) "Both functions give the same answer." else "Both functions do not give the same answer."

if (avgWeight == mean(weight)) "Both functions give the same answer." else "Both functions do not give the same answer."

############################################
# Step 2: Using Min/Max Functions
############################################

# Computing the Maximum Height
maxH <- max(height)
maxH

# Computing the Minimum Weight
minW <- min(weight)
minW

############################################
# Step 3: Vector Math 
############################################

# New Vector for Weight Gain
newWeight <- c(weight+5)
newWeight

# New Weight/Height
newWDivH <- c(newWeight/height)
newWDivH

############################################
# Step 4: Using Conditional IF Statements
############################################

# Test 1 (Question 10)
if (maxH > 60) "Yes, Maximum Height is greater than 60." else "No, Maximum Height is not greater than 60."

# Test 2 (Question 11)
if (minW > a) "Yes, Minimum Weight is greater than variable 'a'." else "No, Minimum Weight is not greater than variable 'a'."
