#' ---
#' title: "Advanced Stats Assignment 1"
#' output:
#'   html_document:
#'     df_print: paged
#' date: "2023-01-21"
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(dplyr)
library(tidyverse)
library(classInt)
library(ISLR)

#' 
#' # Data Exploration Exercise
#' 
#' ## 1. Source and file format
#' 
#' Question(a)
#' 
#' Download the data source from the Machine Learning Repository at http://archive.ics.uci.edu/ml/datasets/Credit+Approval
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
### Download the data to local, no R code needed ###

#' 
#' Question(b)
#' 
#' Convert the crx.data file to a CSV file.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Convert crx.data to csv
data <- read_csv("crx.data")
write.csv(data, file = "data.csv")

#' 
#' Question(c)
#' 
#' Define a variable called source path. Assign to it the full path of the source file, which the data are to be read from (as saved in your local environment).
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Set source path
source_path <- "~/Desktop/UC Davis/Winter/Advanced Stats/data.csv"

#' 
#' ## 2. Load data into R, indexing and printing
#' 
#' Question(a)
#' 
#' Write a command that import the CSV file. Save into a dataframe called data.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Import csv file
data <- read.csv("data.csv", header=TRUE, na.strings=c("?"))
data <- data[,-1]

#' 
#' Question(b)
#' 
#' Write a command that shows only the first 10 observations.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Show first 10 observations
head(data, 10)

#' 
#' Question(c)
#' 
#' Write a command that shows only the target attribute (the true class).
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Rename the columns
colnames(data) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16")

# Show only the target attribute
which(data$V16 == "+")


#' 
#' ## 3. Data Exploration
#' 
#' Question(a)
#' 
#' Write a command that show features data type.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Show data type
str(data)
sapply(data, class)

#' 
#' Question(b)
#' 
#' Create a summary statistics; mean, standard deviation, variance, min, max, median, range, and quantile – for each feature in the data.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Create summary statistics
summary(data)

#' 
#' Question(c)
#' 
#' Write a command that shows features unique (distinct) values.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Show unique values for categorical features
unique(data$V1)
unique(data$V4)
unique(data$V5)
unique(data$V6)
unique(data$V9)
unique(data$V10)
unique(data$V12)
unique(data$V13)
unique(data$V16)


#' 
#' Question(d)
#' 
#' Compute the “Skewness” measure of the target variable. Based on this value, can we apply a balanced classification technique?
#' 
#' Based on the value, the V16 data is imbalanced but right skewed (skewness \> 0), therefore a balanced classification technique can not be applied here.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Transform the data
data$V16 <- ifelse(data$V16 == "+", 1, 0)

# Function to compute skewness
skew <- function(x){
  m3 <- sum((x-mean(x))^3)/length(x)
  s3 <- sqrt(var(x))^3
  m3/s3
}

# See how many 1 are there in V16
length(which(data$V16 == "1"))

# Compute skewness
skew(data$V16)

#' 
#' Question(e)
#' 
#' Compute the “Skewness” measure of the variable V1. What does it tell us about V1 variable?
#' 
#' Based on the positive value of skewness, the V1 data is right skewed.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Transform the data
data$V1 <- ifelse(data$V1 == "a", 1, 0)

# See how many 1 are there in V1
length(which(data$V1 == "1"))

# Omit NA and compute skewness
skew(na.omit(data$V1))

#' 
#' Question(f)
#' 
#' Compute the “Kurtosis” measure of the variable V8. Is it from a Gaussian distribution?
#' 
#' No, V8 is not from a Gaussian distribution, since its kurtosis is not 0.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Function to compute kurtosis
kurtosis <- function(x){
  m4 <- sum((x-mean(x))^4)/length(x)
  s4 <- var(x)^2
  m4/s4 - 3
}

# Kurtosis of V8
kurtosis(data$V8)

#' 
#' Question(g)
#' 
#' Assume V10 variable indicates if the person has a first degree. Compute the frequency table, marginal frequency and row- wise proportions of V10 and the target variable.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Frequency table
crosstab <- table(data$V10, data$V16)
crosstab

# Marginal Frequency
margin.table(crosstab, 1)
margin.table(crosstab, 2)

# Proportion
prop.table(crosstab, 1)

# Accuracy
(208) / (208 + 86)

#' 
#' ## 4. Missing Values
#' 
#' Question(a)
#' 
#' Write a command that finds all missing values
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Find missing values
which(is.na(data) == TRUE)

#' 
#' Question(b)
#' 
#' Categorical variables – replace all missing values for a variable with the most frequent value
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Replace NA with mode
mode <- function(x){
  distinct_values <- unique(x)
  distinct_tabulate <- tabulate(match(x, distinct_values))
  distinct_values[which.max(distinct_tabulate)]
}

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mode(data[,i])
}

#' 
#' ## 5. Data Normalization and Transformation
#' 
#' Question(a)
#' 
#' Replace the target attribute to a binary attribute (logical data type).
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Replace target attribute to a binary attribute
data$V16 <- as.logical(data$V16)

#' 
#' Question(b)
#' 
#' Discretization - discretize variable V2 using the equal frequencies or equal width binning algorithm.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Equal width binning
classIntervals(data$V2, 5, style = 'equal')

#' 
#' Question(c)
#' 
#' Scaling - after examining the range and distribution of each feature (explain how you did that), apply on each numerical feature a relevant scaling. Use at least 2 different scaling methods and explain why each method was chosen.
#' 
#' According to the summary statistics of numeric data, the distribution of different variables varies dramatically, we definitely need to scale them to eliminate units of measurements. Two scaling methods chosen here are standardization and normalization. Standardization is the process of transforming data so that the new data will have a mean of 0 and standard deviation of 1, whereas normalization transforms the data to a range of 0 to 1. Based on the histograms, it's pretty obvious that the data does not come from a normal distribution, therefore normalization scaling method can be chosen. Also, since standardization has no bounding range, making it more flexible at handling outliers, it is chosen as the second scaling method.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Subset numeric columns
numeric <- c("V2", "V3", "V8", "V11", "V14", "V15")
numeric_data <- data[numeric]
numeric_data <- na.omit(numeric_data)

# Examine the range and distribution of each feature
summary(numeric_data)

# Plot histogram to see if data come from a normal distribution
hist(numeric_data$V2)
hist(numeric_data$V3)
hist(numeric_data$V8)
hist(numeric_data$V11)
hist(numeric_data$V14)
hist(numeric_data$V15)

# Plot boxplot to see outliers
boxplot(numeric_data)

# First Method: Standardization
numeric_data <- as.data.frame(scale(numeric_data))

# Second Method: Normalization
numeric_data <- as.data.frame(sapply(numeric_data, function(x) (x-min(x))/(max(x)-min(x))))

#' 
#' ## 6. Outliers and Visualizations
#' 
#' Question(a)
#' 
#' Create a boxplot of the target variable by V3.
#' 
#' i. What can be inferred about V3 by this graphical presentation? 
#' 
#' ii. Find which samples are outliers (the output should be row numbers)
#' 
#' The value of V3 tends to be higher when V16=TRUE, suggesting that there may be an association between V3 and V16.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(V3~V16,data=data, main="V16 vs V3",
   xlab="V16", ylab="V3")

outliers <- boxplot(V3~V16,data=data, main="V16 vs V3",
   xlab="V16", ylab="V3", plot = FALSE)$out
which(data$V3 %in% outliers)

#' 
#' Question(b)
#' 
#' Select 2 numerical features to plot against each other and add a regression line. Explain the plot.
#' 
#' Based on the shape of scatter plot, V2 and V3 have slight positive correlation. However, we can observe the values of V2 concentrate between 20 to 30, suggesting some transformation (such as log transformation) should be performed to choose the suitable functional form of regression model.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Linear model
model <- lm(V3 ~ V2, data = data)
summary(model)

# Plot between V2 and V3
plot(data$V2, data$V3)
abline(model , col = "blue")

#' 
#' Question(c)
#' 
#' Plot the density of V2.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot the density plot
plot(density(data$V2, na.rm=TRUE))

#' 
#' ## 7. Correlation Analysis
#' 
#' Question(a)
#' 
#' Correlation test (Pearson) - calculate the “degree of association” between V3 and V15.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the correlation between V3 and V15
cor(data$V3, data$V15)

#' 
#' Question(b)
#' 
#' Plot the correlation matrix of all variables. Which variables are redundant? Explain.
#' 
#' An attribute (column or feature of data set) is called redundant if it can be derived from any other attribute or set of attributes. From the correlation matrix, we can see V8 has a relatively high correlation (although it's still pretty mild relationship) in comparison to other, making it a redundant variable.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Subset numeric columns
numeric <- c("V2", "V3", "V8", "V11", "V14", "V15")
numeric_data <- data[numeric]

# Drop NA to aviod 0 in correlation matrix
numeric_data <- na.omit(numeric_data)

# Convert data type
numeric_data$V2 <- as.numeric(numeric_data$V2)

# Correlation matrix for all variables
cor(numeric_data, method = "pearson", use = "pairwise.complete.obs")

#' 
#' Question(c)
#' 
#' What can be done with these redundant features to improve our algorithms?
#' 
#' We should remove the redundant features from our algorithm since they not only lead to higher computational complexity but also reduce the accuracy and efficiency of classification methods.
#' 
#' ## Part 2. Simple Linear Regression
#' 
#' Question 1
#' 
#' Model equation is: mpg = 39.93 - 0.158\*Horsepower
#' 
#' The model has an R-square value of 60.59% ie the variation in horsepower is able to explain the variation in mpg by 60.59%. We observe that both intercept and coefficient are statistically significant for any value of significance level. And for a unit increase in horse power, there is on average, 0.1578 unit decrease in mpg. This means that there is a negative relationship between both variables.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
df <- data.frame(Auto)
head(df)

model1 <- lm(mpg ~ horsepower, data=df)
summary(model1)

# We can confirm this by checking correlation 
cor(df$mpg, df$horsepower)
# We observe a moderate negative correlation between both variables

#' 
#' Question 2
#' 
#' The scatter plot has a bending pattern, hence the relationship between the two variables may not be linear.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
{plot(df$horsepower, df$mpg) 
  abline(lm(df$mpg ~ df$horsepower)) }

#' 
#' Question 3
#' 
#' From the Normal QQ plot, we see that the residuals are not normally distributed which is a violation of the assumptions for linear regression. The Residuals vs fit plot is not symmetric, we need to change the functional form. This is also confirmed by the plot between HP and mpg in the prev question, a reciprocal model maybe required.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
df$residuals <- resid(model1)

# Plot residuals vs X
plot(df$horsepower, df$residuals,type =)

# Diagnostic plot for the least squares residuals
{par(mfrow=c(2,2))
plot(model1)}

#' 
#' Question 4
#' 
#' Assuming that HP of 98% refers to the horsepower=98 and predicting confidence interval and prediction interval for the same
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
#Confidence interval
predict(model1, data.frame(horsepower = 98), interval = "confidence", level = 0.95)
#Prediction interval
predict(model1, data.frame(horsepower = 98), interval = "prediction", level = 0.95)

#Predicted mpg value for HP of 98 is 24.467

