## Loading and preprocessing the data  

unzip("activity.zip")  
data <- read.csv("activity.csv")
library(dplyr)  
data$date <- as.Date(data$date)
#Sys.setlocale("LC_TIME","English") If necessary

## What is mean total number of steps taken per day?
totalStepsDay <- tapply(data$steps, data$date, sum)  
hist(totalStepsDay,
      breaks = length(totalStepsDay),
      axes = FALSE,
      xlab = "Day",
      ylab = "Total Steps", 
      main = "Histogram of the total number of steps")

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
