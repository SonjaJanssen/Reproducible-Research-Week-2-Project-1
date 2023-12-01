
##  Reproducible Research - Week 2- Project 1 
#   Lectures: By Professors PhD Jeffrey Leek, PhD Roger D. Peng, PhD Brian Caffo
#   University: John Hopkins University
#   Codes: Sonja Janssen-Sahebzad
#   Date:  17 September 2023

##  Script: All Codes are tested, assignment completed: codes all ok!
##  This script was created with:
#               - RStudio     Version  0.97.124 for windows
#               - R           Version  4.2.3    for windows
#               - dplyr       Version  1.1.3    Grammar of data manipulation
#               - data.table  Version  1.14.8   Extension of dataframe
#               - ggplot2     Version  3.4.3    Create elegant data visual graphics
#               - lubridate   Version  1.9.2    Make dealing with Dates a little easier 
#               - lattice     Version  0.21-8   Trellis graphics for R 
#               - rstudioapi  Version  0.15.0   safely access RStudio API
#               - readr       Version  2.1.4    Read rectangular text data
#               - gridExtra   Version  2.3      Miscellaneous Functions for Grid Graphics
#               - data was downloaded at date: 'Sun September 17  20:00:10 2023'

#******************************************************************************* 
## Command rm(list=ls()) removes all objects from the current workspace (R memory)
rm(list=ls()) 
## Check if there are any objects 
ls()

#_____________________________Start Step 1_____________________________________
# Loading and preprocessing the data
# Show any code that is needed to
# 1. Load the data (i.e. read.csv())
# 2. Process/transform the data (if necessary) into a format suitable for your analysis


# Load packages
vector.packages <- c("readr", 
                     "ggplot2", 
                     "dplyr",
                     "gridExtra",
                     "rstudioapi", 
                     "data.table", 
                     "lubridate", 
                     "lattice",
                     "ggraph")  # Packages codenames tested by adding "ggraph" = ok! 
                                # chr [1:11]

# load libraries
Funtion.packages <- function(vector.packages){
    for (i in vector.packages){  #Installs packages if not yet installed
        if(!require(i, character.only = T)) install.packages(i)
        suppressPackageStartupMessages(library(i, character.only = T))
        print(i)
    }
}      
Funtion.packages(vector.packages) 

# Load the dataset
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
data <- data.table::fread(input = "data/activity.csv")
# data 17568 observations  of 3 variables

# what's in the database
glimpse(data)                           
data
as.data.frame(sort(names(data)))   #  date  interval  steps
lapply(data, summary)

pairs(data[, 1:3])
png("scatterplot_matrix.png")


#______________________________Start Step 2_____________________________________
# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# 1. Make a histogram of the total number of steps taken each day
# 2. Calculate and report the mean and median total number of steps taken per day


# libraries used. 
library(dplyr)
library(ggplot2)

# Calculate the total number of steps taken each day (ignoring missing values)
total_steps_per_day <- data %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps, na.rm = TRUE))
# total_steps_per_day 61 obs. of 2 variables

# Reset plot area layout
par(mfrow = c(1, 1))


# Create a histogram of the total number of steps taken each day
hist(total_steps_per_day$total_steps, 
     main = "Histogram of Total Steps Taken Each Day",
     col =  blues9,
     breaks = 20,
     xlab = "Total Steps", 
     ylab = "Frequency"
)  

# Store Rplot 1 histogram total steps per day
png("Rplot 1 Histogram of Total Steps Taken Each Day.png")

# Calculate and report the mean total number of steps taken per day
mean_steps_per_day <- mean(total_steps_per_day$total_steps)
cat("Mean Total Number of Steps Taken per Day (Ignoring Missing Values):", mean_steps_per_day, "\n")
# Mean Total Number of Steps Taken per Day (Ignoring Missing Values): 9354.23 

# Calculate and report the median total number of steps taken per day
median_steps_per_day <- median(total_steps_per_day$total_steps)
cat("Median Total Number of Steps Taken per Day (Ignoring Missing Values):", median_steps_per_day, "\n")
# Median Total Number of Steps Taken per Day (Ignoring Missing Values): 10395

 
# Mean Total Number of Steps Taken per Day (Ignoring Missing Values): 9354.23
# Median Total Number of Steps Taken per Day (Ignoring Missing Values): 10395 
# Codes tested: all OK!

#______________________________Start Step 3_____________________________________
# What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#    and the average number of steps taken, averaged across all days (y-axis)

# 2. Which 5-minute interval, on average across all the days in the dataset, 
#    contains the maximum number of steps?
#    Plot 2

# libraries used
library(dplyr)
library(ggplot2)

# Calculate the average number of steps per 5-minute interval across all days
avg_steps_per_interval <- data %>%
    group_by(interval) %>%
    summarise(avg_steps = mean(steps, na.rm = TRUE))

# Create a time series plot (line plot) of average steps per 5-minute interval
ggplot(avg_steps_per_interval, aes(x = interval, y = avg_steps)) +
    geom_line(color = "green") +
    labs(title = "Time series plot of Average Daily Activity Pattern",
         x = "5-minute interval",
         y = "Average Number of Steps") 

# + theme_minimal()
# Store Rplot 2 Average Steps Per Interval
png("Rplot 2 Average Steps Per Interval.png")

# Find the 5-minute interval with the maximum average steps
max_avg_interval <- avg_steps_per_interval %>%
    filter(avg_steps == max(avg_steps)) %>%
    select(interval)
# 1 obs of 1 variable  
  
cat("The 5-minute interval with the maximum average steps is:", max_avg_interval$interval, "\n")
# The 5-minute interval with the maximum average steps is: 835 
# Codes tested: All ok!



#_______________________________Start Step 4____________________________________
# Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as NA). 
# The presence of missing days may introduce bias into some calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
#    The strategy does not need to be sophisticated. For example, 
#    you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

# 4. Make a histogram of the total number of steps taken each day and 
#    Calculate and report the mean and median total number of steps taken per day. 
#    Do these values differ from the estimates from the first part of the assignment? 
#    What is the impact of imputing missing data on the estimates of the total daily number of steps?

# libraries used    
library(dplyr)
library(ggplot2)

# 1. Calculate and report the total number of missing values in the dataset
total_missing_values <- sum(is.na(data$steps))
cat("Total Number of Missing Values in the Dataset:", total_missing_values, "\n")
# Total Number of Missing Values in the Dataset: 2304 

# 2. Devise a strategy for filling in all of the missing values
# For simplicity, we'll impute missing steps with the mean steps for that 5-minute interval.
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

# Impute missing values
data_imputed <- data %>%
    group_by(interval) %>%
    mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))  # 17568 obs. of 3 variables

# Save the imputed dataset to a new CSV file
write.csv(data_imputed, "activity_imputed.csv", row.names = FALSE)

# Check the first few rows of the imputed dataset
head(data_imputed)

## A tibble: 6 × 3
# Groups:               interval [6]
# steps date                interval
#       <dbl>    <IDate>       <int>
#   1   1.72    2012-10-01        0
#   2   0.340   2012-10-01        5
#   3   0.132   2012-10-01       10
#   4   0.151   2012-10-01       15
#   5   0.0755  2012-10-01       20
#   6   2.09    2012-10-01       25


# 4. Make a histogram of the total number of steps taken each day 
daily_totals_imputed <- data_imputed %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))        # 61 obs. of 2 variables 

# Reset plot area layout
par(mfrow = c(1, 1))

# R Colors: blues9, rainbow(n), heat.colors(n), terrain.colors(n), topo.colors(n), and cm.colors(n).
hist(daily_totals_imputed$total_steps, 
     main = "Histogram of Total Steps Taken Each Day (After Imputation)",
     col = cm.colors(18),
     breaks = 20,      # Adjust the number of bins if needed
     xlab = "Total Steps", ylab = "Frequency") 

png("Rplot 3 Histogram of Total Steps Taken Each Day After Imputation.png")


# Calculate and report the mean and median total number of steps taken per day
mean_steps_imputed <- mean(daily_totals_imputed$total_steps)
median_steps_imputed <- median(daily_totals_imputed$total_steps)

cat("Mean total number of steps taken per day (After Imputation):", mean_steps_imputed, "\n")
cat("Median total number of steps taken per day (After Imputation):", median_steps_imputed, "\n")
# Mean total number of steps taken per day (After Imputation): 10766.19 
# Median total number of steps taken per day (After Imputation): 10766.19 

# Mean Total Number of Steps Taken per Day (Ignoring Missing Values): 9354.23
# Median Total Number of Steps Taken per Day (Ignoring Missing Values): 10395 

#____________________________questions and answers______________________________
# Do these values differ from the estimates from the first part of the assignment? 
# Yes, the values of mean and median total daily steps may differ after imputing missing data compared to the estimates from the first part. 

# Mean total number of steps taken per day (After Imputation): 10766.19 
# Median total number of steps taken per day (After Imputation): 10766.19 
# Mean Total Number of Steps Taken per Day (Ignoring Missing Values): 9354.23
# Median Total Number of Steps Taken per Day (Ignoring Missing Values): 10395 


#_____________________Comparing Data before and after imputation ____________________
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
data <- data.table::fread(input = "data/activity.csv")

# Calculate the total steps taken each day before imputation
daily_totals_before <- data %>%
    group_by(date) %>%
    summarise(total_steps_before = sum(steps, na.rm = TRUE))

# Impute missing values (you can use your preferred imputation method)
data_imputed <- data %>%
    group_by(interval) %>%
    mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

# Calculate the total steps taken each day after imputation
daily_totals_after <- data_imputed %>%
    group_by(date) %>%
    summarise(total_steps_after = sum(steps))

# Create a layout for two histograms below each other
layout(matrix(c(1, 2), nrow = 2))

# Create the first histogram (before imputation)
hist(daily_totals_before$total_steps_before, 
     main = "Histogram of Total Steps Taken Each Day (Before Imputation)",
     col = blues9,
     breaks = 20, 
     xlab = "Total Steps", ylab = "Frequency")

# Create the second histogram (after imputation)
hist(daily_totals_after$total_steps_after, 
     main = "Histogram of Total Steps Taken Each Day (After Imputation)",
     col = cm.colors(18),
     breaks = 20, 
     xlab = "Total Steps", ylab = "Frequency")

# What is the impact of imputing missing data on the estimates of the total daily number of steps?

# "The imputation of missing data has led to a notable change in the estimates of the total daily number of steps. After imputation, both the mean and median total daily steps increased to approximately 10,766.19, whereas, in the first part of the assignment, the mean and median were 9,354.23 and 10,395, respectively. This indicates that imputing missing data has a substantial impact on the estimates, resulting in higher values for the total daily steps."

# "Imputing missing data significantly increased the estimated total daily number of steps compared to the analysis with non-imputed data. This was done to address the presence of missing values and provide more complete and representative insights into daily step patterns. Imputation helps mitigate potential biases that missing data can introduce, leading to more accurate and comprehensive analyses of activity patterns."

# In conclusion: Imputing missing data in the total daily number of steps can:
# Improve data completeness.
# Alter mean and median values.
# Affect data variability.
# Potentially introduce bias.
# Impact hypothesis testing.
# Require careful interpretation.
# Imputation should consider data characteristics and potential biases.

# Codes tested: ALL OK!

# ___________________________Start Step 5_______________________________________
# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels 
# -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

# 2. Make a panel plot containing a time series plot 
#   (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
#   averaged across all weekday days or weekend days (y-axis). 

# libraries used
library(gridExtra)
library(ggplot2)

# Create a new factor variable to indicate weekdays and weekends
data_imputed <- data_imputed %>%
    mutate(day_type = ifelse(weekdays(date) 
                             %in% c("Monday", 
                                    "Tuesday", 
                                    "Wednesday", 
                                    "Thursday", 
                                    "Friday"), 
                             "weekday", 
                             "weekend"))

# Aggregate data for weekdays and weekends
averages <- data_imputed %>%
    group_by(interval, day_type) %>%
    summarise(avg_steps = mean(steps, na.rm = TRUE))
head(averages)

## A tibble: 6 × 3
# Groups:   interval [3]
# interval day_type avg_steps
# <int> <chr>        <dbl>
#    1        0 weekday     2.25  
#    2        0 weekend     0.215 
#    3        5 weekday     0.445 
#    4        5 weekend     0.0425
#    5       10 weekday     0.173 
#    6       10 weekend     0.0165


# Create a panel plot with different colors for comparison
ggplot(averages, aes(x = interval, y = avg_steps, color = day_type)) +
    geom_line() +
    facet_wrap(~day_type) +
    xlab("5-minute interval") +
    ylab("Average Number of Steps") +
    labs(title = "Average Number of Steps Taken per 5-Minute Interval (Weekdays vs. Weekends)") +
    scale_color_manual(values = c("weekday" = "blue", "weekend" = "red"))



# Reset plot area layout
par(mfrow = c(1, 1))
#_____________________________END OF ASSIGNMENT_________________________________
