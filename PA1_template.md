Reproducible Research: Peer Assessment 1
========================================================
# Libraries

```r
require(reshape2)
library(plyr)
library(ggplot2)
library(knitr)
```

# Loading and preprossessing the data

```r
df<-read.csv("activity.csv", header = TRUE, sep = ",")
```

# What is mean total number of steps taken per day?

```r
#1.Historgram of total number of steps taken each day

df_sum<-aggregate(df$steps,list(df$date), sum, na.rm=TRUE)

hist(df_sum$x,xlab="Total Number of steps",main = "Total Number of Steps Taken Each Day", col="Blue")
```

![plot of chunk Hist_Totsteps_eachDay](figure/Hist_Totsteps_eachDay-1.png) 

```r
#2. Calculation and report of Mean and Median of the total number of steps taken per day
df_mean<-mean(df_sum$x)
print(df_mean)
```

```
## [1] 9354.23
```

```r
df_median<-median(df_sum$x)
print(df_median)
```

```
## [1] 10395
```

```r
## What is the average daily activity pattern ?

#1. A time series plot (i.e. type = "l") of the 5-minute interval(x-axis) and the average number of steps taken, averaged across all days (y-axis)

interval_mean<-aggregate(df$steps,list(df$interval), sum, na.rm=TRUE)
ggplot(interval_mean, aes(Group.1, x)) + geom_line() + xlab("5-minute Interval") + ylab("Average Number Of steps")
```

![plot of chunk average_daily_activity_pattern](figure/average_daily_activity_pattern-1.png) 

```r
#2. 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps

df_max= max(interval_mean$x)
print(df_max)
```

```
## [1] 10927
```

```r
# Inputing missing values
## 1. Calculate and report the total number of missing values in the dataset (i.e. the total numbers of rows with NAs)

nrows_NA<-nrow(df)
nrows_NoNA<-nrow(na.omit(df))
Tot_NA<-nrows_NA-nrows_NoNA
## 2. Strategy for filing in all missing values in the dataset

## 3. New dataset that is equal to the original dataset but with missing data field in.

## 4. Histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.


# Do these values differ from the estimates from the first part of the assignment ?

# What is the impact of imputing missing data on the estimates of the total daily number of steps ?

# Are there differences in activty pattern between weekdays and weekends ?

## 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


## 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axix)
```



