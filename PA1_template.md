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
echo = TRUE
df<-read.csv("activity.csv", header = TRUE, sep = ",")
```

# What is mean total number of steps taken per day?

```r
#1.Historgram of total number of steps taken each day

df_sum<-aggregate(df$steps,list(df$date), sum, na.rm=TRUE)

hist(df_sum$x,xlab="Total Number of steps",main = "Total Number of Steps Taken Each Day", col="Blue")
```

![plot of chunk my_sum_mean_median](figure/my_sum_mean_median-1.png) 

```r
#2. Calculation and report of Mean and Median of the total number of steps taken per day
df_mean<-mean(df_sum$x)

df_median<-median(df_sum$x)
```

```r
# What is the average daily activity pattern ?
#write("df_sum.csv",df_sum)
```



