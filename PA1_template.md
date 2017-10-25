---
title: "Reproducible Research: Peer Assignment 1"
output: html_document
---

# Introduction

"This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day."

"The data for this assignment can be downloaded from the course web site:  
Dataset: Activity monitoring data [52K]

The variables included in this dataset are:  
- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
- date: The date on which the measurement was taken in YYYY-MM-DD format  
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset."

# Assignments

### 0. Load the respective libraries.

```r
library(dplyr)
library(knitr)
```

### 1. Code for reading the dataset and/or processing the data.

```r
activity <- read.csv("activity.csv")
activity$date <- as.character(activity$date)
activity$interval <- as.numeric(activity$interval)
act <- filter(activity, activity$steps != "NA") # dataset with missing values excluded
```

### 2. Histogram of the total number of steps taken each day.

```r
total_number_per_day <- aggregate(act[, 1], list(act$date), sum)
hist(total_number_per_day$x, xlab="Total number of steps per day", main="Histogram of total number of steps per day")
```

![plot of chunk histogram](figure/histogram-1.png)

### 3. Mean and median number of steps taken each day.

```r
mean_per_day <- aggregate(act$steps, list(act$date), mean)
names(mean_per_day) <- c("date", "mean_steps")
median_per_day <- aggregate(act$steps, list(act$date), median)
names(median_per_day) <- c("date", "median_steps")
mm_per_day <- merge(mean_per_day, median_per_day, by="date")
kable(mm_per_day)
```



|date       | mean_steps| median_steps|
|:----------|----------:|------------:|
|2012-10-02 |  0.4375000|            0|
|2012-10-03 | 39.4166667|            0|
|2012-10-04 | 42.0694444|            0|
|2012-10-05 | 46.1597222|            0|
|2012-10-06 | 53.5416667|            0|
|2012-10-07 | 38.2465278|            0|
|2012-10-09 | 44.4826389|            0|
|2012-10-10 | 34.3750000|            0|
|2012-10-11 | 35.7777778|            0|
|2012-10-12 | 60.3541667|            0|
|2012-10-13 | 43.1458333|            0|
|2012-10-14 | 52.4236111|            0|
|2012-10-15 | 35.2048611|            0|
|2012-10-16 | 52.3750000|            0|
|2012-10-17 | 46.7083333|            0|
|2012-10-18 | 34.9166667|            0|
|2012-10-19 | 41.0729167|            0|
|2012-10-20 | 36.0937500|            0|
|2012-10-21 | 30.6284722|            0|
|2012-10-22 | 46.7361111|            0|
|2012-10-23 | 30.9652778|            0|
|2012-10-24 | 29.0104167|            0|
|2012-10-25 |  8.6527778|            0|
|2012-10-26 | 23.5347222|            0|
|2012-10-27 | 35.1354167|            0|
|2012-10-28 | 39.7847222|            0|
|2012-10-29 | 17.4236111|            0|
|2012-10-30 | 34.0937500|            0|
|2012-10-31 | 53.5208333|            0|
|2012-11-02 | 36.8055556|            0|
|2012-11-03 | 36.7048611|            0|
|2012-11-05 | 36.2465278|            0|
|2012-11-06 | 28.9375000|            0|
|2012-11-07 | 44.7326389|            0|
|2012-11-08 | 11.1770833|            0|
|2012-11-11 | 43.7777778|            0|
|2012-11-12 | 37.3784722|            0|
|2012-11-13 | 25.4722222|            0|
|2012-11-15 |  0.1423611|            0|
|2012-11-16 | 18.8923611|            0|
|2012-11-17 | 49.7881944|            0|
|2012-11-18 | 52.4652778|            0|
|2012-11-19 | 30.6979167|            0|
|2012-11-20 | 15.5277778|            0|
|2012-11-21 | 44.3993056|            0|
|2012-11-22 | 70.9270833|            0|
|2012-11-23 | 73.5902778|            0|
|2012-11-24 | 50.2708333|            0|
|2012-11-25 | 41.0902778|            0|
|2012-11-26 | 38.7569444|            0|
|2012-11-27 | 47.3819444|            0|
|2012-11-28 | 35.3576389|            0|
|2012-11-29 | 24.4687500|            0|

### 4. Time series plot of the average number of steps taken.

```r
mean_per_interval <- aggregate(act$steps, list(act$interval), mean)
names(mean_per_interval) <- c("interval", "mean_steps")
mean_per_interval$interval <- as.numeric(mean_per_interval$interval)
plot(mean_per_interval$interval, mean_per_interval$mean_steps, type="l", xlab="Interval", ylab="Average number of steps", main="Daily activity pattern")
```

![plot of chunk time_series](figure/time_series-1.png)

### 5. The 5-minute interval that, on average, contains the maximum number of steps.

```r
max_interval <- filter(mean_per_interval, mean_per_interval$mean_steps == max(mean_per_interval$mean_steps))
kable(max_interval)
```



| interval| mean_steps|
|--------:|----------:|
|      835|   206.1698|

### 6. Code to describe and show a strategy for imputing missing data.

```r
# number of missing data
miss <- mutate(filter(activity, is.na(activity$steps) == TRUE), h=1)
missing_data <- sum(miss$h)
print(missing_data)
```

```
## [1] 2304
```

Missing data will be imputed by the mean of the respective interval (averaged over all days).

```r
# impute missing data by the mean of the respective interval (averaged over all days)
activity <- merge(activity, mean_per_interval, by="interval")
activity$steps_imputed <- ifelse(is.na(activity$steps)==1,activity$mean_steps,activity$steps)
# mean and median for the imputed values
mean_per_day_imputed <- aggregate(activity$steps_imputed, list(activity$date), mean)
names(mean_per_day_imputed) <- c("date", "mean_steps_imputed")
median_per_day_imputed <- aggregate(activity$steps_imputed, list(activity$date), median)
names(median_per_day_imputed) <- c("date", "median_steps_imputed")
mm_per_day_imputed <- merge(mean_per_day_imputed, median_per_day_imputed, by="date")
kable(mm_per_day_imputed)
```



|date       | mean_steps_imputed| median_steps_imputed|
|:----------|------------------:|--------------------:|
|2012-10-01 |         37.3825996|             34.11321|
|2012-10-02 |          0.4375000|              0.00000|
|2012-10-03 |         39.4166667|              0.00000|
|2012-10-04 |         42.0694444|              0.00000|
|2012-10-05 |         46.1597222|              0.00000|
|2012-10-06 |         53.5416667|              0.00000|
|2012-10-07 |         38.2465278|              0.00000|
|2012-10-08 |         37.3825996|             34.11321|
|2012-10-09 |         44.4826389|              0.00000|
|2012-10-10 |         34.3750000|              0.00000|
|2012-10-11 |         35.7777778|              0.00000|
|2012-10-12 |         60.3541667|              0.00000|
|2012-10-13 |         43.1458333|              0.00000|
|2012-10-14 |         52.4236111|              0.00000|
|2012-10-15 |         35.2048611|              0.00000|
|2012-10-16 |         52.3750000|              0.00000|
|2012-10-17 |         46.7083333|              0.00000|
|2012-10-18 |         34.9166667|              0.00000|
|2012-10-19 |         41.0729167|              0.00000|
|2012-10-20 |         36.0937500|              0.00000|
|2012-10-21 |         30.6284722|              0.00000|
|2012-10-22 |         46.7361111|              0.00000|
|2012-10-23 |         30.9652778|              0.00000|
|2012-10-24 |         29.0104167|              0.00000|
|2012-10-25 |          8.6527778|              0.00000|
|2012-10-26 |         23.5347222|              0.00000|
|2012-10-27 |         35.1354167|              0.00000|
|2012-10-28 |         39.7847222|              0.00000|
|2012-10-29 |         17.4236111|              0.00000|
|2012-10-30 |         34.0937500|              0.00000|
|2012-10-31 |         53.5208333|              0.00000|
|2012-11-01 |         37.3825996|             34.11321|
|2012-11-02 |         36.8055556|              0.00000|
|2012-11-03 |         36.7048611|              0.00000|
|2012-11-04 |         37.3825996|             34.11321|
|2012-11-05 |         36.2465278|              0.00000|
|2012-11-06 |         28.9375000|              0.00000|
|2012-11-07 |         44.7326389|              0.00000|
|2012-11-08 |         11.1770833|              0.00000|
|2012-11-09 |         37.3825996|             34.11321|
|2012-11-10 |         37.3825996|             34.11321|
|2012-11-11 |         43.7777778|              0.00000|
|2012-11-12 |         37.3784722|              0.00000|
|2012-11-13 |         25.4722222|              0.00000|
|2012-11-14 |         37.3825996|             34.11321|
|2012-11-15 |          0.1423611|              0.00000|
|2012-11-16 |         18.8923611|              0.00000|
|2012-11-17 |         49.7881944|              0.00000|
|2012-11-18 |         52.4652778|              0.00000|
|2012-11-19 |         30.6979167|              0.00000|
|2012-11-20 |         15.5277778|              0.00000|
|2012-11-21 |         44.3993056|              0.00000|
|2012-11-22 |         70.9270833|              0.00000|
|2012-11-23 |         73.5902778|              0.00000|
|2012-11-24 |         50.2708333|              0.00000|
|2012-11-25 |         41.0902778|              0.00000|
|2012-11-26 |         38.7569444|              0.00000|
|2012-11-27 |         47.3819444|              0.00000|
|2012-11-28 |         35.3576389|              0.00000|
|2012-11-29 |         24.4687500|              0.00000|
|2012-11-30 |         37.3825996|             34.11321|

### 7. Histogram of the total number of steps taken each day after missing values are imputed.

```r
total_number_per_day_imputed <- aggregate(activity$steps_imputed, list(activity$date), sum)
hist(total_number_per_day_imputed$x, xlab="Total number of steps per day", main="Histogram of total number of steps per day", sub="Missing values are imputed.")
```

![plot of chunk hist_imputed](figure/hist_imputed-1.png)

### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.

```r
activity <- mutate(activity, weekday = as.POSIXlt(as.Date(activity$date))$wday)
activity$weekend <- ifelse(activity$weekday==0 | activity$weekday==6,1,0)
weekend <- filter(activity, activity$weekend==1)
weekday <- filter(activity, activity$weekend==0)
mean_per_interval_weekend <- aggregate(weekend$steps_imputed, list(weekend$interval), mean)
names(mean_per_interval_weekend) <- c("interval","mean_steps_weekend")
mean_per_interval_weekday <- aggregate(weekday$steps_imputed, list(weekday$interval), mean)
names(mean_per_interval_weekday) <- c("interval","mean_steps_weekday")
mean_per_interval_week <- merge(mean_per_interval_weekend, mean_per_interval_weekday, by="interval")
par(mfrow=c(2,1))
plot(mean_per_interval_week$interval, mean_per_interval_week$mean_steps_weekend, type="l", ylim=c(0,250), xlab="Interval", ylab="Average number of steps", main="Daily activity pattern on weekends")
plot(mean_per_interval_week$interval, mean_per_interval_week$mean_steps_weekday, type="l", ylim=c(0,250), xlab="Interval", ylab="Average number of steps", main="Daily activity pattern on weekdays")
```

![plot of chunk week](figure/week-1.png)
