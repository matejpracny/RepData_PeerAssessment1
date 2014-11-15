# Reproducible Research: Peer Assessment 1
## Setting the enviroment

```r
knitr::opts_chunk$set(echo=TRUE)
setwd("C:/Osobni/Dropbox/Coursera/10_Data Science/05_Reproducible Research/Peer assigment 1")
Sys.setlocale(category = "LC_ALL", locale = "C")
```

```
## [1] "C"
```

## Loading and preprocessing the data

```r
# Unzip data
unzip("activity.zip")

# 1. Load the data (i.e. read.csv())
activity<-read.csv("activity.csv", stringsAsFactors = FALSE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
# 2.Process/transform the data (if necessary) into a format suitable for your analysis
activity$date<-as.Date(activity$date, format = '%Y-%m-%d')
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?


```r
# Aggregate data by date
total_daily_steps <- aggregate(steps~date, activity,sum, na.rm=TRUE)

# 1. Make a histogram of the total number of steps taken each day
histogram<-barplot(total_daily_steps$steps, names.arg =total_daily_steps$date, xlab = "Date",ylab="Total Daily Steps", main="Number of Steps per Day")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

```r
# 2. Calculate and report the mean and median total number of steps taken per day
daily_mean_steps<- mean(total_daily_steps$steps,na.rm=TRUE)
daily_median_steps<-median(total_daily_steps$steps,na.rm=TRUE)
```


```r
daily_mean_steps
```

```
## [1] 10766
```

```r
daily_median_steps
```

```
## [1] 10765
```

The daily mean is 10766 steps and the median is 10765 steps.

## What is the average daily activity pattern?

#### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
average_interval <- aggregate(steps ~ interval, activity, mean)
plot(average_interval, type = "l", xlab="Intervals", ylab="Average Steps per interval", main="Average steps per interval")
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
average_interval$interval[which.max(average_interval$steps)]
```

```
## [1] 835
```


## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Means for the 5-minute intervals are used to fill in for the missing values

#### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity <- merge(activity,average_interval, by = "interval", suffixes = c("", ".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]
```

#### 4.1 Make a histogram of the total number of steps taken each day.


```r
total_steps_per_day <- aggregate(steps ~ date,activity,sum)
barplot(total_steps_per_day$steps, names.arg = total_steps_per_day$date, xlab = "Date", ylab = "Total number of Steps",main="Total Steps per Day")
```

![plot of chunk unnamed-chunk-9](./PA1_template_files/figure-html/unnamed-chunk-9.png) 

#### 4.2 Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#Mean
mean(total_steps_per_day$steps)
```

```
## [1] 10766
```

```r
#Median
median(total_steps_per_day$steps)
```

```
## [1] 10766
```

After replacing, the mean is the same but the median is a little bit different. Now the mean and median are equal to each other.

## Are there differences in activity patterns between weekdays and weekends?

#### 1.Create a new factor variable in the dataset with two levels  weekday and weekend indicating whether a given date is a weekday or weekend day.


```r
activity$dayType <- ifelse(weekdays(activity$date) %in%  c("Saturday", "Sunday"),'Weekend','Weekday')

head(activity)
```

```
##   interval steps       date dayType
## 1        0 1.717 2012-10-01 Weekday
## 2        0 0.000 2012-11-23 Weekday
## 3        0 0.000 2012-10-28 Weekend
## 4        0 0.000 2012-11-06 Weekday
## 5        0 0.000 2012-11-24 Weekend
## 6        0 0.000 2012-11-15 Weekday
```

```r
table(activity$dayType)
```

```
## 
## Weekday Weekend 
##   12960    4608
```

#### 2.Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(ggplot2)
qplot(x=interval, y=steps,data=subset(activity, complete.cases(activity)),geom='smooth', stat='summary', fun.y=mean) + facet_grid(dayType~.) + facet_wrap(~dayType,nrow=2) + theme(strip.background = element_rect(fill="#ffe5cc")) + labs(title=' Average steps per days, analyzing weekdays and weekend patterns')
```

![plot of chunk unnamed-chunk-12](./PA1_template_files/figure-html/unnamed-chunk-12.png) 
