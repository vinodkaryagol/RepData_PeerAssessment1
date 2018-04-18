---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activities <- read.csv("activity/activity.csv")
activities$date <- as.Date(activities$date)
str(activities)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


##Mean total number of steps taken per day.

```r
steps_per_day<-tapply(activities$steps,activities$date,sum,na.rm=TRUE)
```
Histogram for the total number of steps taken per day,

```r
hist(steps_per_day,10,main="Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
---
###Mean and median for the total number of steps taken per day.

```r
mean(steps_per_day)
```

```
## [1] 9354.23
```

```r
median(steps_per_day)
```

```
## [1] 10395
```


## Average daily activity pattern


```r
daily_activities<-tapply(activities$steps,activities$interval,mean,na.rm=TRUE)
```

Plot for average daily activity pattern.

```r
plot(y=daily_activities,x=names(daily_activities),type="l",xlab="5-Minute Interval",ylab="Frequency",main="Average daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Maximum average of the number of steps for the day

```r
daily_activities[daily_activities==max(daily_activities)]
```

```
##      835 
## 206.1698
```

## Imputing missing values
Count of missing values for the activity-steps.

```r
sum(is.na(activities$steps))
```

```
## [1] 2304
```
Count of missing values for the activity-intervals.

```r
sum(is.na(activities$interval))
```

```
## [1] 0
```
Fill the missing values by the mean of the 5-minute interval values.
Create a new dataset with missing values imputed.

```r
activities_imputed<-activities
activities_imputed[which(is.na(activities$steps)),1]<-daily_activities[as.character(activities_imputed[which(is.na(activities_imputed$steps)),3])]
```
Histogram for the new dataset with missing values imputed.

```r
steps_per_day_imputed<-tapply(activities_imputed$steps,activities_imputed$date,sum)
hist(steps_per_day_imputed)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
Mean and median for the new dataset with missing values imputed.

```r
mean(steps_per_day_imputed)
```

```
## [1] 10766.19
```

```r
median(steps_per_day_imputed)
```

```
## [1] 10766.19
```
##Differences between the activity patterns of weekdays and weekends.

Factorise the data with two levels-weekday and weekend and find out the average.

```r
activities_imputed_test<-activities_imputed
activities_imputed_test$dateType<-ifelse(as.POSIXlt(activities_imputed_test$date)$wday %in% c(0,6), 'weekend', 'weekday')
average_activities_imputed<-aggregate(steps~interval+dateType,data=activities_imputed_test,mean)
```
Time-series plot of the 5-minute interval and the average nuber of steps taken of weekdays and weekends.

```r
library(ggplot2)
ggplot(average_activities_imputed,aes(interval,steps))+geom_line()+facet_grid(dateType~.)+xlab("5-minute interval")+ylab("Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
---
So from the plot we can see that there is a higher peak earlier on weekdays, and more overall activity on weekends.
