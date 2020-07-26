---
title: "Reproducible Research: Peer Assessment 1"
date: "26/07/2020"
output: 
  html_document:
    keep_md: true
---
# Loading and preprocessing the data
## 1. Load the data (i.e. read.csv()):

```r
df <- read.csv("activity.csv")
head(df)
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

## 2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
library(tidyverse)
```

```
## ── Attaching packages ────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
## ✓ tibble  3.0.3     ✓ dplyr   1.0.0
## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
```

```
## ── Conflicts ───────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
df$date <- as.Date(df$date)
```

# What is mean total number of steps taken per day? For this part of the assignment, you can ignore the missing values in the dataset. 
##1.Calculate the total number of steps taken per day

```r
totalsteps <- aggregate(steps~date, df, sum, na.rm=TRUE)
```

##2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(totalsteps$steps, xlab="steps", main="Total Number of Steps Each Day")
```

![](PA1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

##3.Calculate and report the mean and median of the total number of steps taken per day

```r
meansteps <- mean(totalsteps$steps)
meansteps
```

```
## [1] 10766.19
```

```r
mediansteps <- median(totalsteps$steps)
mediansteps
```

```
## [1] 10765
```

#What is the average daily activity pattern?
##1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsperinterval <- aggregate(steps~interval, df, mean, na.rm=TRUE)
plot(steps~interval, stepsperinterval, type="l")
```

![](PA1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

##2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxsteps <- stepsperinterval[which.max(stepsperinterval$steps),]$interval      
maxsteps
```

```
## [1] 835
```

#Imputing missing values
##1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
totalmissing <- is.na(df$steps)
sum(totalmissing)
```

```
## [1] 2304
```

##2.Devise a strategy for filling in all of the missing values in the dataset. 
##The strategy is to use the mean of the number of steps per interval to replace all of the missing values.

```r
m <- mean(stepsperinterval$steps)
```

##3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
df_im<-df
df_im[totalmissing,1]<-m
head(df_im)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

##4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalsteps_im <- aggregate(steps~date, df_im, sum, na.rm=TRUE) 
hist(totalsteps_im$steps, xlab="Steps", 
     main="Total Number of Steps Each Day With Imoputed Data")
```

![](PA1_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

### Mean and meaidn total number of steps taken per day are calculated and reported as follow:

```r
meansteps_im <- mean(totalsteps_im$steps)
meansteps_im
```

```
## [1] 10766.19
```

```r
mediansteps_im <- median(totalsteps_im$steps)
mediansteps_im
```

```
## [1] 10766.19
```
###The mean didn’t change after the replacements of NAs, the median changed about 0.1% of the original value.

#Are there differences in activity patterns between weekdays and weekends?
##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
#1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
df_im$date<-as.Date(df_im$date)
df_2<-df_im %>%
        mutate(Daytype= ifelse(weekdays(df_im$date)=="Saturday" | 
                                       weekdays(df_im$date)=="Sunday", 
                               "Weekend", "Weekday"))
head(df_2)
```

```
##     steps       date interval Daytype
## 1 37.3826 2012-10-01        0 Weekday
## 2 37.3826 2012-10-01        5 Weekday
## 3 37.3826 2012-10-01       10 Weekday
## 4 37.3826 2012-10-01       15 Weekday
## 5 37.3826 2012-10-01       20 Weekday
## 6 37.3826 2012-10-01       25 Weekday
```

#2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
activity_by_day <- aggregate(steps~interval + Daytype, df_2, mean)
plot<- ggplot(activity_by_day, 
              aes(x = interval, y = steps, color = Daytype)) +
        geom_line() +
        labs(title = "Averaged Daily Steps Weekday vs. Weekend", 
             x = "Interval", y = "Average number of steps") +
        facet_wrap(~Daytype, nrow=1, ncol = 2)
print(plot)
```

![](PA1_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
