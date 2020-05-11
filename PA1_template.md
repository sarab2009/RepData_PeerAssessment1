---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
Show any code that is needed to

1. Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis  


```r
# Unziping the file
zipfile <- "activity.zip"
datafile <- "activity.csv"
if(!file.exists(datafile)){
    unzip(zipfile, files = NULL, exdir=".")
}
activityDf <- read.csv(datafile, header = T, na.strings = "NA")
```
  
  
## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.  

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day  


```r
suppressMessages(library(dplyr))
library(ggplot2)
# Group by data by date
groupByDateDf <- activityDf %>% group_by(date) %>% summarise(TotalStepsPerDay = sum(steps, na.rm = T))
ggplot(groupByDateDf, aes(x=TotalStepsPerDay)) + geom_histogram(bins = 30)+
    xlab("Total steps per day") + ylab("Frequency")
```

![](PA1_template_files/figure-html/hist_TotalStepsPerDay-1.png)<!-- -->
  
3. Calculate and report the mean and median of the total number of steps taken per day  
 

```r
summary(groupByDateDf)
```

```
##          date    TotalStepsPerDay
##  2012-10-01: 1   Min.   :    0   
##  2012-10-02: 1   1st Qu.: 6778   
##  2012-10-03: 1   Median :10395   
##  2012-10-04: 1   Mean   : 9354   
##  2012-10-05: 1   3rd Qu.:12811   
##  2012-10-06: 1   Max.   :21194   
##  (Other)   :55
```
  
  
## What is the average daily activity pattern?

1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  


```r
# Group by data by date
groupByDateAndIntervalDf <- activityDf %>% group_by(interval) %>% summarise(AverageDailyActivity = mean(steps, na.rm = T))
ggplot(groupByDateAndIntervalDf, aes(x=interval, y=AverageDailyActivity)) + geom_line() +
    xlab("Interval") + ylab("Average of steps")
```

![](PA1_template_files/figure-html/average_daily_activity-1.png)<!-- -->
  
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


```r
groupByDateAndIntervalDf[groupByDateAndIntervalDf$AverageDailyActivity == max(groupByDateAndIntervalDf$AverageDailyActivity),]
```

```
## # A tibble: 1 x 2
##   interval AverageDailyActivity
##      <int>                <dbl>
## 1      835                 206.
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)  


```r
nrow(activityDf[is.na(activityDf$steps),])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

**I used the mean for that 5-minute interval as my strategy.**

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  


```r
get_mean <- function(x){
    groupByDateAndIntervalDf[groupByDateAndIntervalDf$interval == x,2]
}
activityWithoutNADf <- activityDf
for (i in 1:nrow(activityWithoutNADf)) {
    interval_mean <- groupByDateAndIntervalDf[groupByDateAndIntervalDf$interval == activityWithoutNADf[i,3],2]
    activityWithoutNADf[i,1] <- ifelse(is.na(activityWithoutNADf[i,1]), interval_mean, activityWithoutNADf[i,1])
}

groupByDateDf <- activityWithoutNADf %>% group_by(date) %>% summarise(TotalStepsPerDay = sum(steps, na.rm = T))
ggplot(groupByDateDf, aes(x=TotalStepsPerDay)) + geom_histogram(bins = 30)+
    xlab("Total steps per day") + ylab("Frequency")
```

![](PA1_template_files/figure-html/filling_missing_values-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.  

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activityWithoutNADf$week <- factor(ifelse(weekdays(as.Date(activityWithoutNADf$date)) == "Saturday" | weekdays(as.Date(activityWithoutNADf$date)) == "Sunday","weekend","weekday"))
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  



```r
groupByIntervalWeekdDf <- activityWithoutNADf %>% group_by(interval,week) %>% summarise(TotalStepsPerDay = mean(steps, na.rm = T))

g <-ggplot(groupByIntervalWeekdDf, aes(x=interval, y=TotalStepsPerDay)) + geom_line()
g + facet_grid(week ~ .)+ xlab("Interval") + ylab("Total steps per day")
```

![](PA1_template_files/figure-html/weekday_plot-1.png)<!-- -->
