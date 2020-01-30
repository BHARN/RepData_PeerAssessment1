---
title: 'Reproducible Research: Peer Assessment 1'
output: 
    html_document:
        keep_md: true
---

## Loading and preprocessing the data

```r
activityData <- read.csv(file="activity.csv", header=TRUE)
```

## What is mean total number of steps taken per day?

```r
#Calculate the total steps taken per day
totalSteps <- aggregate(steps ~ date, activityData, FUN=sum)

#Make a histogram of the total number of steps taken per day
hist(totalSteps$steps, main = "Mean Total Steps per Day", xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#Calculate and report the mean and median of total steps taken per day
meanSteps <- mean(totalSteps$steps, na.rm = TRUE)
meanSteps
```

```
## [1] 10766.19
```

```r
medSteps <- median(totalSteps$steps, na.rm = TRUE)
medSteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
#Compute the means of steps accross all days for each interval
meanData <- aggregate(steps ~ interval, activityData, FUN=mean, na.rm=TRUE)
                       
#Compute the time series plot
plot(meanData$interval, 
     meanData$steps, 
     type="l",
     xlab="5-minute Interval", 
     ylab="Average number of steps", 
     main="Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#Which 5-minute interval across all days contain the maximum number of steps
maxInt <- meanData[which.max(meanData$steps),]
maxInt
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

```r
#Calculate and report the total number of missing values in the dataset
NA_count <- sum(is.na(activityData$steps))
NA_count
```

```
## [1] 2304
```

```r
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
impActivityData <- transform(activityData,
    steps = ifelse(is.na(activityData$steps),
        meanData$steps[match(activityData$interval, meanData$interval)],
        activityData$steps))

#Make a histogram of the total number of steps taken each day and report the mean and median.
impStepsByInt <- aggregate(steps ~ date, impActivityData, FUN=sum)
hist(impStepsByInt$steps, main = "Imputed Number of Steps Per Day", xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
impMeanSteps <- mean(impStepsByInt$steps, na.rm = TRUE)
impMeanSteps
```

```
## [1] 10766.19
```

```r
impMedSteps <- median(impStepsByInt$steps, na.rm = TRUE)
impMedSteps
```

```
## [1] 10766.19
```

```r
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
diffMean = impMeanSteps - meanSteps
diffMean
```

```
## [1] 0
```

```r
diffMed = impMedSteps - medSteps
diffMed
```

```
## [1] 1.188679
```

```r
diffTotal = sum(impStepsByInt$steps) - sum(totalSteps$steps)
diffTotal
```

```
## [1] 86129.51
```

## Are there differences in activity patterns between weekdays and weekends?

```r
#Create a new factor variable in the dataset with two levels - "weekend" and "weekday"
DayType <- function(date) {
    day <- weekdays(date)
    if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
        return ("weekeday")
    else if (day %in% c('Saturday', 'Sunday'))
        return ("weekend")
    else
        stop ("Invalid Date Format.")
}
impActivityData$date <- as.Date(impActivityData$date)
impActivityData$day <- sapply(impActivityData$date, FUN = DayType)

#Make a panel plot containnig a time-series plot of the 5-minute interval and the average number of steps taken across all weekdays or weekends
meanStepsByDay <- aggregate(steps ~ interval + day, impActivityData, FUN=mean)

library(ggplot2)
ggplot(data = meanStepsByDay, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(day ~ .) +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
