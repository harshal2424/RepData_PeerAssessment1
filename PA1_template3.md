---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: true
editor_options: 
  chunk_output_type: console
---


## Loading and preprocessing the data

``` r
  knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
fD <- read.csv("activity.csv")
fD$date <- as.Date(fD$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

``` r
sPD <- aggregate(steps ~ date, fD, FUN = sum)
g <- ggplot (sPD, aes (x = steps))
g + geom_histogram(fill = "yellow", binwidth = 1000) +
  labs(title = " Histogram of Steps Taken Each Day ", x = "Steps", y = "Frequency")
```

![](PA1_template3_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

``` r
sM <- mean(sPD$steps, na.rm=TRUE)
sM
```

```
## [1] 10766.19
```

``` r
sMd <- median(sPD$steps, na.rm=TRUE)
sMd
```

```
## [1] 10765
```
## What is the average daily activity pattern?

``` r
sPI <- aggregate(steps ~ interval, fD, mean)
h <- ggplot (sPI, aes(x=interval, y=steps))
h + geom_line()+ labs(title = " Time Series Plot of Average Steps per Interval", x = "Interval", y = "Average Steps across All Days")
```

![](PA1_template3_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
maxInterval <- sPI[which.max(sPI$steps), ] 
maxInterval
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

``` r
noMissingValue <- nrow(fD[is.na(fD$steps),])
noMissingValue
```

```
## [1] 2304
```

``` r
fD1 <- read.csv("activity.csv", header=TRUE,sep=",")
fD1$day <- weekdays(as.Date(fD1$date))
stepsAvg1 <- aggregate(steps ~ interval + day, fD1, mean)
nadata <- fD1 [is.na(fD1$steps),]
newdata1 <- merge(nadata, stepsAvg1, by=c("interval", "day"))
cD <- fD1 [!is.na(fD1$steps),]
newdata2 <- newdata1[,c(5,4,1,2)]
colnames(newdata2) <- c("steps", "date", "interval", "day")
mergeData <- rbind (cD, newdata2)
sPF <- aggregate(steps ~ date, mergeData, FUN = sum)
g1 <- ggplot (sPF, aes (x = steps))
g1 + geom_histogram(fill = "green", binwidth = 1000) +
  labs(title = " Histogram of Steps Taken Each Day ", x = "Steps", y = "Frequency")
```

![](PA1_template3_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
sMF <- mean(sPF$steps, na.rm=TRUE)
sMF
```

```
## [1] 10821.21
```

``` r
sMdF <- median(sPF$steps, na.rm=TRUE)
sMdF
```

```
## [1] 11015
```


## Are there differences in activity patterns between weekdays and weekends?

``` r
mergeData$DayType <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

stepsPerIntervalDT <- aggregate(steps ~ interval+DayType, mergeData, FUN = mean)
j <- ggplot (stepsPerIntervalDT, aes(x=interval, y=steps))
j + geom_line()+ labs(title = " Time Series Plot of Average Steps per Interval: weekdays vs. weekends", x = "Interval", y = "Average Number of Steps") + facet_grid(DayType ~ .)
```

![](PA1_template3_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


