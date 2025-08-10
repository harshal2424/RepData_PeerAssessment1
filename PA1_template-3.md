---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: true
editor_options: 
  chunk_output_type: console
---


## Loading and preprocessing the data
```{r}



  knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
fD <- read.csv("activity.csv")
fD$date <- as.Date(fD$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
```{r}



sPD <- aggregate(steps ~ date, fD, FUN = sum)
g <- ggplot (sPD, aes (x = steps))
g + geom_histogram(fill = "yellow", binwidth = 1000) +
  labs(title = " Histogram of Steps Taken Each Day ", x = "Steps", y = "Frequency")
sM <- mean(sPD$steps, na.rm=TRUE)
sM
sMd <- median(sPD$steps, na.rm=TRUE)
sMd
```
## What is the average daily activity pattern?
```{r}


sPI <- aggregate(steps ~ interval, fD, mean)
h <- ggplot (sPI, aes(x=interval, y=steps))
h + geom_line()+ labs(title = " Time Series Plot of Average Steps per Interval", x = "Interval", y = "Average Steps across All Days")
maxInterval <- sPI[which.max(sPI$steps), ] 
maxInterval

```

## Imputing missing values
```{r}


noMissingValue <- nrow(fD[is.na(fD$steps),])
noMissingValue
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
sMF <- mean(sPF$steps, na.rm=TRUE)
sMF
sMdF <- median(sPF$steps, na.rm=TRUE)
sMdF

```


## Are there differences in activity patterns between weekdays and weekends?
```{r}



mergeData$DayType <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

stepsPerIntervalDT <- aggregate(steps ~ interval+DayType, mergeData, FUN = mean)
j <- ggplot (stepsPerIntervalDT, aes(x=interval, y=steps))
j + geom_line()+ labs(title = " Time Series Plot of Average Steps per Interval: weekdays vs. weekends", x = "Interval", y = "Average Number of Steps") + facet_grid(DayType ~ .)
```


