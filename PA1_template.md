---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true


---
<style type="text/css">
p.import { color:green;}
</style>


## Loading and preprocessing the data


```r
 load<-read.csv("activity/activity.csv", header=TRUE,stringsAsFactors=FALSE)
data<-load
```


## What is mean total number of steps taken per day?


```r
 aggmean<-aggregate(as.numeric(data$steps),list(data$date),mean,na.action=na.omit)
aggsum<-aggregate(as.numeric(data$steps),list(data$date),sum,na.rm=TRUE)

names(aggsum)<-c("Date","Total Steps")
names(aggmean)<-c("Date","Average Steps")


meanperdaymean<-mean(aggmean$"Average Steps", na.rm=TRUE)

meanperalldays<-mean(aggsum$"Total Steps",na.rm=TRUE)
total<-sum(aggsum$"Total Steps",na.rm=TRUE)
 medsteps<-median(aggsum$"Total Steps", na.rm=TRUE)
```
### <p class=import>Total Steps taken in the 2 months: 5.70608 &times; 10<sup>5</sup> steps</p>

### <p class=import>There are an average of 37.3825996 steps per interval. </p>

### <p class=import>There are an average of 9354.2295082 steps taken per day. </p>

### <p class=import>There is a median of 1.0395 &times; 10<sup>4</sup> steps taken per day. </p>

### Histogram of total steps.

```r
hist(aggsum$"Total Steps",breaks=length(aggsum$"Date"))
```

![plot of chunk total steps histogram](figure/total steps histogram-1.png) 



## What is the average daily activity pattern?


Take the average of each interval.
Plot a time series of averages for 1-288 intervals.

```r
aggin<-aggregate(as.numeric(data$steps),list(data$interval),mean, na.rm=TRUE)
names(aggin)<-c("Interval","Average Steps")
 times<-ts(aggin$"Average Steps",start=1,end=288, frequency=1)
plot(times)
```

![plot of chunk average steps per interval](figure/average steps per interval-1.png) 



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
