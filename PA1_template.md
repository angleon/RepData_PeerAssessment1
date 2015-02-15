# Reproducible Research: Peer Assessment 1

## Introduction
The goal of this assignment to practice skills needed for reproducible research. Specifically this assignment use R markdown to write a report that answers the questions detailed in the sections below.

## Data
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. The data for this assignment can be downloaded from the course web site: Dataset: Activity monitoring data [52K] The variables included in this dataset are:

-steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
-date: The date on which the measurement was taken in YYYY-MM-DD format  
-interval: Identifier for the 5-minute interval in which measurement was taken The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data
Load input data from a zip file from the current R working directory.


```r
filename <- unzip("activity.zip")
activity <- read.csv(filename, stringsAsFactors = FALSE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity$date <- as.Date(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

## What is mean total number of steps taken per day?
- Create a new dataset ignoring missing data NA  
- Plot a histogram of the total number of steps taken each day  
- Report the mean and median total number of steps taken per day



```r
activity_rm<-activity[which(!is.na(activity$steps)),]

perday<-tapply(activity_rm$steps, activity_rm$date, sum)
```

Plot histogram of the total number of steps taken each day:
  

```r
hist(perday,10, main = "Total number of steps taken per day", xlab = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Report mean and median of steps:

```r
mean(perday)
```

```
## [1] 10766.19
```

```r
median(perday)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
- Calculate average steps for each of 5-minute interval during a 24-hour period.  
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  
- Report which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
Observer and comment the average daily activity pattern


```r
dailyact<-tapply(activity_rm$steps, activity_rm$interval, mean)
```
Plot time series of the 5-minute interval and the average number of steps taken, averaged across all days


```r
plot(y = dailyact, x = names(dailyact), type = "l", xlab = "5-Minute-Interval", 
    main = "Daily Activity Pattern", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

Report the 5-min interval, on average across all the days in the dataset, contains the maximum number of steps:


```r
dailyact[dailyact==max(dailyact)]
```

```
##      835 
## 206.1698
```

Observations:

Based on steps taken pattern, the person's daily activity peaks around 8:35am.

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data. In this section:

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  
- Implement a strategy for filling in all of the missing values in the dataset. For this assignment the strategy is to use the mean for that 5-minute interval to replace missing valuse. Create a new dataset that is equal to the original dataset but with the missing data filled in.  
- Make a histogram of the total number of steps taken each day  
- Calculate and report the mean and median total number of steps taken per day.  
- Make following comments: Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
sum(is.na(activity))
```

```
## [1] 2304
```

Implement a strategy for filling in all of the missing values in the dataset. For this assignment the strategy is to use the mean for that 5-minute interval to replace missing values. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
act_new <- activity
act_new[which(is.na(act_new$steps)),1]<-
        dailyact[as.character(act_new[which(is.na(act_new$steps)),3])]


sum(is.na(act_new))
```

```
## [1] 0
```

```r
perday_new<-tapply(act_new$steps, act_new$date, sum)
```

Make a histogram of the total number of steps taken each day.


```r
par(mfrow=c(1,2))
hist(perday,10, main = "Total number of steps taken per day", xlab = "Steps"
     , ylim =c(0, 25))
abline(v = median(perday), col = 4, lwd = 4)
hist(perday_new,10, main = "Total number of steps taken per day  
     (missing values replaced with mean of interval)", xlab = "Steps",
     ylim =c(0, 25))
abline(v = median(perday_new), col = 4, lwd = 4)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

Calculate and report the mean and median total number of steps taken per day.

```r
mean(perday_new)
```

```
## [1] 10766.19
```

```r
median(perday_new)
```

```
## [1] 10766.19
```

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
mean(perday_new)-mean(perday)
```

```
## [1] 0
```

```r
median(perday_new)-median(perday)
```

```
## [1] 1.188679
```
Observations:

Do these values (mean and median) differ from the estimates from the first part of the assignment? Not Really.

What is the impact of imputing missing data on the estimates of the total daily number of steps? The shape of the histogram remains the same as the histogram from removed missing values. However, the frequency counts increased as expected. In this case, it seems that the data imputation strategy should work for the downstream data analysis and modeling.


## Are there differences in activity patterns between weekdays and weekends?
- Use the dataset with the filled-in missing values for this part. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.  
- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  

Create a factor variable weektime with two levels (weekday, weekend). The folowing dataset t5 dataset contains data: average number of steps taken averaged across all weekday days and weekend days, 5-min intervals, and a facter variable weektime with two levels (weekday, weekend).



```r
act_new$wd<-weekdays(act_new$date)
str(act_new)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ wd      : chr  "lunes" "lunes" "lunes" "lunes" ...
```

```r
act_new$fwd<- as.factor(c("weekend", "weekday"))
act_new[act_new$wd == "Sunday" | act_new$wd == "Saturday" ,5]<- factor("weekend")
act_new[!(act_new$wd == "Sunday" | act_new$wd == "Saturday"),5 ]<- factor("weekday")



act_new_we <- subset(act_new, fwd == "weekend") 
act_new_wd <- subset(act_new, fwd == "weekday") 

str(act_new_wd)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ wd      : chr  "lunes" "lunes" "lunes" "lunes" ...
##  $ fwd     : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
str(act_new_we)
```

```
## 'data.frame':	0 obs. of  5 variables:
##  $ steps   : num 
##  $ date    :Class 'Date'  num(0) 
##  $ interval: int 
##  $ wd      : chr 
##  $ fwd     : Factor w/ 2 levels "weekday","weekend":
```

```r
dailyact_we<-tapply(act_new_we$steps, act_new_we$interval, mean)
dailyact_wd<-tapply(act_new_wd$steps, act_new_wd$interval, mean)
par(mfrow=c(2,1))
```


Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
plot(y = dailyact_wd, x = names(dailyact_wd), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekdays", ylab = "Average number of steps",
     xlim =c(0,3000),
     ylim =c(0, 250))
```

![](PA1_template_files/figure-html/scatterplot-1.png) 


```r
plot(y = dailyact_we, x = names(dailyact_we), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekends", ylab = "Average number of steps", 
     xlim =c(0,3000),
     ylim =c(0, 250))
```

![](PA1_template_files/figure-html/scatterplot2-1.png) 



