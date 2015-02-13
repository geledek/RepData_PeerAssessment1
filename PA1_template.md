# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
require(data.table)
dt <- fread("activity.csv")
```

## What is mean total number of steps taken per day?

1.Make a histogram of the total number of steps taken each day


```r
step_per_day <- aggregate(steps ~ date, dt, sum)
library(ggplot2)
g <- ggplot(step_per_day, aes(x=steps))
g + geom_histogram()
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
mean(step_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(step_per_day$steps)
```

```
## [1] 10765
```
## What is the average daily activity pattern?
1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
step_avg <- aggregate(steps ~ interval, dt, mean)
plot(x = step_avg$interval, y = step_avg$steps, type = "l", xlab = "time", ylab = "number of steps", main = "average number of steps taken in 5-minute interval across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
step_avg[step_avg$steps == max(step_avg$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(dt))
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# imputating missing values with the mean of the 5-minute interval calculated earlier
new_dt <- dt
for (i in 1:nrow(new_dt)){
  if (is.na(new_dt$steps[i])){
    interval_val <- new_dt$interval[i]
    steps_val <- step_avg$steps[step_avg$interval == interval_val]
    new_dt$steps[i] <- steps_val
  }
}
```


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
new_step_per_day <- aggregate(steps ~ date, new_dt, sum)
hist(new_step_per_day$steps, xlab = "number of steps", main = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
mean(new_step_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(new_step_per_day$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
library(timeDate)
new_dt$date <- as.Date(new_dt$date, "%Y-%m-%d")
new_dt$day <- weekdays(new_dt$date)
new_dt$weekday <- ifelse(isWeekday(new_dt$date), "weekday", "weekend")
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
new_dt$weekday <- as.factor(new_dt$weekday)
new_step_interval <- aggregate(steps ~ interval+weekday, new_dt, mean)
library(ggplot2)
qplot(interval, steps, data=new_step_interval, geom=c("line"),
      xlab="Interval", ylab="Number of steps", mean="") + 
      facet_wrap(~weekday, ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

rm(list=ls())
