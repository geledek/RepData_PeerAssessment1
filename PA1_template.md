# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
require(data.table)
require(ggplot2)
dt <- fread("activity.csv")
```

## What is mean total number of steps taken per day?

1.Make a histogram of the total number of steps taken each day


```r
step_per_day <- aggregate(steps ~ date, dt, sum)

g <- ggplot(step_per_day, aes(x=steps))
g <- g + geom_histogram()
g <- g + scale_y_continuous(breaks=seq(0,9,1))
g <- g + theme_bw()
g
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

g <- ggplot(step_avg, aes(x=interval, y=steps))
g <- g + geom_line()
g <- g + scale_x_continuous(name="time", 
                            breaks = seq(0,2300,by=100),
                            labels = c(paste(seq(from=0, to=11),"AM", sep=""), "NOON", paste(seq(from=1, to=11),"PM", sep="")))
g <- g + theme_bw() + theme(axis.text.x = element_text(angle=45), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black"))
g
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

g <- ggplot(new_step_per_day, aes(x=steps))
g <- g + geom_histogram()
g <- g + scale_y_continuous(breaks=seq(0,12,2))
g <- g + theme_bw()
g
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

g <- qplot(interval, steps, data=new_step_interval, geom=c("line"),
      xlab="Interval", ylab="Number of steps", mean="") + 
      facet_wrap(~weekday, ncol=1)
g <- g <- g + scale_x_continuous(name="time", 
                            breaks = seq(0,2300,by=100),
                            labels = c(paste(seq(from=0, to=11),"AM", sep=""), "NOON", paste(seq(from=1, to=11),"PM", sep="")))
g <- g + theme_bw() + theme(axis.text.x = element_text(angle=45), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.background = element_blank(), axis.line = element_line(colour = "black"))
g
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

rm(list=ls())
