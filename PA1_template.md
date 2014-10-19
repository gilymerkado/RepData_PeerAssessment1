# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load the data into R and create a list of dates with the steps of each day.


```r
    activity <- read.csv("activity.csv")
    activity.days <- split(activity$steps, as.factor(activity$date))
```




## What is mean total number of steps taken per day?

A histogram of the total number of steps taken each day:


```r
    activity.days.sum <- sapply(activity.days, sum)
    hist(activity.days.sum)
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean and median of the total number of steps taken each day:


```r
    activity.days.mean <- mean(activity.days.sum, na.rm = TRUE)
    activity.days.median <- median(activity.days.sum, na.rm = TRUE)
```

The mean of the total number of steps taken each day is 1.0766189\times 10^{4}.

The median of the total number of steps taken each day is 10765.


## What is the average daily activity pattern?

Calculate the average number of steps taken on each 5-minute interval over all days.


```r
    activity.interval <- do.call(rbind, activity.days)
    activity.interval.means <- colMeans(activity.interval, na.rm = TRUE)
```

plot a time series of the 5-minute interval and the average number of steps taken, averaged across all days:


```r
    plot(activity$interval[1:288], activity.interval.means, type = "l",
         main = "Average number of steps", xlab = "5-minute interval", 
         ylab = "Average number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Calculate which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.


```r
    maximum.interval.index <- which.max(activity.interval.means)
    maximum.interval <- activity$interval[maximum.interval.index]
```

The interval 835 has the maximum number of steps on average.

## Imputing missing values

The total number of missing values in the dataset:

```r
    missing.values <- is.na(activity$steps)
    sum(missing.values)
```

```
## [1] 2304
```

In order to fill all the missing values in the data set I will use the mean of the 5-minute interval that is missing.


```r
    activity.days.filled <- activity.days
    for (l in 1:61) {
         day <- activity.days.filled[[l]]
        for (i in 1:288) {
            if (is.na(day[i])) {
                day[i] <- activity.interval.means[i]
                activity.days.filled[[l]][i] <- day[i]
            }
        }
    }
    activity.interval.filled <- do.call(rbind, activity.days.filled)
```

Plot a histogram of the total number of steps taken each day:

```r
    activity.days.filled.sum <- sapply(activity.days.filled, sum)
    hist(activity.days.filled.sum)
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

The mean and median of the total number of steps taken each day:


```r
    activity.days.filled.mean <- mean(activity.days.filled.sum)
    activity.days.filled.median <- median(activity.days.filled.sum)
```

The mean of the total number of steps taken each day is 1.0766189\times 10^{4}.

The median of the total number of steps taken each day is 1.0766189\times 10^{4}.

Only the median differ from the estimates from the first part of the assignment. Since the filled data was equal to the median, the median vlaues did not change. But the median changed and with the filled data it is now equal to the median.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
My computer's locale is Hebrew and therfore the days are written in Hebrew by default. In order to make sure that the days are written in English I will set the locale to English.

```r
    Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

The filled data of the activity intervals is currently saved as a matrix. I will change this to a data frame so it will be easier to manipulate and add a column of the weekdays. I then convert the days vector into a logical vector where TRUE values mean that the day is a weekday. Finaly I convert the logical vector into factors with two labels: "Weekend"and "Weekday".


```r
    activity.interval.filled.dat <- as.data.frame(activity.interval.filled)
    colnames(activity.interval.filled.dat) <- activity$interval[1:288]
    activity.interval.filled.dat$days <- weekdays(as.POSIXlt(
        rownames(activity.interval.filled.dat)))
    activity.interval.filled.dat$days <- activity.interval.filled.dat$days %in% 
    c("Monday", "Tuesday","Wednesday", "Thursday", "Friday")
    activity.interval.filled.dat$days <- factor(activity.interval.filled.dat$days,
                                                labels = c("Weekend", "Weekday"))
```

Use aggregate to compute the means of each interval by weekday and weekend and plot the average number of steps on each time interval for weekends and weekdays.


```r
    agg <- aggregate(activity.interval.filled.dat, 
                     list(activity.interval.filled.dat$days), mean)
```

```
## Warning in mean.default(X[[1L]], ...): argument is not numeric or logical:
## returning NA
```

```
## Warning in mean.default(X[[2L]], ...): argument is not numeric or logical:
## returning NA
```

```r
    par(mfrow = c(2, 1))
    plot(activity$interval[1:288], agg[1, 2:289], type = "l", 
         main = "Average number of steps on weekends", xlab = "5-minute interval", 
         ylab = "Average number of steps")
    plot(activity$interval[1:288], agg[2, 2:289], type = "l", 
         main = "Average number of steps on weekdays", xlab = "5-minute interval", 
         ylab = "Average number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

