---
title: "Assignment1"
author: "Thomas Cronin"
date: 'July 16, 2025'
output:
  html_document:
    keep_md: yes
---


```r
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, fig.width = 10, fig.height = 5,
                      fig.keep = 'all' ,fig.path = 'figures\ ')
```

<br><br>

### 1. Loading and preprocessing the data:

```r
# load the data
activity <- read.csv("activity.csv")
# check the data
head(activity)
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

```r
# change date to calss factor to class Date
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

<br><br>

### 2. What is mean total number of steps taken per day?

```r
# the total number of steps taken per day is stored in the variable called "total_step"
total_step <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
head(total_step)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

#### Histogram:

```r
par(mfrow = c(1, 1))
# use base plotting system and more bins than the default setting
hist(total_step$steps, breaks = 20, 
     main = "Total Number of Steps Taken Each Day",
     col = "grey", border = "white", xlab = "Step", axes = FALSE)
```

![](figures unnamed-chunk-4-1.png)<!-- -->

```r
axis(1)
```

![](figures unnamed-chunk-4-2.png)<!-- -->

```r
axis(2, las = 1)
```

![](figures unnamed-chunk-4-3.png)<!-- -->

**Mean and Median** number of steps taken each day:

```r
mean(total_step$steps)
```

```
## [1] 10766.19
```

```r
median(total_step$steps)
```

```
## [1] 10765
```

<br><br>

### 3. What is the average daily activity pattern?

#### Time series plot:

```r
avg_step <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(avg_step$interval, avg_step$steps, type = "l", lwd = 2, col = "navy",
     main = "Time Series: Average Number of Steps Taken", axes = FALSE,
     xlab = "5-minute interval", ylab = "Average number of steps")
```

![](figures unnamed-chunk-6-1.png)<!-- -->

```r
axis(1)
```

![](figures unnamed-chunk-6-2.png)<!-- -->

```r
axis(2, las = 1)
```

![](figures unnamed-chunk-6-3.png)<!-- -->

#### The 5-minute interval contains the max number of steps:

```r
avg_step$interval[which.max(avg_step$steps)]
```

```
## [1] 835
```

<br><br>

### 4. Imputing missing values:

The total missing values.

```r
sum(is.na(activity)) # or dim(activity[activity$steps == "NA", ])[1]
```

```
## [1] 2304
```

assign avg to all NA in a new dataset:

```r
imp <- activity # new dataset called imp
for (i in avg_step$interval) {
    imp[imp$interval == i & is.na(imp$steps), ]$steps <- 
        avg_step$steps[avg_step$interval == i]
}
head(imp) # no NAs
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
sum(is.na(imp)) # should be 0
```

```
## [1] 0
```

#### Histogram after missing values are imputed:

```r
total_step_imp <- aggregate(steps ~ date, data = imp, sum, na.rm = TRUE)
hist(total_step_imp$steps, breaks = 20, 
     main = "Total Number of Steps Taken Each Day (Imputed)",
     col = "grey", border = "white", xlab = "Step", axes = FALSE)
```

![](figures unnamed-chunk-10-1.png)<!-- -->

```r
axis(1)
```

![](figures unnamed-chunk-10-2.png)<!-- -->

```r
axis(2, las = 1)
```

![](figures unnamed-chunk-10-3.png)<!-- -->

#### Mean and median after missing values are imputed:

```r
mean(total_step_imp$steps)
```

```
## [1] 10766.19
```

```r
median(total_step_imp$steps)
```

```
## [1] 10766.19
```
<br><br>

### 5. Differences in activity patterns between weekdays and weekends:

Create new factor variables.:

```r
imp$day <- weekdays(imp$date)
imp$week <- ""
imp[imp$day == "Saturday" | imp$day == "Sunday", ]$week <- "weekend"
imp[!(imp$day == "Saturday" | imp$day == "Sunday"), ]$week <- "weekday"
imp$week <- factor(imp$week)
```


```r
imp$day <- weekdays(imp$date)
imp$week <- ""
imp[imp$day == "Saturday" | imp$day == "Sunday", ]$week <- "weekend"
imp[!(imp$day == "Saturday" | imp$day == "Sunday"), ]$week <- "weekday"
imp$week <- factor(imp$week)
```

Panel plot:


```r
avg_step_imp <- aggregate(steps ~ interval + week, data = imp, mean)
library(lattice)
xyplot(steps ~ interval | week, data = avg_step_imp, type = "l", lwd = 2,
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Average number of steps",
       main = "Average Number of Steps Taken (across all weekday days or weekend days)")
```

![](figures unnamed-chunk-14-1.png)<!-- -->
