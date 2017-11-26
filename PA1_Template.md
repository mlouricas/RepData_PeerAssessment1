---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



##Loading library's, Data and Changing the date field to Date.
##Tidy the data by adding a week field and create clean data set with no NA's (n = 2,304) for later use



```r
library(data.table)
library(ggplot2)
library(timeDate)
library(scales)

act <- read.csv("activity.csv", header = TRUE, sep = ",")

act[,2]<-as.Date(act$date)

summary(act)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
str(act)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
##Create week field and see if it is correct
act$week <- ifelse(weekdays(act$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

unique(act$week)
```

```
## [1] "weekday" "weekend"
```

```r
##pulling data without Na's and compare datasets
clean_act <- act[!is.na(act$steps),]

##2304 Na's
dim(clean_act)
```

```
## [1] 15264     4
```

```r
dim(act)
```

```
## [1] 17568     4
```
 
##What is mean total number of steps taken per day?

###For this part of the assignment, you can ignore the missing values in the dataset.

1.  Calculate the total number of steps taken per day


```r
steps_day <- aggregate(steps ~ date,act, sum)
```

2.  Make a histogram of the total number of steps taken each day


```r
g <- ggplot(steps_day, aes(x = steps))

g + geom_histogram(fill = "midnightblue", binwidth = 3000) +
        xlab("Steps") + ylab("Frequency") + 
        ggtitle("Histogram - Steps per Day") + 
        theme(plot.title = element_text(color="grey25", face="bold",               hjust=0.5)) 
```

![](PA1_Template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3.  Calculate and report the mean and median of the total number of steps      taken per day


```r
##Mean - 10766.19
Mean_steps_day <- mean(steps_day$steps)
print(Mean_steps_day)
```

```
## [1] 10766.19
```

```r
##Median - 10765
Median_steps_day <- median(steps_day$steps)
print(Median_steps_day)
```

```
## [1] 10765
```

*  Mean Steps   = 10,766.19 ~ 10,766
*  Median Steps = 10,765

 
## What is the average daily activity pattern?
 
1.  Make a time series plot of the 5-minute interval (x-axis) and the          average number of steps taken, averaged across all days (y-axis)


```r
## Calculate the average steps per 5 min interval
averages <- aggregate(clean_act$steps, list(interval =                                as.numeric(clean_act$interval)), FUN = "mean")

names(averages)[2] <- "Avg.Steps"

head(averages)
```

```
##   interval Avg.Steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
## Plot Time Series
t <- ggplot(averages, aes(interval, Avg.Steps)) + 
     geom_line(color = "midnightblue", size = 1) + 
     ggtitle("Time Series Plot of the 5-minute Intervals") +
     xlab("5-minute intervals") + ylab("Average Number of Steps Taken") +
     theme(plot.title = element_text(color="grey25", face="bold",              hjust=0.5)) 

print(t)
```

![](PA1_Template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
## Max steps in 5 minute intervals
maxSteps <- max(averages$Avg.Steps)

## 206.17
print(maxSteps)
```

```
## [1] 206.1698
```

```r
## Which interval contains the maximum average number of steps
## 835
averages[averages$Avg.Steps==maxSteps,1]
```

```
## [1] 835
```

*  At the 835th interval the Max steps were 206.17 ~ 206

## Imputing missing values

1.  Calculate and report hte total number of missing values in the dataset
    (i.e. the total number of rows with NA's)

```r
sum(is.na(act$steps))
```

```
## [1] 2304
```
*  Count of Na's is 2,304

2.  Devise a strategy for filling in all of the missing values in the         dataset

Strategy - Replace NA's with Avg steps per interval from previous dataset


3.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.


```r
DataImp <- act
for (i in 1:nrow(DataImp)) {
        if (is.na(DataImp$steps[i])) {
              DataImp$steps[i] <- averages[which(DataImp$interval[i] 
              == averages$interval), ]$Avg.Steps
        }
}        

## Check if Avg's from avg step dataset replaced all NA's

sum(is.na(DataImp$steps)) 
```

```
## [1] 0
```

There are no NA's left in the data after being replaced by the Avg Steps dataset

4.  Make a histogram of the total number of steps taken each day and 
    Calculate and report the mean and median total number of steps taken 
    per day.

Calculate Steps per day on new dataset


```r
steps_day_Imp <- aggregate(steps ~ date,DataImp, sum)
```

Plot the Histogram


```r
gI <- ggplot(steps_day_Imp, aes(x = steps))

gI <- gI + geom_histogram(fill = "midnightblue", binwidth = 3000) +
        xlab("Steps") + ylab("Frequency") + 
        ggtitle("Histogram - Steps per Day") + 
        theme(plot.title = element_text(color="grey25", face="bold",               hjust=0.5)) 
print(gI)
```

![](PA1_Template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Mean and Median calculations


```r
##Mean - 10766.19 ~ 10766
Mean_steps_day_Imp <- mean(steps_day_Imp$steps)
print(Mean_steps_day_Imp)
```

```
## [1] 10766.19
```

```r
##Median - 10766.19 ~ 10766
Median_steps_day_Imp <- median(steps_day_Imp$steps)
print(Median_steps_day_Imp)
```

```
## [1] 10766.19
```


5.  Do these values differ from the estimates from the first part of the 
    assignment?  What is the impact of imputing missing data on the           estimates of the total daily number of steps?
    
Mean = 10766.19 ~ 10766 - Stayed the same as the first time
Median = 10766.19 ~ 10766 - Increased by 1.19 compared to the first time
Histogram shape - No change in the overall distribution

Impact of imputing missing data on total number of steps


```r
## Imputed Data set
sum(DataImp$steps)
```

```
## [1] 656737.5
```

```r
## Clean dataset with NA's removed so we can sum
sum(clean_act$steps)
```

```
## [1] 570608
```

Total number of steps increases in the imputed missing data set because I added values to the Na's.  Without those new values the Na's wouldn't have a value and be added in the sum
 
## Are there differences in activity patterns between weekdays and weekends?

1.  Created a factor variable earlier in the Tidying of the data where I      made the date field a date as opposed to factor field and created a       new field using timedate that assigned "weekday" and "weekend" values     to corresponding dates

2.  Make a panel plot containing atime series plot of the 5-minute            interval (x-axis)and the average number of steps taken, averaged          across all weekday days or weekend days(y-axis).


```r
## calculate avg number of steps taken across weekday_weekends
averages_Imp <- aggregate(DataImp$steps, by=list(DataImp$week, DataImp$interval), mean)
names(averages_Imp)[1] ="week"
names(averages_Imp)[2] ="interval"
names(averages_Imp)[3] ="steps"

head(averages_Imp)
```

```
##      week interval      steps
## 1 weekday        0 2.25115304
## 2 weekend        0 0.21462264
## 3 weekday        5 0.44528302
## 4 weekend        5 0.04245283
## 5 weekday       10 0.17316562
## 6 weekend       10 0.01650943
```

```r
## Panel Plot

ggplot(averages_Imp, aes(interval, steps, color=(week))) +
  geom_line() +
  facet_grid(week ~ .) +
  ggtitle("Time Series Plot of the 5-minute Intervals \nby Weekday/Weekend") + 
  xlab("Intervals") +  ylab("Steps") +
  theme(plot.title = element_text(color="grey25", face="bold", hjust=0.5)) 
```

![](PA1_Template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

This Time Series plot shows that overall there are more steps taken more consistently on the weekend and the majority of the steps are taken in the beginning of the day during weekdays.  One can potentially conlude causality due to working avg time of 9am-5pm
