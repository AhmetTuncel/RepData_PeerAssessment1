# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
##### 1. Load the data (i.e. read.csv())

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
Activity <- read.csv('activity.csv')
```
-----

## What is mean total number of steps taken per day?

```r
TotalStepsByDay <- tapply(Activity$steps, Activity$date, sum, na.rm=TRUE)
```

##### 1. Make a histogram of the total number of steps taken each day

```r
qplot(TotalStepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

##### 2. Calculate and report the mean and median total number of steps taken per day

```r
Mean <- mean(TotalStepsByDay)
Median <- median(TotalStepsByDay)
```
* Mean: 9354.2295082
* Median:  10395

-----

## What is the average daily activity pattern?

```r
Averages <- aggregate(x=list(Mean=Activity$steps), by=list(interval=Activity$interval), FUN=mean, na.rm=TRUE)
```

##### 1. Make a time series plot

```r
ggplot(data=Averages, aes(x=interval, y=Mean)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
Most <- which.max(Averages$MeanSteps)
MostStepsPerTime <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", Averages[Most,'interval'])
```

* Most Steps at: 

----

## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset 

```r
Missing <- length(which(is.na(Activity$steps)))
```

* Number of missing values: 2304

##### 2. Devise a strategy for filling in all of the missing values in the dataset.
##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
FillData <- Activity
FillData$steps <- impute(Activity$steps, fun=mean)
```


##### 4. Make a histogram of the total number of steps taken each day 

```r
FillValue <- tapply(FillData$steps, FillData$date, sum)
qplot(FillValue, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

##### ... and Calculate and report the mean and median total number of steps taken per day. 

```r
MeanFillValue <- mean(FillValue)
MedianFillValue <- median(FillValue)
```
* Mean (Imputed): 1.0766189 &times; 10<sup>4</sup>
* Median (Imputed):  1.0766189 &times; 10<sup>4</sup>


----

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels ‚<U+0080><U+0093> ‚<U+0080><U+009C>weekday‚<U+0080>ù and ‚<U+0080><U+009C>weekend‚<U+0080>ù indicating whether a given date is a weekday or weekend day.


```r
FillData$dateType <-  ifelse(as.POSIXlt(FillData$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

##### 2. Make a panel plot containing a time series plot


```r
averageActivityData <- aggregate(steps ~ interval + dateType, data=FillData, mean)
ggplot(averageActivityData, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

