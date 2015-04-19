
Loading and preprocessing the data
------------------------------------


```r
Sys.setlocale("LC_TIME", "C") # LOCAL TIME -> English
```

```
## [1] "C"
```

```r
df <- read.csv("Activity.csv")
```

What is mean total number of steps taken per day?
----------------------------------------------------

1. Calculate the total number of steps taken per day


```r
stepsday <-aggregate(as.numeric(df$steps)
                     , by=list(df$date)
                     , FUN=sum
                     , na.rm= T)
colnames(stepsday) <- c("date", "steps")
```

2. Make a histogram of the total number of steps taken each day


```r
hist(stepsday$steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


3. Calculate and report the mean and median of the total number of steps taken per day.


```r
mean(stepsday$steps)
```

```
## [1] 9354.23
```

```r
median(stepsday$steps)
```

```
## [1] 10395
```

What is the average daily activity pattern?
-------------------------------------------------

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsInterval <-aggregate(as.numeric(df$steps)
                          , by=list(df$interval)
                          , FUN=sum
                          , na.rm= T)
colnames(stepsInterval) <- c("interval", "steps")
plot(stepsInterval$interval,stepsInterval$steps,type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsInterval[stepsInterval$steps == max(stepsInterval$steps),]
```

```
##     interval steps
## 104      835 10927
```


Imputing missing values
-------------------------

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(df[df$steps == "NA",])
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

*Strategy* : substitue NA valuea with the mean of that day.


```r
df2 <- merge(x = df, y = stepsday, by = "date", all.x=TRUE)
df2[is.na(df2$steps.x),2]  = df2[is.na(df2$steps.x),4]
df2 <- df2[,1:3]
```


3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsday2 <-aggregate(as.numeric(df2$steps.x)
                      , by=list(df2$date)
                      , FUN=sum)
colnames(stepsday2) <- c("date", "steps")
hist(stepsday2$steps)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
mean(stepsday2$steps)
```

```
## [1] 9354.23
```

```r
median(stepsday2$steps)
```

```
## [1] 10395
```

*Answer* : No differences found, as the NA values always correspond to a whole day and the replacement value chosen was the mean by day.

Are there differences in activity patterns between weekdays and weekends?
-----------------------------------------------------------------------------

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
df2$date <- as.POSIXct(df2$date, format="%Y-%m-%d")
df2$dateType <- ifelse((weekdays(df2$date) %in% c("Saturday", "Sunday"))
                       , "Weekend"
                       , "Weekday")
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
stepsInterval2 <-aggregate(as.numeric(df2$steps.x), by=list(df2$interval, df2$dateType), FUN=sum)
colnames(stepsInterval2) <- c("interval", "dateType", "steps")

par(mfrow = c(2,1), mar= c(2,4,1,1) , oma=c(1,1,1,1))
{
    # Plot 1
    plot(stepsInterval2[stepsInterval2$dateType == "Weekday", 1]
         , stepsInterval2[stepsInterval2$dateType == "Weekday", 3]
         ,type="l"
         , main = "Weekday"
         , xlab =""
         , ylab = ""
         , ylim = c(0,10000)
         , xlim= c(0,2400))
    # Plot 2
    plot(stepsInterval2[stepsInterval2$dateType == "Weekend", 1]
         ,stepsInterval2[stepsInterval2$dateType == "Weekend", 3]
         ,type="l"
         , main = "Weekend"
         , xlab=""
         , ylab= ""
         , ylim = c(0,10000)
         , xlim= c(0,2400))
    mtext("steps", 2, outer = T)
    mtext("interval",1, outer =T)
}
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
