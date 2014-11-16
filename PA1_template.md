Warning: package 'ggplot2' was built under R version 3.0.3
Reproducible Research: Peer Assessment 1
Loading and preprocessing the data
unzip(zipfile="repdata-data-activity.zip")
data <- read.csv("activity.csv")
What is mean total number of steps taken per day?
Make a histogram of the total number of steps taken each day

total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
plot of chunk unnamed-chunk-3

Mean total number of steps taken per day:

mean(total.steps, na.rm=TRUE)
Median total number of steps taken per day:

median(total.steps, na.rm=TRUE)
What is the average daily activity pattern?
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")
plot of chunk graph

On average across all the days in the dataset, the 5-minute interval contains the maximum number of steps?

averages[which.max(averages$steps),]
Imputing missing values
Total number of missing values:

missing <- is.na(data$steps)
table(missing)
Replace each missing value with the mean value of its 5-minute interval:

fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
Histogram, Mean and Median ot the total number of steps each day with the new data set:

total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day with no missing data")
plot of chunk unnamed-chunk-9

mean(total.steps)
median(total.steps)
Mean and median values were shorter before imputing missing data because missing values NA were considered as “zero” and after miputing those values are replaced by mean values.

Are there differences in activity patterns between weekdays and weekends?
Create a new factor varible in the dataset with two levels - “weekday” and “weekend”.

weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
Make a panel plot containing plots of average number of steps taken on weekdays and weekends.

averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
xlab("5-minute interval") + ylab("Number of steps")
plot of chunk unnamed-chunk-11
