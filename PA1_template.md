# Reproducible Research: Peer Assessment 1

This project is meant to practice documenting in such a way as to make the 
work reproducible. An assignment (README.md) was given with data (activity.csv)
to use with basic analysis techniques to document. The sub-headings match with 
the headings in the assigment, but only the answers and some narrative is 
provided. Please refer to the assignment for more information on why specific
analysis tecniques are being used.

### Loading and Preprocessing Data
Once the activity.csv has been unzipped from the activity.zip file included with
the assignment, the data can be loaded into R with the following commands:

```r
# Read in the initial data and setup the factor names as well as the classes
raw<-read.csv(file="activity.csv",header=TRUE,col.names=c("steps","date","interval"),colClasses=c("numeric","Date","numeric"))

# Split up the data into vectors that can then be worked with and analyzed
steps<-raw$steps
date<-raw$date
int<-raw$interval
```

### What is mean total number of steps taken per day?
After summing the data by date, the total number of steps per day were plotted in a histogram.

```r
# Display the histogram as required. NOTE: breaks=8 and breaks=15 give the same plot. Anything beyond 15 becomes very noisy.
hist(by(raw$steps,date,sum),xlab="Total Number of Steps Taken per Day",main="Histogram of Total Number of Steps Taken per Day", breaks=15)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Once the plot is done, the mean number of steps per day and the median number of steps per day were calculated using the following code.

The mean per day is: 10766.18867 
The median per day is: 17065  


```r
# Calculate the mean and median number of steps per day. Ignore the NA's.
mean_per_day<-mean(by(raw$steps,date,sum),na.rm=TRUE)
median_per_day<-median(by(raw$steps,date,sum),na.rm=TRUE)
```

### What is the average daily activity pattern?

The average of interval for the entire timeframe was plotted with respect to the 5 minute interval in which that average occured in. Using this data, the interval in which the maxiumum average number steps occured was found.

```r
# Get the mean data averaged over the 5 minute interval for the full timeframe
data<-aggregate(.~interval,FUN=mean,data=raw)

# Plot the data
plot(data$interval,data$steps,type="l", xlab="Interval (5 min)",ylab="Average Number of Steps",main="Average Number of Steps vs. 5-Minute Interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
# Get max averaged steps per time interval
max_per_time<-data[which.max(data$steps),]
```
The max average number of steps is: 206.1698  
The timeframe in which this max occured is: 835  

### Imputing missing values

The first step in this process was to find out how many rows had missing data.


```r
# Find how much data is missing
num_na<-length(raw[is.na(raw)])
```
Number of Rows with NA's: 2304  

Now that we knew how much data was missing, the next step was to fill in the data and create a new dataset. In order to fill in the data, we would be using the average of that specific 5 minute interval across the timeframe. This is done to keep the average and trends consistent. 


```r
# append average to base dataset and use to fill in NA's
bigraw<-cbind(raw,data$steps)
names(bigraw)<-c(names(raw),"int_avg_steps")
bigraw$steps[is.na(bigraw$steps)]<-bigraw[,4]
```

```
## Warning: number of items to replace is not a multiple of replacement
## length
```

```r
head(bigraw,n=20)
```

```
##      steps       date interval int_avg_steps
## 1  1.71698 2012-10-01        0       1.71698
## 2  0.33962 2012-10-01        5       0.33962
## 3  0.13208 2012-10-01       10       0.13208
## 4  0.15094 2012-10-01       15       0.15094
## 5  0.07547 2012-10-01       20       0.07547
## 6  2.09434 2012-10-01       25       2.09434
## 7  0.52830 2012-10-01       30       0.52830
## 8  0.86792 2012-10-01       35       0.86792
## 9  0.00000 2012-10-01       40       0.00000
## 10 1.47170 2012-10-01       45       1.47170
## 11 0.30189 2012-10-01       50       0.30189
## 12 0.13208 2012-10-01       55       0.13208
## 13 0.32075 2012-10-01      100       0.32075
## 14 0.67925 2012-10-01      105       0.67925
## 15 0.15094 2012-10-01      110       0.15094
## 16 0.33962 2012-10-01      115       0.33962
## 17 0.00000 2012-10-01      120       0.00000
## 18 1.11321 2012-10-01      125       1.11321
## 19 1.83019 2012-10-01      130       1.83019
## 20 0.16981 2012-10-01      135       0.16981
```

Finally, a histogram is made of the number of steps taken each day. By filling in missing values with the average, the distribution of the histogram hasn't changed much, but the frequency has - as expected.


```r
# Display the histogram as required. NOTE: breaks=8 and breaks=15 give the same plot. Anything beyond 15 becomes very noisy.
hist(by(bigraw$steps,date,sum),xlab="Total Number of Steps Taken per Day",main="Histogram of Total Number of Steps Taken per Day", breaks=15)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Once the plot is done, the mean number of steps per day and the median number of steps per day were calculated using the following code. The mean has not changed (which was the point of using averages to fill in the missing data), but the median has shifted to match the mean, because now, that value is a part of the dataset.

The mean per day is: 10766.18867  
The median per day is: 10766.18867  


```r
# Calculate the mean and median number of steps per day. Ignore the NA's.
bigmean_per_day<-mean(by(bigraw$steps,date,sum),na.rm=TRUE)
bigmedian_per_day<-median(by(bigraw$steps,date,sum),na.rm=TRUE)
```

### Are there differences in activity patterns between weekdays and weekends?

In order to find trends within the activity pattern of weekdays and those with weekends, each observation was labeled as either a weekend or weekday based on the date. This was added to the expanded dataset bigraw, which already had NA values filled in with the averages for the individual time intervals.


```r
# Create weekend and weekday factors
weekday <- weekdays(as.Date(raw$date, "%Y-%m-%d"))
for (i in 1:length(weekday)) {
    if ((weekday[i] == "Saturday") | (weekday[i] == "Sunday")) 
    weekday[i] = "weekend" else weekday[i] = "weekday"}
bigraw$weekday <- as.factor(weekday)
head(bigraw)
```

```
##     steps       date interval int_avg_steps weekday
## 1 1.71698 2012-10-01        0       1.71698 weekday
## 2 0.33962 2012-10-01        5       0.33962 weekday
## 3 0.13208 2012-10-01       10       0.13208 weekday
## 4 0.15094 2012-10-01       15       0.15094 weekday
## 5 0.07547 2012-10-01       20       0.07547 weekday
## 6 2.09434 2012-10-01       25       2.09434 weekday
```

Plotting the weekday and weekend data on a panal plot show the differences in steps for an average weekday vs and average weekend.


```r
# Create separate datasets for weekdays and weeknds
daydata<-split(bigraw,bigraw$weekday)
weekdaydata<-daydata$weekday
weekenddata<-daydata$weekend

# Get the averages of every interval for the timeframe. Use the weekday set to get only weekday data and use the weekend set to get only weekend data.
databyday<-by(weekdaydata$steps,weekdaydata$interval,mean)
databyend<-by(weekenddata$steps,weekenddata$interval,mean)
par(mfrow=c(2,1))
plot(data$interval,databyday,type="l",xlab="Interval (5 min)",ylab="Average Number of Steps",main="Weekday Data")
plot(data$interval,databyend,type="l", xlab="Interval (5 min)",ylab="Average Number of Steps",main="Weekend Data")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
