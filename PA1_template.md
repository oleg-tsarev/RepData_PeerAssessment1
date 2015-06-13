---
title: "**PERSONAL MOVEMENT DATA**"
author: "Oleg Tsarev"
date: "12/06/2015"
output: html_document
---

## **Synopsis**
In this report we aim to explore large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up.  
The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.  
  
## **Loading the Raw data**
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.  

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
              ,"./repdata%2Fdata%2Factivity.zip",method = "wget")
unzip(zipfile = "./repdata%2Fdata%2Factivity.zip",overwrite = T,exdir = "./")
amd <- read.csv("./activity.csv",T,",",na.strings = "NA")
```
After reading we check the first rows in this dataset.

```r
dim(amd)
```

```
## [1] 17568     3
```

```r
head(amd)
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
So loading was successful.  
Let's add new time column.  

```r
amd$time = strptime(x = paste(amd$interval %/% 100,amd$interval - amd$interval %/% 100 * 100),format = "%H %M")
```

## **Q1: What is mean total number of steps taken per day?**
Firstly, let's aggregate steps quantity by date.

```r
sum_steps <- na.omit(aggregate(amd$steps,by=list(amd$date),FUN=sum,na.rm=T))
mean_steps <- round(x = mean(sum_steps$x),digits = 0)
median_steps <- median(sum_steps$x)
```
Secondly, let's make a histogram to show the total number of steps taken per day.

```r
hist(sum_steps$x,breaks=12,xlab="Total number of steps per day",col="grey",main="Histogram of total number of steps per day")
abline(v = mean_steps,col="blue")
abline(v = median_steps,col="green")
legend(x="topright",legend=c("mean","median"),col=c("blue","green"),lwd=3,bty="n")
```

![plot of chunk 1.2](figure/1.2-1.png) 
  
So, as you can see above **mean equals 9354, median equals 10395**.  

## **Q2: What is the average daily activity pattern?**
Firstly, let's aggregate steps quantity by date.

```r
day_pattern <- na.omit(aggregate(amd$steps,by=list(amd$interval,as.numeric(amd$time)),FUN=mean,na.rm=T))
```
Secondly, let's make a plot that shows total number of steps taken per day.

```r
plot(day_pattern$Group.1,day_pattern$x,main="Average daily activity pattern",type="l",
     xlab="time",ylab="average number of steps",xaxt='n')
axis(1, at=0:6*100*4, labels=paste(0:6*4,"00",sep=":"))
```

![plot of chunk 2.2](figure/2.2-1.png) 

```r
intens_interval <- day_pattern[day_pattern$x==max(day_pattern$x),"Group.1"]
intens_interval <- paste(intens_interval %/% 100,intens_interval - intens_interval %/% 100 * 100,sep=":")
```
**The most intensive 5 minutes interval starts about 8:35.**  

## **Imputing missing values**
Now let's see how many empty values (NA's) in the column "steps".

```r
summary(amd$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```
Let's replace them on the means that calculated as a mean(mean by date,mean by interval).

```r
na_vector <- amd[amd$steps=="NA",]
adj_amd <- amd
for(i in 1:nrow(amd)){
    if(is.na(amd[i,"steps"])){
        adj_amd[i,"steps"] <- mean(c(mean(amd[amd$date==amd[i,"date"],"steps"],na.rm=T),
                                     mean(amd[amd$interval==amd[i,"interval"],"steps"],na.rm=T)),na.rm=T)
    }
}
summary(adj_amd$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   27.00  806.00
```
Now let's see histogram of adjusted number of steps.  

```r
sum_steps <- na.omit(aggregate(adj_amd$steps,by=list(adj_amd$date),FUN=sum,na.rm=T))
mean_steps <- as.integer(mean(sum_steps$x))
median_steps <- as.integer(median(sum_steps$x))
hist(sum_steps$x,breaks=12,xlab="Total number of steps per day",col="grey",main="Histogram of total number of steps per day")
abline(v = mean_steps,col="blue")
abline(v = median_steps,col="green")
legend(x="topright",legend=c("mean","median"),col=c("blue","green"),lwd=3,bty="n")
```

![plot of chunk 3.3](figure/3.3-1.png) 

```r
amd_sum <- as.integer(sum(amd$steps,na.rm = T))
adj_amd_sum <- as.integer(sum(adj_amd$steps))
```
  
So, as you can see above **mean = median = 10766**.  
Due to the imputation total number of steps was increased from 570608 to 656737.  

## **Q3: Are there differences in activity patterns between weekdays and weekends?**
Let's make some data processing in order to answer on this question.

```r
library(ggplot2)
adj_amd$wday <- ifelse(weekdays(abbreviate = F,strptime(adj_amd$date,"%Y-%m-%d")) %in% c("Saturday","Sunday"),"weekend","weekday")
wday_adj_amd <- na.omit(aggregate(adj_amd$steps,by=list(adj_amd$interval,adj_amd$wday),FUN=mean,na.rm=T))
names(wday_adj_amd) <- c("interval","wday","steps")
```
And let's make a plot with activity patterns for weekdays and weekends.

```r
q <- qplot(interval,steps,data = wday_adj_amd,facets = wday ~ .,geom = "line",main="Activity patterns")
q + scale_x_continuous(breaks=0:6*100*4, labels=paste(0:6*4,"00",sep=":"))
```

![plot of chunk 4.2](figure/4.2-1.png) 

Based on the plot above we can say that during weekdays morning activity is higher than in weekend, it can be explained by trip to work.
But basically daily activity during weekend is higher than during weekdays.
  
  

  
  
  
 .
