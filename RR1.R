# === script preparation === #
setwd("~/Documents/5 - ReproducibleResearch/project_1/")
rm(list=ls())
library(ggplot2)

# === read file === #
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#              ,"./repdata%2Fdata%2Factivity.zip",method = "wget")
#unzip(zipfile = "./repdata%2Fdata%2Factivity.zip",overwrite = T,exdir = "./")
amd <- read.csv("./activity.csv",T,",",na.strings = "NA")

# === processing 1 ===#
sum_steps <- na.omit(aggregate(amd$steps,by=list(amd$date),FUN=sum,na.rm=T))
barplot(sum_steps$x,names.arg=sum_steps$Group.1,main="Total number of steps per day")

mean_steps <- na.omit(aggregate(amd$steps,by=list(amd$date),FUN=mean,na.rm=T))
median_steps <- na.omit(aggregate(amd$steps,by=list(amd$date),FUN=median,na.rm=T))
barplot(mean_steps$x,names.arg=mean_steps$Group.1,main="Mean and median total number of steps taken per day")
lines(median_steps$Group.1,median_steps$x,col="red")

# === processing 2 ===#
day_pattern <- na.omit(aggregate(amd$steps,by=list(amd$interval),FUN=mean,na.rm=T))
plot(day_pattern$Group.1,day_pattern$x,main="asd",type="l")
intens_interval <- day_pattern[max(day_pattern$x),"Group.1"]

# === processing 3 ===#
na_vector <- amd[amd$steps=="NA",]

adj_amd <- amd
for(i in 1:nrow(amd)){
    if(is.na(amd[i,"steps"])){
        adj_amd[i,"steps"] <- mean(c(mean(amd[amd$date==amd[i,"date"],"steps"],na.rm=T),
                                     mean(amd[amd$interval==amd[i,"interval"],"steps"],na.rm=T)),na.rm=T)
    }
}

hist(adj_amd$steps)
hist(amd$steps)
#hist(df$Global_active_power,col="red",main = "Global Active Power",xlab = "Global Active Power (kilowatts)")

adj_sum_steps <- na.omit(aggregate(adj_amd$steps,by=list(adj_amd$date),FUN=sum,na.rm=T))
barplot(adj_sum_steps$x,names.arg=adj_sum_steps$Group.1,main="Total number of steps per day")

adj_mean_steps <- na.omit(aggregate(adj_amd$steps,by=list(adj_amd$date),FUN=mean,na.rm=T))
adj_median_steps <- na.omit(aggregate(adj_amd$steps,by=list(adj_amd$date),FUN=median,na.rm=T))
barplot(adj_mean_steps$x,names.arg=adj_mean_steps$Group.1,main="Adjusted mean and median total number of steps taken per day")
lines(adj_median_steps$Group.1,adj_median_steps$x,col="red")

barplot(adj_sum_steps$x - sum_steps$x,names.arg=adj_sum_steps$Group.1,main="Total number of steps per day")

# === processing 4 ===#
adj_amd$wday <- ifelse(weekdays(strptime(adj_amd$date,"%Y-%m-%d")) %in% c("Saturday","Sunday"),"weekend","weekday")

wday_adj_amd <- na.omit(aggregate(adj_amd$steps,by=list(adj_amd$interval,adj_amd$wday),FUN=mean,na.rm=T))
names(wday_adj_amd) <- c("interval","wday","steps")

qplot(interval,steps,data = wday_adj_amd,facets = wday ~ .,geom = "line")
qplot(interval,steps,data = wday_adj_amd,facets = . ~ wday,geom = "line")


