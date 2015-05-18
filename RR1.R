# === script preparation === #
setwd("~/Documents/5 - ReproducibleResearch/project_1/")
rm(list=ls())

# === read file === #
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#              ,"./repdata%2Fdata%2Factivity.zip",method = "wget")
#unzip(zipfile = "./repdata%2Fdata%2Factivity.zip",overwrite = T,exdir = "./")
amd <- read.csv("./activity.csv",T,",",na.strings = "NA")

# === processing 1 ===#
#amd$date <- strptime(amd$date,"%Y-%m-%d")

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
dim(na_vector)[[1]]

head(amd)
str(amd)






















