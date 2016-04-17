#Load the plyr package to perform separate, apply, combine functions on data
library(plyr)

#Load the data into r from the activity.csv file
stepsData <- read.csv("activity.csv", na.strings="NA")

#Use the plyr package to group the data by date, taking the sum of steps on each day in a variable
#called totalSteps
sum_steps_by_day<-ddply(stepsData, .(date), summarize, totalSteps=sum(steps, na.rm=TRUE))

##plot total steps by day in a histogram
hist(sum_steps_by_day$totalSteps, main="Histogram of Total Number of Steps Per Day", xlab="Ranges of Total Number of Steps Per Day", ylab="Number of Days")

##Calculate and report the mean and median total number of steps taken per day
mean(sum_steps_by_day$totalSteps)
median(sum_steps_by_day$totalSteps)

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
time_series_avg_steps<-ddply(stepsData, .(interval), summarize, meanSteps=mean(steps,na.rm=TRUE))
plot(time_series_avg_steps$interval, time_series_avg_steps$meanSteps, type="l", main="Average Steps Per Time Interval", xlab="Time Interval", ylab="Average Number of Steps")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxSteps<-max(time_series_avg_steps$meanSteps)
maxInterval<-subset(time_series_avg_steps, time_series_avg_steps$meanSteps==maxSteps)


#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
nullRows<-stepsData[is.na(stepsData[,"steps"]),]
countNulls<-nrow(nullRows)


#Create a new dataset with the missing step values filled in
stepsData2<-stepsData
for (i in 1:length(stepsData$steps)) {
  if (is.na(stepsData$steps[i])){
    replacement<-subset(time_series_avg_steps,interval==stepsData$interval[i])[,"meanSteps"]
    stepsData2$steps[i]<-replacement
  }
}

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
sum_steps_by_day_adj<-ddply(stepsData2, .(date), summarize, totalSteps=sum(steps, na.rm=TRUE))
hist(sum_steps_by_day_adj$totalSteps, main="Histogram of Total Number of Steps Per Day", xlab="Ranges of Total Number of Steps Per Day", ylab="Number of Days")
mean(sum_steps_by_day_adj$totalSteps)
median(sum_steps_by_day_adj$totalSteps)






#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
weekday<-rep(NA, length(stepsData2$steps))
stepsData2=cbind(stepsData2,weekday)
weekday_var=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
for (i in 1:length(stepsData2$steps)){
  dayOfWeek<-weekdays(as.Date(stepsData2$date[i], abbreviate=FALSE))
  if (dayOfWeek %in% weekday_var){
    stepsData2$weekday[i]<-"Weekday"
  } else {
    stepsData2$weekday[i]<-"Weekend"
  }
}


#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
stepsData2_weekdays<-subset(stepsData2,weekday=="Weekday")
stepsData2_weekends<-subset(stepsData2,weekday=="Weekend")
avg_weekday_steps<-ddply(stepsData2_weekdays, .(interval, weekday), summarize, avg_steps=mean(steps))
avg_weekend_steps<-ddply(stepsData2_weekends, .(interval, weekday), summarize, avg_steps=mean(steps))
combined_avg_steps<-rbind(avg_weekday_steps,avg_weekend_steps)
combined_avg_steps <- transform(combined_avg_steps, weekday=factor(weekday))

#use the lattice library to show the weekday data and the weekend data in panels
library(lattice)
xyplot(avg_steps ~ interval | weekday, data = combined_avg_steps, layout=c(1, 2), type="l", ylab="Number of Steps", xlab="Interval")

