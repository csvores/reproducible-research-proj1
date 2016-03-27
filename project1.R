## read the .csv into R
data <- read.csv("activity.csv", header = T)

## see what is in the data
summary(data)
str(data)

## correcting the date and getting it in the correct format
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)

## subsetting the data
## ignoring any missing values
NA_index <- is.na(as.character(data$steps))
data_no_NA <- data[!NA_index,]
head(data_no_NA)

## aggregating the steps per day
## creating a data frame with the steps taken for each day
## adding column names
steps_each_day <- aggregate(steps ~ date, data = data_no_NA, sum)
colnames(steps_each_day) <- c("date", "steps")

## graphing the steps per day
hist(as.numeric(steps_each_day$steps), breaks = 20, 
     col = "blue", xlab = "Number of Steps", 
     main= "Histogram of the total number of steps taken each day")

## mean number of steps per day
mean(steps_each_day$steps)

## median number of steps per day
median(steps_each_day$steps)

## average number of steps across all days 
steps_per_interval <- aggregate(data_no_NA$steps, by=list(interval=data_no_NA$interval), FUN=mean)

colnames(steps_per_interval) <- c("interval", "average_steps")

## plot the pattern of average daily activity 
plot(as.integer(levels(steps_per_interval$interval)), steps_per_interval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")

## sum of missing values
sum(is.na(as.character(data$steps)))
sum(is.na(as.character(data$date)))
sum(is.na(as.character(data$interval)))

## fill in the missing values
## finding the indices of missing values (NAs)
## imputing missing values using the mean for that 5-minute interval
NA_index <- which(is.na(as.character(data$steps)))
complete_data <- data
complete_data[NA_index, ]$steps<-unlist(lapply(NA_index, FUN=function(NA_index)
        steps_per_interval[data[NA_index,]$interval==steps_per_interval$interval,]$average_steps))

## checking the complete data
summary(complete_data)
str(complete_data)

## complete data set
steps_each_day_complete <- aggregate(steps ~ date, data = complete_data, sum)
colnames(steps_each_day_complete) <- c("date", "steps")

#Making the histogram
hist(as.numeric(steps_each_day_complete$steps), breaks = 20, 
     col = "blue", xlab = "Number of Steps", 
     main= "Histogram of the total number of steps taken each day")

## the mean and the median for the complete data set
mean(steps_each_day_complete$steps)
median(steps_each_day_complete$steps)

## comparing the difference between weekdays and weekends
complete_data$day <- as.factor(weekdays(complete_data$date))
complete_data$is_weekday <- ifelse(!(complete_data$day %in% c("Saturday","Sunday")), TRUE, FALSE) 

## calculating the average number of steps for weekdays and weekends
weekdays_data <- complete_data[complete_data$is_weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)
weekends_data <- complete_data[!complete_data$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)

colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")

steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"

week_data <- rbind(steps_per_interval_weekends, steps_per_interval_weekdays)

week_data$day <- as.factor(week_data$day)

## plotting the difference of weekdays and weekends
library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")