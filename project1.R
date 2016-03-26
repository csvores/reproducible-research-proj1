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
