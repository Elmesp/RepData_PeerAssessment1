unzip("activity.zip", "activity.csv")
activities <- read.csv("activity.csv")

daySteps <- sum(activities $steps, na.rm = TRUE)
stepsPerDay <- aggregate(steps~date, data = activities, FUN = sum, na.rm = TRUE)
hist(stepsPerDay $steps, xlab = "Number of steps", ylab = "Days")

stepsPerDayMean <- mean(stepsPerDay $steps)
stepsPerDayMedian <- median(stepsPerDay $steps)

min5Avg <- aggregate(steps~interval, data = activities, FUN = mean, na.rm=TRUE)
plot(x = min5Avg$interval, y = min5Avg$steps, type = "l") 


maxSteps <- max(min5Avg $steps)

maxInterval <- min5Avg[which(min5Avg$steps == maxSteps),] $interval

naCount <- sum(is.na(activities $steps))

clean <- activities
clean[is.na(activities $steps),] $steps <- (stepsPerDayMean/288)
totalStepsClean <- aggregate(steps~date, data=clean, FUN=sum, na.rm=TRUE)
hist(totalStepsClean $steps)

totalStepsCleanMean <- mean(totalStepsClean $steps)
totalStepsCleanMedian <- median(totalStepsClean $steps)

clean $date <- as.Date(clean $date, format = "%Y-%m-%d")

clean $weekday = as.factor(ifelse(weekdays(clean$date) %in% c("Saturday", "Sunday"), "Weekday", "Weekend"))

weekday <- grep("Weekday", clean $weekday)
weekdayV <- clean[which(clean $weekday == "Weekday"),]
weekendV <- clean[which(clean $weekday == "Weekend"),]

min5AvgWeekday <- aggregate(steps~interval, data=weekdayV, FUN=mean, na.rm=TRUE)
min5AvgWeekend <- aggregate(steps~interval, data=weekendV, FUN=mean, na.rm=TRUE)

plot(x = min5AvgWeekday$interval, y = min5AvgWeekday$steps, type = "l")
plot(x = min5AvgWeekend$interval, y = min5AvgWeekend$steps, type = "l")


library(knitr)
render("PA1_template.Rmd")
install.packages("rmarkdown")
