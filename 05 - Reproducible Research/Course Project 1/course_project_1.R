setwd('C:/Users/eugene_lamb/Projects/Data Science/Reproducible Research/Course Project 1/')

# Loading and preprocessing the data

activity <- read.csv('activity.csv')
activity$date <- as.Date(activity$date)

activity_non_null <- subset(activity, !is.na(activity$steps))

library(sqldf)
step_days <- sqldf('SELECT DATE, SUM(STEPS) AS TOTAL_STEPS 
                        FROM activity_non_null
                        GROUP BY DATE')
plot(x=step_days$date, 
     y=step_days$TOTAL_STEPS, 
     type='h', 
     lwd = 10, 
     main = 'Steps Taken Per Day', 
     xlab = 'Day', 
     ylab = 'Number of Steps', 
     col = 'red')

avg_step_intervals <- sqldf('SELECT INTERVAL, AVG(STEPS) AS AVG_STEPS
                                 FROM activity_non_null
                            GROUP BY INTERVAL')
plot(x=avg_step_intervals$interval, 
     y=avg_step_intervals$AVG_STEPS, 
     type='h', 
     lwd = 3, 
     main = 'Average Number of Steps Per Day (in 5-minute intervals)',,
     xlab = 'Time', 
     ylab = 'Avg. Number of Steps', 
     col = 'blue')
