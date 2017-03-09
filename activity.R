library(dplyr)
library(ggplot2)
#read the data table
activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))

#plot the histogram of steps taken each day
perday <- group_by(activity, date)
tgt <- summarise(perday, total = sum(steps, na.rm = T))
ggplot(data = tgt, aes(x = date)) +
      geom_bar(mapping = aes(y = total), stat = "identity")
#calculate the mean and median of the steps taken each day
meansteps <- mean(tgt$total)
mediansteps <- median(tgt$total)

#plot the steps taken in each interval
perinterval <- group_by(activity, interval)
daily <- summarise(perinterval, mean = mean(steps, na.rm = T))
daily$interval <- as.numeric(as.character(daily$interval))
ggplot(data = daily[order(as.numeric(as.character(daily$interval))), ]) +
      geom_line(mapping = aes(x = interval, y = mean), 
                size = 0.8, 
                color = "forestgreen",
                alpha = 0.6) +
      labs(x = "5-minute interval",
           y = "Average number of steps taken") +
      theme(axis.title = element_text(face = "bold", 
                                      color = "grey58", 
                                      family = "serif", 
                                      size = 14),
            axis.text = element_text(face = "bold", 
                                      color = "grey58", 
                                      family = "serif", 
                                      size = 10))
# find the interval that has the most steps
maxinterval <- daily[daily$mean == max(daily$mean), ][[1]]

#calculate the total NAs
nacount <- sum(is.na(activity$steps))
#create new dataset and fill the NAs with averaged steps taken per interval
nafilled <- activity
nafilled$steps <- apply(activity, 1, 
              function(x){
                    if(is.na(x[[1]])){
                          x[[1]] = daily[daily$interval == x[[3]],][[2]]
                    }
                    else{
                          x[[1]]
                    }
                    })
nafilled$steps <- as.numeric(nafilled$steps)
#plot the steps taken each day with filled NAs
perday_nafilled <- group_by(nafilled, date)
tgt_nafilled <- summarise(perday_nafilled, total = sum(steps, na.rm = T))
ggplot(data = tgt_nafilled, aes(x = date)) +
      geom_bar(mapping = aes(y = total), stat = "identity")
#calculate the mean and median steps taken with no NA
meansteps_nafilled <- mean(tgt_nafilled$total)
mediansteps_nafilled <- median(tgt_nafilled$total)

#create new variable with weekday and weekend
nafilled <- mutate(nafilled, 
                   day = as.factor(ifelse(weekdays(date) == "Saturday" | 
                                      weekdays(date) == "Sunday", 
                                "Weekend", "Weekday")))
#plot the number of steps of each interval for weekdays and weekends
perinterval_nafilled <- group_by(nafilled, interval, day)
daily_nafilled <- summarise(perinterval_nafilled, mean = mean(steps, na.rm = T))
daily_nafilled$interval <- as.numeric(as.character(daily_nafilled$interval))
ggplot(data = daily_nafilled) +
      geom_line(mapping = aes(x = interval, y = mean, color = day), size = 1, alpha = 0.6) +
      facet_grid(day ~.) +
      labs(x = "Interval",y = "Averaged steps") +
      theme(axis.title = element_text(hjust = 0.5, face="bold", size=14, 
                                      color = "grey58", family = "serif"),
            axis.text = element_text(hjust = 0.5, size=8, 
                                     color = "grey28", family = "serif"),
            strip.background = element_rect(fill = "orange"),
            strip.text = element_text(face="bold", 
                                      size=12, 
                                      color = "white", 
                                      family = "serif"),
            legend.position = "none")
  
      













