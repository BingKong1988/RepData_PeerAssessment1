activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))

library(dplyr)
library(ggplot2)
perday <- group_by(activity, date)
tgt <- summarise(perday, total = sum(steps, na.rm = T))
ggplot(data = tgt, aes(x = date)) +
      geom_bar(mapping = aes(y = total), stat = "identity")
meansteps <- mean(tgt$total[tgt$total != 0])
mediansteps <- median(tgt$total[tgt$total != 0])

perinterval <- group_by(activity, interval)
daily <- summarise(perinterval, mean = mean(steps, na.rm = T))
daily$interval <- as.numeric(as.character(daily$interval))
ggplot(data = daily[order(as.numeric(as.character(daily$interval))), ]) +
      geom_line(mapping = aes(x = seq(1, nrow(daily), 1), y = mean), 
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
maxinterval <- daily[daily$mean == max(daily$mean), ][[1]]
