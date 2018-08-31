library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

# boiler 1
b1_data <- read.table("cycle times_B1_341 Bloor.txt", header = TRUE, sep = "\t")
colnames(b1_data) <- c("Timestamp","Status")
b1_data <- mutate(b1_data, Timestamp = dmy_hms(b1_data$Timestamp))
b1_dec <- filter(b1_data, (month(b1_data$Timestamp) == 12))
b1_jan <- filter(b1_data, (month(b1_data$Timestamp) == 1))
b1_feb <- filter(b1_data, (month(b1_data$Timestamp) == 2))
b1_mar <- filter(b1_data, (month(b1_data$Timestamp) == 3))
b1_apr <- filter(b1_data, (month(b1_data$Timestamp) == 4))

# boiler 2
b2_data <- read.table("cycle times_B2_341 Bloor.txt", header = TRUE, sep = "\t")
colnames(b2_data) <- c("Timestamp","Status")
b2_data <- mutate(b2_data, Timestamp = ymd_hms(b2_data$Timestamp))
b2_dec <- filter(b2_data, (month(b2_data$Timestamp) == 12))
b2_jan <- filter(b2_data, (month(b2_data$Timestamp) == 1))
b2_feb <- filter(b2_data, (month(b2_data$Timestamp) == 2))
b2_mar <- filter(b2_data, (month(b2_data$Timestamp) == 3))
b2_apr <- filter(b2_data, (month(b2_data$Timestamp) == 4))

# create two vectors: "on" and "off"
on_off_interval <- function(df,digit) { #returns the vector of on or off intervals depending on the number passed into the function
  # 0 = off, 1 = on
  t <- df[,1]
  status <- df[,2]
  on <- vector()
  off <- vector()
  for (i in 2:length(status)) {
    start <- t[i-1] #previous index
    end <- t[i] # current index
    time.interval <- start %--% end
    time.duration <- as.duration(time.interval) #subtracting the start time from the end time
    if (status[i] == "Off") {
      on <- append(on, as.numeric(time.duration), after = length(on))
    } else if (status[i] == "On") {
      off <- append(off, as.numeric(time.duration), after = length(off))
    }
  }
  if (digit == 0) {
    return(off[off < 10000])
  } else if (digit == 1) {
    return(on[on < 5000])
  }
}

filter <- function(v) {
  temp <- v
  for (i in 1:length(temp)) {
    if (temp[i] > (mean(temp) + sd(temp)) | temp[i] < (mean(temp) - sd(temp)) ) {
      v <- v[-i] #filtering out data more than one standard deviation from the mean
    }
  }
  # <- subset(v, v > 1) #for some reason, the program calculated a few of the cycle times to be 1 second, so I removed them from the set
  return(mean(v) / 60)
}

#Boiler 1
b1_on <- lapply(list(b1_dec, b1_jan, b1_feb, b1_mar, b1_apr), on_off_interval, digit = 1)
b1_off <- lapply(list(b1_dec, b1_jan, b1_feb, b1_mar, b1_apr), on_off_interval, digit = 0)
avg_on_b1 <- sapply(b1_on, filter)
avg_off_b1 <- sapply(b1_off, filter)

avgs_b1 <- data.frame(status = rep(c("On", "Off"), each=5),
                      months = rep(c("Dec","Jan","Feb","Mar","Apr"),2),
                      avg_cycle_len = append(avg_on_b1, avg_off_b1, after = 12), OAT = rep(c(-3.9, -4.4, -0.8, 1.0, 4.5)))

#Boiler 2
b2_on <- lapply(list(b2_dec, b2_jan, b2_feb, b2_mar, b2_apr), on_off_interval, digit = 1)
b2_off <- lapply(list(b2_dec, b2_jan, b2_feb, b2_mar, b2_apr), on_off_interval, digit = 0)
avg_on_b2 <- sapply(b2_on, filter)
avg_off_b2 <- sapply(b2_off, filter)

avgs_b2 <- data.frame(status = rep(c("On", "Off"), each=5),
                      months = rep(c("Dec","Jan","Feb","Mar","Apr"),2),
                      avg_cycle_len = append(avg_on_b2, avg_off_b2, after = 12), OAT = rep(c(-3.9, -4.4, -0.8, 1.0, 4.5)))

#aggregating boilers 1 and 2
total_avgs <- data.frame(status = rep(c("On", "Off"), each=5),
                         months = rep(c("Dec","Jan","Feb","Mar","Apr"),2),
                         avg_cycle_len = append(avg_on_b1 + avg_on_b2, avg_off_b1 + avg_off_b2, after = 12), OAT = rep(c(-3.9, -4.4, -0.8, 1.0, 4.5)))

write.csv(avgs_b1, file = "341 Bloor - B1 cycles.csv", row.names = FALSE)
write.csv(avgs_b2, file = "341 Bloor - B2 cycles.csv", row.names = FALSE)
write.csv(total_avgs, file = "341 Bloor - aggregated cycles.csv", row.names = FALSE)

#plotting
ggplot(data=avgs_b1, aes(x=months)) + scale_x_discrete(name ="months", limits=c("Dec","Jan","Feb","Mar","Apr")) + ggtitle("341 Bloor: Boiler 1 Cycle Lengths by Month") +
  geom_bar(aes(y = avg_cycle_len, fill=status), stat="identity") + geom_line(aes(x = months, y = OAT*(300/60)), stat="identity", colour = "black") + geom_point(aes(x = months, y = OAT*(300/60)), size = 2, shape = 21, fill = "black") + 
  scale_y_continuous(name = expression("Average cycle length (min)"), sec.axis = sec_axis(~ . * 60 / 300 , name = "Outdoor Average Temperature")) 

ggplot(data=avgs_b2, aes(x=months)) + scale_x_discrete(name ="months", limits=c("Dec","Jan","Feb","Mar","Apr")) + ggtitle("341 Bloor: Boiler 2 Cycle Lengths by Month") +
  geom_bar(aes(y = avg_cycle_len, fill=status), stat="identity") + geom_line(aes(x = months, y = OAT*(300/40)), stat="identity", colour = "black") + geom_point(aes(x = months, y = OAT*(300/40)), size = 2, shape = 21, fill = "black") + 
  scale_y_continuous(name = expression("Average cycle length (min)"), sec.axis = sec_axis(~ . * 40 / 300 , name = "Outdoor Average Temperature")) 

ggplot(data=total_avgs, aes(x=months)) + scale_x_discrete(name ="months", limits=c("Dec","Jan","Feb","Mar","Apr")) + ggtitle("341 Bloor: Aggregate Boiler Lengths by Month") +
  geom_bar(aes(y = avg_cycle_len, fill=status), stat="identity") + geom_line(aes(x = months, y = OAT*(300/40)), stat="identity", colour = "black") + geom_point(aes(x = months, y = OAT*(300/40)), size = 2, shape = 21, fill = "black") + 
  scale_y_continuous(name = expression("Average cycle length (min)"), sec.axis = sec_axis(~ . * 40 / 300 , name = "Outdoor Average Temperature")) 

# https://whatalnk.github.io/r-tips/ggplot2-secondary-y-axis.nb.html