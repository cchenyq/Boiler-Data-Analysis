library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

get_daily_rt <- function(df, num_days) {   # num_days is the # of days in the month
  daily_rt <- vector()
  for (i in 1:num_days) {
    day <- subset(df, (day(df$Timestamp) == i))
    if (nrow(day) > 1) {
      t <- day[,1]
      status <- day[,2]
      sum_rt <- 0
      for (i in 2:length(status)) {
        start <- t[i-1] #previous index
        end <- t[i] # current index
        time.interval <- start %--% end
        time.duration <- as.duration(time.interval) #subtracting the start time from the end time
        if (status[i] == "Off" & (as.numeric(time.duration) / 60) < 6000) {
          sum_rt <- sum_rt + (as.numeric(time.duration) / 60)
        }
      }
    } else if (nrow(day) < 1) {
      sum_rt <- 0
    }
    daily_rt <- append(daily_rt, sum_rt, after = length(daily_rt))
  }
  return(daily_rt) #returns a vector of the runtime of the specific boiler for each day
}

get_daily_gas <- function(df, num_days) {
  #where num_days is the number of days in the month
  daily_gas <- vector()
  for (i in 1:num_days) {
    day <- subset(df, (day(df$Timestamp) == i))
    day <- na.omit(day)
    daily_gas <- append(daily_gas, sum(day$Gas), after = length(daily_gas))
  }
  return(daily_gas)
}

get_avg_status <- function(v) {
  #treat every stage as a pseudo boiler
  if (length(v[v > 0]) >= 15) {
    return(1)
  } else {
    return(0)
  }
}

gas_data <- read.table("hrlygas_341 Bloor.txt", header = TRUE, sep = "\t") #insert text file name here
colnames(gas_data) <- c("Timestamp", "Gas")
gas_data <- mutate(gas_data, Timestamp = dmy_hms(gas_data$Timestamp)) #reformatting the timestamp to make it easier to subset the data by date
usable <- filter(gas_data, (year(gas_data$Timestamp) == 2017 | year(gas_data$Timestamp) == 2018))
gas_jan <- filter(usable, (month(usable$Timestamp) == 1))
gas_feb <- filter(usable, (month(usable$Timestamp) == 2))
gas_mar <- filter(usable, (month(usable$Timestamp) == 3))
gas_apr <- filter(usable, (month(usable$Timestamp) == 4))
gas_dec <- filter(usable, (month(usable$Timestamp) == 12))

#boiler 1
b1_data <- read.table("cycle times_B1_341 Bloor.txt", header = TRUE, sep = "\t")
colnames(b1_data) <- c("Timestamp","Status")
b1_data <- mutate(b1_data, Timestamp = dmy_hms(b1_data$Timestamp))
b1_jan <- filter(b1_data, (month(b1_data$Timestamp) == 1))
b1_feb <- filter(b1_data, (month(b1_data$Timestamp) == 2))
b1_mar <- filter(b1_data, (month(b1_data$Timestamp) == 3))
b1_apr <- filter(b1_data, (month(b1_data$Timestamp) == 4))
b1_dec <- filter(b1_data, (month(b1_data$Timestamp) == 12))

#boiler 2
b2_data <- read.table("cycle times_B2_341 Bloor.txt", header = TRUE, sep = "\t")
colnames(b2_data) <- c("Timestamp","Status")
b2_data <- mutate(b2_data, Timestamp = ymd_hms(b2_data$Timestamp))
b2_jan <- filter(b2_data, (month(b2_data$Timestamp) == 1))
b2_feb <- filter(b2_data, (month(b2_data$Timestamp) == 2))
b2_mar <- filter(b2_data, (month(b2_data$Timestamp) == 3))
b2_apr <- filter(b2_data, (month(b2_data$Timestamp) == 4))
b2_dec <- filter(b2_data, (month(b2_data$Timestamp) == 12))

b1_rt <- mapply(get_daily_rt, list(b1_dec, b1_jan, b1_feb, b1_mar, b1_apr), num_days = c(31,31,28,31,30))
b2_rt <- mapply(get_daily_rt, list(b2_dec, b2_jan, b2_feb, b2_mar, b2_apr), num_days = c(31,31,28,31,30))
daily_gas <- mapply(get_daily_gas, list(gas_dec, gas_jan, gas_feb, gas_mar, gas_apr), num_days = c(31,31,28,31,30))
avg_daily_gas <- sapply(daily_gas, mean)
total_runtime <- mapply(sum, b1_rt, b2_rt, SIMPLIFY = FALSE)
avg_daily_runtime <- mapply(function (x,y) x/y, total_runtime, c(31,31,28,31,30))
avg_status_b1 <- sapply(b1_rt, get_avg_status)
avg_status_b2 <- sapply(b2_rt, get_avg_status)
avg_numOn <- avg_status_b1 + avg_status_b2

OBU <- 100*(avg_daily_runtime / (avg_numOn*1440)) #operating boiler utilization
TCU <- 100*(avg_daily_runtime / (2*1440)) #total capacity utilization
avg <- data.frame(c("Dec","Jan","Feb","Mar","Apr"), c(-3.9,-4.4,-0.8,1.0, 4.5), avg_daily_gas, avg_daily_runtime, OBU, TCU)
colnames(avg) <- c("Month", "OAT", "Average daily gas consumption", "Total average daily boiler runtime", "Average OBU", "Average TCU")
write.csv(avg, file = "341 Bloor - Daily Runtime, Gas, OBU, and TCU.csv", row.names = FALSE)