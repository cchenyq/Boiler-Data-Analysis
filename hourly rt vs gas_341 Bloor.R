library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

b1_data <- read.table("cycle times_B1_341 Bloor.txt", header = TRUE, sep = "\t")
colnames(b1_data) <- c("Timestamp","Status")
b1_data <- mutate(b1_data, Timestamp = dmy_hms(b1_data$Timestamp))
b1_jan <- filter(b1_data, (month(b1_data$Timestamp) == 1))
b1_feb <- filter(b1_data, (month(b1_data$Timestamp) == 2))
b1_mar <- filter(b1_data, (month(b1_data$Timestamp) == 3))
b1_apr <- filter(b1_data, (month(b1_data$Timestamp) == 4))
b1_dec <- filter(b1_data, (month(b1_data$Timestamp) == 12))

b2_data <- read.table("cycle times_B2_341 Bloor.txt", header = TRUE, sep = "\t")
colnames(b2_data) <- c("Timestamp","Status")
#b2_data <- mutate(b2_data, Timestamp = ymd_hms(b2_data$Timestamp))
b2_jan <- filter(b2_data, (month(b2_data$Timestamp) == 1))
b2_feb <- filter(b2_data, (month(b2_data$Timestamp) == 2))
b2_mar <- filter(b2_data, (month(b2_data$Timestamp) == 3))
b2_apr <- filter(b2_data, (month(b2_data$Timestamp) == 4))
b2_dec <- filter(b2_data, (month(b2_data$Timestamp) == 12))

gas_data <- read.csv(file="341 Bloor gas.csv", header=TRUE, sep=",")
gas_jan <- gas_data[,3]
gas_feb <- gas_data[,4]
gas_mar <- gas_data[,5]
gas_apr <- gas_data[,6]
gas_dec <- gas_data[,2]

get_hrly_runtimes <- function(df) {
  days <- split(df, day(df[,1]), drop = FALSE) #splitting each data frame (already organized by hour) by day
  rt_at_hour <- vector() #initializing a vector that will contain the runtime at a certain hour on each day of the month
  for (i in 1:length(days)) {
    temp <- days[[i]] #accessing each element in days
    rt_on_i <- 0 # where i is the day
    for (j in 1:length(temp)) {
      if (nrow(temp) > 1) {
        for (k in 2:nrow(temp)) {
          start <- temp[k-1,1]
          end <- temp[k,1]
          time.interval <- start %--% end #finding the difference between "on" and "off" timestamps
          time.duration <- as.duration(time.interval)
          if (temp[k,2] == "Off") {
            rt_on_i <- rt_on_i + as.numeric(time.duration) / 60
          }
        }
      }
    } 
    rt_at_hour <- c(rt_at_hour, rt_on_i) # appending to rt_at_hour with the runtime calculated for every subsequent day
  } 
  return(mean(rt_at_hour))
}

#splitting the data frame with runtimes over the monthly period by hour. Results in a list of data frames corresponding to the runtimes at each hour
b1_jan_hour <- split(b1_jan, hour(b1_jan$Timestamp), drop = FALSE) 
b1_hrly_rt_jan <- sapply(b1_jan_hour, get_hrly_runtimes) #applying the get_hrly_runtimes function to each element in b1_jan_hour
b2_jan_hour <- split(b2_jan, hour(b2_jan$Timestamp), drop = FALSE)
b2_hrly_rt_jan <- sapply(b2_jan_hour, get_hrly_runtimes)
january <- data.frame(b1_hrly_rt_jan, b2_hrly_rt_jan, gas_jan)
colnames(january) <- c("runtime_1", "runtime_2", "gas")

# aggregated boiler runtimes were used when generating scatter plots
attach(january)
plot(gas, runtime_1 + runtime_2, main="341 Bloor: Gas vs Runtime in January",
     xlab="gas", ylab="boiler runtime", pch=19) 

b1_feb_hour <- split(b1_feb, hour(b1_feb$Timestamp), drop = FALSE)
b1_hrly_rt_feb <- sapply(b1_feb_hour, get_hrly_runtimes)
b2_feb_hour <- split(b2_feb, hour(b2_feb$Timestamp), drop = FALSE)
b2_hrly_rt_feb <- sapply(b2_feb_hour, get_hrly_runtimes)
february <- data.frame(b1_hrly_rt_feb, b2_hrly_rt_feb, gas_feb)
colnames(february) <- c("runtime_1", "runtime_2", "gas")

attach(february)
plot(gas, runtime_1 + runtime_2, main="341 Bloor: Gas vs Runtime in February",
     xlab="gas", ylab="boiler runtime", pch=19)

b1_mar_hour <- split(b1_mar, hour(b1_mar$Timestamp), drop = FALSE)
b1_hrly_rt_mar <- sapply(b1_mar_hour, get_hrly_runtimes)
b2_mar_hour <- split(b2_mar, hour(b2_mar$Timestamp), drop = FALSE)
b2_hrly_rt_mar <- sapply(b2_mar_hour, get_hrly_runtimes)
march <- data.frame(b1_hrly_rt_mar, gas_mar)
colnames(march) <- c("runtime_1", "gas")

attach(march)
plot(gas, runtime_1, main="341 Bloor: Gas vs Runtime in March",
     xlab="gas", ylab="boiler runtime", pch=19)

b1_apr_hour <- split(b1_apr, hour(b1_apr$Timestamp), drop = FALSE)
b1_hrly_rt_apr <- sapply(b1_apr_hour, get_hrly_runtimes)
b2_apr_hour <- split(b2_apr, hour(b2_apr$Timestamp), drop = FALSE)
b2_hrly_rt_apr <- sapply(b2_apr_hour, get_hrly_runtimes)
april <- data.frame(b1_hrly_rt_apr, b2_hrly_rt_apr, gas_apr)
colnames(april) <- c("runtime_1", "runtime_2", "gas")

attach(april)
plot(gas, runtime_1 + runtime_2, main="341 Bloor: Gas vs Runtime in April",
     xlab="gas", ylab="boiler runtime", pch=19)