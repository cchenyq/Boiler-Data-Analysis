#history:Parma90/Prm Hot Water Supply vs. Return Temp
#14-Nov-17 2:10:00 PM EST to 28-Feb-18 11:40:00 AM EST

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

flow_data <- read.table("proxy demand_341 Bloor.txt", header = TRUE, sep = "\t")
colnames(flow_data) <- c("Timestamp", "HWS Temp", "HWR Temp")
flow_data <- mutate(flow_data, Timestamp = dmy_hms(flow_data$Timestamp))

# nov_data <- filter(flow_data, month(flow_data$Timestamp) == 11) 
dec_data <- filter(flow_data, month(flow_data$Timestamp) == 12)
jan_data <- filter(flow_data, month(flow_data$Timestamp) == 1)
feb_data <- filter(flow_data, month(flow_data$Timestamp) == 2)
mar_data <- filter(flow_data, month(flow_data$Timestamp) == 3)
apr_data <- filter(flow_data, month(flow_data$Timestamp) == 4)

hws_hwr <- function(df) { #takes in a data frame and outputs a data frame containing the columns "hour", "HWS temp", and "HWR temp"
  avg_hws <- rep(0,24)
  avg_hwr <- rep(0,24)
  accum <- rep(0,24)
  t <- df[,1] #time vector
  hws <- df[,2] #HW supply temp vector
  hwr <- df[,3] #HW return temp vector
  
  for (i in 1:length(hws)) {
    h <- c(0:23)
    if (is.nan(hws[i]) == FALSE & is.nan(hwr[i]) == FALSE) {
      if (hour(t[i]) == 0) {
        avg_hws[1] <- avg_hws[1] + hws[i]
        avg_hwr[1] <- avg_hwr[1] + hwr[i]
        accum[1] <- accum[1] + 1
      } else if (hour(t[i]) == 1) {
        avg_hws[2] <- avg_hws[2] + hws[i]
        avg_hwr[2] <- avg_hwr[2] + hwr[i]
        accum[2] <- accum[2] + 1
      } else if (hour(t[i]) == 2) {
        avg_hws[3] <- avg_hws[3] + hws[i]
        avg_hwr[3] <- avg_hwr[3] + hwr[i]
        accum[3] <- accum[3] + 1
      } else if (hour(t[i]) == 3) {
        avg_hws[4] <- avg_hws[4] + hws[i]
        avg_hwr[4] <- avg_hwr[4] + hwr[i]
        accum[4] <- accum[4] + 1
      } else if (hour(t[i]) == 4) {
        avg_hws[5] <- avg_hws[5] + hws[i]
        avg_hwr[5] <- avg_hwr[5] + hwr[i]
        accum[5] <- accum[5] + 1
      } else if (hour(t[i]) == 5) {
        avg_hws[6] <- avg_hws[6] + hws[i]
        avg_hwr[6] <- avg_hwr[6] + hwr[i]
        accum[6] <- accum[6] + 1
      } else if (hour(t[i]) == 6) {
        avg_hws[7] <- avg_hws[7] + hws[i]
        avg_hwr[7] <- avg_hwr[7] + hwr[i]
        accum[7] <- accum[7] + 1
      } else if (hour(t[i]) == 7) {
        avg_hws[8] <- avg_hws[8] + hws[i]
        avg_hwr[8] <- avg_hwr[8] + hwr[i]
        accum[8] <- accum[8] + 1
      } else if (hour(t[i]) == 8) {
        avg_hws[9] <- avg_hws[9] + hws[i]
        avg_hwr[9] <- avg_hwr[9] + hwr[i]
        accum[9] <- accum[9] + 1
      } else if (hour(t[i]) == 9) {
        avg_hws[10] <- avg_hws[10] + hws[i]
        avg_hwr[10] <- avg_hwr[10] + hwr[i]
        accum[10] <- accum[10] + 1
      } else if (hour(t[i]) == 10) {
        avg_hws[11] <- avg_hws[11] + hws[i]
        avg_hwr[11] <- avg_hwr[11] + hwr[i]
        accum[11] <- accum[11] + 1
      } else if (hour(t[i]) == 11) {
        avg_hws[12] <- avg_hws[12] + hws[i]
        avg_hwr[12] <- avg_hwr[12] + hwr[i]
        accum[12] <- accum[12] + 1
      } else if (hour(t[i]) == 12) {
        avg_hws[13] <- avg_hws[13] + hws[i]
        avg_hwr[13] <- avg_hwr[13] + hwr[i]
        accum[13] <- accum[13] + 1
      } else if (hour(t[i]) == 13) {
        avg_hws[14] <- avg_hws[14] + hws[i]
        avg_hwr[14] <- avg_hwr[14] + hwr[i]
        accum[14] <- accum[14] + 1
      } else if (hour(t[i]) == 14) {
        avg_hws[15] <- avg_hws[15] + hws[i]
        avg_hwr[15] <- avg_hwr[15] + hwr[i]
        accum[15] <- accum[15] + 1
      } else if (hour(t[i]) == 15) {
        avg_hws[16] <- avg_hws[16] + hws[i]
        avg_hwr[16] <- avg_hwr[16] + hwr[i]
        accum[16] <- accum[16] + 1
      } else if (hour(t[i]) == 16) {
        avg_hws[17] <- avg_hws[17] + hws[i]
        avg_hwr[17] <- avg_hwr[17] + hwr[i]
        accum[17] <- accum[17] + 1
      } else if (hour(t[i]) == 17) {
        avg_hws[18] <- avg_hws[18] + hws[i]
        avg_hwr[18] <- avg_hwr[18] + hwr[i]
        accum[18] <- accum[18] + 1
      } else if (hour(t[i]) == 18) {
        avg_hws[19] <- avg_hws[19] + hws[i]
        avg_hwr[19] <- avg_hwr[19] + hwr[i]
        accum[19] <- accum[19] + 1
      } else if (hour(t[i]) == 19) {
        avg_hws[20] <- avg_hws[20] + hws[i]
        avg_hwr[20] <- avg_hwr[20] + hwr[i]
        accum[20] <- accum[20] + 1
      } else if (hour(t[i]) == 20) {
        avg_hws[21] <- avg_hws[21] + hws[i]
        avg_hwr[21] <- avg_hwr[21] + hwr[i]
        accum[21] <- accum[21] + 1
      } else if (hour(t[i]) == 21) {
        avg_hws[22] <- avg_hws[22] + hws[i]
        avg_hwr[22] <- avg_hwr[22] + hwr[i]
        accum[22] <- accum[22] + 1
      } else if (hour(t[i]) == 22) {
        avg_hws[23] <- avg_hws[23] + hws[i]
        avg_hwr[23] <- avg_hwr[23] + hwr[i]
        accum[23] <- accum[23] + 1
      } else if (hour(t[i]) == 23) {
        avg_hws[24] <- avg_hws[24] + hws[i]
        avg_hwr[24] <- avg_hwr[24] + hwr[i]
        accum[24] <- accum[24] + 1
      }
    }
  }
  for (i in 1:24) {
    avg_hws[i] <- avg_hws[i] / accum[i]
    avg_hwr[i] <- avg_hwr[i] / accum[i]
  }
  m <- data.frame(h,avg_hws,avg_hwr)
  return(m)
}

#nov <- hws_hwr(nov_data)
#summary(nov)
dec <- hws_hwr(dec_data)
summary(dec)
jan <- hws_hwr(jan_data)
summary(jan)
feb <- hws_hwr(feb_data)
summary(feb)
mar <- hws_hwr(mar_data)
summary(mar)
apr <- hws_hwr(apr_data)
summary(apr)
write.csv(rbind(dec,jan,feb,mar,apr), file = "341 Bloor - proxy demand 12.17 to 04.18.csv", row.names = FALSE)


ggplot(dec,aes(x = dec[,1])) + geom_line(aes(y = dec[,2], colour = "avg_hws"), size=2) + geom_line(aes(y = dec[,3], colour = "avg_hwr"), size=2) + 
  geom_line(aes(y = (dec[,2]-dec[,3]) + 68, colour = "delta_t"), size=1) + scale_colour_manual(name='', values = c('avg_hws' = 'red', 'avg_hwr' = 'blue', 'delta_t' = 'black')) + 
  coord_cartesian(xlim=c(0,23), ylim = c(68,78)) + xlab("Time (h)") + ggtitle("341 Bloor: Proxy Demand - December") + scale_y_continuous(name = expression("Temperature (C)"), sec.axis = sec_axis(~ . -68, name = "delta T"))

ggplot(jan,aes(x = jan[,1])) + geom_line(aes(y = jan[,2], colour = "avg_hws"), size=2) + geom_line(aes(y = jan[,3], colour = "avg_hwr"), size=2) + 
  geom_line(aes(y = (jan[,2]-jan[,3]) +78, colour = "delta_t"), size=1) + scale_colour_manual(name='', values = c('avg_hws' = 'red', 'avg_hwr' = 'blue', 'delta_t' = 'black')) + 
  coord_cartesian(xlim=c(0,23), ylim = c(77,87)) + xlab("Time (h)") + ggtitle("341 Bloor: Proxy Demand - January") + scale_y_continuous(name = expression("Temperature (C)"), sec.axis = sec_axis(~ . -78, name = "delta T"))

ggplot(feb,aes(x = feb[,1])) + geom_line(aes(y = feb[,2], colour = "avg_hws"), size=2) + geom_line(aes(y = feb[,3], colour = "avg_hwr"), size=2) + 
  geom_line(aes(y = (feb[,2]-feb[,3]) +67, colour = "delta_t"), size=1) + scale_colour_manual(name='', values = c('avg_hws' = 'red', 'avg_hwr' = 'blue', 'delta_t' = 'black')) + 
  coord_cartesian(xlim=c(0,23), ylim = c(69,76)) + xlab("Time (h)") + ggtitle("341 Bloor: Proxy Demand - February") + scale_y_continuous(name = expression("Temperature (C)"), sec.axis = sec_axis(~ . -67, name = "delta T"))

ggplot(mar,aes(x = mar[,1])) + geom_line(aes(y = mar[,2], colour = "avg_hws"), size=2) + geom_line(aes(y = mar[,3], colour = "avg_hwr"), size=2) + 
  geom_line(aes(y = (mar[,2]-mar[,3]) +65, colour = "delta_t"), size=1) + scale_colour_manual(name='', values = c('avg_hws' = 'red', 'avg_hwr' = 'blue', 'delta_t' = 'black')) + 
  coord_cartesian(xlim=c(0,23), ylim = c(66,74)) + xlab("Time (h)") + ggtitle("341 Bloor: Proxy Demand - March") + scale_y_continuous(name = expression("Temperature (C)"), sec.axis = sec_axis(~ . -65, name = "delta T"))

ggplot(apr,aes(x = apr[,1])) + geom_line(aes(y = apr[,2], colour = "avg_hws"), size=2) + geom_line(aes(y = apr[,3], colour = "avg_hwr"), size=2) + 
  geom_line(aes(y = (apr[,2]-apr[,3]) +65, colour = "delta_t"), size=1) + scale_colour_manual(name='', values = c('avg_hws' = 'red', 'avg_hwr' = 'blue', 'delta_t' = 'black')) + 
  coord_cartesian(xlim=c(0,23), ylim = c(66,74)) + xlab("Time (h)") + ggtitle("341 Bloor: Proxy Demand - April") + scale_y_continuous(name = expression("Temperature (C)"), sec.axis = sec_axis(~ . -65, name = "delta T"))