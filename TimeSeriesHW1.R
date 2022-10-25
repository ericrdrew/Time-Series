#TimeSeries Homework 1
#Date: 8/26/2022
#Author: Eric Drew
#library
library(tidyverse)
library(ggfortify)
library(stringr)

#read in the data
data <- read.csv('https://raw.githubusercontent.com/sjsimmo2/TimeSeries/master/AECO_hourly%20data.csv')

#roll up data from hourly into daily data
data$day <- ''
data$month <- ''
data$year <- ''

for(i in 1:nrow(data)){
data$day[i] <- str_split(data$datetime_beginning_ept, '/')[[i]][2]
data$month[i] <- str_split(data$datetime_beginning_ept, '/')[[i]][1]
data$year[i] <- str_split(data$datetime_beginning_ept, '/')[[i]][3]
data$year[i] <- str_split(data$year, ' ')[[i]][1]
}

data$day <- as.numeric(data$day)
data$month <- as.numeric(data$month)
data$year <- as.numeric(data$year)


dayTot <- data %>% group_by(year,month,day) %>% summarise(TOTALMW =sum(mw))


median(dayTot$TOTALMW)

#create time series object
energy <- ts(dayTot$TOTALMW, frequency=7)
decomp_stl <- stl(energy, s.window=7)
autoplot(decomp_stl)


# time plot of TOTALMW
autoplot(energy)+ ggtitle('Time Plot for Total Daily MW')+
  xlab('Weeks From Aug 1 2021') + ylab('Total Daily MW')


