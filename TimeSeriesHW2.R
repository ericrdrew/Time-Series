#Time Series HW2
#Date: 9/2/2022
#Author: Blue Team #12\
#packages
library(tidyverse)
library(tseries)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(ggplot2)
library(seasonalview)
library(aTSA)
library(imputeTS)
library(reticulate)

#read in data
data <- read.csv('https://raw.githubusercontent.com/sjsimmo2/TimeSeries/master/AECO_hourly%20data.csv')

data2 <- data %>%
  rowwise()%>%
  mutate(date = str_split(datetime_beginning_ept," ")[[1]][1])%>%
  group_by(date)%>%
  summarise(TOTALMW = sum(mw))

data2$date <- as.Date(data2$date,"%m/%d/%Y")
data2 = data2[order(data2$date),]


#create time series object
energy <- ts(data2$TOTALMW, frequency=7)

#split up train,test,and validation data
training = subset(energy, end=length(energy)-21)
validation = subset(energy, start=344, end=357)
test = subset(energy, start=358)


#plot decomposition with the values overlaid with the trend component
decomp_energy <- stl(training, s.window=7)
autoplot(decomp_energy)

#Make first plot 
TrainMW = as.numeric(training)
TREND = as.numeric(decomp_energy$time.series[,2])
dates = data2[1:343,'date']
firstplot = cbind(dates,TrainMW, TREND)
colors <- c("Daily MW" = 'black', "Trend Component" = 'red')


ggplot(firstplot, aes(x= date)) + geom_line(aes(y=TrainMW/1000, color='Daily MW'), alpha=.4) + scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle=45, hjust= 1)) +
  labs(x= "Date", y= "Megawatts (Thousands)",
       title= "Total Daily Energy Usage (Aug. 1, 2021 - Jul. 30, 2022)") + 
  geom_line(aes(y=TREND/1000), color='red') + scale_color_manual(values=colors) + labs(color='Legend')


#Fit SES model
SES.energy <- ses(training, initial='simple', h=24)
summary(SES.energy)

test.results=forecast::forecast(SES.energy,h=14)
test.results$mean

error = validation-as.numeric(test.results$mean)
MAE = mean(abs(error))
MAE

MAPE = mean(abs(error)/abs(validation))
MAPE

autoplot(SES.energy)+
  autolayer(fitted(SES.energy),series="Fitted")+ylab("Daily Energy Consumption(MW)") +
  geom_vline(xintercept = 49.9,color="orange",linetype="dashed")

round(accuracy(SES.energy,2))

#######
#Holt ESM
LES.energy <- holt(training, initial='optimal', h=24)
summary(LES.energy)
autoplot(LES.energy)+
  autolayer(fitted(LES.energy),series="Fitted")+ylab("Daily Energy Consumption(MW)") +
  geom_vline(xintercept = 49.9,color="orange",linetype="dashed")
round(accuracy(LES.energy),2)

test.results=forecast::forecast(LES.energy,h=14)

error = validation-as.numeric(test.results$mean)
MAE = mean(abs(error))
MAE

MAPE = mean(abs(error)/abs(validation))
MAPE

#Damped Holt eSM
LDES.energy <- holt(training, initial='optimal', h=24, damped=TRUE)
summary(LDES.energy)
autoplot(LDES.energy)+
  autolayer(fitted(LDES.energy),series="Fitted")+ylab("Daily Energy Consumption(MW)") +
  geom_vline(xintercept = 49.9,color="orange",linetype="dashed")
round(accuracy(LDES.energy),2)

test.results=forecast::forecast(LDES.energy,h=14)
test.results$mean

error = validation-as.numeric(test.results$mean)
MAE = mean(abs(error))
MAE

MAPE = mean(abs(error)/abs(validation))
MAPE

###
#winters smoothing(additive)
HWES.energy <- hw(training, seasonal='additive')
summary(HWES.energy)
autoplot(HWES.energy)+
  autolayer(fitted(HWES.energy),series="Fitted")+ylab("Daily Energy Consumption(MW)") +
  geom_vline(xintercept = 49.9,color="orange",linetype="dashed")
round(accuracy(HWES.energy),2)


test.results=forecast::forecast(HWES.energy,h=14)

error = validation-as.numeric(test.results$mean)
MAE = mean(abs(error))
MAE

MAPE = mean(abs(error)/abs(validation))
MAPE

#winters smoothing(multiplicative)
HWESm.energy <- hw(training, seasonal='multiplicative')
summary(HWESm.energy)
autoplot(HWESm.energy)+
  autolayer(fitted(HWESm.energy),series="Fitted")+ylab("Daily Energy Consumption(MW)") +
  geom_vline(xintercept = 49.9,color="orange",linetype="dashed")
round(accuracy(HWESm.energy),2)


#predict 14 days ahead to compare with validation set
test.results=forecast::forecast(HWESm.energy,h=14)

error = validation-as.numeric(test.results$mean)
MAE = mean(abs(error))
MAE

MAPE = mean(abs(error)/abs(validation))
MAPE

# plot forecast and validation
test_val <- cbind(test.results$mean, validation)

# create dataframe to pass to ggplot
forecast_val_df <- as_tibble(test_val)

# add back date column to the 
forecast_val_df <- forecast_val_df %>% 
  mutate(date = data2 %>% slice(344:357) %>% pull(date))

# rename columns
colnames(forecast_val_df) <- c("forecast", "validation", "date")

# scale the forecast and validation numbers to thousands
forecast_val_df <- forecast_val_df %>% 
  mutate(forecast = forecast / 1000,
         validation = validation / 1000)


# create line plot using ggplot with dates
ggplot(forecast_val_df) + 
  geom_line(aes(x= date, y= forecast, color = "Forecast"), size = 1, linetype = "dashed") + 
  geom_line(aes(x= date, y= validation, color = "Validation"), size = 1) + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b %Y") +
  theme(axis.text.x = element_text(angle=45, hjust= 1)) +
  labs(x= "Date", y= "Megawatts (Thousands)", title= "Forecasted Total Daily Energy Usage (Jul. 10, 2022 - Jul. 23, 2022)") +
  scale_color_manual(name = "Line", values = c("Forecast" = "red", "Validation" = "gray"))

###
#ETS
ets.energy <- ets(training)
summary(ets.energy)
autoplot(ets.energy)+
  autolayer(fitted(ets.energy),series="Fitted")+ylab("Daily Energy Consumption(MW)") +
  geom_vline(xintercept = 49.9,color="orange",linetype="dashed")

round(accuracy(ets.energy),2)

test.results <- forecast::forecast(ets.energy, h=14)
error= validation-as.numeric(test.results$mean)
MAE = mean(abs(error))
MAE
MAPE = mean(abs(error)/abs(validation))
MAPE
##########    ETS with multiplicative error/season, no trend term has lowest MAPE   ########




#Roll training and validation sets back together for final modeling/testing
trainingFinal = subset(energy, end=length(energy)-7)

#re-fit the holt winters multiplicative
#winters smoothing(multiplicative)
HWESm.energy <- hw(trainingFinal, seasonal='multiplicative')
summary(HWESm.energy)
autoplot(HWESm.energy)+
  autolayer(fitted(HWESm.energy),series="Fitted")+ylab("Daily Energy Consumption(MW)") +
  geom_vline(xintercept = 51.9,color="orange",linetype="dashed")

#forecast 7 out
test.results=forecast::forecast(HWESm.energy,h=7)
test.results$mean

#find MAE and MAPE on test set
error = test-as.numeric(test.results$mean)
MAE = mean(abs(error))
MAE

MAPE = mean(abs(error)/abs(test))
MAPE

#ETS final testing
ets.Final <- ets(trainingFinal, model='MNN')
summary(ets.Final)
test.results=forecast::forecast(ets.Final,h=7)
error = test-as.numeric(test.results$mean)
MAE = mean(abs(error))
MAE

MAPE = mean(abs(error)/abs(test))
MAPE

autoplot(ets.Final)+
  autolayer(fitted(HWESm.energy),series="Fitted")+ylab("Daily Energy Consumption(MW)") +
  geom_vline(xintercept = 51.9,color="orange",linetype="dashed")


