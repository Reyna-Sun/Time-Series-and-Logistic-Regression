#REYNA VARGAS ANTONIO
#ID: X23127635
#==================================
# LIBRARIES
#=================================
install.packages("ggplottimeseries")
library(dplyr) 
library(ggplot2)
library(fpp2)
library(tseries)
library(ggfortify)
library(zoo)
library(forecast)
library(GGally)
library(tidyr)
library(devtools)
library("imputeTS")

#==================================
#TIME SERIES ANALYSIS
#=================================

#---------------------------------
# LOADING DATA
#---------------------------------
# Switch your working directory to where ever you have downloaded the file.
setwd('C:/Users/reyna/Documents/MSc in Data Analytics/Statistics for data analytics/0.1 Final Project')

# Read in our csv and put it in a data.frame.
weather <- read.csv('weather_revised.csv',  header=T, na.strings=c(""), stringsAsFactors = T)
print(weather)

#Variable according the last number of student ID (x23127635): 5->gmin - Grass Minimum Temperature (C)
df<-weather[c(1,4)]
str(df$date)
str(df$gmin.Grass.Minimum.Temperature...degrees.C.)

plot(df$gmin.Grass.Minimum.Temperature...degrees.C.)
hist(x = df$gmin.Grass.Minimum.Temperature...degrees.C.,
     main = "Grass Minimun Temperature (C)",
     xlab = "Temperature",
     ylab = "Frequency",
     breaks = 20, # 20 Bins
     col = "cadetblue4", # Filling Color
     border = "white") # Border Color

boxplot(df$gmin.Grass.Minimum.Temperature...degrees.C.,
        main="Grass Minimun Temperature (C) Boxplot",
        ylab="Temperature (C)",
        xlab="No.of observations",
        col="cadetblue4",
        notch=TRUE)

summary(df)
class(df)

#extract last row in data frame
last_row <- tail(df, n=5)
last_row
#==================================
#Creating a Time Series Object containing 29,889 daily observations
#from January 1942 to October 2023
#==================================
dfts<-ts(df$gmin.Grass.Minimum.Temperature...degrees.C., start =c(1942,1), frequency=365)
class(dfts)
last_row2 <- tail(dfts, n=5)
last_row2

start(dfts)
end(dfts)
frequency(dfts)
#=================================
# 1. Plot The Time Series
#=================================
autoplot(dfts)+ ggtitle("Grass Minimum Temperature (C)")+ xlab("Year") + ylab("Temperature (C)")
boxplot(dfts~cycle(dfts))

#Horizontal or Stationary trend: If no pattern observed then it is called a Horizontal or stationary trend.
#has not trend, seasonality or cyclic behavior.at least, at first view

#=================================
# Subset the time series (January  2019 to 2022)
#=================================
df_fit<- window(dfts, start=c(2019,1), end=c(2022,365))
autoplot(df_fit)+ ggtitle("Grass Minimum Temperature (C)")+ xlab("Year") + ylab("Temperature (C)")
boxplot(df_fit~cycle(df_fit))
tail(df_fit, n=5)
#there is a horizontal pattern, which means the data fluctuate around a constant mean, showing a stationary trend,
#the variability of the time series is a constant over time.Additional, it seams the time series is affected by a seasonal factor,
#having a increase in similar time y decreasing in the last months of the year.

#---------------------------------
# Missing values
#---------------------------------
ggplot_na_distribution(df_fit)
statsNA(df_fit)

#---------------------------------
# Decomposition of the Time Serie
#---------------------------------
ggmonthplot(df_fit)
ggseasonplot(df_fit)

#seasonal decomposition using additive
df_fit.decom<- decompose(df_fit, type = "additive")
autoplot(df_fit.decom)

ggtsdisplay(df_fit)

#=================================
# Subset the time series (January  2023 to October 2023)
#=================================
df_eva<- window(dfts, start=c(2023,1), end=c(2023,324))
autoplot(df_eva) + ggtitle("Grass Minimum Temperature (C)")+ xlab("Year") + ylab("Temperature (C)")
boxplot(df_eva~cycle(df_eva))
tail(df_eva, n=5)

#=================================
# SIMPLE TIME SERIE
#=================================
#---------------------------------
# Simple Moving Average
#---------------------------------
#Plot a simple moving average
autoplot(df_fit, main="Raw Time Series")
autoplot(ma(df_fit,5))
autoplot(ma(df_fit,12))

autoplot(df_fit)+ autolayer(ma(df_fit,5))+ autolayer(ma(df_fit,12))
ggtsdisplay(df_fit)

#Fitting and comparing some simple time series models
#Comparing with 'Basic' models
#mean model
fcast_mean<- meanf(df_eva, h=5)
summary(fcast_mean)

autoplot(fcast_mean)

#---------------------------------
# Naive Model
#---------------------------------
fcas_naive<- naive(df_eva, h=5)
summary(fcas_naive)

autoplot(fcas_naive)

#=================================
# EXPONENTIAL SMOOTHING
#=================================
#Hot exponential smoothing
fcas_holt<- holt(df_eva, h=5)
summary(fcas_holt)
fcas_holt$model

autoplot(fcas_holt)

#---------------------------------
#Simple exponential smoothing
fcas_ses<- ses(df_eva, h=5)
summary(fcas_ses)

autoplot(fcas_ses)

#---------------------------------
# ARIMA/ SARIMA
#---------------------------------
ggtsdisplay(df_fit)
#Augmented Dickey-Fuller Test
adf.test(df_fit)

autoplot(df_fit)
acf(df_fit)
pacf(df_fit)

#first iteration
difserie<- diff(df_fit)
autoplot(difserie)
ggtsdisplay(difserie)

acf(difserie)
pacf(difserie)

#second iteration
difserie2<- diff(difserie)
autoplot(difserie2)
ggtsdisplay(difserie2)

acf(difserie2)
pacf(difserie2)

auto.arima(df_fit)

#---------------------------------
# PREDICTION
#---------------------------------

model<- Arima(df_eva, order = c(4,1,1))
summary(model)

checkresiduals(model)

model%>%forecast(h=12)%>%autoplot()

forecast(model, h=4)

#Now use CrossValidation and compare RMSE
cross<-df_eva %>% tsCV(forecastfunction=rwf, drift=TRUE, h=1)
cross^2 %>% mean(na.rm=TRUE) %>% sqrt()

