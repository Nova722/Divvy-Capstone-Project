data = read.csv("C:/Users/chris/Desktop/Divvy/Divvy_Trips.csv", header = TRUE, sep = ",")
library(tidyr)
library(lubridate)
library(dplyr)
library(png)

#Created subsets in order to further explore trips to and from this station
FROM <- subset(data, FROM.STATION.NAME == "Lake Shore Dr & Monroe St")
TO <- subset(data, TO.STATION.NAME == "Lake Shore Dr & Monroe St")

##FROM Data Prep
#created a df with start.time only
FROMts <- FROM[ -c(1, 3:22) ]

#removed the time element, only date is relevant since this is a daily forecast
FROMts <- separate(FROMts, START.TIME, c("start.date", "start.time", "start.time.ampm"), sep = " ")
FROMts <- FROMts[ -c(2:3) ]

FROMts$start.date <- mdy(FROMts$start.date)
str(FROMts)
FROMtstest <- FROMts

FROMts$year <- year(FROMts$start.date)
FROMts$month <- month(FROMts$start.date)
FROMts$day <- day(FROMts$start.date)

#created the date and count of daily observations table
dailyFROM <- FROMts %>%
  group_by(year, month, day)

FROMper_day <- summarize(dailyFROM, daily_observations = n())
FROMper_day <- unite(FROMper_day, "date", year,month,day, sep = "-")

#exported to excel to remove leap years and add missing days with the average from the previous day
write.csv(FROMper_day, "FROM_Divvy.csv")
#imported the clean data
FROMper_day = read.csv("C:/Users/chris/Documents/GitHub/Divvy-Capstone-Project/FROM_Divvy.csv", header = TRUE, sep = ",")
#1644 observation lines on the clean data

#Formatted to a time object
FROMper_day$date <- mdy(FROMper_day$date)
str(FROMper_day)
head(FROMper_day)

train.FROM = FROMper_day[1:1524,]
test.FROM = FROMper_day[1525:1644,] #forecast 4 months out

library(forecast)
library(TSA)

#Checked for seasonality
date_ts <- ts(train.FROM$daily_observations,frequency = 1)
p <- periodogram(date_ts)
m <- p$freq[which.max(p$spec)]
#Find the seasonality in the data
seasonality <- 1/m
seasonality #seasonality of 384
1/p$freq[order(p$spec)]

#created the ts formatted to recognize the time
inds <- seq(as.Date("2013-06-27"), as.Date("2017-8-30"), by = "day")

train1 <- ts(train.FROM$daily_observations,start = c(2013, as.numeric(format(inds[1], "%j"))),frequency = 384)
plot(train1,xlab='Year',ylab="Trip count")
acf(train1)
pacf(train1)

inds <- seq(as.Date("2017-08-31"), as.Date("2017-12-28"), by = "day")

test <- ts(test.FROM$daily_observations,start = c(2017, as.numeric(format(inds[1], "%j"))),frequency = 384)
plot(test,xlab='Year',ylab="Trip count")

###BASELINE MODEL
#created the arima 1,1,1 model as a baseline
FROMmodel.base = Arima(train1, order = c(1,1,1))
summary(FROMmodel.base) 
#Forecasted baseline model
FROMmodel.basefor = forecast(FROMmodel.base, 120)
plot(FROMmodel.basefor) 
#checked the accuracy of the baseline model
model.base.acc = accuracy(FROMmodel.basefor, test.FROM$daily_observations)
model.base.acc
#Seasonality not accounted for

###AUTO-ARIMA
FROMmodel.1 = auto.arima(train1)
summary(FROMmodel.1) 
#Forecasted baseline model
FROMmodel.1for = forecast(FROMmodel.1, 120)
plot(FROMmodel.1for) 
#checked the accuracy of the baseline model
model.base.acc = accuracy(FROMmodel.1for, test.FROM$daily_observations)
model.base.acc

###Model Exploration
pvar<-2:4
dvar<-1:2
qvar<-2:5

OrderGrid<-expand.grid(pvar,dvar,qvar)

ModFit <- function(x, dat){
  m = Arima(dat, order=c(x[[1]], x[[2]], x[[3]]), seasonal = list(order = c(0,1,0), period = 384))
  summary(m)
  forc = forecast(m, 120)
  plot(forc)  
  print(accuracy(forc, test.FROM$daily_observations))
} 

Fits <- plyr::alply(OrderGrid, 1, ModFit, dat = train1)
Fits

###Models based on acf and pacf
acfpacffrom = Arima(train1, order = c(7,0,7), seasonal = list(order = c(0,1,0), period = 384))
summary(acfpacffrom) 
acfpacffromfor = forecast(acfpacffrom, 120)
plot(acfpacffromfor)
#checked the accuracy of the baseline model
acfpacffromfor.acc = accuracy(acfpacffromfor, test.FROM$daily_observations)
acfpacffromfor.acc

acfpacffrom1 = Arima(train1, order = c(7,1,7), seasonal = list(order = c(0,1,0), period = 384))
summary(acfpacffrom1) 
acfpacffromfor = forecast(acfpacffrom1, 120)
plot(acfpacffromfor)
#checked the accuracy of the baseline model
acfpacffromfor.acc = accuracy(acfpacffromfor, test.FROM$daily_observations)
acfpacffromfor.acc

acfpacffrom2 = Arima(train1, order = c(7,2,7), seasonal = list(order = c(0,1,0), period = 384))
summary(acfpacffrom2) 
acfpacffromfor = forecast(acfpacffrom2, 120)
plot(acfpacffromfor)
#checked the accuracy of the baseline model
acfpacffromfor.acc = accuracy(acfpacffromfor, test.FROM$daily_observations)
acfpacffromfor.acc

###Model later visualization
bestmodelFROM = Arima(train1, order = c(4,1,4), seasonal = list(order = c(0,1,0), period = 384))
summary(bestmodelFROM) 
bestmodelfromfor = forecast(bestmodelFROM, 120)
plot(acfpacffromfor)
#checked the accuracy of the baseline model
bestmodelfrom.acc = accuracy(acfpacffromfor, test.FROM$daily_observations)
bestmodelfrom.acc


###TO Models
#import clean TO data
TOper_day = read.csv("C:/Users/chris/Documents/GitHub/Divvy-Capstone-Project/TO_Divvy.csv", header = TRUE, sep = ",")

#Formatted to a time object
library(lubridate)
TOper_day$date <- mdy(TOper_day$date)
str(TOper_day)
head(TOper_day)

train.TO = TOper_day[1:1524,]
test.TO = TOper_day[1525:1645,] #forecast 3 months out

library(forecast)
library(TSA)


trainto <- ts(train.TO$daily_observations,start = c(2013, as.numeric(format(inds[1], "%j"))),frequency = 384)
plot(train1,xlab='Year',ylab="Trip count")
acf(trainto)
pacf(trainto)

testto <- ts(test.FROM$daily_observations,start = c(2017, as.numeric(format(inds[1], "%j"))),frequency = 384)
plot(testto,xlab='Year',ylab="Trip count")

#Checked for seasonality
date_tsto <- ts(train.TO$daily_observations,frequency = 1)
p <- periodogram(date_tsto)
m <- p$freq[which.max(p$spec)]
#Find the seasonality in the data
seasonality <- 1/m
seasonality #seasonality of 384

1/p$freq[order(p$spec)]
#created the ts formatted to recognize the time
inds <- seq(as.Date("2013-06-27"), as.Date("2017-08-31"), by = "day")

###BASELINE MODEL
TOmodelbase = Arima(trainto, order = c(1,1,1))
summary(TOmodelbase)
TOmodelbasefor = forecast(TOmodelbase, 121)
plot(TOmodelbasefor)
#checked the accuracy of the baseline model
model.base.accbase = accuracy(TOmodelbasefor, test.TO$daily_observations)
model.base.accbase

###Auto.Arima
TOmodel.1 =auto.arima(trainto)
summary(TOmodel.1)
TOmodel.1for = forecast(TOmodel.1, 121)
plot(TOmodel.1for)
#checked the accuracy of the baseline model
model.auto.to = accuracy(TOmodel.1for, test.TO$daily_observations)
model.auto.to

#Model Exploration
pvar<-2:4
dvar<-1:2
qvar<-2:5

OrderGrid<-expand.grid(pvar,dvar,qvar)

ModFit2 <- function(x, dat){
  m = Arima(dat, order=c(x[[1]], x[[2]], x[[3]]), seasonal = list(order = c(0,1,0), period = 384))
  summary(m)
  forc = forecast(m, 121)
  plot(forc)  
  print(accuracy(forc, test.TO$daily_observations))
} 

Fits <- plyr::alply(OrderGrid, 1, ModFit2, dat = trainto)
Fits

##Models based on acf & pacf estimations
acfpacf = Arima(trainto, order = c(7,0,7), seasonal = list(order = c(0,1,0), period = 384))
summary(acfpacf) 
acfpacffor = forecast(acfpacf, 121)
plot(acfpacffor)
#checked the accuracy 
acfpacffor.acc = accuracy(acfpacffor, test.TO$daily_observations)
acfpacffor.acc

acfpacf2 = Arima(trainto, order = c(7,1,7), seasonal = list(order = c(0,1,0), period = 384))
summary(acfpacf2) 
acfpacffor2 = forecast(acfpacf2, 121)
plot(acfpacffor2)
#checked the accuracy 
acfpacffor.acc2 = accuracy(acfpacffor2, test.TO$daily_observations)
acfpacffor.acc2

acfpacf3 = Arima(trainto, order = c(7,2,7), seasonal = list(order = c(0,1,0), period = 384))
summary(acfpacf3) 
acfpacffor3 = forecast(acfpacf3, 121)
plot(acfpacffor3)
#checked the accuracy 
acfpacffor.acc3 = accuracy(acfpacffor3, test.TO$daily_observations)
acfpacffor.acc3
