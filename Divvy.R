data = read.csv("C:/Users/chris/Desktop/Divvy/Divvy_Trips.csv", header = TRUE, sep = ",")
library(tidyr)
library(lubridate)
library(dplyr)

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
test.FROM = FROMper_day[1525:1644,] #forecast 3 months out

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

###found the ideal "simple" pdq values
###model to try out different parameters for the best AIC comparing it to the baseline of 18021.96
pvar<-0:5
dvar<-0:3
qvar<-0:7


OrderGrid<-expand.grid(pvar,dvar,qvar)

ModFit <- function(x, dat){
  m = Arima(dat, order=c(x[[1]], x[[2]], x[[3]]))
  return(m)
} 

Fits <- plyr::alply(OrderGrid, 1, ModFit, dat = train1)
Fits #models with the highest AIC included (5,2,7), (4,2,7),(5,1,7) 
Fits[1:26] #no lower aic values found in 1:26

###Using the ideal models found in my Fits table to see if they outperform the basline

#Model 1
FROMmodel.1 = Arima(train1, order = c(5,2,7))
summary(FROMmodel.1) 
#Forecasted baseline model
FROMmodel.1for = forecast(FROMmodel.1, 120)
plot(FROMmodel.1for)  
#checked the accurac of the baseline model
model.base.acc1 = accuracy(FROMmodel.1for, test.FROM$daily_observations)
model.base.acc1

#Model 2
FROMmodel.2 = Arima(train1, order = c(4,2,7))
summary(FROMmodel.2) 
#Forecasted baseline model
FROMmodel.2for = forecast(FROMmodel.2, 120)
plot(FROMmodel.2for) #This model is not a decent use case as the forecast swings to the negative
#checked the accuracy of the baseline model
model.base.acc2 = accuracy(FROMmodel.2for, test.FROM$daily_observations)
model.base.acc2

#Model 3
FROMmodel.3 = Arima(train1, order = c(5,1,7))
summary(FROMmodel.3) 
#Forecasted baseline model
FROMmodel.3for = forecast(FROMmodel.3, 120)
plot(FROMmodel.3for) #This model is not a decent use case as the forecast swings to the negative
#checked the accuracy of the baseline model
model.base.acc3 = accuracy(FROMmodel.3for, test.FROM$daily_observations)
model.base.acc3

###auto.arima model
FROMmodel1aa = auto.arima(train1)
summary(FROMmodel1aa)
FROMmodel1aafor = forecast(FROMmodel1aa, 120)
plot(FROMmodel1aafor)
#checked the accuracy of the baseline model
model.base.acc1aa = accuracy(FROMmodel1aafor, test.FROM$daily_observations)
model.base.acc1aa 

###MODELS WITH SEASONALITY
#Model 1s
model1s = Arima(train1, order = c(5,2,7), seasonal = list(order = c(0,1,0), period = 384))
summary(model1s)
FROMmodel1sfor = forecast(model1s, 120)
plot(FROMmodel1sfor)
#checked the accuracy of the baseline model
model.base.acc1s = accuracy(FROMmodel1sfor, test.FROM$daily_observations)
model.base.acc1s 

#Model 2s
model2s = Arima(train1, order = c(4,2,7), seasonal = list(order = c(0,1,0), period = 384))
summary(model2s)
FROMmodel2sfor = forecast(model2s, 120)
plot(FROMmodel2sfor)
#checked the accuracy of the baseline model
model.base.acc2s = accuracy(FROMmodel1sfor, test.FROM$daily_observations)
model.base.acc2s 

#Model 3s
model3s = Arima(train1, order = c(5,1,7), seasonal = list(order = c(0,1,0), period = 384))
summary(model3s)
FROMmodel3sfor = forecast(model3s, 120)
plot(FROMmodel3sfor)
#checked the accuracy of the baseline model
model.base.acc3s = accuracy(FROMmodel3sfor, test.FROM$daily_observations)
model.base.acc3s 

#Model 4s
model4s = Arima(train1, order = c(2,1,7), seasonal = list(order = c(0,1,0), period = 384))
summary(model4s)
FROMmodel4sfor = forecast(model4s, 120)
plot(FROMmodel4sfor)
#checked the accuracy of the baseline model
model.base.acc4s = accuracy(FROMmodel4sfor, test.FROM$daily_observations)
model.base.acc4s 

#Model 5s
model5s = Arima(train1, order = c(3,1,3), seasonal = list(order = c(0,1,0), period = 384))
summary(model5s)
FROMmodel5sfor = forecast(model5s, 120)
plot(FROMmodel5sfor)
#checked the accuracy of the baseline model
model.base.acc5s = accuracy(FROMmodel5sfor, test.FROM$daily_observations)
model.base.acc5s 

#Model 6s
model6s = Arima(train1, order = c(3,1,7), seasonal = list(order = c(0,1,0), period = 384))
summary(model6s)
FROMmodel6sfor = forecast(model6s, 120)
plot(FROMmodel6sfor)
#checked the accuracy of the baseline model
model.base.acc6s = accuracy(FROMmodel6sfor, test.FROM$daily_observations)
model.base.acc6s  

#Model 7s
model7s = Arima(train1, order = c(5,1,8), seasonal = list(order = c(0,1,0), period = 384))
summary(model7s)
FROMmodel7sfor = forecast(model7s, 120)
plot(FROMmodel7sfor)
#checked the accuracy of the baseline model
model.base.acc7s = accuracy(FROMmodel7sfor, test.FROM$daily_observations)
model.base.acc7s  

#Model 8s
model8s = Arima(train1, order = c(5,1,7), seasonal = list(order = c(0,1,0), period = 7))
summary(model8s)
FROMmodel8sfor = forecast(model8s, 120)
plot(FROMmodel8sfor)
#checked the accuracy of the baseline model
model.base.acc8s = accuracy(FROMmodel8sfor, test.FROM$daily_observations)
model.base.acc8s  

#Model 9s
model9s = Arima(train1, order = c(5,3,7), seasonal = list(order = c(0,1,0), period = 384))
summary(model9s)
FROMmodel9sfor = forecast(model9s, 120)
plot(FROMmodel9sfor)
#checked the accuracy of the baseline model
model.base.acc9s = accuracy(FROMmodel9sfor, test.FROM$daily_observations)
model.base.acc9s  

#Model 10s
model10s = Arima(train1, order = c(2,1,2), seasonal = list(order = c(0,1,0), period = 384))
summary(model10s)
FROMmodel10sfor = forecast(model10s, 120)
plot(FROMmodel10sfor)
#checked the accuracy of the baseline model
model.base.acc10s = accuracy(FROMmodel10sfor, test.FROM$daily_observations)
model.base.acc10s  

#Model 11s
model11s = Arima(train1, order = c(2,2,2), seasonal = list(order = c(0,1,0), period = 384))
summary(model11s)
FROMmodel11sfor = forecast(model11s, 120)
plot(FROMmodel11sfor)
#checked the accuracy of the baseline model
model.base.acc11s = accuracy(FROMmodel11sfor, test.FROM$daily_observations)
model.base.acc11s

#Model 12s
model12s = Arima(train1, order = c(3,2,3), seasonal = list(order = c(0,1,0), period = 384))
summary(model12s)
FROMmodel12sfor = forecast(model12s, 120)
plot(FROMmodel12sfor)
#checked the accuracy of the baseline model
model.base.acc12s = accuracy(FROMmodel12sfor, test.FROM$daily_observations)
model.base.acc12s

#Model 13s
model13s = Arima(train1, order = c(3,0,3), seasonal = list(order = c(0,1,0), period = 384))
summary(model13s)
FROMmodel13sfor = forecast(model13s, 120)
plot(FROMmodel13sfor)
#checked the accuracy of the baseline model
model.base.acc13s = accuracy(FROMmodel13sfor, test.FROM$daily_observations)
model.base.acc13s

#Model 14s
model14s = Arima(train1, order = c(5,2,8), seasonal = list(order = c(0,1,0), period = 384))
summary(model14s)
FROMmodel14sfor = forecast(model14s, 120)
plot(FROMmodel14sfor)
#checked the accuracy of the baseline model
model.base.acc14s = accuracy(FROMmodel14sfor, test.FROM$daily_observations)
model.base.acc14s

#Model 15s
model15s = Arima(train1, order = c(3,1,5), seasonal = list(order = c(0,1,0), period = 384))
summary(model15s)
FROMmodel15sfor = forecast(model15s, 120)
plot(FROMmodel15sfor)
#checked the accuracy of the baseline model
model.base.acc15s = accuracy(FROMmodel15sfor, test.FROM$daily_observations)
model.base.acc15s

#Model 16s
model16s = Arima(train1, order = c(5,0,5), seasonal = list(order = c(0,1,0), period = 384))
summary(model16s)
FROMmodel16sfor = forecast(model16s, 120)
plot(FROMmodel16sfor)
#checked the accuracy of the baseline model
model.base.acc16s = accuracy(FROMmodel16sfor, test.FROM$daily_observations)
model.base.acc16s

#Model 17s
model17s = Arima(train1, order = c(3,3,3), seasonal = list(order = c(0,1,0), period = 384))
summary(model17s)
FROMmodel17sfor = forecast(model17s, 120)
plot(FROMmodel17sfor)
#checked the accuracy of the baseline model
model.base.acc17s = accuracy(FROMmodel17sfor, test.FROM$daily_observations)
model.base.acc17s

#Model 18s
model18s = Arima(train1, order = c(2,1,5), seasonal = list(order = c(0,1,0), period = 384))
summary(model18s)
FROMmodel18sfor = forecast(model18s, 120)
plot(FROMmodel18sfor)
#checked the accuracy of the baseline model
model.base.acc18s = accuracy(FROMmodel18sfor, test.FROM$daily_observations)
model.base.acc18s

#Model 19s
model19s = Arima(train1, order = c(3,2,1), seasonal = list(order = c(0,1,0), period = 384))
summary(model19s)
FROMmodel19sfor = forecast(model19s, 120)
plot(FROMmodel19sfor)
#checked the accuracy of the baseline model
model.base.acc19s = accuracy(FROMmodel19sfor, test.FROM$daily_observations)
model.base.acc19s
