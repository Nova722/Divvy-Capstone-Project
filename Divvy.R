data = read.csv("C:/Users/chris/Desktop/Divvy/Divvy_Trips.csv", header = TRUE, sep = ",")

#Reviewed Basic Data Structure
head(data)
summary(data)
str(data)

#Tidy up the data for general visualizations on gender, age & trip duration

GENDER <- data.frame(data$GENDER, data$BIRTH.YEAR, data$START.TIME, data$STOP.TIME)
summary(GENDER$data.GENDER)

library(tidyr)
GENDER <- separate(GENDER, data.START.TIME, c("start.date", "start.time", "start.ampm"), sep = " ")
GENDER <- separate(GENDER, data.STOP.TIME, c("stop.date", "stop.time", "stop.ampm"), sep = " ")

library(tidyr)
GENDER <- unite(GENDER, "start.time", start.time, start.ampm, sep = " ")
GENDER <- unite(GENDER, "stop.time", stop.time, stop.ampm, sep = " ")

#Tidyd the data
library(dplyr)
TIME_SERIES = GENDER %>% 
  group_by(start.date) %>% 
  count()

library(lubridate)
TIME_SERIES$start.date <- mdy(TIME_SERIES$start.date)

names(TIME_SERIES)[1] <- "date"

TIME_SERIES$DayN <- as.numeric(format(as.Date(TIME_SERIES$date),"%d"))
TIME_SERIES$MonthN <- as.numeric(format(as.Date(TIME_SERIES$date),"%m"))
TIME_SERIES$YearN <- as.numeric(format(as.Date(TIME_SERIES$date),"%Y"))

write.csv(TIME_SERIES, "Monthly OVerview.csv")


library(ggplot2)
TIME_SERIES_GRAPH2<- ggplot(TIME_SERIES2, aes(x = MonthN, y = nn, group = YearN, colour=YearN)) + 
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks = data$MonthN, labels = data$MonthN)


library(ggplot2)
TIME_SERIES_GRAPH <- ggplot(TIME_SERIES, aes(x = date, y = n)) + 
  geom_point() + scale_x_date(limits = "%m")

library(ggplot2)
TIME_SERIES_GRAPH <- ggplot(TIME_SERIES, aes(x = MonthN, y = n, group = YearN)) + 
  geom_point()

library(scales)
TIME_SERIES_GRAPH + scale_x_date(breaks = date_breaks("months"),
                                 labels = date_format("%b"))


#converted AM/PM to military time in order to format properly for lubridate
GENDER$start.time<- (format(strptime(GENDER$start.time, "%I:%M:%S %p"), "%H:%M:%S"))
GENDER$stop.time<- (format(strptime(GENDER$stop.time, "%I:%M:%S %p"), "%H:%M:%S"))

library(tidyr)
GENDER <- unite(GENDER, "Start.Date", start.date, start.time, sep = " ")
GENDER <- unite(GENDER, "Stop.Date", stop.date, stop.time, sep = " ")

#converted start & stop to time objects
library(lubridate)
GENDER$Stop.Date <- mdy_hms(GENDER$Stop.Date)
GENDER$Start.Date <- mdy_hms(GENDER$Start.Date)

#subracted the stop and start time to get trip duration
library(dplyr)
GENDER_NEW <- mutate(GENDER, trip.length = Stop.Date - Start.Date)
GENDER_NEW$trip.length <- GENDER_NEW$trip.length / 60

summary(as.numeric(GENDER_NEW$trip.length))
summary(GENDER_NEW)

#Visualized trip duration
library(ggplot2)
ggplot(GENDER_NEW, aes(x = as.numeric(trip.length))) + geom_histogram()

#added a column for age
library(dplyr)
GENDER_NEW <- mutate(GENDER_NEW, age = 2018 - data.BIRTH.YEAR)
summary(GENDER_NEW$age)
str(GENDER_NEW)

GENDER_NEW_AGE <- GENDER_NEW[!is.na(GENDER_NEW$age), ]
sd(GENDER_NEW_AGE$age)
str(GENDER_NEW_AGE)

library(dplyr)
AGE_SUMMARY = GENDER_NEW_AGE %>% 
  group_by(age) %>% 
  count() 

library(dplyr)
AGE_GENDER_SUMMARY = GENDER_NEW_AGE %>% 
  group_by(data.GENDER,age) %>% 
  count() 

#Exported AGE to re-work to compare it to the Chicago data
write.csv(AGE_SUMMARY, "Age_Divvy.csv")
write.csv(AGE_GENDER_SUMMARY, "Age_Gender_Divvy.csv")

#tidyd up the data in order to visualize gender and number of rides
library(tidyr)
GENDER_ <- separate(GENDER_NEW, Stop.Date, c("stop.date", "stop.time"), sep = " ")
GENDER_ <- separate(GENDER_, stop.date, c("year", "month", "day"), sep = "-")

library(dplyr)
GENDER_TIME = GENDER_ %>% 
  group_by(data.GENDER) %>% 
  count(year) 

GENDER_TIME <- GENDER_TIME[-c(1:6, 17), ] 
names(GENDER_TIME)[1]<-"Gender"
names(GENDER_TIME)[3]<-"Rides"

library(ggplot2)
GENDER_TIME_GRAPH <- ggplot(GENDER_TIME, aes(x = year, y = Rides, fill = Gender)) +
  geom_bar(position = "stack", stat="identity") +
  ggtitle("Rides & Gender Over Time")+
  theme(plot.title = element_text(size = 20, face = "bold")) +
  ylab("Rides") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

GENDER_TIME_GRAPH + scale_fill_manual(values = c("Hot Pink", "Royal Blue"))

#compared Age and Gender
divvyagegender = read.csv("C:/Users/chris/Documents/GitHub/Divvy-Capstone-Project/Age_GENDER_Divvy.csv", header = TRUE, sep = ",")

GENDER_AGE_GRAPH <- ggplot(divvyagegender, aes(x = age, y = n, fill = Gender)) +
  geom_bar(position = "stack", stat="identity") +
  ggtitle("Divvy Gender & Age")+
  theme(plot.title = element_text(size = 20, face = "bold")) +
  ylab("Rides") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

GENDER_AGE_GRAPH + scale_fill_manual(values = c("Hot Pink", "Royal Blue"))

#Visualized and analyzed age over time

library(dplyr)
AGE_TIME = GENDER_ %>% 
  group_by(age) %>% 
  count(year) 

AGE_TIME <- AGE_TIME[-c(1:6, 291:391), ] 
names(AGE_TIME)[3]<-"Rides"


library(ggplot2)
ggplot(AGE_TIME, aes (x = factor(age), y = Rides, color = year, size = Rides)) +geom_point()+ 
  ggtitle("Rides & Age Over Time")+
  labs(x = "Age", y = "Rides", col = "Year") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  ylab("Rides") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

       
#Took a random sample size in order to visualize and further analyze
library(tidyr)
GENDER_SAMPLE <- sample_n(GENDER_NEW, 1200, replace = FALSE, weight = NULL, .env = NULL)

GENDER_SAMPLE <- subset(GENDER_SAMPLE, select = -c(2:4)) 

#Analyzed possible correlations with ggpairs
library(GGally)
ggpairs(GENDER_SAMPLE)


#Reviewed different stations forecasting options regarding trips from, originally wanted to choose Federal St & Polk St, decided against it, Lake Shore Dr. & Monroe has the highest number of observations
FROM.STATION.NAMES <- data$FROM.STATION.NAME
summary(FROM.STATION.NAMES)

#Reviewed different stations forecasting options regarding trips to.  Lake Shore Dr. & Monroe St. has the 2nd most observations, therefore since observation number was high in both from and to I will forecast the tripts taken from and to this station
TO.STATION.NAMES <- data$TO.STATION.NAME
summary(TO.STATION.NAMES)

#Created subsets in order to further explore trips to and from this station
FROM <- subset(data, FROM.STATION.NAME == "Lake Shore Dr & Monroe St")
TO <- subset(data, TO.STATION.NAME == "Lake Shore Dr & Monroe St")

#There are a total of 210255 trips from the Lake Shore Dr. & Monroe St. station.  first trip recorded on 6/28/2013, last trip recorded 12/28/2017
summary(FROM)
head(FROM)
tail(FROM)

#There are a total of 203014 trips to the Lake Shore Dr. & Monroe St. station.  first trip recorded on 6/28/2013, last trip recorded 12/31/2017
summary(TO)
head(TO)
tail(TO)

#separated the date and time in start and stop time columns order to make the analysis easier 
library(tidyr)
FROM_sepstart <- separate(FROM, START.TIME, c("start.date", "start.time", "start.time.ampm"), sep = " ")
FROM <- separate(FROM_sepstart, STOP.TIME, c("start.date", "start.time", "start.time.ampm"), sep = " ")
View(FROM)

library(tidyr)
TO_sepstart <- separate(TO, START.TIME, c("stop.date", "stop.time", "stop.time.ampm"), sep = " ")
TO <- separate(TO_sepstart, STOP.TIME, c("stop.date", "stop.time", "stop.time.ampm"), sep = " ")
View(TO)

#combined the AM/PM in the time
library(tidyr)
FROM <- unite(FROM, "start.time", start.time, start.time.ampm, sep = " ")
TO <- unite(TO, "stop.time", stop.time, stop.time.ampm, sep = " ")

#converted AM/PM to military time for easy analysis
TO$stop.time<- (format(strptime(TO$stop.time, "%I:%M:%S %p"), "%H:%M:%S"))
FROM$start.time <- (format(strptime(FROM$start.time, "%I:%M:%S %p"), "%H:%M:%S"))

#separated the dates into day, month, year 
library(tidyr)
FROM <- separate(FROM, start.date, c("month", "day", "year"), sep = "/")
View(FROM)

library(tidyr)
TO <- separate(TO, stop.date, c("month", "day", "year"), sep = "/")
View(TO)

#converted dates to numeric for easier analysis
FROM$month <- as.numeric(unlist(FROM$month))
class(FROM$month)
FROM$day <- as.numeric(unlist(FROM$day))
class(FROM$day)
FROM$year <- as.numeric(unlist(FROM$year))
class(FROM$year)

TO$month <- as.numeric(unlist(TO$month))
class(TO$month)
TO$day <- as.numeric(unlist(TO$day))
class(TO$day)
TO$year <- as.numeric(unlist(TO$year))
class(TO$year)

#searched for any NA values in TO & FROM
library(dplyr)
summary(is.na(TO$year))
summary(is.na(TO$month))
summary(is.na(TO$day))

summary(is.na(FROM$year))
summary(is.na(FROM$month))
summary(is.na(FROM$day))

#Grouped observation numbers by year 
library(dplyr)
TO_YEARS = TO %>% 
  group_by(year) %>% 
  count(year) 

library(dplyr)
FROM_YEARS = FROM %>% 
  group_by(year) %>% 
  count(year) 

#visualized the years in ggplot
library(ggplot2)
ggplot(FROM_YEARS, aes(factor(year))) + geom_bar(fill = "#0072B2")

ggplot(TO_YEARS, aes(x = year, y = n, label = n)) +
  geom_bar(stat = "identity", fill = "Royal Blue") +
  geom_text(aes(label= n), size = 6, position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Divvy Bikes Taken To the Station")+
  theme(plot.title = element_text(size = 20, face = "bold")) +
  ylab("number of rides") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(FROM_YEARS, aes(x = year, y = n, label = n)) +
  geom_bar(stat = "identity", fill = "Royal Blue") +
  geom_text(aes(label= n), size = 6, position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Divvy Bikes Taken From the Station")+
  theme(plot.title = element_text(size = 20, face = "bold")) +
  ylab("number of rides") +
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  

#Grouped observation numbers by year, month, day in TO
library(dplyr)
TO_Obs <- 
  TO %>% 
  count(year, month, day)
View(TO_Obs)


#Grouped & Visulaized observation numbers by year, month, day in FROM
library(dplyr)
FROM_Obs <- 
  FROM %>% 
  count(year, month, day)
View(FROM_Obs)

library(ggplot2)
ggplot(FROM_Obs, aes( x= factor(month), y = n, fill = year)) + geom_point(col ="#0072B2")+ 
  facet_grid(.~ year) + labs( x = "month", y = "number of observations") +
  ggtitle("Divvy Bikes Taken From the Station")+
  theme(plot.title = element_text(size = 20, face = "bold")) +
  ylab("number of rides") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks=seq(1, 12, 2))


#Exported TO_Obs & FROM_obs in excel for easy viewing
write.csv(FROM_Obs, "FROM_Observations.csv")
write.csv(TO_Obs, "TO_Observations.csv")


#united the dates as year, month, day for easy visualization
library(tidyr)
FROM_Obs <- unite(FROM_Obs, "Date", year, month, day, sep = "")
View(FROM_Obs)

library(ggplot2)
ggplot(FROM_Obs, aes( x= Date, y = n )) + geom_point(position = "jitter") + labs( x = "time", y = "number_of_observations")

#visualized observations on a day level

library(tidyr)
TO_Obs <- unite(TO_Obs, "Date", year, month, day, sep = "")
View(TO_Obs)

library(ggplot2)
ggplot(TO_Obs, aes( x= Date, y = n )) + geom_point(position = "jitter") + labs( x = "time", y = "number_of_observations") +
  ggtitle("TO") + theme(plot.title = element_text(hjust = 0.5))

#combined the date/time and used lubridate to properly classify them
library(tidyr)
TO <- unite(TO, "stop.date", month, day, year, sep = "-")
TO <- unite(TO, "date.time", stop.date, stop.time, sep = " ")
library(lubridate)
class(TO$date.time)
TO$date.time <- mdy_hms(TO$date.time)
str(TO$date.time)

library(tidyr)
FROM <- unite(FROM, "start.date", month, day, year, sep = "-")
FROM <- unite (FROM, "date.time", start.date, start.time, sep = " ")
library(lubridate)
FROM$date.time <- mdy_hms(FROM$date.time)
str(FROM$date.time)

library(dplyr)
df <- FROM %>%
  group_by(date.time) %>%
  summarise(DateObservations = length(date.time)) %>%
              summarise(DatePct = date.time/nrow(FROM))

library(ggplot2)
ggplot(FROM, aes(x = date.time, y = count)) +geom_line()

#experimented with plotly visualizations
library(plotly)
p <- plot_ly(
  x = TO_YEARS$year,
  y = TO_YEARS$n,
  name = "TO",
  type = "bar")

api_create(p, filename = "TO Years")

library(plotly)
p2 <- plot_ly(
  x = FROM_YEARS$year,
  y = FROM_YEARS$n,
  name = "FROM",
  type = "bar")

api_create(p2, filename = "FROM Years")

=======
ggplot(TO_Obs, aes( x= Date, y = n )) + geom_point(position = "jitter") + labs( x = "time", y = "number_of_observations")
>>>>>>> parent of 380bb90... Updated visualizations & date/time using lubridate

_______________________________________________________

#converted characters to dates
TO$stop.date <- as.Date(TO$stop.date, format = "%m/%d/%Y", tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
class(TO$stop.date)
FROM$start.date <- as.Date(FROM$start.date, format = "%m/%d/%Y", tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
class(FROM$start.date)

#grouped




Sys.setenv("plotly_username"="tcarr1989")
Sys.setenv("plotly_api_key"="2I9jpljKFZun4xTzGYj3")



