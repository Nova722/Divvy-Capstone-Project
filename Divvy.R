
data = read.csv("C:/Users/chris/Desktop/Divvy/Divvy_Trips.csv", header = TRUE, sep = ",")

#Reviewed Basic Data Structure
head(data)
summary(data)
str(data)

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

#separated the date and time in start and stop time columns in FROM in order to make the analysis easier 
library(tidyr)
FROM_sepstart <- separate(FROM, START.TIME, c("start.date", "start.time", "start.time.ampm"), sep = " ")
FROM <- separate(FROM_sepstart, STOP.TIME, c("start.date", "start.time", "start.time.ampm"), sep = " ")
View(FROM)

#separated the date and time in start and stop time columns in TO in order to make the analysis easier 
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

#separated the dates into day, month, year in FROM 
library(tidyr)
FROM <- separate(FROM, start.date, c("month", "day", "year"), sep = "/")
View(FROM)

#separated the dates into day, month, year in TO
library(tidyr)
TO <- separate(TO, stop.date, c("month", "day", "year"), sep = "/")
View(TO)

#converted dates to numeric in FROM
FROM$month <- as.numeric(unlist(FROM$month))
class(FROM$month)
FROM$day <- as.numeric(unlist(FROM$day))
class(FROM$day)
FROM$year <- as.numeric(unlist(FROM$year))
class(FROM$year)

#converted dates to numeric in TO
TO$month <- as.numeric(unlist(TO$month))
class(TO$month)
TO$day <- as.numeric(unlist(TO$day))
class(TO$day)
TO$year <- as.numeric(unlist(TO$year))
class(TO$year)

#Grouped observation numbers by year in TO
library(dplyr)
TO %>% 
  group_by("year") %>% 
  count(year) 

#Grouped observation numbers by year in FROM
library(dplyr)
FROM %>% 
  group_by("year") %>% 
  count(year) 


#Grouped observation numbers by year, month, day in TO
library(dplyr)
TO_Obs <- 
  TO %>% 
  count(year, month, day)
View(TO_Obs)

#Grouped observation numbers by year, month, day in FROM
library(dplyr)
FROM_Obs <- 
  FROM %>% 
  count(year, month, day)
View(FROM_Obs)

#Exported TO_Obs & FROM_obs in excel for easy viewing
write.csv(FROM_Obs, "FROM_Observations.csv")
write.csv(TO_Obs, "TO_Observations.csv")

_______________________________________________________

#converted characters to dates
TO$stop.date <- as.Date(TO$stop.date, format = "%m/%d/%Y", tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
class(TO$stop.date)
FROM$start.date <- as.Date(FROM$start.date, format = "%m/%d/%Y", tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
class(FROM$start.date)

#grouped

  
  
  

 

  
