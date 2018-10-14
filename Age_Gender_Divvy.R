censusdata = read.csv("C:/Users/chris/Desktop/Chicago_Age_Distribution.csv", header = TRUE, sep = ",")
income = read.csv("C:/Users/chris/Desktop/chicago_income_distribution.csv", header = TRUE, sep = ",")


#Visualized Age distribution in Chicagoland
sum(censusdata$Total)

library(dplyr)
censusdata <- mutate(censusdata, Percent = (Total / 1930544) * 100 )

library(ggplot2)
AGE <- ggplot(censusdata, aes(x = Age.Range, y = Percent)) +
  geom_bar(stat="identity") +
  ggtitle("Chicagoland Age Distribution")+
  scale_y_continuous(limits = c(0, 30)) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  ylab("Percentage of People") +
  xlab("Age Range") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

AGE + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Visualized Age distribution in Divvy
divvyage = read.csv("C:/Users/chris/Documents/GitHub/Divvy-Capstone-Project/Age_Divvy.csv", header = TRUE, sep = ",")

sum(divvyage$n)

library(dplyr)
divvyage <- mutate(divvyage, Percent = (n / 9997491) * 100 )

library(ggplot2)
AGE_Divvy <- ggplot(divvyage, aes(x = age, y = Percent)) +
  geom_bar(stat="identity", fill = "Royal Blue") +
  ggtitle("Divvy Age Distribution")+
  scale_y_continuous(limits = c(0, 30)) +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  ylab("Percentage of People") +
  xlab("Age Range") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

AGE_Divvy + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Visualized Chicago Age Distribution
library(ggplot2)
INCOME <- ggplot(income, aes(x = Income.Range, y = Total)) +
  geom_bar(stat="identity") +
  ggtitle("Chicagoland Income Distribution")+
  theme(plot.title = element_text(size = 20, face = "bold")) +
  ylab("Number of People") +
  xlab("Income Range") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

INCOME + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

new = rbind(df$`18`,df2$`20`)

#tidyd up the data to get the std deviation in afe for Divvy
divvyagestdev = read.csv("C:/Users/chris/Documents/GitHub/Divvy-Capstone-Project/Age_Divvy_stdev.csv", header = TRUE, sep = ",")

library(magicfor)   
magic_for(print, silent = TRUE)
for (i in 1:8401) {
  print(18)
}
df <- magic_result_as_dataframe() 

df[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:16353) {
  print(20)
}
df1 <- magic_result_as_dataframe() 
df1[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:33959) {
  print(21)
}
df2 <- magic_result_as_dataframe() 
df2[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:230075) {
  print(23)
}
df3 <- magic_result_as_dataframe() 
df3[[1]] <- NULL

library(magicfor)   
magic_for(print, silent = TRUE)
for (i in 1:2045756) {
  print(27)
}
df4 <- magic_result_as_dataframe() 

magic_for(print, silent = TRUE)
for (i in 1:2599259) {
  print(32)
}
df5 <- magic_result_as_dataframe() 

magic_for(print, silent = TRUE)
for (i in 1:1746513) {
  print(37)
}
df6 <- magic_result_as_dataframe() 

magic_for(print, silent = TRUE)
for (i in 1:986494) {
  print(42)
}
df7 <- magic_result_as_dataframe()

magic_for(print, silent = TRUE)
for (i in 1:753746) {
  print(47)
}
df8 <- magic_result_as_dataframe()

magic_for(print, silent = TRUE)
for (i in 1:616610) {
  print(52)
}
df9 <- magic_result_as_dataframe()

magic_for(print, silent = TRUE)
for (i in 1:511929) {
  print(57)
}
df10 <- magic_result_as_dataframe()

magic_for(print, silent = TRUE)
for (i in 1:139727) {
  print(60)
}
df11 <- magic_result_as_dataframe()
df11[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:165668) {
  print(63)
}
df12 <- magic_result_as_dataframe()
df12[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:76343) {
  print(65)
}
df13 <- magic_result_as_dataframe()
df13[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:66658) {
  print(68)
}
df14 <- magic_result_as_dataframe()
df14[[1]] <- NULL


names(df)[1] = "n" 
names(df1)[1] = "n" 
names(df2)[1] = "n" 
names(df3)[1] = "n" 
names(df4)[1] = "n" 
names(df5)[1] = "n" 
names(df6)[1] = "n" 
names(df7)[1] = "n" 
names(df8)[1] = "n" 
names(df9)[1] = "n" 
names(df10)[1] = "n" 
names(df11)[1] = "n" 
names(df12)[1] = "n" 
names(df13)[1] = "n" 
names(df14)[1] = "n" 

library(dplyr)
standarddevdivvy <- rbind(df,df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11, df12, df13, df14)

sd(standarddevdivvy$n)

#tidyd up the data to get the std deviation in age for chicago
chiagestdev = read.csv("C:/Users/chris/Documents/GitHub/Divvy-Capstone-Project/Age_Chi_stdev.csv", header = TRUE, sep = ",")

library(magicfor)   
magic_for(print, silent = TRUE)
for (i in 1:67862) {
  print(18)
}
cdf <- magic_result_as_dataframe() 

cdf[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:35639) {
  print(20)
}
cdf1 <- magic_result_as_dataframe() 
cdf1[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:35846) {
  print(21)
}
cdf2 <- magic_result_as_dataframe() 
cdf2[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:133786) {
  print(23)
}
cdf3 <- magic_result_as_dataframe() 
cdf3[[1]] <- NULL

library(magicfor)   
magic_for(print, silent = TRUE)
for (i in 1:288032) {
  print(27)
}
cdf4 <- magic_result_as_dataframe() 

magic_for(print, silent = TRUE)
for (i in 1:254158) {
  print(32)
}
cdf5 <- magic_result_as_dataframe() 

magic_for(print, silent = TRUE)
for (i in 1:201327) {
  print(37)
}
cdf6 <- magic_result_as_dataframe() 
cdf6[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:171763) {
  print(42)
}
cdf7 <- magic_result_as_dataframe()
cdf7[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:164748) {
  print(47)
}
cdf8 <- magic_result_as_dataframe()
cdf8[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:161728) {
  print(52)
}
cdf9 <- magic_result_as_dataframe()
cdf9[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:159607) {
  print(57)
}
cdf10 <- magic_result_as_dataframe()
cdf10[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:60155) {
  print(60)
}
cdf11 <- magic_result_as_dataframe()
cdf11[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:85566) {
  print(63)
}
cdf12 <- magic_result_as_dataframe()
cdf12[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:48319) {
  print(65)
}
cdf13 <- magic_result_as_dataframe()
cdf13[[1]] <- NULL

magic_for(print, silent = TRUE)
for (i in 1:62008) {
  print(68)
}
cdf14 <- magic_result_as_dataframe()
cdf14[[1]] <- NULL


names(cdf)[1] = "n" 
names(cdf1)[1] = "n" 
names(cdf2)[1] = "n" 
names(cdf3)[1] = "n" 
names(cdf4)[1] = "n" 
names(cdf5)[1] = "n" 
names(cdf6)[1] = "n" 
names(cdf7)[1] = "n" 
names(cdf8)[1] = "n" 
names(cdf9)[1] = "n" 
names(cdf10)[1] = "n" 
names(cdf11)[1] = "n" 
names(cdf12)[1] = "n" 
names(cdf13)[1] = "n" 
names(cdf14)[1] = "n" 

library(dplyr)
standarddevchi <- rbind(cdf,cdf1,cdf2,cdf3,cdf4,cdf5,cdf6,cdf7,cdf8,cdf9,cdf10,cdf11, cdf12, cdf13, cdf14)

sd(standarddevchi$n)

