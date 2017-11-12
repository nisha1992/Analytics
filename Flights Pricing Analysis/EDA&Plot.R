#Analysis is in progress. This is just incomplete code.

#install.packages("chron")
#install.packages("lubridate")
library(lubridate)
library(chron)
library(readxl)
library(tidyr)
library(dplyr)
library(psych)
library(ggplot2)
library(reshape2)
library(xlsx)
Flights_DF <- read_xlsx("/Pathoffolderwheredataexcelfileislocated/Data.xlsx", sheet = "FlightData")
str(Flights_DF)

#EDA

#check for missing values and impute missing values
#Aircraft
checkAircraft <- is.na(Flights_DF$Aircraft)
table(Flights_DF$Carrier[checkAircraft])
Flights_DF$Aircraft[checkAircraft] <- "Airbus A319"

sapply(Flights_DF, function(x) sum(is.na(x) | x == ""))

#END check for missing values and impute missing values

#FEATURE ENGINEERING

#extract TimeOfDeparture from column Departure Time
Flights_DF$Departure_Time <- c(length = length(Flights_DF))
str(Flights_DF$Departure_Time)
Flights_DF$Departure_Time <- lapply(Flights_DF$`Departure Time`, function(x){
  substr(x,12,16)
})
Flights_DF$Departure_Time <- unlist(Flights_DF$Departure_Time)
Flights_DF$Departure_Time <- times(paste0(Flights_DF$Departure_Time, ":00"))
#Convert time to morning evening etc
tod <- data.frame(Flights_DF$Departure_Time)
tbreaks <- c(0, 4, 12, 17, 24) / 24
tlabels <- c("Night", "Morning", "Afternoon", "Evening")
Flights_DF$DepartureTimeofDay <- cut(x=tod$Flights_DF.Departure_Time, breaks=tbreaks, labels=tlabels, include.lowest=TRUE)
names(Flights_DF)

#extract DateOfDeparture from column Departure Time
Flights_DF$Departure_Date <- c(length = length(Flights_DF))
str(Flights_DF$Departure_Date)
Flights_DF$Departure_Date <- lapply(Flights_DF$`Departure Time`, function(x){
  substr(x,1,10)
})
Flights_DF$Departure_Date <- lapply(Flights_DF$Departure_Date, function(x){
  as.Date(x)
})
Flights_DF$Departure_Date <- do.call("c",Flights_DF$Departure_Date)
str(Flights_DF)
#Removing redundant variables
Flights_DF$`Departure Date` <- Flights_DF$Departure_Date
Flights_DF$`Departure Time` <- Flights_DF$Departure_Time
Flights_DF$Departure_Date <- NULL
Flights_DF$Departure_Time <- NULL

#convert prices(chr) to numeric
Flights_DF$Prices <- lapply(Flights_DF$Prices, function(x){
  as.numeric(x)
})
Flights_DF$Prices <- do.call("c",Flights_DF$Prices)

#convert date of search to date object
Flights_DF$`Date of Search` <- lapply(Flights_DF$`Date of Search`, function(x){
  gsub("_","-",x)
})
Flights_DF$`Date of Search` <- do.call("c",Flights_DF$`Date of Search`)
Flights_DF$`Date of Search` <- lapply(Flights_DF$`Date of Search`, function(x){
  as.Date(x)
})
Flights_DF$`Date of Search` <- do.call("c",Flights_DF$`Date of Search`)

#Convert time of search to time object
Flights_DF$`Time of Search` <- times(paste0(Flights_DF$`Time of Search`, ":00:00"))
tod1 <- data.frame(Flights_DF$`Time of Search`)
tbreaks1 <- c(0, 4, 12, 17, 24) / 24
tlabels1 <- c("Night", "Morning", "Afternoon", "Evening")
Flights_DF$`TimeofDay of Search` <- cut(x=tod1$Flights_DF..Time.of.Search., breaks=tbreaks1, labels=tlabels1, include.lowest=TRUE)

#extract weekday from departure time
Flights_DF$`Departure Weekday` <- weekdays(Flights_DF$`Departure Date`)

#extract weekday from Date of search
Flights_DF$`Search Weekday` <- weekdays(Flights_DF$`Date of Search`)

#Extracting Date, Month from Departure Date
Flights_DF$DepartureDOM <- substring(Flights_DF$`Departure Date`,9,10)
Flights_DF$DepartureMOY <- months(Flights_DF$`Departure Date`)

Flights_DF$SearchDOM <- substring(Flights_DF$`Date of Search`,9,10)
Flights_DF$SearchMOY <- months(Flights_DF$`Date of Search`)
Flights_DF$TOSvsDODLag <- as.numeric(Flights_DF$`Departure Date` - Flights_DF$`Date of Search`)

#segregating with respect to weekday or weekend from Departure
WdayWeekend <- (Flights_DF$`Departure Weekday` == "Saturday" | Flights_DF$`Departure Weekday` == "Sunday")
Flights_DF$isitWeekendDep <- c(length = length(Flights_DF))
Flights_DF$isitWeekendDep[WdayWeekend] <- "Weekend"
isitoff <- Flights_DF$isitWeekendDep != "Weekend"
Flights_DF$isitWeekendDep[isitoff] <- "Weekday"

SdayWeekend <- (Flights_DF$`Search Weekday` == "Saturday" | Flights_DF$`Search Weekday` == "Sunday")
Flights_DF$isitWeekendSearch <- c(length = length(Flights_DF))
Flights_DF$isitWeekendSearch[SdayWeekend] <- "Weekend"
isitoffS <- Flights_DF$isitWeekendSearch != "Weekend"
Flights_DF$isitWeekendSearch[isitoffS] <- "Weekday"

Flights_DF$DepartureDOM <- as.numeric(Flights_DF$DepartureDOM)
Flights_DF$SearchDOM <- as.numeric(Flights_DF$SearchDOM)

#END FEATURE ENGINEERING

#Final Data frame
str(Flights_DF)
Flights_DF$TOSvsDODLag

#write final data to excel
TempFlightData <- subset(Flights_DF, Flights_DF$`Date of Search` == "2017-09-25")
write.xlsx(TempFlightData, "TempFlightData.xlsx")

#write.xlsx(Flights_DF, "FinalFlightData.xlsx")

#Univariate Analysis
??Describe
??summary
describe(Flights_DF)
summary(Flights_DF$Prices)
sort(table(Flights_DF$Aircraft))
unique(Flights_DF$Aircraft)
unique(Flights_DF$Carrier)

#PLOT
#Dividing data on the basis of outliers
HigherPrices <- subset(Flights_DF, Flights_DF$Prices >= 12000)
LowerPrices <- subset(Flights_DF, Flights_DF$Prices < 12000)
nrow(HigherPrices)*100/269987
nrow(LowerPrices)*100/269987
#Boxplot Prices
ggplot(Flights_DF, aes(x = "", y = Prices)) + geom_boxplot(outlier.colour = "Red") + scale_y_continuous(breaks = seq(2000,54000, by = 2000))
ggplot(HigherPrices, aes(x = "", y = Prices)) + geom_boxplot(outlier.colour = "Red") + scale_y_continuous(breaks = seq(11000,54000, by = 2000))
ggplot(LowerPrices, aes(x = "", y = Prices)) + geom_boxplot(outlier.colour = "Red") + scale_y_continuous(breaks = seq(2000,13000, by = 2000))

#Are outliers because of a specific independent variable
tempT <- c(length = length(Flights_DF))
tempT <- format(strptime(Flights_DF$`Departure Time`, "%H:%M:%S"), '%H')
tempT
Flights_DF$DepartureH <- tempT
ggplot(Flights_DF, aes(x = DepartureH, y = Prices)) + geom_boxplot(outlier.colour = "Red") + scale_y_continuous(breaks = seq(2000,54000, by = 2000))
sort(tapply(HigherPrices$Prices, HigherPrices$DepartureTimeofDay, mean))
sort(table(HigherPrices$DepartureTimeofDay))
sort(table(Flights_DF$DepartureTimeofDay))

#Prices vs Lag
ggplot(Flights_DF,aes(x = TOSvsDODLag, y = Prices, col = DepartureTimeofDay)) + geom_point() + scale_y_continuous(breaks = seq(2000,54000,by=2000)) + theme(axis.text.x = element_text(angle = 90)) + scale_x_continuous(breaks = seq(0,15,by=1)) + labs(title = "Variation of Prices wrt Lag b/w Search and Departure", x = "Lag")
ggplot(Flights_DF, aes(x = `Departure Date`, y = Prices, color = TOSvsDODLag)) + geom_point() + scale_y_continuous(breaks = seq(2000,54000,by=2000)) + theme(axis.text.x = element_text(angle = 90)) + scale_color_gradientn(colours = rainbow(15)) +scale_x_date(date_breaks = "1 day") + labs(title = "Variation of Prices wrt Lag b/w Search and Departure", x = "Departure Dates")
LowestPrices <- subset(Flights_DF, Flights_DF$Prices < 8000) 
ggplot(LowestPrices, aes(x = `Departure Date`, y = Prices, color = TOSvsDODLag)) + geom_point() + scale_y_continuous(breaks = seq(2000,20000,by=2000)) + theme(axis.text.x = element_text(angle = 90)) + scale_color_gradientn(colours = rainbow(15)) +scale_x_date(date_breaks = "1 day") + labs(title = "Variation of Prices wrt Lag b/w Search and Departure", x = "Departure Dates")
