#install.packages("chron")
#install.packages("lubridate")
library(lubridate)
library(grid)
library(chron)
library(readxl)
library(tidyr)
library(dplyr)
library(psych)
library(ggplot2)
library(reshape2)
library(xlsx)
#read Data from xlsx
Flights_DF <- read_xlsx("/Users/Nishachandrani/Documents/Analytics/Projects/WebAnalytics/DataCollect/Data.xlsx", sheet = "FlightData")
str(Flights_DF)

#check for missing values and impute missing values
sapply(Flights_DF, function(x) sum(is.na(x) | x == ""))

#Aircraft
checkAircraft <- is.na(Flights_DF$Aircraft)
table(Flights_DF$Carrier[checkAircraft])
Flights_DF$Aircraft[checkAircraft] <- "Airbus A319"

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
tbreaks <- c(0,4,12,17,20,24) / 24
tlabels <- c("Late Night", "Morning", "Afternoon", "Evening", "Night")
Flights_DF$DepartureTimeofDay <- cut(x=tod$Flights_DF.Departure_Time, breaks=tbreaks, labels=tlabels, include.lowest=TRUE)
names(Flights_DF)
str(Flights_DF)

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
tbreaks1 <- c(0,4,12,17,20,24) / 24
tlabels1 <- c("Late Night", "Morning", "Afternoon", "Evening", "Night")
Flights_DF$`TimeofDay of Search` <- cut(x=tod1$Flights_DF..Time.of.Search., breaks=tbreaks1, labels=tlabels1, include.lowest=TRUE)

#extract weekday from departure time
Flights_DF$`Departure Weekday` <- weekdays(Flights_DF$`Departure Date`)

#extract weekday from Date of search
Flights_DF$`Search Weekday` <- weekdays(Flights_DF$`Date of Search`)
str(Flights_DF)
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

#segregating with respect to weekday or weekend from Search
SdayWeekend <- (Flights_DF$`Search Weekday` == "Saturday" | Flights_DF$`Search Weekday` == "Sunday")
Flights_DF$isitWeekendSearch <- c(length = length(Flights_DF))
Flights_DF$isitWeekendSearch[SdayWeekend] <- "Weekend"
isitoffS <- Flights_DF$isitWeekendSearch != "Weekend"
Flights_DF$isitWeekendSearch[isitoffS] <- "Weekday"

#Convert Date of Month to numeric
Flights_DF$DepartureDOM <- as.numeric(Flights_DF$DepartureDOM)
Flights_DF$SearchDOM <- as.numeric(Flights_DF$SearchDOM)

#Extract Departure Hour from Departure Time
tempT <- c(length = length(Flights_DF))
tempT <- format(strptime(Flights_DF$`Departure Time`, "%H:%M:%S"), '%H')
Flights_DF$DepartureH <- tempT

#Extract Search Hour from Time of Search
tempTS <- c(length = length(Flights_DF))
tempTS <- format(strptime(Flights_DF$`Time of Search`, "%H:%M:%S"), '%H')
Flights_DF$SearchH <- tempTS

#Group lags
Flights_DF$groupLag <- as.character(Flights_DF$TOSvsDODLag)
Flights_DF$groupLag <- lapply(Flights_DF$groupLag, function(x){
  if(x == "0"){
    x = "Same Day"
  }
  else if(x == "1" | x=="2") {
    x = "1-2 Days"
  }
  else if(x == "3" | x == "4" | x=="5" |x=="6"){
    x = "3-7 Days"
  }
  else{
    x = "A week or more"
  }

})
table(Flights_DF$TOSvsDODLag)
Flights_DF$groupLag <- unlist(Flights_DF$groupLag)
table(Flights_DF$groupLag)
str(Flights_DF)
Flights_DF$groupLag <- factor(Flights_DF$groupLag,levels = c("Same Day", "1-2 Days","3-7 Days","A week or more"))
Flights_DF <- Flights_DF %>% arrange(desc(groupLag))

#Since Each "time of search" is not available for all the dates, it can create false trend, so we will remove the times with very less searches and keep the nearby times with more searches
#It seems like that, Times : 2, 5, 8, 11, 14, 17, 20, 23 are there in almost all the "Date of Search", so we will keep these times and remove others. For the dates which do not contain these times, we will impute those with nearest value or remove them.
sort(table(Flights_DF$`Time of Search`))
table(Flights_DF$`Date of Search`, Flights_DF$`Time of Search`)
names(Flights_DF)
#delete 25-09-2017,26-09-2017,27-09-2017
# Flights_DF <- subset(Flights_DF,Flights_DF$`Date of Search` != "2017-09-25")

# Flights_DF <- subset(Flights_DF,Flights_DF$`Time of Search` != "00:00:00")

#END FEATURE ENGINEERING

#Final Data frame
str(Flights_DF)
names(Flights_DF)
#write final data to excel
TempFlightData <- subset(Flights_DF, Flights_DF$`Date of Search` == "2017-09-28")
write.xlsx(TempFlightData, "TempFlightData.xlsx")

#EDA & Visualization
#Univariate Analysis
??Describe
??summary
describe(Flights_DF)
summary(Flights_DF$Prices)
sort(table(Flights_DF$Aircraft))
unique(Flights_DF$Aircraft)
unique(Flights_DF$Carrier)

#Dividing data on the basis of outliers
iqrRange <- IQR(Flights_DF$Prices)
upperFence <- 8775 + 1.5*iqrRange
lowerFence <- 6067 - 1.5*iqrRange
iqrRange
upperFence
lowerFence
HigherPrices <- subset(Flights_DF, Flights_DF$Prices >= 12837 | Flights_DF$Prices <= 2005)
LowerPrices <- subset(Flights_DF, Flights_DF$Prices < 12837 & Flights_DF$Prices > 2005)
nrow(HigherPrices)*100/nrow(Flights_DF)
nrow(LowerPrices)*100/nrow(Flights_DF)
summary(HigherPrices$Prices)
summary(LowerPrices$Prices)
IQR(HigherPrices$Prices)
HigherPrices2 <- subset(Flights_DF, Flights_DF$Prices >= 12837 & Flights_DF$Prices <= 23837)
nrow(HigherPrices2)*100/nrow(Flights_DF)
summary(HigherPrices2$Prices)
IQR(HigherPrices2$Prices)

#what are these 1 % outliers?
HighestPrices <- subset(Flights_DF, Flights_DF$Prices >= 23837)
sort(HighestPrices$Prices)
#write.xlsx(HighestPrices, "HighestPrices.xlsx")
nrow(HighestPrices)*100/nrow(Flights_DF)
unique(HighestPrices$TOSvsDODLag)
table(HighestPrices$TOSvsDODLag)
sort(table(HighestPrices$`Departure Weekday`)*100/nrow(HighestPrices))
sort(table(HighestPrices$`Search Weekday`))
sort(HighestPrices$Prices[HighestPrices$TOSvsDODLag == 0])
sort(table(HighestPrices$Aircraft)*100/nrow(HighestPrices))
HighestPrices
str(Flights_DF)

#Boxplot Prices
ggplot(Flights_DF, aes(x = "", y = Prices)) + geom_boxplot(outlier.colour = "steelblue4", fill = "lightblue3", col = "grey58") + scale_y_continuous(breaks = seq(2000,54000, by = 2000)) + labs(title = "Airfare Boxplot", x="", y = "Fare (INR)") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey58"), axis.title.x = element_text(colour = "grey58"), axis.title.y = element_text(colour = "grey58"), axis.ticks.x = element_line(colour = "grey58"), axis.ticks.y = element_line(colour = "grey58"), axis.text.y = element_text(colour = "grey58"),  axis.text.x = element_text(colour = "grey58"), axis.line = element_line(colour = "grey15"))

#Are outliers because of a specific independent variable
ggplot(Flights_DF, aes(x = TOSvsDODLag, y = Prices)) + geom_point(colour = "steelblue4") + scale_y_continuous(breaks = seq(2000,54000, by = 2000)) + labs(title = "Airfare vs Lag", x="Lag") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey58"), axis.title.x = element_text(colour = "grey58"), axis.title.y = element_text(colour = "grey58"), axis.ticks.x = element_line(colour = "grey58"), axis.ticks.y = element_line(colour = "grey58"), axis.text.y = element_text(colour = "grey58"),  axis.text.x = element_text(colour = "grey58"), axis.line = element_line(colour = "grey15"))

#Prices vs Lag
#Heatmap
Flights_DF$GenPrices <- lapply(Flights_DF$Prices, function(x) {
  as.integer(x/1000)
})
Flights_DF$GenPrices <- unlist(Flights_DF$GenPrices)
str(Flights_DF)
#HeatMapData <- as.matrix(table(Flights_DF$GenPrices, Flights_DF$TOSvsDODLag))
#heatmap(HeatMapData)
#UniqueFlights <- (distinct(Flights_DF,`Departure Date`, `Departure Time`, Prices, .keep_all = TRUE))
#str(UniqueFlights)
TileData <- as.data.frame(table(Flights_DF$GenPrices, Flights_DF$groupLag))
names(TileData) <- c("Prices", "Lag", "Freq")
unique(TileData$Prices)
#Tileplot
nrow(Flights_DF)*0.25
sort(TileData$Freq)
TileData$Lag <- factor(TileData$Lag, levels = c("Same Day", "1-2 Days","3-7 Days","A week or more" ))
TileData <- TileData %>% arrange(desc(Lag))
TileData$Freq[TileData$Lag == "Same Day"]
#ggplot() + geom_tile(data = TileData, aes(x=Lag, y = Prices, fill = Freq)) + scale_fill_gradientn(breaks = seq(0,7132,by=500), colours = c("White","slategray3","paleturquoise3","lightpink4","Steelblue4","gray20","Red")) + scale_y_discrete(breaks = seq(2,52,by=4),labels = c("2k-6k","6k-10k","10k-14k","14k-18k","18k-22k","22k-26k","26k-30k","30k-34k","34k-38k","38k-42k","42k-46k","46k-50k","50k-54k")) + labs(title = "Prices vs Lag HeatMap", x="Lag") + theme_linedraw()+ theme(legend.key.size = unit(1, "cm"),title = element_text(family = "sans", face = "bold", colour = "grey58"), axis.title.x = element_text(colour = "grey58"), axis.title.y = element_text(colour = "grey58"), axis.ticks.x = element_line(colour = "grey58"), axis.ticks.y = element_line(colour = "grey58"), axis.text.y = element_text(colour = "grey58"),  axis.text.x = element_text(colour = "grey58"), axis.line = element_line(colour = "grey15"))
#+geom_line(axes = FALSE,data = MedianPrices, mapping = aes(x = LagM, y = MedianP)) + scale_x_discrete(breaks = seq(0,14,by=1))
ggplot(TileData, aes(x = Lag, y = Prices)) +geom_tile(data = subset(TileData, Freq!=0), aes(fill = Freq), color = "white", size=0.3)+labs(title = "Airfare vs Lag HeatMap", x="Lag (in days)", y="Fare (INR)", fill = "Frequency of Flights")+ theme(legend.key.size = unit(1, "cm"),title = element_text(family = "sans", face = "bold", colour = "grey15", size = 15), axis.title.x = element_text(colour = "grey15"), axis.title.y = element_text(colour = "grey15"), axis.ticks.x = element_line(colour = "grey15"), axis.ticks.y = element_line(colour = "grey15"), axis.text.y = element_text(colour = "grey15", face = "bold"),  axis.text.x = element_text(colour = "grey15", angle = 90, face = "bold"), axis.line = element_line(colour = "grey15"), legend.title.align = 0.5, legend.title = element_text(size = 10)) + scale_y_discrete(breaks = c(2,6,10,14,18,22,26,30,39,45),labels = c("2k-3k","6k-7k","10k-11k","14k-15k","18k-19k","22k-23k","26k-27k","30k-31k","39k-40k","45k-46k")) + scale_fill_continuous(trans = 'reverse') + scale_fill_gradientn(breaks = c(100,12000,20000, 50448), colours = c("lightskyblue3","steelblue2","steelblue4","blue4"))

#Boxplot for Airfare vs Lag
ggplot(Flights_DF,aes(x = groupLag, y = Prices, group = groupLag))+ geom_boxplot(outlier.colour = "steelblue4", fill = "lightblue3", col = "grey58") + scale_y_continuous(breaks = seq(2000,54000, by = 2000)) + labs(title = "Airfare vs Lag Boxplot",subtitle = "Lag : No. of days between the search and departure dates" ,x="Lag", y = "Fare (INR)") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey15"), axis.title.x = element_text(colour = "grey15"), axis.title.y = element_text(colour = "grey15"), axis.ticks.x = element_line(colour = "grey15"), axis.ticks.y = element_line(colour = "grey15"), axis.text.y = element_text(colour = "grey15"),  axis.text.x = element_text(colour = "grey15"), axis.line = element_line(colour = "grey15"))

#Airfare vs Departure Dates
ggplot(Flights_DF, aes(x = `Departure Date`, y = Prices, group = groupLag)) + geom_point(aes(shape = groupLag, colour = groupLag, size = groupLag),alpha = 0.5) + scale_y_continuous(breaks = seq(2000,54000,by=2000)) + theme(axis.text.x = element_text(angle = 90))+scale_x_date(date_breaks = "4 day") + labs(title = "Airfare vs Departure Dates", x = "Departure Dates", y = "Fare (INR)", color = "Lag", shape = "Lag", size = "Lag") +theme(legend.key.size = unit(1,"cm")) + scale_colour_manual(values = c("lightskyblue3","steelblue4","blue3", "darkblue"))

PricesAvg <- data.frame(aggregate(Flights_DF$Prices, by = list(Flights_DF$`Departure Date`, Flights_DF$groupLag), FUN = mean))
str(PricesAvg)
names(PricesAvg) <- c("Date", "Lag", "Avg Fare")
sort(PricesAvg$Date[PricesAvg$Lag=="1-2 Days"])
PricesAvg <- subset(PricesAvg, PricesAvg$Date >= "2017-10-05" & PricesAvg$Date <= "2017-10-26")
ggplot(PricesAvg, aes(x = Date, y = `Avg Fare`, colour = Lag)) + geom_line() + scale_x_date(date_breaks = "1 day")+ labs(title = "Departure Dates vs Airfare", x="Departure Dates", y = "Average Fare (INR)") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey22"), axis.title.x = element_text(colour = "grey22"), axis.title.y = element_text(colour = "grey22"), axis.ticks.x = element_line(colour = "grey22"), axis.ticks.y = element_line(colour = "grey22"), axis.text.y = element_text(colour = "grey22"),  axis.text.x = element_text(colour = "grey22",angle =90), axis.line = element_line(colour = "grey15"))

#Distribution of Prices over various aircrafts
LowerFares <- subset(Flights_DF,Flights_DF$Prices <= 13000)
HigherFares <- subset(Flights_DF,Flights_DF$Prices > 13000)
ggplot(LowerFares, aes(x = Prices, group = Aircraft,fill = Aircraft)) + geom_density(adjust=1.5, position = "fill") + scale_x_continuous(breaks = seq(1000,13000,by=1000))+ labs(title = "Distribution of Airfare over Aircrafts", x="Fare (INR)") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey22"), axis.title.x = element_text(colour = "grey22"), axis.title.y = element_text(colour = "grey22"), axis.ticks.x = element_line(colour = "grey22"), axis.ticks.y = element_line(colour = "grey22"), axis.text.y = element_text(colour = "grey22"),  axis.text.x = element_text(colour = "grey22", angle = 90), axis.line = element_line(colour = "grey15"))
ggplot(HigherFares, aes(x = Prices, group = Aircraft,fill = Aircraft)) + geom_density(adjust=1.5, position = "fill") + scale_x_continuous(breaks = seq(13000,54000,by=1000))+ labs(title = "Distribution of Airfare over Aircrafts", x="Fare (INR)") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey22"), axis.title.x = element_text(colour = "grey22"), axis.title.y = element_text(colour = "grey22"), axis.ticks.x = element_line(colour = "grey22"), axis.ticks.y = element_line(colour = "grey22"), axis.text.y = element_text(colour = "grey22"),  axis.text.x = element_text(colour = "grey22", angle = 90), axis.line = element_line(colour = "grey15"))
sort(table(LowerPrices$Aircraft)*100/nrow(LowerPrices))

#Airfare vs Departure Weekday
AvgPricesOverWday1 <- aggregate(Flights_DF$Prices, by = list(Flights_DF$TOSvsDODLag,Flights_DF$`Departure Weekday`), FUN = mean)
names(AvgPricesOverWday1) <- c("Lag","Weekday", "Prices")
str(AvgPricesOverWday1)
daysinorder1<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
AvgPricesOverWday1$Weekday <- factor(AvgPricesOverWday1$Weekday, levels= daysinorder)
AvgPricesOverWday1<-AvgPricesOverWday1[order(AvgPricesOverWday1$Weekday), ]
tail(sort(AvgPricesOverWday1$Prices))
ggplot(AvgPricesOverWday1, aes(x = Lag , y = Prices, colour = Weekday)) + geom_line()   + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Airfare vs Departure Weekday", x = "Lag (in days)", y = "Average Fare (INR)") + scale_x_continuous(breaks = seq(0,15,by=1)) + scale_y_continuous(breaks = seq(5000,11000,by=1000))

#legend TOD

AvgPricesOverWday <- aggregate(Flights_DF$Prices, by = list(Flights_DF$TOSvsDODLag,Flights_DF$`Departure Weekday`,Flights_DF$DepartureTimeofDay), FUN = mean)
names(AvgPricesOverWday) <- c("Lag","Weekday", "TOD", "Prices")
str(AvgPricesOverWday)
daysinorder<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
AvgPricesOverWday$Weekday <- factor(AvgPricesOverWday$Weekday, levels= daysinorder)
AvgPricesOverWday<-AvgPricesOverWday[order(AvgPricesOverWday$Weekday), ]
head(sort(AvgPricesOverWday$Prices))
ggplot(AvgPricesOverWday, aes(x = Weekday , y = Prices, group = TOD, colour = TOD)) + geom_line() +  theme(axis.text.x = element_text(angle = 90)) + labs(title = "Variation of Prices over Departure Weekday", x = "Day", y = "Average Prices") + facet_grid(. ~ Lag)

#legend Departure Hour
AvgPricesOverWdayH <- aggregate(Flights_DF$Prices, by = list(Flights_DF$TOSvsDODLag ,Flights_DF$`Departure Weekday`,Flights_DF$DepartureH), FUN = mean)
names(AvgPricesOverWdayH) <- c("Lag", "Weekday", "Hour" ,"Prices")
str(AvgPricesOverWdayH)
daysinorder<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
AvgPricesOverWdayH$Weekday <- factor(AvgPricesOverWdayH$Weekday, levels= daysinorder)
AvgPricesOverWdayH<-AvgPricesOverWdayH[order(AvgPricesOverWdayH$Weekday), ]

ggplot(AvgPricesOverWdayH, aes(x = Weekday , y = Prices, group = Hour, colour = Hour)) + geom_line() + scale_y_continuous(breaks = seq(2000,54000,by=2000))  + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Variation of Prices over Departure Weekday", x = "Day", y = "Average Prices") + facet_wrap(~ Lag)



#Prices vs Departure Time of the Day
table(Flights_DF$DepartureTimeofDay)

FairDTOD <- data.frame(aggregate(Flights_DF$Prices, by = list(Flights_DF$TOSvsDODLag, Flights_DF$DepartureTimeofDay), FUN=mean))
str(FairDTOD)
names(FairDTOD) <- c("Lag", "Departure Time of Day", "Average Fair")
ggplot(FairDTOD, aes(x = Lag, y = `Average Fair`, colour = `Departure Time of Day`)) + geom_line() + scale_x_continuous(breaks = seq(0,15,by=1)) + labs(title = "Airfare vs Departure Time of the day", x="Lag (in days)", y = "Average Fare (INR)") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey22"), axis.title.x = element_text(colour = "grey22"), axis.title.y = element_text(colour = "grey22"), axis.ticks.x = element_line(colour = "grey22"), axis.ticks.y = element_line(colour = "grey22"), axis.text.y = element_text(colour = "grey22"),  axis.text.x = element_text(colour = "grey22"), axis.line = element_line(colour = "grey15"))

FairDTOD2 <- data.frame(aggregate(Flights_DF$Prices, by = list(Flights_DF$TOSvsDODLag), FUN=mean))
names(FairDTOD2) <- c("Lag", "Average Fair")
ggplot(FairDTOD2, aes(x = Lag, y = `Average Fair`)) + geom_line(col = "steelblue4") + scale_x_continuous(breaks = seq(0,15,by=1))
LowerPrices$Prices[LowerPrices$DepartureTimeofDay=="Late Night"]

#Prices vs Departure Time of the Day vs weekday
FairDTODWday <- data.frame(aggregate(Flights_DF$Prices, by = list(Flights_DF$TOSvsDODLag, Flights_DF$`Departure Weekday`, Flights_DF$DepartureTimeofDay), FUN=mean))
str(FairDTODWday)
names(FairDTODWday) <- c("Lag","Departure Weekday", "Departure Time of Day", "Average Fair")
FairDTODWday$`Departure Weekday` <- factor(FairDTODWday$`Departure Weekday`, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
FairDTODWday <- FairDTODWday %>% arrange(desc(`Departure Weekday`))
ggplot(FairDTODWday, aes(x = Lag, y = `Average Fair`, colour = `Departure Time of Day`)) + geom_line() + scale_x_continuous(breaks = seq(0,15,by=1)) + labs(title = "Airfare vs Departure Time of the day", x="Lag (in days)", y = "Average Fare (INR)") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey22"), axis.title.x = element_text(colour = "grey22"), axis.title.y = element_text(colour = "grey22"), axis.ticks.x = element_line(colour = "grey22"), axis.ticks.y = element_line(colour = "grey22"), axis.text.y = element_text(colour = "grey22"),  axis.text.x = element_text(colour = "grey22", angle = 90), axis.line = element_line(colour = "grey15"))+ facet_wrap(~ LowerPrices$`Departure Weekday`) + facet_wrap(~ `Departure Weekday`, nrow=2)

#Departure DOM vs Airfare
sort(table(LowerPrices$DepartureDOM))
FareDOM <- data.frame(aggregate(Flights_DF$Prices, by=list(Flights_DF$TOSvsDODLag, Flights_DF$DepartureDOM), FUN = mean))
str(FareDOM)
names(FareDOM) <- c("Lag", "DOM", "Avg Fare")
ggplot(FareDOM, aes(x = Lag, y = `Avg Fare`, colour = DOM)) + geom_point() + scale_color_gradientn(colours = c("Red", "Blue", "Grey", "Green","Black"), breaks = seq(1,31,by=7)) + labs(title = "Departure Date of the Month vs Airfare", x="Lag", y = "Average Fare") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey22", size = 12), axis.title.x = element_text(colour = "grey22"), axis.title.y = element_text(colour = "grey22"), axis.ticks.x = element_line(colour = "grey22"), axis.ticks.y = element_line(colour = "grey22"), axis.text.y = element_text(colour = "grey22"),  axis.text.x = element_text(colour = "grey22"), axis.line = element_line(colour = "grey15"))


#Lag vs Airfare
# LagFareStats <- data.frame(aggregate(LowerPrices$Prices, by = list(LowerPrices$TOSvsDODLag), FUN = mean))
# str(LagFareStats)
# names(LagFareStats) <- c("Lag", "Mean")
# LagFareStatsMedian <- data.frame(aggregate(LowerPrices$Prices, by = list(LowerPrices$TOSvsDODLag), FUN = median))
# LagFareStatsMin <- data.frame(aggregate(LowerPrices$Prices, by = list(LowerPrices$TOSvsDODLag), FUN = min))
# LagFareMax <- data.frame(aggregate(LowerPrices$Prices, by = list(LowerPrices$TOSvsDODLag), FUN = max))
# LagFareStats$Median <- LagFareStatsMedian$x
# LagFareStats$Min <- LagFareStatsMin$x
# LagFareStats$Max <- LagFareMax$x
# LagFareStats <- melt(LagFareStats, id.vars = "Lag")
# str(LagFareStats)
# ggplot(LagFareStats, aes(x = Lag, y = value, colour = variable)) + geom_line()


#Weekday/End Departure vs Airfare
PricesWeek_end <- data.frame(aggregate(Flights_DF$Prices, by = list(Flights_DF$TOSvsDODLag, Flights_DF$isitWeekendDep), FUN = mean))
str(PricesWeek_end)
names(PricesWeek_end) <- c("Lag", "Weekdayend", "Avg Fare")
ggplot(PricesWeek_end, aes(x = Lag, y = `Avg Fare`, colour = Weekdayend)) + geom_line() + scale_x_continuous(breaks = seq(0,15,by=2))+ labs(title = "Weekday/end Departure vs Airfare", x="Lag (in days)", y = "Average Fare (INR)") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey22"), axis.title.x = element_text(colour = "grey22"), axis.title.y = element_text(colour = "grey22"), axis.ticks.x = element_line(colour = "grey22"), axis.ticks.y = element_line(colour = "grey22"), axis.text.y = element_text(colour = "grey22"),  axis.text.x = element_text(colour = "grey22"), axis.line = element_line(colour = "grey15"))

LowerPrices$DepartDateTime <- paste(LowerPrices$`Departure Date`,LowerPrices$`Departure Time`)
unique(LowerPrices$DepartDateTime)

#Airfare vs Time of day of Search
sort(table(LowerPrices$`TimeofDay of Search`))
PricesTODSrch <- data.frame(aggregate(Flights_DF$Prices, by = list(Flights_DF$TOSvsDODLag,Flights_DF$`TimeofDay of Search`),FUN = mean))
str(PricesTODSrch)
names(PricesTODSrch) <- c("Lag","Search Time of Day", "Avg Fare")
ggplot(PricesTODSrch, aes(x = Lag, y = `Avg Fare`, colour = `Search Time of Day`)) + geom_line() + scale_x_continuous(breaks = seq(0,15,by=2))+ labs(title = "Search Time of Day vs Airfare", x="Lag (in days)", y = "Average Fare (INR)") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey22"), axis.title.x = element_text(colour = "grey22"), axis.title.y = element_text(colour = "grey22"), axis.ticks.x = element_line(colour = "grey22"), axis.ticks.y = element_line(colour = "grey22"), axis.text.y = element_text(colour = "grey22"),  axis.text.x = element_text(colour = "grey22"), axis.line = element_line(colour = "grey15"))


#Weekday/End Search vs Airfare
PricesWeek_endS <- data.frame(aggregate(Flights_DF$Prices, by = list(Flights_DF$TOSvsDODLag, Flights_DF$isitWeekendSearch), FUN = mean))
str(PricesWeek_endS)
names(PricesWeek_endS) <- c("Lag", "Weekdayend", "Avg Fare")
ggplot(PricesWeek_endS, aes(x = Lag, y = `Avg Fare`, colour = Weekdayend)) + geom_line() + scale_x_continuous(breaks = seq(0,15,by=2))+ labs(title = "Search Weekday/end vs Airfare", x="Lag", y = "Average Fare") + theme_linedraw()+ theme(title = element_text(family = "sans", face = "bold", colour = "grey22"), axis.title.x = element_text(colour = "grey22"), axis.title.y = element_text(colour = "grey22"), axis.ticks.x = element_line(colour = "grey22"), axis.ticks.y = element_line(colour = "grey22"), axis.text.y = element_text(colour = "grey22"),  axis.text.x = element_text(colour = "grey22"), axis.line = element_line(colour = "grey15"))
LowerPrices$DepartDateTime <- paste(LowerPrices$`Departure Date`,LowerPrices$`Departure Time`)
unique(LowerPrices$DepartDateTime)
