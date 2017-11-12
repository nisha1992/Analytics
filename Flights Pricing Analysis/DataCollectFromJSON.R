#GoogleFlight API
API_Key <- "######################"
#install.packages("RCurl")
#install.packages("jsonlite")
#install.packages("xlsx")
library("rvest")
library("httr")
library("RCurl")
library("jsonlite")
library("xlsx")
#ls("package:rvest")
#ls("package:httr")
# x = "/path/JSON_Query/flights_data_rev_2017_09_30_11_0.json"
# Flights_tmp <- fromJSON(x, flatten = TRUE)
# Flights_tmp
# TimeofQ <- substring(x,100,101)
# DateofQ <- substring(x,89,98)
# if(substring(x, 89,91) == "rev"){
#   TimeofQ <- substring(x, 104, 105)
#   DateofQ <- substring(x, 93, 102)
# }
# TimeofQ
# DateofQ
#read data from json using Google QPX Flight API
files <- list.files(path="/Pathofyourfolder/JSONQuery/", pattern = "*.json", full.names = T, recursive = FALSE)
undebug(lapply)
lapply(files, function(x){
  Flights_Data <- fromJSON(x, flatten = TRUE)
  TimeofQ <- substring(x,100,101)
  DateofQ <- substring(x,89,98)
  if(substring(x, 89,91) == "rev"){
    TimeofQ <- substring(x, 104, 105)
    DateofQ <- substring(x, 93, 102)
  }
  #Flights_Data
  #str(Flights_Data)

  #airport details
  airport <- Flights_Data[[2]][[3]][[2]]
  airport$city <- Flights_Data[[2]][[3]][[3]][[3]]
  airport$kind <- NULL
  #str(airport)
  write.table(airport, file="Airports.csv", append = TRUE, sep="   ", col.names = FALSE)
  airport <- NULL

  #aircraft details
  aircraft <- Flights_Data$trips$data$aircraft
  aircraft$kind <- NULL
  #str(aircraft)
  write.table(aircraft, file="Aircrafts.csv", append = TRUE, sep="   ", col.names = FALSE)
  aircraft <- NULL

  #carrier details
  carrier <- Flights_Data$trips$data$carrier
  carrier$kind <- NULL
  #str(carrier)
  write.table(carrier, file="Carriers.csv", append = TRUE, sep="   ", col.names = FALSE)
  carrier <- NULL

  #trip options
  len <- nrow(Flights_Data$trips$tripOption)
  Trip_Options <- data.frame(c(1:len))
  #str(Trip_Options)
  Trip_Options$ID <- Flights_Data$trips$tripOption$id
  Trip_Options$SaleTotal <- Flights_Data$trips$tripOption$saleTotal
  SliceList <- Flights_Data$trips$tripOption$slice
  #str(SliceList)
  #ncol(Trip_Options)
  #str(Trip_Options)
  #Trip_Options
  for(i in 1:len){
    Trip_Options[i,4] <- Flights_Data$trips$tripOption$slice[[i]][3][[1]][[1]][8][[1]][[1]][3]
    Trip_Options[i,5] <- Flights_Data$trips$tripOption$slice[[i]][3][[1]][[1]][8][[1]][[1]][6]
    Trip_Options[i,6] <- Flights_Data$trips$tripOption$slice[[i]][3][[1]][[1]][8][[1]][[1]][7]
    Trip_Options[i,7] <- Flights_Data$trips$tripOption$slice[[i]][3][[1]][[1]][8][[1]][[1]][5]
    Trip_Options[i,8] <- Flights_Data$trips$tripOption$slice[[i]][3][[1]][[1]][8][[1]][[1]][4]
    Trip_Options[i,9] <- Flights_Data$trips$tripOption$slice[[i]][3][[1]][[1]][8][[1]][[1]][10]
    Trip_Options[i,10] <- paste(Flights_Data$trips$tripOption$slice[[i]][3][[1]][[1]][9],Flights_Data$trips$tripOption$slice[[i]][3][[1]][[1]][10], sep="")
    colnames(Trip_Options)[10] <- "Flight Number"
    Trip_Options[i,11] <- DateofQ
    Trip_Options[i,12] <- TimeofQ
    Trip_Options[i,13] <- Flights_Data$trips$tripOption$pricing[[i]][12]
  }

  write.table(Trip_Options, file="Flight_Data.csv", append = TRUE, sep="   ", col.names = FALSE)
  Trip_Options <- NULL

})
