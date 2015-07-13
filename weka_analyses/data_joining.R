rm(list=ls())


library(dplyr)
library(foreign)


data <- read.csv("./raw_data.csv",header = TRUE, stringsAsFactors = FALSE)
spray <- read.csv("./spray.csv", header = TRUE,stringsAsFactors=FALSE)
weather <- read.csv("./weather.csv",header = TRUE,stringsAsFactors=FALSE)

# Assign coords to weather data
station1Lat <- 41.995
station1Lon <- -87.933
station2Lat <- 41.786
station2Lon <- -87.752
weather <- weather %>% 
  mutate(
    Latitude = c(station1Lat,station2Lat)[Station],
    Longitude = c(station1Lon,station2Lon)[Station])


d <- function(lat1,lon1,lat2,lon2){
  # Takes two coordinates and returns an estimate of the cartesian
  # distance between them. At these distances, the earth's surface is roughly Euclidean.
  # Takes distance in degrees, converts to kilometers using 57.29miles/degree.
  return(sqrt((lat2-lat1)^2 + (lon2-lon1)^2) * 57.29)
}

# Remove latitude and longitude from weather data

data <- data %>%
  mutate(closestStation = ifelse(
    d(Latitude,Longitude,station1Lat,station1Lon) >= d(Latitude,Longitude,station2Lat,station2Lon),1,2))

data  <- inner_join(data,select(weather,-Latitude,-Longitude),by = c("Date" = "Date", "closestStation" = "Station"))

to_factors <- c(
  'Dataset','Address','Species','Block','Street','Trap',
  'AddressNumberAndStreet','AddressAccuracy','NumMosquitos',
  'WnvPresent','Depart','Heat','Cool','CodeSum','ResultDir')
to_num <- c(
  'Tmax','Tmin','Tavg','DewPoint','WetBulb',
  'Sunrise','Sunset','PrecipTotal','StnPressure','SeaLevel',
  'ResultSpeed','AvgSpeed')

data[,to_factors] <- lapply(data[,to_factors],as.factor)
data[,to_num] <- lapply(data[,to_num],as.numeric)


data$Date <- as.Date(data$Date , "%m/%d/%Y")
spray$Date <- as.Date(spray$Date, "%m/%d/%Y")


# Join the spray data:
#   - Sprays are listed by date, time, lat , lon
#   - What kind of feature to create?
#     - Days since last spray within x miles? What should x be?
#     - Number of sprays with x miles in the last month?


# a matrix of booleans, where the rows are dates of datapoints, and columns are dates of sprays
# value indicates whether sprays occurred less than a month prior for given datapoint.
date_diff <- outer(sort(unique(data$Date)),sort(unique(spray$Date)), FUN = function(x,y){(x-y) <= 30 & (x-y) >= 0})
rownames(date_diff) <- as.character(sort(unique(data$Date)))
colnames(date_diff) <- as.character(sort(unique(spray$Date)))

data$Month = as.factor(format(data$Date,'%b'))
data$YearMonth = as.factor(format(data$Date,'%Y-%b'))

data <- data[,c(setdiff(names(data),'WnvPresent'),'WnvPresent')]

train <- data %>% filter(Dataset == "Train") %>% select(-Dataset)
test <-  data %>% filter(Dataset == "Test") %>% select(-Dataset)

write.arff(file="./Train.arff",train)
write.arff(file="./Test.arff",test)
write.arff(file="./Data.arff",select(data,-Dataset))
