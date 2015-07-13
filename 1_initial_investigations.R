rm(list=ls())


library(dplyr)
library(ggplot2)
library(ggmap)
# library(animation)
# ani.options(convert = "C:/Program Files/ImageMagick-6.9.1-Q16/convert.exe") # for animation package


train <- read.csv("./train.csv/train.csv", header = TRUE)
test <- read.csv("./test.csv/test.csv", header = TRUE)
spray <- read.csv("./spray.csv//spray.csv", header = TRUE)
weather <- read.csv("./weather.csv//weather.csv",header = TRUE)
train$WnvPresent <- factor(train$WnvPresent)

# We'll begin with the simplest possible model: All negative results.

all_negative <- test %>% transmute(Id, WnvPresent = 0)
write.csv(all_negative,'./submissions/0_all_negative.csv',row.names = FALSE)


# By notes on Kaggle site, weather station 1 is O'Hare (Lat: 41.995 Lon: -87.933)
# weather station 2 is Midway (Lat: 41.786 Lon: -87.752)
# At these latitudes, 1 degree is roughly 80km.

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
  # Takes distance in degrees, converts to kilometers using 80km/degree.
  return(sqrt((lat2-lat1)^2 + (lon2-lon1)^2) * 80)
}

# Assign each observation to the closest weather station:
train <- train %>%
  mutate(closestStation = ifelse(
    d(Latitude,Longitude,station1Lat,station1Lon) >= d(Latitude,Longitude,station2Lat,station2Lon),1,2))
test <- test %>%
  mutate(closestStation = ifelse(
    d(Latitude,Longitude,station1Lat,station1Lon) >= d(Latitude,Longitude,station2Lat,station2Lon),1,2))

# plot the cases over time on a map of Chicago
chicago_map <- readRDS("./mapdata_copyright_openstreetmap_contributors.rds")
