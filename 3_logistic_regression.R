rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggmap)
library(spacetime)

chicago_map <- readRDS("./mapdata_copyright_openstreetmap_contributors.rds")

data <- read.csv('./data.csv',header = TRUE)

data <- data %>% mutate(
  Date = as.Date(Date),
  Month = as.factor(format(Date,"%Y-%m"))
)

dateRange = seq.Date(
  from = min(data$Date),
  to   = max(data$Date),
  by   = 1)

monthRange = 

traps <- data %>%
  group_by(Trap) %>%
  distinct(Trap) %>%
  select(Trap,x = Longitude,y = Latitude) %>%
  arrange(Trap) %>%
  ungroup()
rownames(traps) <- traps$Trap
traps <- traps %>% select(x,y)


