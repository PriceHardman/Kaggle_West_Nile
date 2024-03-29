rm(list=ls())


library(gstat)
library(rgdal)
library(spacetime)
library(xts)
library(maptools)
library(maps)
data(wind)

library("gstat")
data("wind")
wind.loc$y <- as.numeric(char2dms(as.character(wind.loc[["Latitude"]])))
wind.loc$x <- as.numeric(char2dms(as.character(wind.loc[["Longitude"]])))
coordinates(wind.loc) <- ~ x + y
proj4string(wind.loc) <- "+proj=longlat +datum=WGS84"

wind$time <- ISOdate(wind$year + 1900, wind$month, wind$day)
wind$jday <- as.numeric(format(wind$time, "%j"))

stations <- 4:15
windsqrt <- sqrt(0.5148 * as.matrix(wind[stations]))
Jday <- 1:366
windsqrt <- windsqrt - mean(windsqrt)
daymeans <- sapply(split(windsqrt, wind$jday), mean)
meanwind <- lowess(daymeans ~ Jday, f = 0.1)$y[wind$jday]
velocities <- apply(windsqrt, 2, function(x) { x - meanwind })

wind.loc <- wind.loc[match(names(wind[4:15]), wind.loc$Code), ]
pts <- coordinates(wind.loc[match(names(wind[4:15]), wind.loc$Code), ])
rownames(pts) <- wind.loc$Station
pts <- SpatialPoints(pts, CRS("+proj=longlat +datum=WGS84"))

utm29 <- CRS("+proj=utm +zone=29 +datum=WGS84")
pts <- spTransform(pts, utm29)

wind.data <- stConstruct(velocities,space = list(values = 1:ncol(velocities)), time = wind$time,SpatialObj = pts, interval = TRUE)

m <- map2SpatialLines(map("world", xlim = c(-11, -5.4),ylim = c(51, 55.5), plot = FALSE))
proj4string(m) <- "+proj=longlat +datum=WGS84"
m <- spTransform(m, utm29)

grd <- SpatialPixels(SpatialPoints(makegrid(m, n = 300)),proj4string = proj4string(m))

wind.data <- wind.data[, "1961-04"]
n <- 10
tgrd <- xts(1:n, seq(min(index(wind.data)), max(index(wind.data)),length = n))
pred.grd <- STF(grd, tgrd)

v <- list(space = vgm(0.6, "Exp", 750000),time = vgm(1, "Exp", 1.5 * 3600 * 24))
pred <- krigeST(values ~ 1, wind.data, pred.grd, v)
wind.ST <- STFDF(grd, tgrd, data.frame(sqrt_speed = pred))

layout <- list(list("sp.lines", m, col = "gray"),list("sp.points", pts, first = FALSE, cex = 0.5))
stplot(wind.ST, col.regions = brewer.pal(11, "RdBu")[-c(10, 11)],at = seq(-1.375, 1, by = 0.25),par.strip.text = list(cex = 0.7), sp.layout = layout)

