
##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## Created by Jessica Ruff and David March (2021)

# Update package and standarized field names following Sequeria et al., 2021
# by Javier Menéndez-Blázquez | @jmenblaz



##------------------------------------------------------------------------------------------------------------------------------##
## 2.5 Process data for day vs. night calculations
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
## day vs. night separation
## result is two separate dataframes named: "day" and "night"


# Process day/night for location --- already processed in ttdr process (01_tracking/scr/04_process_ttdr.R)
# here also calculate dawn and dusk

# library(maptools)
library(suntools) # use suntools (newer) instead of maptools
library(raster)

data <- ttdr

### Incorporate sun and lunar metrics
### Calculate: sunrise and sunset times
### Derive: day/night, time to sunset, absolute diff time to sunrise/sunset

data$sunrise <- suntools::sunriset(crds=cbind(data$longitude, data$latitude), dateTime=data$time, direction=c("sunrise"), POSIXct.out=TRUE)$time
data$sunset <- suntools::sunriset(crds=cbind(data$longitude, data$latitude), dateTime=data$time, direction=c("sunset"), POSIXct.out=TRUE)$time
data$dawn <- suntools::crepuscule(crds=cbind(data$longitude, data$latitude), dateTime=data$time, solarDep=12, direction="dawn", POSIXct.out=TRUE)$time  # nautical(12), astronomic(18), civil(6)
data$dusk <- suntools::crepuscule(crds=cbind(data$longitude, data$latitude), dateTime=data$time, solarDep=12, direction="dusk", POSIXct.out=TRUE)$time  # nautical(12), astronomic(18), civil(6)


## Derive day/night
time<-data$time[1]
dawn<-data$dawn[1]
dusk<-data$dusk[1]

daynight <- function (time, sunrise, sunset){
  ifelse(time >= sunrise & time <= sunset, "day", "night")
}

data$daynight <- mapply(daynight, time=data$time, sunrise=data$sunrise, sunset=data$sunset)

# select 
day <- data[data$daynight=="day",]
night <- data[data$daynight=="night",]

rm(data)

##------------------------------------------------------------------------------------------------------------------------------##
