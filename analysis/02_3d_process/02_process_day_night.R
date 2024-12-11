
##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles

## Created by Jessica Ruff and David March (2021)

# Update package and standarized field names following Sequeria et al., 2021
# by Javier Menéndez-Blázquez | @jmenblaz


##------------------------------------------------------------------------------------------------------------------------------##
## 2. Process data for day vs. night calculations
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
## day vs. night separation
## result is two separate dataframes named: "day" and "night"

# Process day/night for location --- already processed in ttdr process (01_tracking/scr/04_process_ttdr.R)
# here also calculate dawn and dusk


source("setup.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # custom functions for 3d process

library(suntools) # use suntools (newer) instead of maptools
library(raster)

# 0) custom function -------------------------------

daynight <- function (time, sunrise, sunset){
  ifelse(time >= sunrise & time <= sunset, "day", "night")
}


# 1) Import preprocessed ttdr data --------------------------------------------
# Use L3 level (see 01_3d_pre_process.R)

ttdr_files <- list.files(paste0(main_dir,"/input/tracking/ttdr/L3"), full.names = TRUE, pattern = "L3_ttdr.csv")
organismIDs <- sub("_L3_ttdr\\.csv$", "", basename(ttdr_files))


# 2) obtain day, night, sunrise, sunset, dawn and dusk information

t <- Sys.time()

cores <- detectCores() - 2
cl <- makeCluster(cores)
registerDoParallel(cl)

getDoParWorkers() # backend information

# pararell processing

foreach(ttdr = ttdr_files, .packages=c("dplyr", "raster", "suntools", "lubridate")) %dopar% {  

  # extract organismID from L3_ttdr fiel name
  organismID <- sub("_L3_ttdr\\.csv$", "", basename(ttdr))
  
  # read data
  data <- read.csv(ttdr, dec=",", head=TRUE)
  # parse / format time date for ttdr data and conver to numeric
  data$time <- lubridate::parse_date_time(data$time, "Ymd HMS")
  data <- data |> mutate(across(c(depth_upper_error, depth_lower_error, depth, drange), as.numeric))
  
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
  
  
  # apply function created previously
  data$daynight <- mapply(daynight, time=data$time, sunrise=data$sunrise, sunset=data$sunset)
  
  # split ttdr data based on day and night records
  day <- data[data$daynight=="day",]
  night <- data[data$daynight=="night",]
  
  rm(data) # cleand enviroment
  
  # export pre-processed data of ttdr L3 by day and night ----------------------

  output_data <- paste0(input_dir,"/","tracking/ttdr/L3")
  if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)
  
  L3_day   <- paste0(output_data, "/",organismID,"_L3_ttdr_day.csv")
  L3_night <- paste0(output_data, "/",organismID,"_L3_ttdr_night.csv")
  
  write.csv(day, L3_day, row.names = FALSE)
  write.csv(night, L3_night, row.names = FALSE)
  
  # info 
  message("Processing individual:", i,"/",length(organismIDs), " -- Finished -- \n")
  message(" · organismID:", organismID)
  message("\n")
  message("L3 TTDR and Locations data processed for 3D analysis")
  message("\n")
  message("\n")
  
}

Sys.time() - t # 5:30 min
stopCluster(cl)
  
# info 
cat("- 3D pre-process completed - \n
         -- L3 TTDR and Locations data processed")
message("\n")
message("\n")

