
# Title: GFW API extract fishhing event info

#-------------------------------------------------------------------------------
# 02. GFW day / night apparent fishing effort maps -> extract fishhing event info
#-------------------------------------------------------------------------------

# Javier Menéndez-Blázquez | @jmenblaz


#' Use gfw::get_event function to extract gear_type and day and night information about the
#' different apparent fishing events

#' Fishing event has a information about datetime and position (lat/lon) that
#' correspond to centroid of the AIS vessel positions designed as fishing activities
#' by Global Fishing Watch algorithm

#' This could be comply some issues depend of the analysis resolution, but here
#' using 10x10 km2 for further analysis

# 03_gfw_api_event_daynight_map transform the data downloaded here into a raster
# by gear_types and day / and night time to detailed fishing activities analysis


# --------------------------------------------------------------------

# 1) Set study area and period 
# 2) Get vessels info for the study area and datetime
#   - L0 raw information got from GFW API

# 3) Identify fishing gear for vessels id in apparent fishing events detected
# 4) Combine fishing events with fishing gear info by $vesselId
# 5) filter by fishing gear of interests
# 6) Calculate apparent fishing hours of each fishing event
# 7) Calculate day/night of apparent fishing operation by start and end time
#   - L1 final data set with fishing event and its vessel and other associted information









# load packages
library(lubridate)
library(suntools)

library(data.table)
library(tidyverse)
library(qdapRegex)
library(readr)
library(sp)
library(sf)
library(ggplot2)
library(raster)

# geojson files
library(jsonlite)

# remote install gfwr R package (see setup.R)
# Global Fishing Watch
library(gfwr)


# outpaths
outfolder <- paste0(input_dir,"/gfw/daynight/raw")
if (!dir.exists(outfolder)) dir.create(outfolder, recursive = TRUE)



# 0) ------------------------------------------------------------------------------
# APY key for GFW 
# load in setup.R
key # check it is loaded propertly

# a) load vessel information from GFW
# Note that we can't use this GFW provided info due that vesselIds are not
# gathered in this database, only MMSI and/or IMO number
# vessels_info <- read.csv(paste0(input_dir,"/gfw/gfw-fishing-vessels-v3.csv"))

# b) Alternative use gfwr functions to obtain datasets



# 1) Set study area and period--------------------------------------------------
# select study area (created previously)                   ------------------

# load area from geojson (or spatial object)
# area should be a sf class object
# area_json <- fromJSON(paste0(input_dir,"/gis/study_area.geojson"))
# area_json <- read_lines(paste0(input_dir,"/gis/study_area.geojson"))
area <- st_read(paste0(input_dir,"/gis/study_area.geojson"))

# study years
# 2012 and 2013 processed during code testing
years <- (2014:2024)


# 2) Get vessels info for the study area and datetime   -----------------------

for (y in 1:length(years)) {
  
  year <- years[y]
  
  # get_event function right properly by week (timeout request APIs for 2024 by month)
  # for this extension area == balance between area extension and temporal range
  
  # Set study dates:
  period_start_date <- paste0(year,'-01-01')
  period_end_date <- paste0(year,'-12-31')
  
  # all month-year dates sequency for get fishing events
  dates <- seq.Date(as.Date(period_start_date), as.Date(period_end_date), by = "weeks")
  
  
  # ----------------------------------------------------------------------------------
  # 3) get vessels info for the study area and datetime                   ----------
  # Note that for extend area or hide temporal range APIs call doesn't work properlly
  # Make different call and combine theses results into same dataset
  
  # for each months get vessel events and combine
  # manage errors and pauses for the APIs requests
  
  # empty list to store monthly fishing event data
  
  event_list <- list()
  
  
  t <- Sys.time()
  
  for (i in seq_along(dates)) {
    
    # for each date
    start_date <- dates[i]
    end_date <- start_date %m+% weeks(1) - days(1)  # end of the week (logic: 7 seven days - 1 day (start week 2))
    # info
    cat("Searching fishing events:", as.character(start_date), "/", as.character(end_date), "| week",i,"/",length(dates), "\n")
    cat("\n")
    
    # errors
    success <- FALSE
    while (!success) {
      
      tryCatch({
        
        # get event information
        ev <- gfwr::get_event(event_type = "FISHING",
                              start_date = as.character(start_date),
                              end_date = as.character(end_date),
                              region = area,
                              region_source = "USER_SHAPEFILE",
                              quiet = TRUE,
                              key = key) #Authorization token. Can be obtained with gfw_auth function
        # check results --- 
        if (!is.null(ev) && nrow(ev) > 0) {
          event_list[[length(event_list) + 1]] <- ev
          cat(" · Fishing events: ", nrow(ev), "\n")
          cat("\n")
        } else {
          cat(" · No fishing event found for this period in the area\n")
          cat("\n")
        }
        
        # Time counter (timer) for API request pause time
        for (j in 1:2) {
          cat(paste0("    · 00:00:0",j, " [Next search in 2s]\r"))
          Sys.sleep(1)
        }
        cat(rep("\n",2))
        
        # flag
        success <- TRUE  # exit loop si la llamada fue exitosa
        
        # some errors --- 
      }, error = function(e) {
        message(" ··· Error in API request; period:", start_date, " -> ", conditionMessage(e), "\n")
        cat(rep("\n",1))
        
        for (j in 1:10) {
          cat(paste0("    · 00:00:0",j, " [Next search in 10s]\r"))
          Sys.sleep(1)
        }
        cat(rep("\n",2))
      })
    }
  }
  
  # combine findings
  event_data <- rbindlist(event_list, fill = TRUE)
  
  # potential dates for next year, remove it
  event_data <- event_data %>%
    filter(start >= as.Date(period_start_date) & start <= as.Date(period_end_date))
  head(event_data)
  
  # remove listed information into dataframe (see GFW for documentation)
  event_data <- event_data[, !sapply(event_data, is.list), with = FALSE]
  # remove columns 'vessel_next_port'
  # data.table (fishing evet format)
  event_data <- event_data[, !("vessel_nextPort"), with = FALSE]
  
  # export event data by year as backup in output folder
  write.csv(event_data, paste0(outfolder,"/gfw_fishing_event_data_",year,"_L0.csv"), row.names = FALSE)
  
  Sys.time() - t 
  
  
  
  
  
  # -------------------------------------------------------------------------------
  # 3) Identify fishing gear for vessels id in apparent fishing events detected
  
  # for all event data compiled select uniques IDs
  # Use unique vesselId in order to reduce processing time due that a vesselId 
  # could apper repeatably 
  
  # create data frame with GFW vessel id and its gear type detected in the fishing events
  vesselIds <- data.frame(vesselId  = unique(event_data$vesselId),
                          gear_type = NA,
                          ship_name = NA,
                          length_m  = NA,
                          flag      = NA,
                          ssvid     = NA,
                          imo       = NA,
                          tonnageGt = NA,
                          stringsAsFactors = FALSE)
  
  cat(nrow(vesselIds), " - Unique vesselIds in the fishing events \n")
  
  
  
  t <- Sys.time()
  
  for (i in 1:nrow(vesselIds)) {
    
    id <- vesselIds$vesselId[i]
    cat("Searching information for vesselId:", id, " | vessel id:", i, "/", nrow(vesselIds), "\n")
    
    # potential errors processing
    success <- FALSE # error flag
    attempt <- 1 # try two times if there is some error
    
        # 1º -> Potential API request error o conexion
        # 2º -> Same, but if the error perxist, could be due that there is no info
        #       about this id, then next id
    
    while (!success && attempt <= 2) {
      tryCatch({
        # get vessel info
        vesselId_info <- gfwr::get_vessel_info(search_type = "id",
                                               ids = id,
                                               quiet = TRUE,
                                               key = key)
        
        if (nrow(vesselId_info$registryInfo) > 0 &&
            "geartypes" %in% colnames(vesselId_info$registryInfo)) {
          
          reg <- vesselId_info$registryInfo
          
          vesselIds$gear_type[i]  <- reg$geartypes[1]
          vesselIds$ship_name[i]  <- reg$shipname[1]
          vesselIds$length_m[i]   <- reg$lengthM[1]
          vesselIds$flag[i]       <- reg$flag[1]
          vesselIds$ssvid[i]      <- reg$ssvid[1]
          vesselIds$imo[i]        <- reg$imo[1]
          vesselIds$tonnageGt[i]  <- reg$tonnageGt[1]
          
          success <- TRUE
          cat(" · Success \n")
          
        } else {
          cat(" · No vessel information or missing columns for this id.\n")
          success <- TRUE  # Succes even if there are no info after two attempts
        }
        
        # error handling
      }, error = function(e) {
        message(" · Error (attempt ", attempt, "): ", conditionMessage(e))
      })
      
      if (!success) {
        for (j in 1:5) {
          cat(paste0("    · 00:00:0",j, " [retrying in 5s]\r"))
          Sys.sleep(1)
        }
        cat("\n\n")
      }
      attempt <- attempt + 1
    }
    
    # After process or attempts -> Succes, next into two seconds
    if (success) {
      
      for (j in 1:2) {
        cat(paste0("    · 00:00:0",j, " [Next search in 2s]\r"))
        Sys.sleep(1)
      }
      cat("\n\n")
    }
  }
  
  Sys.time() - t
  

  # Export vesselIds info by year as backup
  write.csv(vesselIds, paste0(outfolder,"/gfw_fishing_event_data_vesselIds_",year,"_L0.csv"), row.names = FALSE)
  
  
  
  
  # --- API searchin finish | Process information ----
  
  # 4) Combine fishing events with fishing gear info by $vesselId ----------------
  event_data <- event_data %>%
    left_join(vesselIds, by = "vesselId")
  
  
  # 5) filter by fishing gear of interests
  # for our study we will filter by "TRAWLERS" and "DRIFTING_LONGLINES" ----------
  event_data <- event_data %>% filter(gear_type == "TRAWLERS" | 
                                        gear_type == "DRIFTING_LONGLINES")
  
  
  # 6) Calculate apparent fishing hours of each fishing event --------------------
  # check POSIXct class in datetime info
  # class(event_data$start)
  # class(event_data$end)
  
  # calculate apparent fishing effort hours
  event_data$fishing_effort_hour <- as.numeric(difftime(event_data$end, event_data$start, units = "hours"))
  
  
  # 7) Calculate day/night of apparent fishing operation by start and end time ---
  
  # Note: if all the fishing operation was carried out during night -> night
  # Note: if all the fishing operation was carried out during day   -> day
  # Note: if fishing operation was carried out between day and night, I calculate
  #  the proportion of time for night and day -> >50% is assigned for daynight period
  #   · example: 2 hours day and 3 hours night -> night
  
  # Create function to apply differnet fishing events
  
  # # testing:
  # start_time <- event_data$start[1]
  # end_time <- event_data$end[1]
  # 
  # lat <- event_data$lat[1]
  # lon <- event_data$lon[1]
  
  # create custom function to classify fishing operation in day/night time based in the
  # geogrpahic position (1 coordinate -> GFW) and start and end time
  
  daynight_class <- function(start_time, end_time, lat, lon) {
    
    # Check sun position by each minute during fishing operation
    # calculate sequency of minutes
    time_seq <- seq(from = start_time, to = end_time, by = "1 min")
    
    # create dataframe from time sequency
    time_seq <- data.frame(datetime = time_seq)
    
    # Per each min or sequency, calculate if it is day or night 
    # based on the sun elevation 
    # vertical angles from horizont - ángulo vertical respecto al horizonte
    
    # calculate sun elevation
    time_seq$sun_eleveation <- suntools::solarpos(crds=cbind(lon, lat), dateTime = time_seq$datetime)
    
    # ::solarpos gives Azimuth (horizontal angle from north) and solar elevation as a matrix
    # time_seq$sun_eleveation[,2]
    
    prop_day <- mean(time_seq$sun_eleveation[,2] > 0, na.rm = TRUE)
    
    # asing day night class from day time proportion:
    if (prop_day == 1) {
      return("day")
    } else if (prop_day == 0) {
      return("night")
    } else if (prop_day > 0.5) {
      return("day")
    } else {
      return("night")
    }
  }
  
  
  # apply function to fishing event data
  event_data[, daynight := mapply(daynight_class, start, end, lat, lon)]
  
  # 8) Export regenerated database for further spatial analysis ->
  #    day/night map fishing effort maps by fishing gear
  write.csv(event_data, paste0(outfolder,"/gfw_fishing_event_",year,"_L1.csv"), row.names = FALSE)
  
  message (" -- Processing ",year," finished --")
  
}






























































