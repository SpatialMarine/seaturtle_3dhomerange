
# Title: Mapping GFW fishhing event by gear and daynight time

#-------------------------------------------------------------------------------
# 03. GFW day / night apparent fishing effort map
#-------------------------------------------------------------------------------

# Javier Menéndez-Blázquez | @jmenblaz

#' Processing fishing event information L1 obtained in 02_gfw_api_get_event_info.R

#' Fishing event has a information about datetime and position (lat/lon) that
#' correspond to centroid of the AIS vessel positions designed as fishing activities
#' by Global Fishing Watch algorithm

#' This could be comply some issues depend of the analysis resolution, but here
#' using 10x10 km2 for further analysis

# 03_gfw_api_event_daynight_map transform the data downloaded (L1) into a raster
# by gear_types and day / and night time to detailed fishing activities analysis yearly


# ------------------------------------------------------------------------------

# L1 fishing event into raster same resolution than GFW derive productos (1x1 km) 
# apparenrt fishign effort (h/pixel dimension)

# Later transform to 10x10 for further analysis



# 1) create raster grid for the study area
# 2) Create yearly day/night fishing effort maps and fishing gear from fishing event data 



library(sf)
library(foreach)
library(doParallel)
library(terra)
library(dplyr)


# 1) create raster grid for fishing effort ----------------------------------

# load study area 
area <- st_read(paste0(input_dir,"/gis/study_area.geojson"))

# Bounding box for sa extent and crs 
bbox <- st_bbox(area)
crs = st_crs(area)$wkt # extract CRS

# create empty raster mesh (using terra)
# aprox 1km
r <- rast(xmin = bbox[1], xmax = bbox[3], ymin = bbox[2], ymax = bbox[4],
          crs = crs,
          resolution = c(0.01, 0.01)) # grades! EPSG:4326-WGS84 # 1.1km



# ----------------------------------------------------------------------------
# 2) Create yearly day/night fishing effort maps and fishing gear from fishing event data 

# read and load L1 processed data by year 
files <- list.files(paste0(input_dir,"/gfw/daynight/raw"), pattern = "L1.csv", full.names = TRUE)

# parallel computing
# foreach(){} %doparaell% {}


for (f in 1:length(files)) {  # file by year
  
  # select file
  file <- files[f]
  # extract year from file name
  year <- stringr::str_extract(file, "\\d{4}")
  
  # read yearly fishing events
  data <- read.csv(file)
  
  # duplicate coordinates
  # from csv to sf
  data$longitude <- data$lon
  data$latitude <- data$lat
  # convert to sf for spatial analysis
  data <- st_as_sf(data, coords = c("longitude", "latitude"))
  
  # check that calculate fisjhing hours are numeric
  data$fishing_effort_hour <- as.numeric(data$fishing_effort_hour)
  
  # identified fishing gears
  gears <- unique(data$gear_type)     # "TRAWLERS" "DRIFTING_LONGLINES"
  
  # 3.1) Split into day and night data  -------------------------------------
  day_data <- data %>% filter(daynight == "day")
  night_data <- data %>% filter(daynight == "night")
  
  rm(data) # clean
  

  # 3.1.1) DAY: processing fishing effort for day and gear types -----------

    for (g in gears) {
      # info
      cat("Processing DAY time data for fishing gear:",g, "| year:", year, "\n")
      
      # filter day data by gear type
      data_gear <- day_data %>% filter(gear_type == g)
      
      # check
      # plot(data_gear$geometry)
      
      # density map using terra package
      # count number of point from sf object into raster mesh
      r <- terra::rasterize(data_gear, r, field = "fishing_effort_hour", fun = sum, background = 0)
      # plot(r)
      
      # export result raster
      writeRaster(r, filename = paste0(input_dir,"/gfw/daynight/gfw_fishing_effort_",year,"_day_",g,".tif"), 
                  overwrite = TRUE)
      
      Sys.sleep(1)
    }

  
  # 3.1.2) NIGHT: processing fishing effort for night and gear types -----------
  
    for (g in gears) {
      # info
      cat("Processing NIGHT time data for fishing gear:",g, "| year:", year, "\n")
      
      # filter night data by gear type
      data_gear <- night_data %>% filter(gear_type == g)
      # check
      # plot(data_gear$geometry)
      
      # density map using terra package
      # count number of point from sf object into raster mesh
      r <- terra::rasterize(data_gear, r, field = "fishing_effort_hour", fun = sum, background = 0)
      # plot(r)
      
      # export result raster
      writeRaster(r, filename = paste0(input_dir,"/gfw/daynight/gfw_fishing_effort_",year,"_night_",g,".tif"), 
                  overwrite = TRUE)
      
      Sys.sleep(1)
    }
  
}

  
  
  
  
  

  
  
  
  
  

  
  
  
























