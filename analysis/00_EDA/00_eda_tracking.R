

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz

# Exploratory Data Analysis and data for suplementaty tables

# 1) For dives
# 2) For track (SSM - Tracking)

library(dplyr)
library(ggplot2)
library(sf)
library(tibble)


# 0) Prepare and summary data ---------------------------------

# Filter obs per study area extent
e <- read_sf(paste0(input_dir,"/gis/study_area.gpkg"))  
e <- st_bbox(e)


# 1) load dive data previusly generated (gen further scripts)
# Read previously exported data
data <- read.csv(paste0(input_dir,"/tracking/dives/dives_metrics.csv"))

# As factor
data$season <- as.factor(data$season)
data$daynight <- as.factor(data$daynight)
data$moon_bright_class <- as.factor(data$moon_bright_class)

# summary
# mean daily maximum depth (day and night time) and VMR
data$day_of_year <- as.numeric(format(as.Date(data$date), "%j"))

# filter dives into study area
data <- subset(data,
    longitude >= e["xmin"] &
    longitude <= e["xmax"] &
    latitude  >= e["ymin"] &
    latitude  <= e["ymax"]
)

# max dive depth per organismID
stats <- data %>%
  group_by(organismID) %>%
  summarise(
    maxdep <- max(maxdep, na.rm = TRUE))
  
# num of DAYS TRACKED
stats <- data %>%
  group_by(organismID) %>%
  summarise(
    days_track <- n_distinct(date)) 

# num of dives recored
stats <- data %>%
  group_by(organismID) %>%
  summarise(
    dives <- n()) 

