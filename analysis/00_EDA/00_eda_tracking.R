

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz

# Exploratory Data Analysis and data for suplementaty tables

# 1) For dives
# 2) For track (SSM - Tracking)
# 3) Supplementary table of % of dive per depth ranges

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



# Supplementary table ---------------------------------------------------

str(data)

library(dplyr)
library(tidyr)

depth_bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120, 140, 160, 180, 200, 240, 280, 320)

global_summary <- data %>%
  mutate(depth_bin = cut(maxdep, breaks = depth_bins, include.lowest = TRUE, right = FALSE)) %>%
  group_by(depth_bin) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(percentage = (n / sum(n)) * 100)

View(global_summary)
print(global_summary)

# export suplementaty table
write.csv(global_summary, paste0(output_dir,"/05_statistics_results/sup_table_depth_bin_dive_prop.csv"), row.names = FALSE)


