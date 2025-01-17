
# Title:

#-------------------------------------------------------------------------------
# 00. Download GFW data - calculate fishing effort within a customized area
#-------------------------------------------------------------------------------

# GFW-tools repository as reference to create the script presented here 
# https://github.com/LeiaNH/GFW-tools/tree/main

# load packages
library(tidyverse)
library(qdapRegex)
library(readr)
library(sf)
library(ggplot2)
library(raster)

# geojson files
library(jsonlite)


# remote install gfwr R package (see setup.R)
# Global Fishing Watch
library(gfwr)


# 0) ------------------------------------------------------------------------------
# APY key for GFW 
# load in setup.R
key # check it is loaded propertly 


# 1) Set study area and period--------------------------------------------------
# select study area (created previously)

# load area from geojson (or spatial object)
# area should be a sf class object
area <- st_read(paste0(input_dir,"/gis/study_area.geojson"))

# area_json <- fromJSON(paste0(input_dir,"/gis/study_area.geojson"))
# 
# area_json <- read_lines(paste0(input_dir,"/gis/study_area.geojson"))


# 1.2area# 1.2. Set study years:
years <- c(2023:2024)



#2. Download fishing effort summary data-----------------------------------------

# let's loop it across years
for(i in seq_along(years)){
  #i = 1
  y <- years[i]  
  start_date <- as.Date(paste0(y, '-01-01'))
  end_date <- as.Date(paste0(y, '-12-31'))
  
  # get information: GFW base function to get raster from API and convert response to data frame
  raw <- get_raster(spatial_resolution = 'HIGH', # Can be "low" = 0.1 degree or "high" = 0.01 degree
                    temporal_resolution = 'YEARLY', # Can be 'daily','monthly','yearly'
                    group_by = 'GEARTYPE', # Can be 'vessel_id', 'flag', 'geartype', 'flagAndGearType
                    start_date = start_date,
                    end_date = end_date,
                    region = area, # geojson or GFW region code (i.e. option 2) or sf object (i.e. option 1)
                    region_source = 'USER_SHAPEFILE', #source of the region ('EEZ','MPA', 'RFMO' or 'USER_SHAPEFILE')
                    key = key) #Authorization token. Can be obtained with gfw_auth function

  # save data per year if you wish:
  dir_output <- paste0(input_dir, "/gfw/rawdata")
  if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
  file_name <- paste0(dir_output, "/",y,".csv")
  write.csv(summary, file_name, row.names = F)
  
  Sys.sleep(5)  # Pause for 5 seconds between requests to avoid rate limits
}


#3. Save fishing effort summary data--------------------------------------------
dir_output <- paste0(input_data, "/gfw/rawdata/high_resolution")
setwd(dir_output)
# List all CSV files in the directory
file_list <- list.files(pattern = "\\.csv$")
# Read all CSV files into a list of data frames
data_list <- lapply(file_list, read_csv)
# Convert each tibble in the list to a data frame
data_list <- lapply(data_list, as.data.frame)

# Check data:
summary(data_list)
head(data_list[1])
str(as.data.frame(data_list[1]))

# Merge all dataframes: 
outputraw <- do.call(rbind, data_list)

# Check gear types:
gears <- unique(outputraw$geartype)
gears

# filter trawlers
outputraw <- outputraw %>%
  dplyr::filter(geartype %in% "trawlers")
head(outputraw)
str(outputraw)

# summarize total fishing effort per area:
summary_data <- outputraw %>%
  dplyr::rename('FishingHours' = 'Apparent Fishing Hours',
                'Year' = 'Time Range',
                'Vessel_IDs' = 'Vessel IDs') %>%
  group_by(Lat, Lon) %>%
  summarize(
    TotalVessel_IDs = sum(Vessel_IDs),
    TotalFishingHours = sum(FishingHours))

# Save your filtered data (across years, bottom trawling):
dir_output <- paste0(input_data, "/gfwr/summarydata")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
file_name <- paste0(dir_output, "/summaryTrawlEffort.csv")
write.csv(summary_data, file_name, row.names = F)


#3. Quick checking map-----------------------------------------------------------
# First read the file with gridded data already saved
setwd(main_dir)
fishingEffort <- read.csv("input/gfwr/summarydata/summaryTrawlEffort.csv")
str(fishingEffort)

# Make zoomed in map 
#Mask
mask<- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

# Bounding box if you rather setting it to the raster limits:
#extent <- coord_sf(xlim = c(min(fishingeffort$Lon), max(fishingeffort$Lon)), 
#                   ylim = c(min(fishingeffort$Lat), max(fishingeffort$Lat)))

# Create a ggplot object
p <- ggplot() +
  # land mask
  geom_sf(data = mask) +
  # add tracks
  geom_tile(data = fishingEffort, aes(x = Lon, y = Lat, fill = TotalFishingHours))+
  
  #Set spatial bounds
  coord_sf(xlim = c(-6, 40), ylim = c(30, 46), expand = TRUE) + #change by extent if you wish to fit it to data
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +  
  # theme
  theme_bw() +
  # Use a viridis color scale with log transformation
  scale_fill_viridis_c(trans="log10")+ 
  # Remove grids
  theme(panel.grid = element_blank())

plot(p)

# export plot as png:
p_png <- paste0(dir_output, "/FishingEffort.png")
ggsave(p_png, p, width=23, height=17, units="cm", dpi=300)


#4. Save as raster -------------------------------------------------------------

# Define the extent based on xlim and ylim
ext <- extent(-6, 40, 30, 46)

# Create a raster with the specified extent and resolution
r <- raster(ext, ncol=length(seq(ext@xmin, ext@xmax, by=0.1)),
            nrow=length(seq(ext@ymin, ext@ymax, by=0.1)),
            crs="+proj=longlat +datum=WGS84")
# Set the resolution
res(r) <- 0.1

# Convert fishingEffort to a raster
fishing_effort_raster <- rasterFromXYZ(fishingEffort[, c("Lon", "Lat", "TotalFishingHours")], crs=crs(r))
plot(fishing_effort_raster)

# Save:
dir_output <- paste0(input_data, "/gfwr/summarydata")
geotiff_file <- paste0(dir_output, "/FishingEffort.tif")
writeRaster(fishing_effort_raster, filename = geotiff_file, format = "GTiff", overwrite = TRUE)