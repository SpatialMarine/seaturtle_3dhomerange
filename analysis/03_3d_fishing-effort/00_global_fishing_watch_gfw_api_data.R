
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
# area_json <- fromJSON(paste0(input_dir,"/gis/study_area.geojson"))
# area_json <- read_lines(paste0(input_dir,"/gis/study_area.geojson"))
area <- st_read(paste0(input_dir,"/gis/study_area.geojson"))

# Set study years:
years <- c(2012:2024)


# ------------------------------------------------------------------------------
# 2) Download fishing effort summary data---------------------------------------

t <- Sys.time()

# let's loop it across years
for(i in seq_along(years)){
  
  y <- years[i]
  # info
  cat(paste0("Processing year: ", y," / ",last(years)))
  
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

  # save raw data per year
  dir_output <- paste0(input_dir, "/gfw/raw")
  if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)
  file_name <- paste0(dir_output, "/",y,"_gfw.csv")
  write.csv(raw, file_name, row.names = F)
  
  # summarize total fishing effort per flag and geartype
  summary <- raw %>%
    dplyr::rename('fishing_hours' = 'Apparent Fishing Hours',
                  'year' = 'Time Range',
                  'gear_type' = 'geartype') %>%
    dplyr::group_by(year, gear_type) %>%
    summarize(
      Fishing_Hours = sum(fishing_hours, na.rm = T)
    )
  # save summary per year
  file_name <- paste0(dir_output, "/",y,"_gfw_summary.csv")
  write.csv(raw, file_name, row.names = F)
  # sleep for a 5 sec until process next year
  for (i in seq_along(5)) {
    for (j in 5:1) {  # descending counter
      cat(sprintf("Processing next year in: %d s\r", j))
      Sys.sleep(1)
    }
  }
  # info
  cat("\n")
  message("- Global Fishing Watch raw data downloaded - ")
}

Sys.time() - t # 6 min for 2012-2024 data




# -----------------------------------------------------------------------------
# 3) Combine all raw data and processing mean fishing effort for study area
#    and explore it

# List all CSV files in the directory 
# note: not list summary files
files <- list.files(dir_output, pattern = "\\_gfw.csv$", full.names = T)
# Read all CSV files into a list of data frames
gfw <- lapply(files, read_csv)

# Convert each tibble in the list to a data frame
gfw <- lapply(gfw, as.data.frame)

# combine / merge dataframes: 
gfw <- do.call(rbind, gfw)

# cheack head and end of the gfw data obtained
head(gfw)
tail(gfw)

# rename columns
gfw <- gfw %>% rename('fishing_hours' = 'Apparent Fishing Hours',
                      'year' = 'Time Range',
                      'gear_type' = 'geartype',
                      'vessel_id' = 'Vessel IDs')
# Check gear types:
unique(gfw$gear_type)



# 3.1) Filering and summary by fishing gear type of interest ---------
##  3.1.1) Drinfting longlines --------
drifting_longline <- gfw %>%
  filter(gear_type == "drifting_longlines")

# summarize total fishing effort per area:
summary <- drifting_longline %>%
  group_by(Lat, Lon) %>%
  summarize(
    total_vessels_ids = sum(vessel_id),
    total_fishing_hours = sum(fishing_hours)
    )

# Save your filtered data:
dir_output <- paste0(input_dir, "/gfw") # created previously
file <- paste0(dir_output, "/drifting_longlines_fishing_effort_summary.csv")
write.csv(summary, file, row.names = F)


##  3.1.2) Trawlers  ------------
# filter by type gear
trawlers <- gfw %>%
  filter(gear_type == "trawlers")

# summarize total fishing effort per area:
summary <- trawlers %>%
  group_by(Lat, Lon) %>%
  summarize(
    total_vessels_ids = sum(vessel_id),
    total_fishing_hours = sum(fishing_hours)
  )
# Save
dir_output <- paste0(input_dir, "/gfw") # created previously
file <- paste0(dir_output, "/trawlers_fishing_effort_summary.csv")
write.csv(summary, file, row.names = F)





# ------------------------------------------------------------------------------
# 4. Export raster and quick checking map                  ------------------

# colRamp
# https://github.com/jmenblaz/colRamps/tree/main/thematic

library(RColorBrewer)
# custom color ramp for fishing effort
colRamp <- colorRampPalette(c('#6CA6CD','#B0E2FF','#FFFACD','#FFEC8B','#FFA07A','#8B3626', '#130705'))(50)


# trawlers, drifting_longlines
files <- list.files(paste0(input_dir,"/gfw"), full.names = TRUE, pattern = "_summary.csv")

for (f in files){
  
  # 4.1) Quick map ----------------------------------
  # extract gear type by name
  gear_type <- sub(".*/(.*)_fishing_effort_summary\\.csv", "\\1", f)
  # read data
  fishingEffort <- read.csv(paste0(input_dir,"/gfw/",gear_type,"_fishing_effort_summary.csv"))

  # Make zoomed in map 
  # land mask
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # Bounding box if you rather setting it to the raster limits:
  extent <- coord_sf(xlim = c(min(fishingEffort$Lon), max(fishingEffort$Lon)), 
                     ylim = c(min(fishingEffort$Lat), max(fishingEffort$Lat)))
  
  # Create a ggplot object
  p <- ggplot() +
    # land mask
    # geom_sf(data = world, fill = "#3A4354", color = "#73829D") +
    # black tones
    geom_sf(data = world, fill = "#11161D", color = "grey10") +
    # add tracks
    geom_tile(data = fishingEffort, aes(x = Lon, y = Lat, fill = total_fishing_hours))+
    # Set spatial bounds or extention of map
    # extent + # use extent object for extention in ggplot2
    coord_sf(xlim = c(ext@xmin, ext@xmax), 
             ylim = c(ext@ymin, ext@ymax), 
             expand = FALSE) +  # Elimina los bordes extra
    
    # extent + # used for used extent object directly
    # theme
    theme_bw() +
    # Use a viridis color scale with log transformation
    scale_fill_viridis_c(option = "F", trans="log10") +
  
    # Customize legend and axis titles
    labs(fill = "Fishing effort (Hours)",  # legend title
    ) +
    
    theme_bw() +
    theme(panel.background = element_rect(fill = "#19212C"), # change the color of sea in this case
          panel.border = element_rect(color = "grey5", fill = NA, linewidth = 1.1),
          panel.grid = element_blank(),
          # axis
          axis.title = element_blank(),
          axis.text.y = element_text(size = 11),
          axis.text.x = element_text(size = 11),
          axis.ticks = element_line(size = 0.75),
          axis.ticks.length = unit(6, "pt"),  # negative lenght -> ticks inside the plot
          # legend
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8.5)
          )
  
  p
  
  # export plot as png:
  p_png <- paste0(input_dir,"/gfw/",gear_type,"_fishing_effort_summary.png")
  ggsave(p_png, p, width=23, height=18, units="cm", dpi=350)
  
  
  
  # 4.2) export as raster layer / file ----------------------------http://127.0.0.1:9825/graphics/plot_zoom_png?width=1136&height=744
  # define raster extension followend fishingEffort objec
  ext <- extent(
    min(fishingEffort$Lon), max(fishingEffort$Lon),
    min(fishingEffort$Lat), max(fishingEffort$Lat)
  )
  
  # Create a raster with the specified extent and resolution
  r <- raster(ext, 
              resolution = 0.1,  # resolution = 0.1 grados; ≈ 10x10 Km
              crs = "+proj=longlat +datum=WGS84")
  
  # Convert fishing effort data into a raster
  fishing_effort_raster <- raster::rasterFromXYZ(fishingEffort[, c("Lon", "Lat", "total_fishing_hours")], crs=crs(r))
  # plot(fishing_effort_raster)
  
  # Save / export raster of Global Fishing Watch data by gear type
  geotiff_file <- paste0(input_dir,"/gfw/",gear_type,"_fishing_effort.tif")
  writeRaster(fishing_effort_raster, filename = geotiff_file, format = "GTiff", overwrite = TRUE)
  
}

