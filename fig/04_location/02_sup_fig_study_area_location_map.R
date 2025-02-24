

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz

# Plot location map for study area 

# load supplementaty package for map plotting

# Suplemmentary Figure - All track seaturtle distribution and all extend

library(viridis)
library(dplyr)
library(data.table)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

library(tidyterra)
library(terra)

library(sf)
library(ggshadow)
library(ggforce)
library(giscoR)

source("analysis/01_tracking/fun/fun_track_reading.R")
# Not run setup or library(raster)
# source("setup.R")



# -------------------------------------------------------------------------

# Load SSM results
# Set path to tracking data

# search files
track_files <- tibble(
  file = list.files(paste0(input_dir,"/tracking/loc/L2/"), pattern = "_L2_loc.csv$", recursive = TRUE, full.names = TRUE))

# batch import
data <- rbindlist(lapply(track_files$file, fread), fill=TRUE)

# rename variables
data <- data %>%
  rename(trip = tripID)


# import landmask and coastline
world <- giscoR::gisco_get_countries(year = "2016", epsg = "4326", resolution = "03")
coastline <- gisco_get_coastallines(year = "2016", epsg = "4326", resolution = "03")

# transform coastline in the same crs that landmask
coastline <- st_transform(coastline, crs = st_crs(world))


# 1.2 ) import bathymetry GEBCO 2024

# for med area
b1 <- terra::rast(paste0(input_dir,"/gis/gebco/GEBCO_2020_Mediterranean_bathymetry.tif"))
# use only values == or < 0 as a bathymetry
b1[b1 > 0] <- NA #0 data as NA

# bathymetry for west_africa area
b2 <- terra::rast(paste0(input_dir,"/gis/gebco/GEBCO_2020_REDUCE_bathymetry.tif"))
# use only values == or < 0 as a bathymetry
b2[b2 > 0] <- NA #0 data as NA

b <- mosaic(b1, b2, fun = mean)

rm(b1, b2)

# color ramp for bathymetry
cols <- colorRampPalette(rev(c('#ecf9ff','#BFEFFF','#97C8EB','#4682B4','#264e76','#162e46')))(100)
cols <- adjustcolor(cols, alpha.f = 0.5) 

plot(b)
# add sutdy area
# sa <- read_sf(paste0(input_dir,"/gis/study_area.gpkg"))
# bb <- sf::st_bbox(sa)


# limit represent in the plot for Mediterranean area
xlim <- c(-25.5, 35)
ylim <- c(13.5, 45.2)



# Obtain values of batyhymetry in the plot area xlim and ylim
visible_data <- crop(b, raster::extent(c(xlim, ylim)))
visible_range <- range(values(visible_data), na.rm = TRUE)


# For color gradiente - normalized time of each track
data[, t_norm := as.numeric(time - min(time)) / as.numeric(max(time) - min(time)),
     by = .(organismID, trip)]


# Plot map ----------------------------
p <- ggplot() +
  
  # add bathymetry
  tidyterra::geom_spatraster(data = b) +
  # tidyterra::geom_spatraster(data = b, interpolate = TRUE, aes(fill = mediterranean_sea_gebco_2024)) +
  # bathymettry color ramp
  scale_fill_gradientn(colors = cols,
                       name = "Depth (m)",
                       limits = visible_range, # limits of values in the represented area
                       guide = guide_colorbar(frame.colour = "grey5", ticks.colour = "grey5"),
                       na.value = "#FFFFFF") +
  
  # add tracks
    # color ramp for time
  # geom_path(data = data,
  #           aes(x = longitude, y = latitude, group = interaction(organismID, trip), 
  #               color = t_norm),
  #           alpha = 0.9, linewidth = 1) +
  # 
  # scale_color_viridis_c(option = "F") +  # Magma gradiente


    # solid track
  geom_path(data = data,
            aes(x = longitude, y = latitude, group = interaction(organismID, trip)),
            size = 2, alpha = 0.22, colour = "#989ce3") +
  geom_path(data = data,
            aes(x = longitude, y = latitude, group = interaction(organismID, trip)),
            size = 1.5, alpha = 0.5, colour = "#65689e") +
  geom_path(data = data,
            aes(x = longitude, y = latitude, group = interaction(organismID, trip)),
            size = 0.2, alpha = 0.9, colour = "grey15") +

  # add land and coastline
  geom_sf(data = world, fill="grey98", colour = "grey85", size = .03) +
  geom_sf(data = coastline, fill = "transparent", colour = "grey60", size = 3, alpha = 0) +
  
  # spatial bounds
  coord_sf(xlim = xlim, ylim = ylim, expand=T) +
  
  # x y labels
  xlab("") +  ylab("") +
  # theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.ticks = element_line(size = 0.75),
        axis.ticks.length = unit(6, "pt"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.justification = "center",
        legend.key.width = unit(15, "pt"),
        legend.key.height = unit(20, "pt"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))

p


# save plot
p_png <- paste0(output_dir, "/fig/sup_location_map.png")
p_svg <- paste0(output_dir, "/fig/sup_location_map.svg")
ggsave(p_png, p, width=20, height=12, units="cm", dpi=450, bg="white")
ggsave(p_svg, p, width=20, height=12, units="cm", dpi=450, bg="white")

