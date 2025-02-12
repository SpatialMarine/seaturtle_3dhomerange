
# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz

# Plot 2D track


# load supplementaty package for map plotting

library(ggplot2)
library(tidyterra)
library(raster)
library(terra)
library(sf)
library(giscoR)

source("setup.R")


# load data for plot track
organismID <- "200043"



# 1) prepare spatial layers  for plot     --------------------------------------

# 1.1) import landmask and coastline (from Gisco, high resolution coastline)
# world <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
world <- giscoR::gisco_get_countries(year = "2016", epsg = "4326", resolution = "03")

# 1.2 ) import bathymetry GEBCO 2024
b <- rast(paste0(input_dir, "/gis/gebco/mediterranean_sea_gebco_2024.tif"))
# use only values == or < 0 as a bathymetry
b[b > 0] <- NA #0 data as NA

# color ramp for bathymetry
cols <- colorRampPalette(rev(c('#ecf9ff','#BFEFFF','#97C8EB','#4682B4','#264e76','#162e46')))(100)
cols <- adjustcolor(cols, alpha.f = 0.5) 

# limit represent in the plot
xlim <- c(0.6, 4.7)
ylim <- c(36.9, 40.1)

# Obtain values of batyhymetry in the plot area xlim and ylim
b <- crop(b, extent(c(xlim, ylim)))
visible_range <- range(values(b), na.rm = TRUE)


# sea-turtle track and locs
track_line <- st_read(paste0(input_dir,"/gis/tracking/",organismID,"_L3_loc_track.gpkg"))
locs <- read.csv(paste0(input_dir,"/tracking/loc/L3/",organismID,"_L3_loc.csv"))
locs$time <- as.factor(locs$time) # for plot, faster than as.factor

# check plotting data
plot(b)
plot(track_line, add = TRUE)


# filter first and last track position
start <- head(locs, 1)
end  <- tail(locs, 1)



# Plot map ----------------------------
p <- ggplot() +
  # add bathymetry
  tidyterra::geom_spatraster(data = b, interpolate = TRUE) +
  # color ramp
  scale_fill_gradientn(colors = cols,
                       name = "Depth (m)",
                       limits = visible_range, # limits of values in the represented area
                       guide = guide_colorbar(frame.colour = "grey5", ticks.colour = "grey5"),
                       na.value = "#FFFFFF") +
  
  # sea-turtle track 
  geom_sf(data = track_line, colour = "grey35", linewidth = 1, alpha = 0.5) +
  geom_sf(data = track_line, colour = "grey15", linewidth = 0.45) +
 
  # geom_path(data = locs,
  #           aes_string(x = "longitude", y = "latitude", group = "organismID", color = "time")) +
  
  # geom_path(data = locs,
  #           aes(x=longitude, y=latitude, color= time, group = organismID), size = 0.8,
  #           alpha = 1) +

  # first and last position registered
  geom_point(data = start, aes(x = longitude, y = latitude),
             color = "grey20",
             shape = 24,
             alpha = 0.4,
             size = 4.5) +
  geom_point(data = start, aes(x = longitude, y = latitude),
             fill = "#B3EE3A",
             color = "#000000",
             shape = 24,
             size = 4) +
  
  # first and last position registered
  geom_point(data = end, aes(x = longitude, y = latitude),
             color = "grey20",
             shape = 22,
             alpha = 0.4,
             size = 4.5) +
  geom_point(data = end, aes(x = longitude, y = latitude),
             fill = "#FF6347",
             color = "#000000",
             shape = 22,
             size = 4) +
  
  # add land and coastline
  geom_sf(data = world, fill="grey98", colour = "grey10", linewidth = 0.3) +
  
  # spatial bounds
  # coord_sf(xlim = xl, ylim = yl, expand= TRUE) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  # x y labels
  xlab("") +  ylab("") +
  # theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 10, color = "grey10"),
        axis.text.x = element_text(size = 10, color = "grey10"),
        axis.ticks = element_line(size = 0.5, color = "grey10"),
        axis.ticks.length = unit(5, "pt"),
        panel.border = element_rect(color = "grey10", fill = NA, size = 1.2),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        # legend
        legend.position = "none",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(28, "pt"),
        legend.key.height = unit(13, "pt"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9))

p


# clean plots
graphics.off()


# # export map
p_png <- paste0(output_dir,"/fig/fig_track2d.png")
p_svg <- paste0(output_dir,"/fig/fig_track2d.svg")
ggsave(p_png, p, width=14, height=14, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=14, height=14, units="cm", dpi=350, bg="white")




