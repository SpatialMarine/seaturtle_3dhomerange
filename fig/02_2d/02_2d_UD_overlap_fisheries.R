
# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz

# Plot 2D 50/95 UD and overlap with LL and TW fishing activities

# ------------------------------------------------------------------------


# load supplementaty package for map plotting

library(rmapshaper)
library(ggplot2)
library(tidyterra)
library(raster)
library(terra)
library(sf)
library(giscoR)
library(smoothr)
library(ggnewscale)

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


# 1.3) track and locs -------------------------------
track_line <- st_read(paste0(input_dir,"/gis/tracking/",organismID,"_L3_loc_track.gpkg"))
locs <- read.csv(paste0(input_dir,"/tracking/loc/L3/",organismID,"_L3_loc.csv"))

# read 2kde results and filter by organismID
kde_result <- read.csv(paste0(output_dir,"/02_kde_2d/kde_2d_res.csv"))
id <- organismID # transform for filtering
kde_result <- kde_result %>% filter (organismID == id)

# read 2kde for organismID
kde <- rast(paste0(output_dir,"/02_kde_2d/",organismID,"/",organismID,"_2dmkde_obj_raster.tif"))
plot(kde)


# 1.4) Process kde results to obtain UD polygons (contours or isolines) --------
# obtain UD thresholds
threshold.50 <- kde_result$threshold.50
threshold.95 <- kde_result$threshold.95

# contours or isolines for UD
terra::contour(kde, levels = threshold.50, add=TRUE)
terra::contour(kde, levels = threshold.95, add=TRUE)

# select pixel >= theshold (binare raster)
kde50 <- kde >= threshold.50
kde50[kde50 < threshold.50] <- NA 

kde95 <- kde >= threshold.95
kde95[kde95 < threshold.95] <- NA 

# Convertir a polígonos
ud50 <- as.polygons(kde50, dissolve = TRUE)
ud95 <- as.polygons(kde95, dissolve = TRUE)

# Convert to sf
ud50 <- st_as_sf(ud50)
ud95 <- st_as_sf(ud95)

# from MULTIPOLYGON TO POLYGON
ud50 <- st_cast(ud50, "POLYGON")
ud95 <- st_cast(ud95, "POLYGON")

# Simplify UD for plotting
# use ms_simplify instead sf::simplify to keep shapes (Visvalingam’s algorithm)
ud50 <- rmapshaper::ms_simplify(ud50, keep = 0.2,
                                keep_shapes = TRUE)

ud95 <- rmapshaper::ms_simplify(ud95, keep = 0.2,
                                          keep_shapes = TRUE)

# smooth shapes
ud95 <- smoothr::smooth(ud95, method = "ksmooth")
ud50 <- smoothr::smooth(ud50, method = "ksmooth")

# filter small UD areas detected for core area 50UD (less than mean of area)
ud50$area <- st_area(ud50)
q <- mean(ud50$area)
ud50 <- ud50 %>% filter(area > q)

ud95$area <- st_area(ud95)
q <- mean(ud95$area)
ud95 <- ud95 %>% filter(area > q)

# Check UD polygons resulted
# plot(ud50)
# plot(ud95)


# add CRS similar to track (WGS84 - EPSG4326)
st_crs(ud50) <- "EPSG:3035"
st_crs(ud95) <- "EPSG:3035"

ud50 <- st_transform(ud50, st_crs(track_line))
ud95 <- st_transform(ud95, st_crs(track_line))


# 1.5) Fishing effort LL and TW

# 1.2 ) import GFW data
# use total for plot (note that analysis were made over kde extension)
LL <- rast(paste0(input_dir,"/gfw/drifting_longlines_fishing_effort.tif"))
TW <- rast(paste0(input_dir,"/gfw/trawlers_fishing_effort.tif"))

LL <- crop(LL, extent(c(xlim, ylim)))
TW <- crop(TW, extent(c(xlim, ylim)))


# from raster to polygon
LL_overlap <- LL >= 0   # select pixel >= theshold (binare raster)
LL_overlap[LL_overlap < 0] <- NA   # select pixel >= theshold (binare raster)
LL_overlap <- as.polygons(LL, dissolve = TRUE)   # polygons
LL_overlap <- st_as_sf(LL_overlap)

# from raster to polygon
TW_overlap <- TW >= 0   # select pixel >= theshold (binare raster)
TW_overlap[TW_overlap < 0] <- NA   # select pixel >= theshold (binare raster)
TW_overlap <- as.polygons(TW, dissolve = TRUE)   # polygons
TW_overlap <- st_as_sf(TW_overlap)

# mask fishing effort with UD for plotting 2D overlaping
LL_overlap <- st_intersection(LL_overlap, ud95)
TW_overlap <- st_intersection(TW_overlap, ud95)



# 1.5) Plot map --------------------------------------------------------------

# 1.5.1 Longlines ---------------
p <- ggplot() +
  # add bathymetry
  tidyterra::geom_spatraster(data = b, interpolate = TRUE, aes(fill = mediterranean_sea_gebco_2024)) +
  # color ramp
  scale_fill_gradientn(colors = cols,
                         name = "Depth (m)",
                         limits = visible_range, # limits of values in the represented area
                         guide = guide_colorbar(frame.colour = "grey5", ticks.colour = "grey5"),
                         na.value = "#FFFFFF") +
  # add fishing effort (LL)
  # geom_sf(data = LL, fill = "#ab634f", colour = "transparent", size = 2, alpha = 0.5) 
  ggnewscale::new_scale_fill() + 
  tidyterra::geom_spatraster(data = LL, interpolate = TRUE, aes(fill = drifting_longlines_fishing_effort), alpha = 0.65) +
  scale_fill_viridis_c(option = "F",
                       trans= "log10",
                       na.value = NA,
                       # dark legend border or guidebar
                       guide = guide_colorbar(frame.colour = "grey5", 
                                              ticks.colour = "grey5")) +

  # add fishing overlap with UDs
  # geom_sf(data = LL_overlap, fill = "#8B3A3A", colour = "transparent", alpha = 0.5) +
  geom_sf(data = LL_overlap, fill = "grey20", colour = "#601212", alpha = 0.45) +
  
  # 50/95 UD
  geom_sf(data = ud95, fill = "#868ada", colour = "grey5", linewidth = 0.37, alpha = 0.35) +
  geom_sf(data = ud50, fill = "#25263C", colour = "grey5", linewidth = 0.4, alpha = 0.4) +
   
  # # 50/95 UD
  # geom_sf(data = ud95, fill = "transparent", colour = "black", size = 2, alpha = 0.20) +
  # geom_sf(data = ud50, fill = "transparent", colour = "black", size = 2, alpha = 0.40) +
  
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

 

graphics.off()


# # export map
p_png <- paste0(output_dir,"/fig/fig_2d_ud_LL.png")
p_svg <- paste0(output_dir,"/fig/fig_2d_ud_LL.svg")
ggsave(p_png, p, width=14, height=14, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=14, height=14, units="cm", dpi=350, bg="white")




# 1.5.2 TRAWLERS  ---------------

p <- ggplot() +
  # add bathymetry
  tidyterra::geom_spatraster(data = b, interpolate = TRUE, aes(fill = mediterranean_sea_gebco_2024)) +
  # color ramp
  scale_fill_gradientn(colors = cols,
                       name = "Depth (m)",
                       limits = visible_range, # limits of values in the represented area
                       guide = guide_colorbar(frame.colour = "grey5", ticks.colour = "grey5"),
                       na.value = "#FFFFFF") +
  # add fishing effort (LL)
  # geom_sf(data = LL, fill = "#ab634f", colour = "transparent", size = 2, alpha = 0.5) 
  ggnewscale::new_scale_fill() + 
  tidyterra::geom_spatraster(data = TW, interpolate = TRUE, aes(fill = trawlers_fishing_effort), alpha = 0.65) +
  scale_fill_viridis_c(option = "F",
                       trans= "log10",
                       na.value = NA,
                       # dark legend border or guidebar
                       guide = guide_colorbar(frame.colour = "grey5", 
                                              ticks.colour = "grey5")) +
  
  # add fishing overlap with UDs
  # geom_sf(data = LL_overlap, fill = "#8B3A3A", colour = "transparent", alpha = 0.5) +
  geom_sf(data = TW_overlap, fill = "grey20", colour = "#601212", alpha = 0.45) +
  
  # 50/95 UD
  geom_sf(data = ud95, fill = "#868ada", colour = "grey5", linewidth = 0.37, alpha = 0.35) +
  geom_sf(data = ud50, fill = "#25263C", colour = "grey5", linewidth = 0.4, alpha = 0.4) +
  
  # # 50/95 UD
  # geom_sf(data = ud95, fill = "transparent", colour = "black", size = 2, alpha = 0.20) +
  # geom_sf(data = ud50, fill = "transparent", colour = "black", size = 2, alpha = 0.40) +
  
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



graphics.off()


# # export map
p_png <- paste0(output_dir,"/fig/fig_2d_ud_TW.png")
p_svg <- paste0(output_dir,"/fig/fig_2d_ud_TW.svg")
ggsave(p_png, p, width=14, height=14, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=14, height=14, units="cm", dpi=350, bg="white")


