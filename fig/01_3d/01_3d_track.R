

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz

# Plot 3D track diorama
# Plot 3D track diorama
# load only panths, no packges.
# some issue between Terra and Raster

# source("setup.R")

## Load libraries
library(sf)
library(plot3D)
library(rgl)
library(plot3Drgl)
library(rasterVis)
# library(ncdf4)

library(rayshader)


# ------------------------------------------------------------------------------
# 1) Prepare data for plot diorama      ---------------------------------

# limit for represent
xlim <- c(0.6, 4.7)
ylim <- c(36.9, 40.1)

# landmask world <- giscoR::gisco_get_countries(year = "2016", epsg = "4326", resolution = "03")
land <- giscoR::gisco_get_countries(year = "2016", epsg = "4326", resolution = "03")

# bounding box
# Definir el bounding box (asegurándonos que el CRS sea el adecuado)
bb <- st_as_sfc(st_bbox(c(xmin = xlim[1], xmax = xlim[2], 
                          ymin = ylim[1], ymax = ylim[2])), 
                crs = st_crs(land)) 
# crop
land <- st_crop(land, bb)
plot(land$geometry)
# select only Balearic islands
land <- land[1,]



# import location data for plot
organismID <- "200043"
# read ttdr data
ttdr <- read.csv(paste0(input_dir,"/tracking/ttdr/L3/",organismID,"_L3_ttdr.csv"))

# first and last position
start <- head(ttdr, 1)
end  <- tail(ttdr, 1)
# latitude and longitude coordinates for positions
start <- c(start$latitude, start$longitude)
end <- c(end$latitude, end$longitude)


# import bathymetry GEBCO 2024
# note bathymetry cropped to mediterranean sea

# Use raster() not rast() from Terra
b <- raster(paste0(input_dir, "/gis/gebco/mediterranean_sea_gebco_2024.tif"))

# use only values == or < 0 as a bathymetry
b[b > 0] <- 50  # surface values as 10 m (flat land mask)

# Obtain values of batyhymetry in the plot area xlim and ylim
b <- crop(b, extent(c(xlim, ylim)))
plot(b) # check plotting area





# -----------------------------------------------------------------------------
# 2) Prepare diorama              -----------------------------------------

# rename object
elevation.raster <- b
rm(b) # clean enviroment
# matrix 
elevation.matrix <- matrix(extract(elevation.raster, extent(elevation.raster), buffer = 100), nrow = ncol(elevation.raster), ncol = nrow(elevation.raster))

# Z scale for elevation matrix
my.z <- 25

# elevantion.matrix %>%
#   sphere_shade(zscale= my.z,
#                texture=create_texture("#E9C68D","#AF7F38",
#                                       "#674F30","#494D30",
#                                        "#B3BEA3")) %>%
#   plot_map()


# test map
elevation.matrix  %>% 
  sphere_shade(sunangle = 35, texture = "imhof3", zscale = my.z) %>%
  plot_map()

# ambient occlusion
elevation.amb.shade <- ambient_shade(elevation.matrix, zscale = my.z, multicore = TRUE, progbar = TRUE)
# plot_map(elevation.amb.shade)

# ray shadow
elevation.ray.shade <- ray_shade(elevation.matrix,sunangle = 35, zscale = my.z, multicore = TRUE)
# plot_map(elevation.ray.shade)


# plot diorama -----------------------------------------------------------------
sf::sf_use_s2(FALSE)

# plot diorama
elevation.matrix  %>% 
  sphere_shade(sunangle = 35, texture = "desert", zscale = my.z) %>%
  # add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.9) %>% 
  add_shadow(elevation.amb.shade) %>% 
  add_shadow(elevation.ray.shade, 0.5) %>%
  plot_3d(heightmap = elevation.matrix, 
          zscale = my.z, 
          fov = 90,
          lineantialias = TRUE,
          # shadow
          shadow_darkness = 0.5,
          # base
          baseshape = "rectangle",
          # camera
          theta = 315,
          phi = 25,
          zoom = 0.5,
          close_previous = TRUE)

# add effects to ploted diorama
render_water(elevation.matrix, zscale = my.z, 
             wateralpha = 0.22, 
             watercolor = "skyblue1",
             waterlinealpha = 0.85,
             waterlinecolor = "lightblue1",
             remove_water = TRUE)


# render path / track of organism ID
render_path(extent = attr(elevation.raster,"extent"), 
            lat = unlist(ttdr$latitude), long = unlist(ttdr$longitude),
            altitude = (ttdr$depth)*-6,
            offset = -2,
            zscale = my.z,
            antialias = FALSE,
            # size = 4,
            color="black",
            clear_previous = TRUE)


# add labels ----- 
# start
render_label(elevation.matrix, lat = start[1], long = start[2], 
             z = 10,
             altitude = 3500,
             offset = 150,
             extent = attr(elevation.raster, "extent"),
             zscale = my.z, 
             text = "", textcolor = "black", linecolor="#8BB92D", linewidth = 3,
             dashed = FALSE,
             clear_previous = TRUE)

render_label(elevation.matrix, lat = start[1], long = start[2], 
             z = 10,
             altitude = 3500,
             offset = 150,
             extent = attr(elevation.raster, "extent"),
             zscale = my.z,
             alpha = 0.8,
             text = "", textcolor = "black", linecolor="grey5", linewidth = 5,
             dashed = FALSE,
             clear_previous = F)


# end
render_label(elevation.matrix, lat = end[1], long = end[2],
             z = 10,
             altitude = 4000, 
             extent = attr(elevation.raster, "extent"),
             zscale = my.z, 
             text = "", textcolor = "white", linecolor="#B74733", linewidth = 3,
             dashed = FALSE,
             offset = 2500,
             clear_previous = FALSE)

render_label(elevation.matrix, lat = end[1], long = end[2],
             z = 10,
             altitude = 4000, 
             extent = attr(elevation.raster, "extent"),
             zscale = my.z, 
             text = "", textcolor = "black", linecolor="grey5", linewidth = 5,
             dashed = FALSE,
             offset = 2500,
             clear_previous = FALSE)


# add land mask  --- 
# extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), 
#                                          proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# attr(elevation.matrix, "extent") = extent_latlong
# land = sf::st_simplify(sf::st_buffer(land,-0.003), dTolerance=0.005)

render_polygons(land, 
                extent = attr(elevation.raster, "extent"),
                top = 2.5,
                color = "grey95",
                alpha = 0.75,
                parallel = FALSE,
                clear_previous = TRUE)


# point of view for differenrts plots ------ 
render_camera(theta = 315, phi = 25, zoom = 0.5, fov = 10)


# export / save diorama --------------------------------------------------------
# render snapshot
render_snapshot(filename = paste0(output_dir, "/fig/fig_track_3d.png"))



# note** high comsuming time process
# highquality
# render_snapshot(filename = paste0(output_dir, "/fig/fig_track_3d.png"),
#                 software_render = TRUE)
# 
# render_highquality(filename = paste0(output_dir, "/fig/fig_track_3d_hd.png"))









