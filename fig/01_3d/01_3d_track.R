

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz

# Plot 3D track diorama


## Load libraries

library(plot3D)
library(rgl)
library(plot3Drgl)
library(RColorBrewer)
library(rasterVis)
library(raster)
library(terra)
library(ncdf4)

library(rayshader)



# plot diorama  --------------------------------------------------------------

# limit for represent
xlim <- c(0.6, 4.7)
ylim <- c(36.9, 40.1)


# import location data for plot
organismID <- "200043"
ttdr <- read.csv(paste0(input_dir,"/tracking/ttdr/L3/",organismID,"_L3_ttdr.csv"))

# import bathymetry GEBCO 2024
# note bathymetry cropped to mediterranean sea

# Use raster() not rast() from Terra
b <- raster(paste0(input_dir, "/gis/gebco/mediterranean_sea_gebco_2024.tif"))

# use only values == or < 0 as a bathymetry
# b[b > 0] <- NA #0 data as NA

# Obtain values of batyhymetry in the plot area xlim and ylim
b <- crop(b, extent(c(xlim, ylim)))
plot(b) # check plotting area

# bath_3035 <- projectRaster(bath, crs = CRS("+init=epsg:3035"))
# elevation.raster <- bath_3035


# Prepare diorama -----------------------------------------------

# rename object
elevation.raster <- b

# matrix 
elevation.matrix <- matrix(extract(elevation.raster, extent(elevation.raster), buffer = 100), nrow = ncol(elevation.raster), ncol = nrow(elevation.raster))

# Z scale for elevation matrix
my.z <- 50

# elevantion.matrix %>%
#   sphere_shade(zscale= my.z,
#                texture=create_texture("#E9C68D","#AF7F38",
#                                       "#674F30","#494D30",
#                                        "#B3BEA3")) %>%
#   plot_map()
# 

# test map
elevation.matrix  %>% 
  sphere_shade(sunangle = 35, texture = "imhof3", zscale = my.z) %>%
  plot_map()

# ambient occlusion
elevation.amb.shade <- ambient_shade(elevation.matrix, zscale = my.z, multicore = TRUE, progbar = TRUE)
plot_map(elevation.amb.shade)

# ray shadow
elevation.ray.shade <- ray_shade(elevation.matrix,sunangle = 35, zscale = my.z, multicore = TRUE)
plot_map(elevation.ray.shade)

rm(elevation.amb.shade, elevation.matrix)

# plot diorama ----------------------

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
          theta = 45,
          phi = 90,
          zoom = 0.7)

# add effects to ploted diorama
render_water(elevation.matrix, zscale = my.z, 
             wateralpha = 0.2, 
             watercolor = "skyblue1",
             waterlinealpha = 0.7,
             waterlinecolor = "lightblue1",
             remove_water = TRUE)



# 
# render_points(extent = attr(elevation.raster,"extent"), 
#               lat = unlist(ttdr$latitude), long = unlist(ttdr$longitude),
#               altitude = (ttdr$depth)*-1,
#               offset = -1,
#               zscale = my.z,
#               size = 6,
#               color="black")

# render path / track of organism ID
render_path(extent = attr(elevation.raster,"extent"), 
            lat = unlist(ttdr$latitude), long = unlist(ttdr$longitude),
            altitude = (ttdr$depth)*-5,
            offset = -2,
            zscale = my.z,
            antialias = FALSE,
            # size = 4,
            color="black")

# add labels
# render_label(heightmap = elevation.matrix, x = 797, y = 963-509, z = 10000, linewidth = 1, zscale = my.z, text = "Hilo")
# render_label(heightmap = elevation.matrix, x = 90, y = 963-304, z = 10000, linewidth = 1, zscale = my.z, text = "Mauna Kea - 4205m")


# render_snapshot("mallorca_render.png")
# render_snapshot()



# point of view for differenrts plots
render_camera(theta = 315, phi = 25, zoom = 0.5, fov = 15)


# export / save image
# note** high comsuming time process
# highquality
# render_snapshot(filename = paste0(output_dir, "/fig/fig_track_3d.png"),
#                 software_render = TRUE)
# 
# render_highquality(filename = paste0(output_dir, "/fig/fig_track_3d_hd.png"))

# render 
render_snapshot(filename = paste0(output_dir, "/fig/fig_track_3d.png"))














# ------------------------------------------------------------------------------





# plot 3D mesh
elevation.matrix <- matrix(extract(k2d, extent(k2d), buffer = 100), nrow = ncol(k2d), ncol = nrow(k2d))










# track
# add track points:

# # extention of map
# extent_vals <- extent(elevation.raster)
# # filter by coordinates
# ttdr <- ttdr %>%
#   filter(longitude >= extent_vals[1] & longitude <= extent_vals[2] & 
#            latitude >= extent_vals[3] & latitude <= extent_vals[4])









