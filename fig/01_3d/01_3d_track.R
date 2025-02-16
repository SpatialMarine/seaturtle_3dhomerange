

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz

# Plot 3D track diorama


## Load libraries
library(sf)
library(plot3D)
library(rgl)
library(plot3Drgl)
library(RColorBrewer)
library(rasterVis)
library(raster)
library(terra)
library(ncdf4)

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

# matrix 
elevation.matrix <- matrix(extract(elevation.raster, extent(elevation.raster), buffer = 100), nrow = ncol(elevation.raster), ncol = nrow(elevation.raster))

# Z scale for elevation matrix
my.z <- 25

plot(elevation.matrix)
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
             wateralpha = 0.22, 
             watercolor = "skyblue1",
             waterlinealpha = 0.75,
             waterlinecolor = "lightblue1",
             remove_water = TRUE)


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
# start
render_label(elevation.matrix, lat = start[1], long = start[2], 
             z = 10,
             altitude = 1000,
             extent = attr(elevation.raster, "extent"),
             zscale = my.z, 
             text = "Start", textcolor = "black", linecolor="darkgreen",
             dashed = FALSE)
# end
render_label(elevation.matrix, lat = end[1], long = end[2],
             z = 5000,
             altitude = 1000, 
             extent = attr(elevation.raster, "extent"),
             zscale = my.z, 
             text = "End", textcolor = "white", linecolor="darkred",
             dashed = FALSE,
             offset = 2500,
             clear_previous = FALSE)



# IN PROCESSS -------------------------- WORKS BUT SOLVE PROBLEM WITH EMPTY POLYGONS
# add countries
sf::sf_use_s2(FALSE)

land = sf::st_simplify(sf::st_buffer(land,-0.003), dTolerance=0.005)
plot(land$geometry)
render_polygons(land, 
                extent = attr(elevation.raster, "extent"),
                top = 50,
                parallel = FALSE)

land <- land

st_is_empty(land)
land <- land[!st_is_empty(land), ]

# extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), 
#                                          proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# attr(elevation.matrix, "extent") = extent_latlong


st_bbox(land)

# point of view for differenrts plots
render_camera(theta = 315, phi = 25, zoom = 0.5, fov = 15)




# export / save diorama --------------------------------------------------------
# render snapshot
render_snapshot(filename = paste0(output_dir, "/fig/fig_track_3d.png"))



# note** high comsuming time process
# highquality
# render_snapshot(filename = paste0(output_dir, "/fig/fig_track_3d.png"),
#                 software_render = TRUE)
# 
# render_highquality(filename = paste0(output_dir, "/fig/fig_track_3d_hd.png"))






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









