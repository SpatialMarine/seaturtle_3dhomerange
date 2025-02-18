

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz

# Plot 3D track diorama
# load only panths, no packges.
# some issue between Terra and Raster
source("setup.R")
source("analysis/z_other/fun/01_other_fun.R")


## Load libraries
library(sf)
library(sp)
library(plot3D)
library(rgl)
library(plot3Drgl)
library(rasterVis)

library(rayshader)
library(raster)
library(dplyr)


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

# import bathymetry GEBCO 2024
# note bathymetry cropped to mediterranean sea

# Use raster() not rast() from Terra
b <- raster(paste0(input_dir, "/gis/gebco/mediterranean_sea_gebco_2024.tif"))

# use only values == or < 0 as a bathymetry
b[b > 0] <- 50  # surface values as 10 m (flat land mask)

# Obtain values of batyhymetry in the plot area xlim and ylim
b <- crop(b, extent(c(xlim, ylim)))
plot(b) # check plotting area


# plot for drifting longlines
fishing_gear <- "LL"


# Prepare and add 3D 50/95UD of organism ID 

# load  kde result by organism ID
kde_res <- read.csv(paste0(main_dir,"/output/01_kde_3d/kde_3d_res.csv"))
kde_res<- kde_res %>% filter(kde_res$organismID == !!organismID)

# filter by threshold
threshold.95 <- kde_res$threshold.95  
threshold.50 <- kde_res$threshold.50


# load kde without interaction of fishing effort
# difference: kde (or UD) WITHOUT fishing impact
rst_file <- paste0(output_dir,"/03_fishing_3d_overlap/",organismID,"_3d_kde_fishing_difference_",fishing_gear,".tif")
difference <- raster::stack(rst_file)
# plot(difference)

# values higher than threshold as NA
# filter raster values by threshold
difference95 <- calc(difference, fun = function(x) { ifelse(x >= threshold.95, x, NA) })
difference50 <- calc(difference, fun = function(x) { ifelse(x >= threshold.50, x, NA) })
rm(difference) # clean enviroment  

# intersect: kde (or UD) WITH fishing impact
rst_file <- paste0(output_dir,"/03_fishing_3d_overlap/",organismID,"_3d_kde_fishing_intersect_",fishing_gear,".tif")
intersect <- raster::stack(rst_file)

intersect95 <- calc(intersect, fun = function(x) { ifelse(x >= threshold.95, x, NA) })
intersect50 <- calc(intersect, fun = function(x) { ifelse(x >= threshold.50, x, NA) })
rm(intersect) # clean enviroment  


names(difference95) <- paste("layer", 1:nlayers(difference95), sep = ".")  # rename layers
names(difference50) <- paste("layer", 1:nlayers(difference50), sep = ".")  # rename layers
names(intersect95) <- paste("layer", 1:nlayers(intersect95), sep = ".")  # rename layers
names(intersect50) <- paste("layer", 1:nlayers(intersect50), sep = ".")  # rename layers

crs(difference95) <- CRS("EPSG:3035")  # add CRS
crs(difference50) <- CRS("EPSG:3035")
crs(intersect95) <- CRS("EPSG:3035")
crs(intersect50) <- CRS("EPSG:3035")

# note 
# plot(intersect95) # check



# -------------------------------------------------------
# load fishing data
LL <- raster::stack(paste0(main_dir,"/output/03_fishing_3d/",organismID,"_3d_fishing-effort_",fishing_gear,".tif"))
crs(LL) <- CRS("EPSG:3035")  # add CRS
names(LL) <- paste("layer", 1:nlayers(LL), sep = ".")  # rename layers
# plot(LL)

# polygonize kde results fitlered by UD threshold
# all fishinf effort is the same for LL, extract only first layer
LL <- LL[[3]]

# poligonize fishing effort
# conver to binary raster
LL[LL > 0] <- 1  # avoid differenet polygons
LL <- raster::rasterToPolygons(LL, dissolve = TRUE)

# Convertir a objeto sf
LL <- st_as_sf(LL)
# plot(LL$geometry) # check results

# conver to CRS of interest for plotting in diorama
st_crs(LL)
st_crs(elevation.raster)
# transform for same CRS of elevation.raster
LL <- st_transform(LL, st_crs(elevation.raster))

# clip / crop for bb extension (for potential bound excess)
LL <- st_crop(LL, bb)
# plot(LL$geometry)

# Note: polygons with "holes" or inside rings are not suitable to plot as 3D mesh
#       in rayshader (for now)
# fill inside level rings
# use fill_inside_rings() custom function (analysis/z_other/fun/01_other_fun.R)
LL <- fill_inside_rings(LL)
# plot(layer$geometry)

# two step smoothing 
LL <- rmapshaper::ms_simplify(LL, keep = 0.3, keep_shapes = T)
LL <- smoothr::smooth(LL, method = "ksmooth")
plot(LL$geometry)





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

## test map
# elevation.matrix  %>% 
#   sphere_shade(sunangle = 35, texture = "imhof3", zscale = my.z) %>%
#   plot_map()

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
  sphere_shade(sunangle = 315, 
               texture = create_texture("#E9D5B5", # Más amarillo para zonas iluminadas  
                                        "#C5AF97", # Suave transición a tonos tierra  
                                        "#9E8B7A", # Tono medio equilibradodo  
                                        "#4A3F38", # Oscuro para zonas de sombra  
                                        "#B2A89E"), # Toque neutro para suavizar contrastes  
               colorintensity = 0.5,
               zscale = my.z) %>%
  # add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.9) %>% 
  add_shadow(elevation.amb.shade, 0.75) %>% 
  add_shadow(elevation.ray.shade, 0.75) %>%
  plot_3d(heightmap = elevation.matrix, 
          zscale = my.z, 
          lineantialias = TRUE,
          # base shadow
          shadow_darkness = 0.5,
          # base
          baseshape = "rectangle",
          # camera
          fov = 10,
          theta = 315,
          phi = 25,
          zoom = 0.5,
          close_previous = T)

# # add effects to ploted diorama
# render_water(elevation.matrix, zscale = my.z,
#              wateralpha = 0.1,
#              watercolor = "transparent",
#              waterlinealpha = 0.85,
#              waterlinecolor = "lightblue1",
#              remove_water = TRUE)


# ----------------------------------------
# add UD95
# rename (as function)
rstack <- difference95

# initial top and bottom values for polygons
top <- 0
bottom <- -5
incr <- 5 # absolute value of increase

for (l in 1:nlayers(rstack)) {
  # info
  cat("Processing layer:", l,"/", nlayers(rstack))
  # check values of layers == NA for layer
  # all NA -> next
  if (all(is.na(values(rstack[[l]])))) {
    # update depths for empty depth
    top <- bottom
    bottom <- bottom - incr
    # and next
    next  
  }
  
  # select layer
  layer <- rstack[[l]]
  # plot(layer)
  
  # poligonize raster -- conver to binary raster
  layer[layer > 0] <- 1  # avoid differenet polygons
  layer <- raster::rasterToPolygons(layer, dissolve = TRUE)
  
  # convert to sf
  layer <- st_as_sf(layer)
  # plot(layer$geometry)
  
  # convert to CRS of interest for plotting in diorama
  # transform for same CRS of elevation.raster
  layer <- st_transform(layer, st_crs(elevation.raster))
  
  # clip / crop for bb extension (for potential bound excess)
  layer <- st_crop(layer, bb)
  # plot(layer$geometry)
  
  # Note: polygons with "holes" or inside rings are not suitable to plot as 3D mesh
  #       in rayshader (for now)
  # fill inside level rings
  # use fill_inside_rings() custom function (analysis/z_other/fun/01_other_fun.R)
  layer <- fill_inside_rings(layer)
  # plot(layer$geometry)
  
  # two step smoothing 
  layer <- rmapshaper::ms_simplify(layer, keep = 0.3, keep_shapes = T)
  layer <- smoothr::smooth(layer, method = "ksmooth")
  # plot(layer$geometry)

  if (l == 1) {
    # render layer polygon -- add fishing layers to and open diorama
    # firts layer clean previously polygons
    render_polygons(layer,
                    extent = attr(elevation.raster, "extent"),
                    top = top,
                    bottom = bottom,
                    scale_data = my.z,
                    holes = 0,
                    color = "#868ada",
                    alpha = 0.15,
                    parallel = F,
                    # light
                    # lit = FALSE,
                    # light_altitude = 10,  
                    # light_direction = 135,  
                    # light_intensity = 0.5,
                    clear_previous = T)
  }

  render_polygons(layer,
                  extent = attr(elevation.raster, "extent"),
                  top = top,
                  bottom = bottom,
                  scale_data = my.z,
                  holes = 0,
                  color = "#868ada",
                  alpha = 0.15,
                  parallel = F,
                  # light parameters
                  # lit = TRUE,  # Keep light
                  # light_altitude = 10,  
                  # light_direction = 135,  
                  # light_intensity = 0.5,  
                  # light_relative = FALSE, 
                  clear_previous = F)
  
  # update top and bottom values
  # Actualizar valores para la siguiente iteración
  top <- bottom
  bottom <- bottom - incr
  
  Sys.sleep(0.5) # pause for avoid render problems
}


#------------------------------------------------------------------------------
# add intersect between fishing effort and UD95
rstack <- intersect95

# initial top and bottom values for polygons
top <- 0.1
bottom <- -5
incr <- 5 # absolute value of increase

for (l in 1:nlayers(rstack)) {
  # info
  cat("Processing layer:", l,"/", nlayers(rstack))
  # check values of layers == NA for layer
  # all NA -> next
  if (all(is.na(values(rstack[[l]])))) {
    # update depths
    top <- bottom
    bottom <- bottom - incr
    # and next
    next
  }
  
  # select layer
  layer <- rstack[[l]]
  # plot(layer)
  
  # poligonize raster -- conver to binary raster
  layer[layer > 0] <- 1  # avoid differenet polygons
  layer <- raster::rasterToPolygons(layer, dissolve = TRUE)
  
  # convert to sf
  layer <- st_as_sf(layer)
  # plot(layer$geometry)
  
  # convert to CRS of interest for plotting in diorama
  # transform for same CRS of elevation.raster
  layer <- st_transform(layer, st_crs(elevation.raster))
  
  # clip / crop for bb extension (for potential bound excess)
  layer <- st_crop(layer, bb)
  # plot(layer$geometry)
  
  # Note: polygons with "holes" or inside rings are not suitable to plot as 3D mesh
  #       in rayshader (for now)
  # fill inside level rings
  # use fill_inside_rings() custom function (analysis/z_other/fun/01_other_fun.R)
  layer <- fill_inside_rings(layer)
  # plot(layer$geometry)
  
  # two step smoothing 
  layer <- rmapshaper::ms_simplify(layer, keep = 0.3, keep_shapes = T)
  layer <- smoothr::smooth(layer, method = "ksmooth")
  # plot(layer$geometry)
  
  render_polygons(layer,
                  extent = attr(elevation.raster, "extent"),
                  top = top,
                  bottom = bottom,
                  scale_data = my.z,
                  holes = 0,
                  color = "#582525",
                  alpha = 0.9,
                  parallel = FALSE,
                  # light parameters
                  # lit = TRUE,  # Keep light
                  # light_altitude = 10,  
                  # light_direction = 135,  
                  # light_intensity = 0.5,  
                  # light_relative = FALSE, 
                  clear_previous = F)
  
  # update top and bottom values
  # Actualizar valores para la siguiente iteración
  top <- bottom
  bottom <- bottom - incr
  
  Sys.sleep(0.5) # pause for avoid render problems
}

# add fishing layers ---------------------------------------
render_polygons(LL,
                extent = attr(elevation.raster, "extent"),
                top = -10,
                bottom = -60,
                scale_data = my.z,
                holes = 0,
                color = "salmon3",
                alpha = 0.3,
                parallel = FALSE,
                clear_previous = F)


# add land mask  --- ---------------------------------------------------
# extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), 
#                                          proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# attr(elevation.matrix, "extent") = extent_latlong
# land = sf::st_simplify(sf::st_buffer(land,-0.003), dTolerance=0.005)

render_polygons(land, 
                extent = attr(elevation.raster, "extent"),
                lit = F,
                top = 2.5,
                color = "grey95",
                alpha = 0.5,
                parallel = FALSE,
                clear_previous = F)


# export / save diorama --------------------------------------------------------
# point of view for differenrts plots -----------
render_camera(theta = 315, phi = 25, zoom = 0.5, fov = 10)
# render snapshot
render_snapshot(filename = paste0(output_dir, "/fig/sup_fig_LL_UD95_overlap_3d.png"))

# point of view for differenrts plots 
render_camera(theta = 90, phi = 0.0, zoom = 0.28, fov = 0)
# export / save diorama 
# render snapshot
render_snapshot(filename = paste0(output_dir, "/fig/sup_fig_LL_UD95_overlap_3d_lowcam_90.png"))

# point of view for differenrts plots 
render_camera(theta = 270, phi = 0.0, zoom = 0.28, fov = 0)
# export / save diorama 
# render snapshot
render_snapshot(filename = paste0(output_dir, "/fig/sup_fig_LL_UD95_overlap_3d_lowcam_270.png"))


# point of view for differenrts plots
render_camera(theta = 0, phi = 90, zoom = 0.6, fov = 0)
# export / save diorama -
# render snapshot
render_snapshot(filename = paste0(output_dir, "/fig/sup_fig_LL_UD95_overlap_3d_cenital.png"))



# note** high comsuming time process
# highquality
# render_snapshot(filename = paste0(output_dir, "/fig/fig_track_3d.png"),
#                 software_render = TRUE)
# 
# render_highquality(filename = paste0(output_dir, "/fig/fig_track_3d_hd.png"))







# ------------------------------------------------------------------------------
# UD 50 ------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 2) Prepare diorama              -----------------------------------------
# 
# # rename object
# elevation.raster <- b
# rm(b) # clean enviroment
# # matrix 
# elevation.matrix <- matrix(extract(elevation.raster, extent(elevation.raster), buffer = 100), nrow = ncol(elevation.raster), ncol = nrow(elevation.raster))

# Z scale for elevation matrix
# my.z <- 25

# elevantion.matrix %>%
#   sphere_shade(zscale= my.z,
#                texture=create_texture("#E9C68D","#AF7F38",
#                                       "#674F30","#494D30",
#                                        "#B3BEA3")) %>%
#   plot_map()

## test map
# elevation.matrix  %>% 
#   sphere_shade(sunangle = 35, texture = "imhof3", zscale = my.z) %>%
#   plot_map()
# 
# # ambient occlusion
# elevation.amb.shade <- ambient_shade(elevation.matrix, zscale = my.z, multicore = TRUE, progbar = TRUE)
# # plot_map(elevation.amb.shade)
# 
# # ray shadow
# elevation.ray.shade <- ray_shade(elevation.matrix,sunangle = 35, zscale = my.z, multicore = TRUE)
# # plot_map(elevation.ray.shade)



# plot diorama -----------------------------------------------------------------
sf::sf_use_s2(FALSE)


# plot diorama
elevation.matrix  %>% 
  sphere_shade(sunangle = 315, 
               texture = create_texture("#E9D5B5", # Más amarillo para zonas iluminadas  
                                        "#C5AF97", # Suave transición a tonos tierra  
                                        "#9E8B7A", # Tono medio equilibradodo  
                                        "#4A3F38", # Oscuro para zonas de sombra  
                                        "#B2A89E"), # Toque neutro para suavizar contrastes  
               colorintensity = 0.5,
               zscale = my.z) %>%
  # add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.9) %>% 
  add_shadow(elevation.amb.shade, 0.75) %>% 
  add_shadow(elevation.ray.shade, 0.75) %>%
  plot_3d(heightmap = elevation.matrix, 
          zscale = my.z, 
          lineantialias = TRUE,
          # base shadow
          shadow_darkness = 0.5,
          # base
          baseshape = "rectangle",
          # camera
          fov = 10,
          theta = 315,
          phi = 25,
          zoom = 0.5,
          close_previous = T)

# # add effects to ploted diorama
# render_water(elevation.matrix, zscale = my.z,
#              wateralpha = 0.1,
#              watercolor = "transparent",
#              waterlinealpha = 0.85,
#              waterlinecolor = "lightblue1",
#              remove_water = TRUE)



# ----------------------------------------------------------------------------
# add UD 50
# rename (as function)
rstack <- difference50

# initial top and bottom values for polygons
top <- 0
bottom <- -6
incr <- 6 # absolute value of increase

alpha <- 0.35

for (l in 1:nlayers(rstack)) {
  # info
  cat("Processing layer:", l,"/", nlayers(rstack))
  # check values of layers == NA for layer
  # all NA -> next
  if (all(is.na(values(rstack[[l]])))) {
    # update depths for empty depth
    top <- bottom
    bottom <- bottom - incr
    # and next
    next  
  }
  
  # select layer
  layer <- rstack[[l]]
  # plot(layer)
  
  # poligonize raster -- conver to binary raster
  layer[layer > 0] <- 1  # avoid differenet polygons
  layer <- raster::rasterToPolygons(layer, dissolve = TRUE)
  
  # convert to sf
  layer <- st_as_sf(layer)
  # plot(layer$geometry)
  
  # convert to CRS of interest for plotting in diorama
  # transform for same CRS of elevation.raster
  layer <- st_transform(layer, st_crs(elevation.raster))
  
  # clip / crop for bb extension (for potential bound excess)
  layer <- st_crop(layer, bb)
  # plot(layer$geometry)
  
  # Note: polygons with "holes" or inside rings are not suitable to plot as 3D mesh
  #       in rayshader (for now)
  # fill inside level rings
  # use fill_inside_rings() custom function (analysis/z_other/fun/01_other_fun.R)
  layer <- fill_inside_rings(layer)
  # plot(layer$geometry)
  
  # two step smoothing 
  layer <- rmapshaper::ms_simplify(layer, keep = 0.3, keep_shapes = T)
  layer <- smoothr::smooth(layer, method = "ksmooth")
  # plot(layer$geometry)
  
  if (l == 1) {
    # render layer polygon -- add fishing layers to and open diorama
    # firts layer clean previously polygons
    render_polygons(layer,
                    extent = attr(elevation.raster, "extent"),
                    top = top,
                    bottom = bottom,
                    scale_data = my.z,
                    holes = 0,
                    color = "#868ada",
                    alpha = alpha,
                    parallel = F,
                    # light
                    # lit = FALSE,
                    # light_altitude = 10,  
                    # light_direction = 135,  
                    # light_intensity = 0.5,
                    clear_previous = T)
  }
  
  render_polygons(layer,
                  extent = attr(elevation.raster, "extent"),
                  top = top,
                  bottom = bottom,
                  scale_data = my.z,
                  holes = 0,
                  color = "#787bc4",
                  alpha = alpha,
                  parallel = F,
                  # light parameters
                  # lit = TRUE,  # Keep light
                  # light_altitude = 10,  
                  # light_direction = 135,  
                  # light_intensity = 0.5,  
                  # light_relative = FALSE, 
                  clear_previous = F)
  
  # update top and bottom values
  # Actualizar valores para la siguiente iteración
  top <- bottom
  bottom <- bottom - incr
  alpha <- alpha + 0.05
  
  Sys.sleep(0.5) # pause for avoid render problems
}


#------------------------------------------------------------------------------
# add intersect between fishing effort and UD95
rstack <- intersect50

# initial top and bottom values for polygons
top <- 0.1
bottom <- -6
incr <- 6 # absolute value of increase

for (l in 1:nlayers(rstack)) {
  # info
  cat("Processing layer:", l,"/", nlayers(rstack))
  # check values of layers == NA for layer
  # all NA -> next
  if (all(is.na(values(rstack[[l]])))) {
    # update depths
    top <- bottom
    bottom <- bottom - incr
    # and next
    next
  }
  
  # select layer
  layer <- rstack[[l]]
  # plot(layer)
  
  # poligonize raster -- conver to binary raster
  layer[layer > 0] <- 1  # avoid differenet polygons
  layer <- raster::rasterToPolygons(layer, dissolve = TRUE)
  
  # convert to sf
  layer <- st_as_sf(layer)
  # plot(layer$geometry)
  
  # convert to CRS of interest for plotting in diorama
  # transform for same CRS of elevation.raster
  layer <- st_transform(layer, st_crs(elevation.raster))
  
  # clip / crop for bb extension (for potential bound excess)
  layer <- st_crop(layer, bb)
  # plot(layer$geometry)
  
  # Note: polygons with "holes" or inside rings are not suitable to plot as 3D mesh
  #       in rayshader (for now)
  # fill inside level rings
  # use fill_inside_rings() custom function (analysis/z_other/fun/01_other_fun.R)
  layer <- fill_inside_rings(layer)
  # plot(layer$geometry)
  
  # two step smoothing 
  layer <- rmapshaper::ms_simplify(layer, keep = 0.3, keep_shapes = T)
  layer <- smoothr::smooth(layer, method = "ksmooth")
  # plot(layer$geometry)
  
  render_polygons(layer,
                  extent = attr(elevation.raster, "extent"),
                  top = top,
                  bottom = bottom,
                  scale_data = my.z,
                  holes = 0,
                  color = "#582525",
                  alpha = 1,
                  parallel = FALSE,
                  # light parameters
                  # lit = TRUE,  # Keep light
                  # light_altitude = 10,  
                  # light_direction = 135,  
                  # light_intensity = 0.5,  
                  # light_relative = FALSE, 
                  clear_previous = F)
  
  # update top and bottom values
  # Actualizar valores para la siguiente iteración
  top <- bottom
  bottom <- bottom - incr
  
  Sys.sleep(0.5) # pause for avoid render problems
}


# add fishing layers ---------------------------------------
render_polygons(LL,
                extent = attr(elevation.raster, "extent"),
                top = -5,
                bottom = -55,
                scale_data = my.z,
                holes = 0,
                color = "salmon3",
                alpha = 0.4,
                parallel = FALSE,
                clear_previous = F)

# add land mask  --- ---------------------------------------------------
# extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), 
#                                          proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# attr(elevation.matrix, "extent") = extent_latlong
# land = sf::st_simplify(sf::st_buffer(land,-0.003), dTolerance=0.005)

render_polygons(land, 
                extent = attr(elevation.raster, "extent"),
                lit = F,
                top = 2.5,
                color = "grey95",
                alpha = 0.5,
                parallel = FALSE,
                clear_previous = F)




# point of view for differenrts plots ----------------------------------------- 
render_camera(theta = 315, phi = 25, zoom = 0.5, fov = 10)
# export / save diorama 
# render snapshot
render_snapshot(filename = paste0(output_dir, "/fig/fig_LL_UD50_overlap_3d.png"))


# point of view for differenrts plots ----------------------------------------- 
render_camera(theta = 90, phi = 0.0, zoom = 0.28, fov = 0)
# export / save diorama 
# render snapshot
render_snapshot(filename = paste0(output_dir, "/fig/sup_fig_LL_UD50_overlap_3d_lowcam_90.png"))


# point of view for differenrts plots ----------------------------------------- 
render_camera(theta = 270, phi = 0.0, zoom = 0.28, fov = 0)
# export / save diorama 
# render snapshot
render_snapshot(filename = paste0(output_dir, "/fig/sup_fig_LL_UD50_overlap_3d_lowcam_270.png"))


# point of view for differenrts plots ----------------------------------------- 
render_camera(theta = 0, phi = 90, zoom = 0.6, fov = 0)
# export / save diorama --------------------------------------------------------
# render snapshot
render_snapshot(filename = paste0(output_dir, "/fig/sup_fig_LL_UD50_overlap_3d_cenital.png"))



# note** high comsuming time process
# highquality
# render_snapshot(filename = paste0(output_dir, "/fig/fig_track_3d.png"),
#                 software_render = TRUE)
# 
# render_highquality(filename = paste0(output_dir, "/fig/fig_track_3d_hd.png"))



plot(land$geometry)
plot(LL$geometry, add = TRUE)



