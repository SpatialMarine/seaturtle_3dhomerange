
##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
## 8. produce 3d plots
##------------------------------------------------------------------------------------------------------------------------------##

## Load libraries

library(mkde)
library(plot3D)
library(rgl)
library(plot3Drgl)
library(RColorBrewer)
library(rasterVis)
library(raster)
library(terra)
library(ncdf4)
library(rayshader)
##------------------------------------------------------------------------------------------------------------------------------##
## Load data -- if not already loaded:
## -- ttdr data frame
## -- mkde object
## -- density threshold estimates for 50 and 95 volumes
##------------------------------------------------------------------------------------------------------------------------------##

# Import data

# # load ttdr dataframe
# 
# load("/Users/jessicaruff/Documents/2020_roarin/Tortugas_3D_2020/3dHR_output_2020_Dec_1/lasi_ttdr_daily.mean.depth.rdata")
# 
# # load mkde object
# 
# load("/Users/jessicaruff/Documents/2020_roarin/Tortugas_3D_2020/3dHR_output_2020_Dec_1/lasi_mkde_daily.mean.depth.rdata")
# 
# # Load density threshold estimates for 50 and 95 volumes
# 
# load("/Users/jessicaruff/Documents/2020_roarin/Tortugas_3D_2020/3dHR_output_2020_Dec_1/lasi_res_daily.mean.depth.rdata")

##------------------------------------------------------------------------------------------------------------------------------##

# Assign thresholds for plotting:

organismID <- 181762
file <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3dmkde_obj.rdata")
load(file)

res <- read.csv(paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3d_res.csv"))
ttdr <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3d_ttdr.rdata")
load(ttdr)



# load("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/34321/34321_3dmkde_obj_night.rdata")


# load("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/34321/34321_3dmkde_obj.rdata")

#res threshoold
vol95 <- res$threshold.95
vol50 <- res$threshold.50

# Get array from mkde object
x=mkde.obj$x
y=mkde.obj$y
z=mkde.obj$z*(-1) # change depth to negative for plotting
F=mkde.obj$d

##------------------------------------------------------------------------------------------------------------------------------##
## Visualize 3dmkde
##------------------------------------------------------------------------------------------------------------------------------##

# 3D plot with volume

ptt <- organismID

isosurf3D(x, y, z, F, level = c(vol50, vol95), 
          col = c("red", "yellow"), 
          clab = "F", alpha = 0.4,  lighting = TRUE, plot=FALSE, main = ptt, zlim = c(0,-120), ticktype = "detailed") 
plotrgl(lighting = TRUE, smooth = TRUE)


isosurf3Drgl(x, y, z, F, 
             col = c("red"), 
             clab = "F", alpha = 0.4,  lighting = TRUE, plot=FALSE, main = ptt, zlim = c(0,-100), ticktype = "detailed")

plotrgl(lighting = TRUE, smooth = TRUE)


# clear3d()

# 3D plot with volume and points
plot3D::points3D(ttdr$x, ttdr$y, ttdr$depth*(-1), col="black",
                 pch = ".", cex = 2, theta = 10, bty = "f", clab = "dg C",
                 colkey = list(side = 1, length = 0.5, width = 0.5,
                               dist = 0.05, shift = -0.2, side.clab = 3, line.clab = 1,
                               cex.clab = 0.8, cex.axis = 0.8), add = TRUE)

# 3D plot with track data using lines
rgl::plot3d(ttdr$x, ttdr$y, ttdr$depth*(-1), type = "l",
            xlab = "", ylab = "", zlab = "",
            col = "blue", size = 5, alpha = 0.05,
            lit = TRUE,
            box = FALSE, axes = FALSE)

isosurf3Drgl()

##------------------------------------------------------------------------------------------------------------------------------##
## Visualize 3dmkde with bathymetry
##------------------------------------------------------------------------------------------------------------------------------##

# GEBCO 2024 Bathymetry
nc <- nc_open(paste0(carto_dir,"/bathymetry/gebco_2024/med/gebco_2024_n47.0_s30.0_w-10.0_e38.0.nc"))
bath <- rast(paste0(carto_dir,"/bathymetry/gebco_2024/med/gebco_2024_n47.0_s30.0_w-10.0_e38.0.tif"))



lat <- ncvar_get(nc, varid="northing")
lon <- ncvar_get(nc, varid="easting")
topo <- ncvar_get(nc, varid="layer")
nc_close(nc)  # closes netcdf

# turtle with volumne
persp3D(lon, lat, z = topo, inttype = 2,  d = 2,
        expand = 0.1, colkey = FALSE, col = "grey98", shade = 0.2,
        lighting = TRUE, box = FALSE, axes = FALSE, plot = FALSE, main = ptt)
isosurf3D(x, y, z, F, level = c(vol50, vol95),colkey = FALSE,
          col = c("red", "yellow"), alpha = 0.4,  lighting = TRUE, add=TRUE)  
plotrgl(lighting = TRUE, smooth = TRUE)

##------------------------------------------------------------------------------------------------------------------------------##































# ---------------------------------------------------------------------------

# plot diorama



bath <- raster(paste0(carto_dir,"/bathymetry/gebco_2024/med/gebco_2024_n47.0_s30.0_w-10.0_e38.0.tif"))



# Definir la extensión de Mallorca (ajusta si es necesario)
# Coordenadas aproximadas: xmin, xmax, ymin, ymax
ext_mallorca <- extent(2.3, 3.5, 38.6, 40.1)

# Recortar el raster con la extensión
bath <- crop(bath, ext_mallorca)
str(bath)

# bath_3035 <- projectRaster(bath, crs = CRS("+init=epsg:3035"))
# elevation.raster <- bath_3035
plot(bath)
elevation.raster <- bath


# matrix 
elevation.matrix <- matrix(extract(elevation.raster, extent(elevation.raster), buffer = 100), nrow = ncol(elevation.raster), ncol = nrow(elevation.raster))

# Z scale
my.z <- 50


# test map
elevation.matrix  %>% 
  sphere_shade(sunangle = 35, texture = "imhof3", zscale = my.z) %>%
  plot_map()

# ambient occlusion
elevation.amb.shade <- ambient_shade(elevation.matrix, zscale = my.z)
# ray shadow
elevation.ray.shade <- ray_shade(elevation.matrix,  sunangle = 35, zscale = my.z, )




elevation.matrix  %>% 
  sphere_shade(sunangle = 35, texture = "desert", zscale = my.z) %>%
  # add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.9) %>% 
  add_shadow(elevation.amb.shade) %>% 
  add_shadow(elevation.ray.shade, 0.7) %>%
  plot_3d(heightmap = elevation.matrix, 
          zscale = my.z, 
          fov = 90,
          lineantialias = TRUE,
          theta = 45,
          phi = 15,
          zoom = 0.7)

render_water(elevation.matrix, zscale = my.z, wateralpha = 0.3, waterlinecolor = "white", watercolor = "skyblue1")
render_camera(theta = 40, phi = 35, zoom = 0.8, fov = 75)

# render_label(heightmap = elevation.matrix, x = 797, y = 963-509, z = 10000, linewidth = 1, zscale = my.z, text = "Hilo")
# render_label(heightmap = elevation.matrix, x = 90, y = 963-304, z = 10000, linewidth = 1, zscale = my.z, text = "Mauna Kea - 4205m")
render_snapshot("mallorca_render.png")
render_snapshot()

# track
# add track points:

# extention of map
extent_vals <- extent(elevation.raster)
# filter by coordinates
ttdr <- ttdr %>%
  filter(longitude >= extent_vals[1] & longitude <= extent_vals[2] & 
           latitude >= extent_vals[3] & latitude <= extent_vals[4])


render_points(extent = attr(elevation.raster,"extent"), 
              lat = unlist(ttdr$latitude), long = unlist(ttdr$longitude),
              altitude = (ttdr$depth)*-1,
              offset = -1,
              zscale = my.z,
              size = 6,
              color="black")

render_path(extent = attr(elevation.raster,"extent"), 
              lat = unlist(ttdr$latitude), long = unlist(ttdr$longitude),
              altitude = (ttdr$depth)*-1,
              offset = -1,
              zscale = my.z,
              antialias = TRUE,
              # size = 4,
              color="grey40")



elevation.matrix <- matrix(extract(k2d, extent(k2d), buffer = 100), nrow = ncol(k2d), ncol = nrow(k2d))














# for 2D
k2d <- raster("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/02_kde_2d/34321/34321_2dmkde_obj_raster.tif")
# reproject to wgs84
crs(k2d) <- CRS("+init=epsg:3035")

# reproject to wgs84
k2d <- projectRaster(k2d, crs = CRS("+init=epsg:4326"))

class(k2d)

# for 3D mkde object....

# extraer extension maxima y minima
# convertirla a wgs84



str(mkde.obj)











# Crear un RasterStack a partir de las dimensiones de 'd'
rasters <- list()

for (i in 1:mkde.obj$nz) {
  # Crear un raster para cada capa z (d), usando las coordenadas 'x' y 'y' en EPSG:3035
  r <- raster(matrix(mkde.obj$d[,,i], nrow = mkde.obj$ny, ncol = mkde.obj$nx), 
              xmn = min(mkde.obj$x), xmx = max(mkde.obj$x),
              ymn = min(mkde.obj$y), ymx = max(mkde.obj$y))
  # Asignar el sistema de coordenadas EPSG:3035
  crs(r) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs" # EPSG:3035
  rasters[[i]] <- r
}

# Crear el RasterStack
raster_stack <- stack(rasters)
plot(raster_stack)


# Reproyectar cada capa del RasterStack de EPSG:3035 a EPSG:4326 (WGS84)
raster_stack_wgs84 <- projectRaster(raster_stack, crs = "+proj=longlat +datum=WGS84 +no_defs")

plot(raster_stack_wgs84)
# Verificar la nueva proyección
crs(raster_stack_wgs84)



# Visualizar con rayshader
# Selecciona el primer raster del stack para visualizarlo
raster_stack[[1]] %>%
  plot_3d(heightmap = ., heightmap_color = c("white", "blue", "green"), windowsize = c(800, 800))






