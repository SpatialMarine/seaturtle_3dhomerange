


# Fishing effort volume
#calculate fishing effort volume for each turtle


#------------------------------------------------------------------------------
## 3D Habitat Use of Loggerhead Turtles

# Update package and standarized field names following Sequeria et al., 2021
# by Javier Menéndez-Blázquez | @jmenblaz

# 1) Extract fishing effort for extent of seaturtle kde estimate.






# Cargar la librería sf
library(sf)
library(raster)


# Assign thresholds for plotting:

organismID <- 181762
file <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3dmkde_obj.rdata")
load(file)

# loaded by load(file)
mkde.obj 

# extract extension from mkde.obj (epsg:3035)
xmax <- max(mkde.obj$x)
xmin <- min(mkde.obj$x)
ymax <- max(mkde.obj$y)
ymin <- min(mkde.obj$y)

# bounding box of seaturtle track extension
bb <- st_as_sfc(st_bbox(c(
    xmin = xmin,
    ymin = ymin,
    xmax = xmax,
    ymax = ymax
  ), crs = st_crs(3035)))

# convert or transform to from EPSG:3035 to EPSG:4326 (WGS84) bounding box
bb <- st_transform(bb, crs = 4326)
bb <- as(bb, "Spatial")  # transfor, from sfc to sp class



# import fishing effort data (Global fishing Watch, Global Marine Traffic, etc)

# MODIFICAR FUTURO
# Global Marine Traffic Data 
file <- list.files(paste0(input_dir,"/fishing/"), full.names = T)
fishing <- raster(file)


# crop fishing effort to bounding-box of tracking data 
# (kernel density estimate extent)
fishing <- crop(fishing, bb)
fishing <- raster::mask(fishing, bb)


# convert from 2D to 3D raster stack fishing effort depends of the fishing gear depth
# example: trawler, etc




# re-convert to epsg 3035 to work with mkde.obj SRC 
fishing_3035 <- raster::projectRaster(fishing, crs = CRS("EPSG:3035"))







# Supongamos que la proyección original es UTM (por ejemplo, EPSG:32633) o alguna otra.

# Cambia esto según la proyección de tu raster original.
crs_original <- CRS("EPSG:3035") # Cambia según sea necesario
crs(raster_brick) <- crs_original  # Asigna la proyección original

plot(raster_brick)

# Proyectar el raster a WGS84
raster_wgs84 <- projectRaster(raster_brick, crs = CRS("+proj=longlat +datum=WGS84"))

plot(raster_wgs84[[1]])



# Paso 2: Crear un mapa del mundo usando 'rnaturalearth'
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Paso 3: Convertir el raster a un formato que ggplot pueda usar
# Convertir el RasterBrick a un data.frame para ggplot
raster_df <- as.data.frame(raster_wgs84[[3]], xy = TRUE, na.rm = TRUE)


# volumeUD for fish3dtrack GitHub
udvolume <- volumeUD(raster_brick, ind.layer = FALSE)





# Paso 4: Plotear el mapa
ggplot() +
  # Mapear el fondo con el mapa mundial
  geom_sf(data = world_map, fill = "lightgray", color = "black") +
  # Añadir el raster transformado en WGS84
  geom_raster(data = raster_df, aes(x = x, y = y)) +
  scale_fill_viridis_c() + # Usar una paleta de colores (puedes cambiarla)
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +  # Limitar la vista
  theme_minimal() +
  labs(title = "Mapa de Datos Raster Transformados a WGS84") +
  theme(axis.text = element_blank(), axis.ticks = element_blank())














# use aspilaga fishtrack3d functions to complite 3D overlap funcionn 


' Spatial overlap between two utilization distributions
#'
#' This function takes two \code{RasterLayer}, \code{RasterStack} or
#' \code{RasterBrick} objects with UD volumes and calculates the proportion
#' of the volume that is overlapped within a probability contour.
#'
#' @param ud1,ud2  \code{RasterLayer}, \code{RasterStack} or \code{RasterBrick}
#'     objects with the UD volumes to overlap. If it is a \code{RasterStack} or
#'     \code{RasterBrick} object, the number and name of the layers must
#'     coincide.
#' @param level UD volume probability contour to be used to calculate the
#'     volume overlap.
#' @param symmetric logical. If \code{TRUE}, the overlapped index is calculated
#'     referred to the total joint volume of the two UDs (volume(overlapped) /
#'     volume(\code{ud1}) + volume(\code{ud2})). If \code{FALSE}, two overlap
#'     indexes are calculated, the first one referred to the volume of
#'     \code{ud1} (volume(overlapped) / volume(\code{ud1})), and the second one
#'     referred to the volume of \code{ud2} (volume(overlapped) /
#'     volume(\code{ud2})).
#'
#' @return A vector with one (if \code{symmetric == TRUE} or two
#'     (\code{symmetric == FALSE}) overlap indexes.
#'
#' @export
#'
#'
volOverlap <- function(ud1, ud2, level, symmetric = TRUE) {
  
  # Check if arguments are correct =============================================
  if (is.null(ud1) | is.null(ud2) | class(ud1) != class(ud2) |
      any(!c(class(ud1), class(ud2)) %in% c("RasterLayer", "RasterBrick",
                                            "RasterStack"))) {
    stop(paste("Both UDs ('ud1' and 'ud2') must be provided as a ",
               "'RasterLayer', 'RasterBrick' or 'RasterStack' object."),
         call. = FALSE)
  }
  
  if (class(ud1) %in% c("RasterBrick", "RasterStack") &
      any(names(ud1) != names(ud2)) |
      any(raster::res(ud1) != raster::res(ud2))) {
    stop("The two UDs must have the same resolution and extension.",
         call. = FALSE)
  }
  
  if (is.null(level) | class(level) != "numeric" | level < 0 | level > 1) {
    stop("The probability contour ('level') must be a value between 0 and 1.",
         call. = FALSE)
  }
  
  data1 <- raster::values(ud1) <= level
  data2 <- raster::values(ud2) <= level
  
  overlap <- sum(data1 & data2)
  
  if (symmetric) {
    total <- sum(data1 | data2)
    return(round(overlap / total, 3))
  } else {
    return(round(c(overlap / sum(data1), overlap / sum(data2)), 3))
  }
  
}










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

