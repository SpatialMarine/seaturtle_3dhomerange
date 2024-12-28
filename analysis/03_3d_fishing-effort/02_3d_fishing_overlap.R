




organismID <- 151935

# calcular volumen

file <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3dmkde_obj.rdata")
load(file)
str(mkde.obj)


 
raster_stack <- stack(paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3dmkde_obj_rstack.tif"))
crs(raster_stack) <- CRS("EPSG:3035")
plot(raster_stack)

# read res for threshold values
res <- read.csv(paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3d_res.csv"))


# import locs and ttdr data for this organismID or ptt ----------------------
ttdr <- paste0(main_dir,"/input/tracking/ttdr/L3/",organismID,"_L3_ttdr.csv")
ttdr <- read.csv(ttdr, dec=",", head=TRUE)

str(ttdr)





threshold_res <- res$threshold.95 # 5.030352e-05




# COMPUTE VALUES PARA UN RASTER STACK

probabilities <- c(0.95, 0.75, 0.5)

thresholds <- computeContourValuesVolume(raster_stack, probabilities)
print(thresholds)



threshold <- thresholds$threshold[1] # 5.030352e-05



res$volume.95 # 2.494e+12

calculateVolumeRasterStack(raster_stack, threshold) # 2.297264e+12



plot(raster_stack)



mean(values(raster_stack))
mean(mkde.obj$d)



####


# Tanto para todo el volumen como para cierto thresholds

#################################################################################
#################################################################################
#################################################################################
#################################################################################

# funcion para calcular volumenes en RasterStacks o Bricks, funciona correctamente:

calculateVolumeRasterStack <- function(raster_stack, threshold) {
  # Verificar que el raster_stack tiene múltiples capas
  if (!inherits(raster_stack, "RasterStack") && !inherits(raster_stack, "RasterBrick")) {
    stop("El objeto debe ser un RasterStack o RasterBrick.")
  }
  
  # Calcular las resoluciones espaciales (asumimos que todas las capas tienen la misma resolución)
  xSz <- raster::xres(raster_stack)
  ySz <- raster::yres(raster_stack)
  zSz <- 10 # Si no tienes una dimensión z explícita, puedes asignarle un valor predeterminado
  
  # Volumen de cada celda (3D)
  av <- xSz * ySz * zSz
  
  # Inicializar volumen total
  total_volume <- 0
  
  # Iterar sobre cada capa del RasterStack
  for (j in 1:nlayers(raster_stack)) {
    # Extraer los valores de la capa
    layer_vals <- raster::values(raster_stack[[j]])
    
    # Identificar celdas que cumplen con el umbral
    indices_above_threshold <- which(layer_vals >= threshold)
    
    # Agregar el volumen de las celdas que cumplen con el umbral
    total_volume <- total_volume + (av * length(indices_above_threshold))
  }
  
  return(total_volume)
}


##############################################
##############################################
##############################################
##############################################
##############################################
##############################################




# COMPUTE VALUES PARA UN RASTER STACK

probabilities <- c(0.95, 0.7, 0.5)

# esta funcion funciona, da el mismo threshold
# mismo valores en el raster_stack

# Esta FUNCION HAY QUE METERLA EN LA DE CALCULAR LOS VOLUMENES ENTONCES...
# estaria bis ajustarla para que si hay proabilidad calcule ese cacho, sino que calcule todo el area ocupada

thresholds <- computeContourValuesVolume(raster_stack, probabilities)
print(thresholds)




computeContourValuesVolume <- function(raster_stack, prob) {
  # Extraer todos los valores del raster stack como un solo vector
  all_values <- values(raster_stack)
  all_values <- as.vector(all_values) # Combinar en un vector único
  all_values <- all_values[!is.na(all_values)] # Remover valores NA
  
  # Validar que hay datos suficientes
  if (length(all_values) == 0) {
    stop("No hay valores válidos en el raster stack.")
  }
  
  # Calcular los umbrales considerando todo el volumen
  d2 <- sort(all_values)
  d3 <- cumsum(d2) / sum(d2)
  a <- 1 - prob
  nq <- length(a)
  thresh <- rep(NA, nq)
  for (j in 1:nq) {
    idx <- which(d3 <= a[j])
    if (length(idx) > 0) {
      thresh[j] <- d2[max(idx)]
    }
  }
  
  # Crear el data.frame de salida
  result <- data.frame(prob = prob, threshold = thresh)
  return(result)
}


# funciona la funciona para calcular el threshold, pero es similar al calculado por mkde
# se puede suprimir o guardar por si acaso se utiliza en otro lado


###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################





























































































# ------------------------------------------------------------------------------
# 3) Transform fishing effort metric (hours of navigation into proportional 0-1 values)

min_value <- min(fishing[], na.rm = TRUE)
max_value <- max(fishing[], na.rm = TRUE)

# Normalize raster from 0 y 1
fishing_normalized <- (fishing - min_value) / (max_value - min_value)




# ------------------------------------------------------------------------------
# 4) Calculate the volume of fishing effort
# using fishingtrack3D function (see 02_3d_proces/fun/fun_fishtrack3d.R)

fishing_volume <- volumeUD(fishing_normalized, ind.layer = FALSE)


values(raster_stack)



# calculate ud volumes for raster stack
# fishtrack3D::volumeUD()
# see also fun/fun_fishtrack3d.R
udvolume <- volumeUD(raster_stack, ind.layer = FALSE)

# export raster brick
# Resamplear el RasterStack usando la interpolación bilineal
# applied to all raster stack layer


rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_3dmkde_obj_rstack.tif")
writeRaster(raster_stack, rst_file, overwrite=TRUE)



rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_3d_UD_volume_rstack.tif")
writeRaster(udvolume, rst_file, overwrite=TRUE)







































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

