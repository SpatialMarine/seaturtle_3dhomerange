#------------------------------------------------------------------------------
## 3D Habitat Use of Loggerhead Turtles

## Created by Javier Menéndez-Blázquez | @jmenblaz

# Update package and standarized field names following Sequeria et al., 2021



# 1) load / import data 
# 2) process 3D overlap between fishing effort data and kde organism ID processed 
#   - 2.1 Load data for processing - kde and fishing effort 
#   - 2.2 Calculate the 3D overlap ------------------------------------------
#   - 2.3 Calculate the 3D overlap volumes  ---------------------------------

# ------------------------------------------------------------

# 0) Load libraries -----
library(mkde)
library(raster)

source("setup.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # custom functions for 3d process


# -----------------------------------------------------------------------

# 1) load importa data ----------------------------------------------------
# kde 3D results
kde_files <- list.files(paste0(main_dir,"/output/01_kde_3d/"), full.names = TRUE, pattern = "_3dmkde_obj_rstack.tif", recursive = TRUE)
# note: volumes in m3
kde_res <- read.csv(paste0(main_dir,"/output/01_kde_3d/kde_3d_res.csv"))
# transform to km3
kde_res$volume.50.km3 <- kde_res$volume.50/1000000000
kde_res$volume.75.km3 <- kde_res$volume.75/1000000000
kde_res$volume.95.km3 <- kde_res$volume.95/1000000000  

# different fishing gears (see 01_3d_fishing-overlap.R)
fishing_gears <- c("LL","TW")  # create a chr chain


# ------------------------------------------------------------------------------
# 2) process 3D overlap between fishing effort data and kde organism ID processed 

# t <- Sys.time()
# 
# cores <- detectCores() - 2
# cl <- makeCluster(cores)
# registerDoParallel(cl)
# 
# getDoParWorkers() # backend information


t <- Sys.time()

for (i in 1:length(kde_files)) { }
  
  # 1) load data for processing - kde and fishing effort --------------
  kde <- kde_files[i]
  # extract organismID from L3_ttdr fiel name
  organismID <- sub("_3dmkde_obj_rstack\\.tif$", "", basename(kde))
  
  # info 
  cat("Processing 3D overlap with fisheries:", i,"/",length(kde_files))
  cat(" · organismID:", organismID, "\n")
  
  # load kde result from mkde 3d functions process
  # note: volumes in km3, transfortmed previously
  kde_res_id <- kde_res %>% filter(kde_res$organismID == !!organismID)
  
  # load 3D kde (rasterstack)
  kde <- raster::stack(kde)
  crs(kde) <- CRS("EPSG:3035")  # add CRS
  names(kde) <- paste("layer", 1:nlayers(kde), sep = ".")  # rename layers
  
  # values 0 as NA in RasterStack
  # double check, implemented before
  kde <- calc(kde, fun = function(x) { 
    x[x == 0] <- NA
    return(x)
  })
  
  # min(values(kde), na.rm = TRUE)
  # plot(kde)

  for (g in 1:length(fishing_gears)) {}
  # select fishing gear
  fishing_gear <- fishing_gears[g]
  
  # load fishing data
  fishing <- raster::stack(paste0(main_dir,"/output/03_fishing_3d/",organismID,"_3d_fishing-effort_",fishing_gear,".tif"))
  crs(fishing) <- CRS("EPSG:3035")  # add CRS
  names(fishing) <- paste("layer", 1:nlayers(kde), sep = ".")  # rename layers
  plot(fishing)
  
  # check min values without NA
  # min(values(fishing), na.rm = TRUE)
  
  
  
  # 2) calculate the 3D overlap ------------------------------------------
  
  # 2.1) calculate interaction between kde and fishing effort
  # multiply kde and fishing effort values to identify areas with high impact
  # high impact = high kde values and high fishing effort
  fishing_interact <- kde * fishing
    plot(fishing_interact)
  
  # 2.2) fishing used as mask in raster::mask()
  # logic: remove voxels where there are fishing impact in order to calculate 
  #   - result: is the volumen affects by fishing activities 
  # (is necessary to calculate the difference between total volume or with threshold)

  kde_fishing_intersect <- raster::mask(kde, fishing)  # provide the UD volume of the impact
    plot(kde_fishing_intersect)
    
  # kde without fishing area of impact(simetrical diference)
  kde_fishing_simdif <- raster::mask(kde, fishing,  inverse=TRUE) # provide the UD volume without impact
    plot(kde_fishing_simdif)
  
    
  # 3) calculate the 3D overlap volumes  ---------------------------------
  threshold.95 <- kde_res_id$threshold.95  
  threshold.75 <- kde_res_id$threshold.75
  threshold.50 <- kde_res_id$threshold.50
  
  z = 10 # depth meters per layer
  # calculate_vol_stacl (custom function in: fun_3d_utils.R)
  # volumes in m3
  volume.95 <- calculate_vol_stack(kde_fishing_simdif, z = z, threshold.95) 
  volume.75 <- calculate_vol_stack(kde_fishing_simdif, z = z, threshold.75)
  volume.50 <- calculate_vol_stack(kde_fishing_simdif, z = z, threshold.50)
  
  # m3 -> km3
  volume.95 <- volume.95/1000000000  
  volume.75 <- volume.75/1000000000
  volume.50 <- volume.50/1000000000
  
  # UD volume without intersect between fishing effort (kde free of interaction)
  udvol95_intersect <- (kde_res_id$volume.95.km3) - volume.95
  udvol95_intersect_percentage <- ((kde_res_id$volume.95.km3 - volume.95) / kde_res_id$volume.95.km3) * 100
  
  udvol75_intersect <- (kde_res_id$volume.75.km3) - volume.75
  udvol75_intersect_percentage <- ((kde_res_id$volume.75.km3 - volume.75) / kde_res_id$volume.75.km3) * 100
  
  udvol50_intersect <- (kde_res_id$volume.50.km3) - volume.50
  udvol50_intersect_percentage <- ((kde_res_id$volume.50.km3 - volume.50) / kde_res_id$volume.50.km3) * 100
  
  # for total volume
  volume.total     <- calculate_vol_stack(kde, z = z)
  volume_intersect <- calculate_vol_stack(kde_fishing_intersect, z = z)
  volume_total_intersect <- volume.total - volume_intersect 
  # **** some problems for calculate volumnes with NA in some layers of raster stack
  
  
  
  # 4) save / export results ----------------------------------------------
  
  
  
  
  
  rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_3dmkde_obj_rstack.tif")
  writeRaster(raster_stack, rst_file, overwrite=TRUE)
  
  
  
  rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_3d_UD_volume_rstack.tif")
  writeRaster(udvolume, rst_file, overwrite=TRUE)






































































organismID <- 34321

# calcular volumen

file <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3dmkde_obj.rdata")
load(file)
str(mkde.obj)



raster_stack <- stack(paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3dmkde_obj_rstack.tif"))
crs(raster_stack) <- CRS("EPSG:3035")
names(raster_stack) <- paste("layer", 1:nlayers(raster_stack), sep = ".")
plot(raster_stack)

min(na.omit(values(raster_stack)))

# values 0 as NA
raster_stack <- calc(raster_stack, fun = function(x) { 
  x[x == 0] <- NA
  return(x)
})




# read res for threshold values
res <- read.csv(paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3d_res.csv"))


# import locs and ttdr data for this organismID or ptt ----------------------
# ttdr <- paste0(main_dir,"/input/tracking/ttdr/L3/",organismID,"_L3_ttdr.csv")
# ttdr <- read.csv(ttdr, dec=",", head=TRUE)

str(ttdr)



min(mkde.obj$d)
mean(mkde.obj$d)
min(na.omit(values(raster_stack)))




# COMPUTE VALUES PARA UN RASTER STACK

quartiles <- c(0.95, 0.75, 0.5)






res$volume.95 # 2.494e+12
res$volume.75 # 9.67e+11 
res$volume.50 # 4.08e+11
res$threshold.95 #  5.030352e-05
res$threshold.75 # 0.0002899344
res$threshold.50 # 0.0006793741




thresholds <- computeContourValuesVolume(raster_stack, quartiles) # funciona la funciona bien

print(thresholds)



threshold = res$threshold.50

threshold = res$threshold.95 # valor umbral del threshold... no la probabilidad
calculateVolumeRasterStack(raster_stack, threshold) # 2.369499e+12





calculateVolumeForThresholds(raster_stack, quartiles)



quartiles <- c(0.95, 0.75, 0.5)
z = 10 
compThresholds(raster_stack, prob = quartiles)

calculate_vol_stack(raster_stack, z = z, threshold)


plot(raster_stack)



mean(values(raster_stack))
mean(mkde.obj$d)












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

