
#-------------------------------------------------------------------------------------
# 01_3d_process   3D Kernel density
#-------------------------------------------------------------------------------------
# This script processes L1 ttdr and L2 locs files for 
# 1) estimate a 3D kernel density
# 2) UD utulization


source("setup.R")
source("analysis/02_3d_process/fun/fun_ks3d.R")
source("analysis/02_3d_process/fun/fun_fishtrack3d.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # functions for 3d process


# load packages for 3D kernel density
library(lubridate)
library(dplyr)
library(akima)
library(ks)




# 0.) Set paths -----------------------------------------------------------------

# for smru2L0 function (fun_track_reading.R)
# datadir = "C:/Users/david/Google Drive/TORTUGAS OCEANOGRAFAS/data"


# Use L1 for ttdr files and L2 locs files (postprocessed tracking data)
# or L2 ttdr.........
ttdr_files <- list.files(paste0(main_dir,"/input/tracking/ttdr/L1"), full.names = TRUE, pattern = "L1_ttdr.csv")
locs_files <- list.files(paste0(main_dir,"/input/tracking/loc/L2"), full.names = TRUE, pattern = "L2_loc.csv")

# extract ids for ptt from ttdr processed files (individual for sea-turtles)
ptts <- sub("_L1_ttdr\\.csv$", "", basename(ttdr_files))

# set output dir
output_data <- paste0(output_dir,"/","01_kde_3d")
if(!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)



# Set parameters for 3D kernel density stimation (kde)
#' @params for ks::kde function

# Voxel size in meters
# integration.step = 10
voxel.xsize = 10000
voxel.ysize = 10000
voxel.zsize = 10
extend.raster = 10000
# planar crs of study area:
crs = "+init=EPSG:3035"
contours = c(0.50, 0.95)
# number of voxel that the kde function use
gridsize = c(50)






# select ppt for processs... for loop 
ptt <- ptts[7]

ppt <- 34321



# plot for one turtle:
# ttdrfile = "C:/Users/david/Google Drive/TORTUGAS OCEANOGRAFAS/data/animal/loggerhead/151935/L1/L1_TTDR_151935.csv"


# import locs and ttdr data for this ptt

ttdr <- paste0(main_dir,"/input/tracking/ttdr/L2/",ptt,"_L2_ttdr.csv")
# read tracking and ttdr data 
ttdr <- read.csv(ttdr, dec=",", head=TRUE)
# parse / format time date for ttdr data
ttdr$time <- parse_date_time(ttdr$time, "Ymd HMS")
# convert to numeric fields:
ttdr <- ttdr |> mutate(across(c(depth_upper_error, depth_lower_error, depth, drange), as.numeric))



ssm <- paste0(main_dir,"/input/tracking/loc/L2/",ptt,"_L2_loc.csv")
ssm <- read.csv(ssm, dec=",", head=TRUE)
# parse / format time date for L2 loc data
ssm$time <- parse_date_time(ssm$time, "Ymd HMS")

# convert to numeric fields
# reduce the decimals number in the View(df), not affects to number stored
ssm <- ssm |> mutate(across(c(longitude, latitude, lon.025, lon.975, lat.025, lat.975), as.numeric))



# Quality control of the process
if (ssm$organismID[1] == ttdr$organismID[1]) {
  cat("The organism IDs are the same.\n")
} else {
  cat("The organism IDs are different.\n")
}



# 1) Vertical and horizontal erros for dive position --------------------------
# Calculate vertical error from TTDR
ttdr$z.error <- z.error(depth.upper = ttdr$depth_upper_error, depth.lower = ttdr$depth_lower_error)

# Calculate horizontal error from SSM data
# erros in meters, see previously scripts
ssm$xy.error <- xy.error(ssm)

















# Interpolate horizontal errors to TTDR data
# TTDR with NA in timestamps produce erros in aspline function
ttdr$xy.error <- aspline(x=(as.numeric(ssm$time)), y=(ssm$xy.error), xout=(as.numeric(ttdr$time)))$y


## Resample TTDR data from 5 min to interpolated timesteps SSM (hours) (review parameter in 03_regularize_ssm.R and main_tracking_process.R)
resamp <- resampTTDR(ttdr, ssm)
ssm <- merge(ssm, resamp, by="time")



# 2) re project to metric system -------------------------------------------------
# use planar proyection for europe
xy <- reproject(ssm$longitude, ssm$latitude, crs = "+init=epsg:3035")
xy <- xy[,-3] # remove logi column

# remove previously X and Y column from ssm file (L2)
ssm <- ssm %>% dplyr::select(-x,-y)

# combine reproject coordinates with ttdr data
ssm <- cbind(ssm, xy)

## Filter out NA data in detph
ssm <- dplyr::filter(ssm, !is.na(depth_mean))



















# use coordinates en Mecator proyection (x) from SSM

x = ssm$x
y = ssm$y
z = ssm$depth_mean
date = ssm$time
# z.error = ssm$z.error_mean
# xy.error = ssm$xy.error

# output file
# MODIFYYY ..
# rasterfile = rasterfile


# for ks::kde function choose de number of voxel that are used for stimate densities
# gridsiize = 


  
  #convert from data frame to matrix
  df <- data.frame(x, y, z)
  
  ## call the plug-in bandwidth estimator 
  ## They decided to multiply this by 3, by a long process in Simpfendorfer
  # w ith multiplier
  if (is.null(multiplier))  H.pi <- ks::Hpi(df, binned=TRUE)
  if (!is.null(multiplier))  H.pi <- ks::Hpi(df, binned=TRUE) * multiplier
  
  
  #---------------------------------------------------------
  # Define the spatial extent and resolution of a 3D MKDE

  
  #to give a buffer around the kde 3d object:
  xmin <- min(x) - extend.raster
  xmax <- max(x) + extend.raster
  ymin <- min(y) - extend.raster
  ymax <- max(y) + extend.raster
  
  #to set up raster for the mkde3d object:
  r <- raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax,
              crs=CRS(crs), resolution=c(voxel.xsize, voxel.ysize), vals=NA)
  
  #to set up parameters for the initialize mkde3d:
  nx <- ncol(r)
  ny <- nrow(r)
  
  # Calculate the number of z levels for the cube
  nz <- round((ceiling(max(z, na.rm=TRUE)) + voxel.zsize) / voxel.zsize, digits=0)
  zmax <- nz * voxel.zsize # meters
 
  
  ## calculate the kernel densities -----------------------------------
  density <- ks::kde(x = df,  H = H.pi, binned = FALSE, xmin = c(xmin, ymin, zll), xmax = c(xmax, ymax, zmax), gridsize = c(50),
                     compute.cont = FALSE,
                     verbose = TRUE)
  
  ## Calculate 50% volume  
  contour50 <- contourLevels(density, cont=50, approx=FALSE)
  vol50 <- contourSizes(density, cont=50)
  
  # calculate the 95% kernel volume (vol95)
  contour95 <- contourLevels(density, cont=95, approx=FALSE)
  vol95 <- contourSizes(density, cont=95)
  
  
  
  
  # export results ------------------------------------------------------------
  
  # export volum, contours -----------------------------------
  # Combine volume data for export
  voldata <- data.frame(prob=c(0.50, 0.95), threshold = c(contour50, contour95), volume = c(vol50, vol95))

  # gather 3d kde information
  out_info <- data.frame(ptt = ptt,
                         multiplier = multiplier,
                         prob = out$volumnes$prob,
                         threshold = out$volumnes$threshold,
                         volume = out$volumnes$volume,
                         prob_voldata = voldata$prob,
                         threshold_voldata = voldata$threshold,
                         volume_voldata = voldata$volume)

  # expot / save results
  write.csv(final_data, file = paste0(output_data,"/",ptt,"/",ptt,"_kde_3d.csv"), row.names = FALSE)
  
  
  # save and export results for kde ----------------------------
  # export density object (kde) as rda file 
  kde_folder <- paste0(output_data,"/",ptt)
  if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)
  
  kde_file <- paste0(kde_folder,"/",ptt,"_kde_3d.rda")
  # save / export kde
  save(density, file = kde_file)
  
  # export raster bricks of estimated densitites --------------
  # Convert to raster and export RasterStack -  RasterBrick
  rbrick <- brick(density$estimate)
  plot(rbrick)
  
  # Exportar el RasterBrick de kde 3D densidades
  writeRaster(rbrick, file = paste0(output_data,"/",ptt,"/",ptt,"_kde_3d.tif"), format = "GTiff", overwrite = TRUE)
  
  

  
  
  
  
  
  
  # 4) Process Utilization Distribution    -------------------------------------
  
  
  # extrac differenrt depths from zmax and nz calculate previously
  depths <- seq(0, zmax, length.out = nz + 1)
  kde <- density
  
  # function from fishktrack3d R package
  # predict UD utilization
  pred <-  predictKde(kde = kde, raster = r, depths = depths)
  
  
  
  plot(pred)

