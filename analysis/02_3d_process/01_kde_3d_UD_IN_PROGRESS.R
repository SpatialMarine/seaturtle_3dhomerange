# borrar 




#-------------------------------------------------------------------------------------
# 01_3d_process   3D Kernel density
#-------------------------------------------------------------------------------------
# This script processes L1 ttdr and L2 locs files for 
# 1) estimate a 3D kernel density
# 2) UD utulization


source("setup.R")
source("analysis/02_3d_process/fun/fun_ks3d.R")
# source("analysis/02_3d_process/fun/fun_fishtrack3d.R")
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
ptts <- sub("_L1_ttdr//.csv$", "", basename(ttdr_files))



# set output dir
output_data <- paste0(output_dir,"/","01_kde_3d")
if(!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)



# load data for one ptt (JMB)


# select ppt for processs... for loop 
ptt <- ptts[22]





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




str(night)

load("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/34321/34321_3d_ttdr_night.rdata")


# ------------------------------------------------------------------------------------------------------
# 2 script Jess -------------------------------------------------------------------


## Calculate vertical error from TTDR
# errors in meters (m)
ttdr$z.error <- z.error(depth.upper = ttdr$depth_upper_error, depth.lower = ttdr$depth_lower_error)

## Calculate horizontal error from SSM data
# errors in meters (m)
ssm$xy.error <- xy.error(ssm)

# Interpolate horizontal errors to TTDR data
# TTDR with NA in timestamps produce erros in aspline function
ttdr$xy.error <- aspline(x=(as.numeric(ssm$time)), y=(ssm$xy.error), xout=(as.numeric(ttdr$time)))$y

# Resample TTDR data from 5 min to interpolated timesteps SSM (hours) 
# (review parameter in 03_regularize_ssm.R and main_tracking_process.R)
resamp <- resampTTDR(ttdr, ssm)
ssm <- merge(ssm, resamp, by="time")


# use planar projection for europe
xy <- reproject(lon = ssm$longitude, lat = ssm$latitude, crs = "+init=epsg:3035")
xy <- xy[,-3] # remove logi column # TRUE == reprojected

# rename "x" and "y" columns from ssm file (L2)
# reminder: Mercator estimated coordinates from apply State Space Models: fit_ssm()
ssm <- ssm %>% rename(x_ssm = x,
                      y_ssm = y)

# combine reproject coordinates with ssm data
ssm <- cbind(ssm, xy)


# Reproject to metric system -- 5 min ttdr
ttdr <- ttdr |> mutate(across(c(longitude, latitude), as.numeric))
xy <- reproject(ttdr$longitude, ttdr$latitude, crs = "+init=epsg:3035")
xy <- xy[,-3] # remove logi column # TRUE == reprojected
ttdr <- cbind(ttdr, xy)


# Filter out NA data in depth
ssm <- dplyr::filter(ssm, !is.na(depth_mean))
ttdr <- dplyr::filter(ttdr, !is.na(depth)) #aslo filtered in L2 ttdr data

# Remove extra objects not needed for enviroment
rm(resamp, xy)






# -----------------------------------------------------------------------------------------
# Script 2.5 Jess ---------------------------------------------------------------

# Process day/night for location --- already processed in ttdr process (01_tracking/scr/04_process_ttdr.R)

# library(maptools)
library(suntools) # use suntools (newer) instead of maptools
library(raster)

data <- ttdr

### Incorporate sun and lunar metrics
### Calculate: sunrise and sunset times; moon phase
### Derive: day/night, time to sunset, absolute diff time to sunrise/sunset

data$sunrise <- suntools::sunriset(crds=cbind(data$longitude, data$latitude), dateTime=data$time, direction=c("sunrise"), POSIXct.out=TRUE)$time
data$sunset <- suntools::sunriset(crds=cbind(data$longitude, data$latitude), dateTime=data$time, direction=c("sunset"), POSIXct.out=TRUE)$time
data$dawn <- suntools::crepuscule(crds=cbind(data$longitude, data$latitude), dateTime=data$time, solarDep=12, direction="dawn", POSIXct.out=TRUE)$time  # nautical(12), astronomic(18), civil(6)
data$dusk <- suntools::crepuscule(crds=cbind(data$longitude, data$latitude), dateTime=data$time, solarDep=12, direction="dusk", POSIXct.out=TRUE)$time  # nautical(12), astronomic(18), civil(6)


## Derive day/night
time<-data$time[1]
dawn<-data$dawn[1]
dusk<-data$dusk[1]

daynight <- function (time, sunrise, sunset){
  ifelse(time >= sunrise & time <= sunset, "day", "night")
}

data$daynight <- mapply(daynight, time=data$time, sunrise=data$sunrise, sunset=data$sunset)


# select 
day <- data[data$daynight=="day",]
night <- data[data$daynight=="night",]









# -----------------------------------------------------------------------------------------
# Script 3 Jess ---------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
## 3. Calculate 3D mkde
##------------------------------------------------------------------------------------------------------------------------------##

# Load libraries
library(mkde)
library(raster)


# set output dir
output_data <- paste0(output_dir,"/","01_kde_3d")
if(!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)



##------------------------------------------------------------------------------------------------------------------------------##
# Calculate 3d mkde steps:
# -- initializeMovementData
# -- initializeMKDE3D
# -- initializeDensity
# -- computeContourValues
# -- computeSizeMKDE
##------------------------------------------------------------------------------------------------------------------------------##

## Add variables to include in parameter table
## Note: column names are different for z error in ttdr vs. ssm
mean.xy.error <- mean(ttdr$xy.error)
mean.z.error <- mean(ttdr$z.error)
ttdr$depth_adjusted <- as.numeric(ttdr$depth_adjusted) # convert to numeric
avgdepth <- mean(ttdr$depth_adjusted, na.rm = T) 
ttdr$yday <- yday(ttdr$time) # numer of day for a year
days.tracked <- length(unique(ttdr$yday)) 

# ptt <- ttdr$ptt[1] # already selected


#--------------------------------------------------------------------------------
# mkde3d         Calculate 3D volumnes using mkde package
#--------------------------------------------------------------------------------

# date          Time stamps in POSIXct format
# x
# y
# z
# z.error
# xy.error
# t.max                 maximum time allowed between locations in minutes
# integration.step      time integration step in minutes. integration time step should be much less than the maximum time step allowed between observed animal locations. 
# voxel.xsize
# voxel.ysize
# voxel.zsize
# zlevels
# zll
# extend.raster
# crs
# contours

## mkde package functions inputs:

x = ttdr$x
y = ttdr$y
z = ttdr$depth_adjusted
date = ttdr$time
z_error = ttdr$z.error  ## changed to avoid overwriting function name
xy_error = ttdr$xy.error  ##changed to avoid overwriting function name
t.max = 250
integration.step = 5
voxel.xsize = 10000
voxel.ysize = 10000
voxel.zsize = 10
extend.raster = 10000
zll = 0
crs = "+init=epsg:3035"
contours = c(0.50, 0.75, 0.95)


#---------------------------------------
# Initialize a movement data list

# -- initializeMovementData
#---------------------------------------

# Location error variance in the xy dimension
sig2obs <- xy_error^2  # name changed to avoid overwriting function name
sig2obs.z <- z_error^2  # name changed to avoid overwriting function name

# Convert time stamps to elapsed minutes from first time
time <- as.numeric(difftime(date, date[1], units="mins")) 

# Set up movement data
# warning message is generated when a vector is used for the z dimension error, but it still works
mv.dat <- initializeMovementData(t.obs = time, x.obs = x, y.obs = y, z.obs = z,
                                 sig2obs= sig2obs, sig2obs.z = sig2obs.z, t.max = t.max)

#---------------------------------------------------------
# Define the spatial extent and resolution of a 3D MKDE
#---------------------------------------------------------

# to give a buffer around the mkde3d object:
xmin <- min(x) - extend.raster
xmax <- max(x) + extend.raster
ymin <- min(y) - extend.raster
ymax <- max(y) + extend.raster

# to set up raster for the mkde3d object:
r <- raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax,
            crs=CRS(crs), resolution=c(voxel.xsize, voxel.ysize), vals=NA)

#to set up parameters for the initialize mkde3d:
nx <- ncol(r)
ny <- nrow(r)

# Calculate the number of z levels for the cube based in the maximum depth recorded max(z)
nz <- round((ceiling(max(z, na.rm=TRUE)) + voxel.zsize) / voxel.zsize, digits=0)

# Define the spatial extent and resolution of a 3D MKDE
# Crate a 3D raster us
# $dimension 3
mkde.obj <- mkde::initializeMKDE3D(xLL = xmin, xCellSize = voxel.xsize, nX = nx, yLL = ymin, yCellSize = voxel.ysize,
                                   nY = ny, zLL = zll, zCellSize = voxel.zsize, nZ = nz)


#---------------------------------------------------------
# Calculate raster of density values for MKDE
#---------------------------------------------------------

# Calculate raster density for 3D
# sometime R sension aborted in this step, re-run again
dens.res <- mkde::initializeDensity(mkde.obj, mv.dat, integration.step)
mkde.obj <- dens.res$mkde.obj  # updated MKDE object 
mv.dat <- dens.res$move.dat  # updated move data object


#---------------------------------------------------------
# Calculate volumes for selected contours
#---------------------------------------------------------

# Set contours
my.quantiles <- contours

# Calculate density thresholds for select contours
res <- mkde::computeContourValues(mkde.obj, my.quantiles)

# Calculate volumes of 3d home ranges
# volume units (m3)
res$volume <- mkde::computeSizeMKDE(mkde.obj, my.quantiles)
#---------------------------------------------------------

# Extract information from the updated move data object 
use.obs <- sum(mv.dat$use.obs == TRUE)  # Used observations for the mkde


#---------------------------------------------------------
# Create data frame to store results
#---------------------------------------------------------

kde_3d_res <- data.frame(ptt,
                  volume.50 = res$volume[res$prob == 0.50],
                  volume.75 = res$volume[res$prob == 0.75],
                  volume.95 = res$volume[res$prob == 0.95],
                  threshold.50 = res$threshold[res$prob == 0.50],
                  threshold.75 = res$threshold[res$prob == 0.75],
                  threshold.95 = res$threshold[res$prob == 0.95],
                  mean.xy.error = mean.xy.error,
                  mean.z.error = mean.z.error, 
                  avgdepth = avgdepth,
                  days.tracked = days.tracked,
                  use.obs.mkde = use.obs)




##------------------------------------------------------------------------------------------------------------------------------##
# To save objects for further plotting / analysis
# 
# To save:
# 1. mkde object: output of 3D HR calculation
# 2. final ttdr dataframe: data used to calculate 3d HR
# 3. res: dataframe containing the volumes and thresholds for plotting
#
##------------------------------------------------------------------------------------------------------------------------------##

# export mkde object and results rdaya and csv files 

output_data <- paste0(output_dir,"/","01_kde_3d")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# create output folder for ptt
kde_folder <- paste0(output_data,"/",ptt)
if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)

# export files as .rdata and csv format
mkdeobjfile <- paste0(kde_folder,"/",ptt,"_3dmkde_obj.rdata")
finalttdrfile <- paste0(kde_folder,"/",ptt,"_3d_ttdr.rdata")
resfile <- paste0(kde_folder,"/",ptt,"_3d_res.rdata")


save(mkde.obj, file = mkdeobjfile)
save(ttdr, file = finalttdrfile)
save(kde_3d_res, file = resfile)
write.csv(kde_3d_res, paste0(output_data,"/",ptt,"_3d_res",".csv"), row.names = TRUE)


# append to global results

kde_3d_res_all <- rbind(kde_3d_res_all, kde_3d_res)
##---------------------------------------------------



Sys.time() - t












### ---------------- SCRIPT 4 JESS

##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
## 4. Calculate 2D mkde
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
##start 2d mkde
##------------------------------------------------------------------------------------------------------------------------------##

# ptt <- ssm$ptt[1]


x = ttdr$x
y = ttdr$y
date = ttdr$time
xy_error = ttdr$xy.error ####renamed so not to overwrite the name of the function
t.max = 250
integration.step = 5
voxel.xsize = 10000
voxel.ysize = 10000
extend.raster = 10000
zll = 0
crs = "+init=epsg:3035"
contours = c(0.50, 0.75, 0.95)

# Location error variance in the xy dimension
sig2obs <- xy_error^2

# Convert time stamps to elapsed minutes from first time
time <- as.numeric(difftime(date, date[1], units="mins"))

# Set up movement data
mv.dat <- initializeMovementData(t.obs = time, x.obs = x, y.obs = y,
                                 sig2obs= sig2obs, t.max = t.max)

#---------------------------------------------------------
# Define the spatial extent and resolution of a 2D MKDE
#---------------------------------------------------------

# to give a buffer around the mkde2d object:
xmin <- min(x) - extend.raster
xmax <- max(x) + extend.raster
ymin <- min(y) - extend.raster
ymax <- max(y) + extend.raster

# to set up raster for the mkde2d object:
r <- raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax,
            crs=CRS(crs), resolution=c(voxel.xsize, voxel.ysize), vals=NA)

# to set up parameters for the initialize mkde2d:
nx <- ncol(r)
ny <- nrow(r)

# initialzie mkde2d
mkde.obj <-  initializeMKDE2D(xLL = xmin, xCellSize = voxel.xsize, nX = nx, yLL= ymin, yCellSize = voxel.ysize, nY = ny)

#---------------------------------------------------------
# Calculate raster of density values for MKDE
#---------------------------------------------------------

# Calculate raster density for 2D
dens.res <- mkde::initializeDensity(mkde.obj, mv.dat, integration.step)

#to update mkde and move data objects
mkde.obj <- dens.res$mkde.obj
mv.dat <- dens.res$move.dat

#---------------------------------------------------------
# Calculate areas for selected contours
#---------------------------------------------------------

# Set contours
my.quantiles <- contours

# Calculate density thresholds for select contours
res <- mkde::computeContourValues(mkde.obj, my.quantiles)

# Calculate areas of 2d home ranges
# for 2D densities stimate calculare areas (m2)
res$volume <- computeSizeMKDE(mkde.obj, my.quantiles)

# Save parameters
use.obs <- sum(mv.dat$use.obs == TRUE)  # Used observations for the mkde
mean.xy.error <- mean(ttdr$xy.error)

#---------------------------------------------------------
# Create data frame to store results
#---------------------------------------------------------

kde_2d_res <- data.frame(ptt = ptt,
                   area.50 = res$volume[res$prob == 0.50],
                   area.75 = res$volume[res$prob == 0.75],
                   area.95 = res$volume[res$prob == 0.95],
                   threshold.50 = res$threshold[res$prob == 0.50],
                   threshold.75 = res$threshold[res$prob == 0.75],
                   threshold.95 = res$threshold[res$prob == 0.95],
                   mean.xy.error = mean.xy.error,
                   days.tracked = days.tracked,
                   use.obs.mkde = use.obs)






##------------------------------------------------------------------------------------------------------------------------------##
# To save objects for further plotting / analysis
##------------------------------------------------------------------------------------------------------------------------------##

# export mkde object and results rdaya and csv files 

output_data <- paste0(output_dir,"/","02_kde_2d")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# create output folder for ptt
kde_folder <- paste0(output_data,"/",ptt)
if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)

# export files as .rdata and csv format
mkdeobjfile <- paste0(kde_folder,"/",ptt,"_2dmkde_obj.rdata")
resfile <- paste0(kde_folder,"/",ptt,"_2d_res.rdata")
finalttdrfile <- paste0(kde_folder,"/",ptt,"_2d_ttdr.rdata")

save(mkde.obj, file = mkdeobjfile)
save(ttdr, file = finalttdrfile)
save(kde_2d_res, file = resfile)
write.csv(kde_2d_res, paste0(output_data,"/",ptt,"_2d_res",".csv"), row.names = TRUE)


# -----------------------


kde_2d_res_all <- rbind(kde_2d_res_all, twod)









# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# script 5 Jess -------------------------------------------------------------------------------------------------









##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
## 5. Calculate DAY vs. NIGHT 3D home ranges
##------------------------------------------------------------------------------------------------------------------------------##

## Compute home range volumes for all turtles -- compute separate home ranges for day and night
# Method: mkde package
#time interval: ttdr, 5 minutes


# extract day and night records from ttdr 3d used in mkde 3D

# load ttdr lives


# select 
day <- ttdr[ttdr$daynight=="day",]
night <- ttdr[ttdr$daynight=="night",]









##------------------------------------------------------------------------------------------------------------------------------##
## DAY:
##------------------------------------------------------------------------------------------------------------------------------##

## Inputs to 3dmkde
## Dataframe is named "day"


ptt <- day$organismID[1]

x = day$x
y = day$y
z = as.numeric(day$depth_adjusted)
date = day$time
z_error = day$z.error ## changed to avoid overwriting function name
xy_error = day$xy.error ##changed to avoid overwriting function name
t.max = 250
integration.step = 5
voxel.xsize = 10000
voxel.ysize = 10000
voxel.zsize = 10
extend.raster = 10000
zll = 0
crs = "+init=epsg:3035"
contours = c(0.50, 0.75, 0.95)

# Load libraries
library(mkde)
library(raster)

#---------------------------------------
# Initialize a movement data list
#---------------------------------------

# Location error variance in the xy dimension
sig2obs <- xy_error^2
sig2obs.z <- z_error^2

# Convert time stamps to elapsed minutes from first time
time <- as.numeric(difftime(date, date[1], units="mins")) 

# Set up movement data
# warning message is generated when a vector is used for the z dimension error, but it still works
mv.dat <- initializeMovementData(t.obs = time, x.obs = x, y.obs = y, z.obs = z,
                                 sig2obs= sig2obs, sig2obs.z = sig2obs.z, t.max = t.max)

#---------------------------------------------------------
# Define the spatial extent and resolution of a 3D MKDE
#---------------------------------------------------------

#to give a buffer around the mkde3d object:
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

# Define the spatial extent and resolution of a 3D MKDE
mkde.obj <- initializeMKDE3D(xLL = xmin, xCellSize = voxel.xsize, nX = nx, yLL = ymin, yCellSize = voxel.ysize,
                             nY = ny, zLL = zll, zCellSize = voxel.zsize, nZ = nz)


#---------------------------------------------------------
# Calculate raster of density values for MKDE
#---------------------------------------------------------

# Calculate raster density for 3D
dens.res <- initializeDensity(mkde.obj, mv.dat, integration.step)
mkde.obj <- dens.res$mkde.obj  # updated MKDE object 
mv.dat <- dens.res$move.dat  # updated move data object

#---------------------------------------------------------
# Calculate volumes for selected contours
#---------------------------------------------------------

# Set contours
my.quantiles <- contours

# Calculate density thresholds for select contours
res <- computeContourValues(mkde.obj, my.quantiles)

# Calculate volumes of 3d home ranges
res$volume <- computeSizeMKDE(mkde.obj, my.quantiles)

# Add variables to include in parameter table:
#note: column names are different for z error in ttdr vs. ssm
mean.xy.error <- mean(day$xy.error)
mean.z.error <- mean(day$z.error)
avgdepth <- mean(day$depth_adjusted, na.rm = T)

# Extract information from the updated move data object 
use.obs <- sum(mv.dat$use.obs == TRUE)  # Used observations for the MKDE ### add this Feb 2 2021

#---------------------------------------------------------
# Create data frame to store results
#---------------------------------------------------------

newday3d <- data.frame(ptt,
                       day.night = "day",
                       volume.50 = res$volume[res$prob == 0.50],
                       volume.75 = res$volume[res$prob == 0.75],
                       volume.95 = res$volume[res$prob == 0.95],
                       threshold.50 = res$threshold[res$prob == 0.50],
                       threshold.75 = res$threshold[res$prob == 0.75],
                       threshold.95 = res$threshold[res$prob == 0.95],
                       mean.xy.error = mean.xy.error,
                       mean.z.error = mean.z.error, 
                       avgdepth = avgdepth,
                       use.obs.mkde = use.obs)

daynight3d <- rbind(daynight3d, newday3d)


##------------------------------------------------------------------------------------------------------------------------------##
## Save objects for further plotting / analysis
##------------------------------------------------------------------------------------------------------------------------------##

# export mkde object and results rdaya and csv files 

output_data <- paste0(output_dir,"/","01_kde_3d")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# create output folder for ptt
kde_folder <- paste0(output_data,"/",ptt)
if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)

# export files as .rdata and csv format
mkdeobjfile <- paste0(kde_folder,"/",ptt,"_3dmkde_obj_day.rdata")
resfile <- paste0(kde_folder,"/",ptt,"_3d_res_day.rdata")
finalttdrfile <- paste0(kde_folder,"/",ptt,"_3d_ttdr_day.rdata")


save(mkde.obj, file = mkdeobjfile)
save(day, file = finalttdrfile)
save(newday3d, file = resfile)
write.csv(newday3d, paste0(kde_folder,"/",ptt,"_3d_res_day",".csv"), row.names = TRUE)







load("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/34321/34321_3d_ttdr_night.rdata")


str(night)

night$depth_adjusted <- as.numeric(night$depth_adjusted)
##------------------------------------------------------------------------------------------------------------------------------##
##   ##NIGHT:
##------------------------------------------------------------------------------------------------------------------------------##

## Inputs to 3dmkde
## Dataframe is named "night"

x = night$x
y = night$y
z = as.numeric(night$depth_adjusted)
date = night$time
z_error = night$z.error ## changed to not overwrite function
xy_error = night$xy.error  ## changed to not overwrite function
t.max = 250
integration.step = 5
voxel.xsize = 10000
voxel.ysize = 10000
voxel.zsize = 10
extend.raster = 10000
zll = 0
crs = "+init=epsg:3035"
contours = c(0.50, 0.75, 0.95)

#---------------------------------------
# Initialize a movement data list
#---------------------------------------

# Location error variance in the xy dimension
sig2obs <- xy_error^2
sig2obs.z <- z_error^2

# Convert time stamps to elapsed minutes from first time
time <- as.numeric(difftime(date, date[1], units="mins")) 

# Set up movement data
# warning message is generated when a vector is used for the z dimension error, but it still works
mv.dat <- initializeMovementData(t.obs = time, x.obs = x, y.obs = y, z.obs = z,
                                 sig2obs= sig2obs, sig2obs.z = sig2obs.z, t.max = t.max)

library(mkde)
library(raster)
library(sp)



# problem with is.na condition in th function ----------------------

initializeMovementData <- function(t.obs, x.obs, y.obs, z.obs=NULL, 
                                   sig2obs=0.0, sig2obs.z=NA, t.max=max(diff(t.obs), na.rm=TRUE)) {
  # CHECK LENGTHS
  if (is.null(z.obs)) {
    dimension = 2
  } else {
    dimension = 3
  }
  
  n <- length(t.obs)
  a.obs <- rep(NA, n)
  
  # Verificación de sig2obs
  if (length(sig2obs) == 1) {
    sig2obs.vec <- rep(sig2obs, n)
  } else if (length(sig2obs) == n) {
    sig2obs.vec <- sig2obs
  } else {
    stop("The length of sig2obs is not correct.")
  }
  
  # Verificación de sig2obs.z
  if (all(is.na(sig2obs.z))) {  # Se usa all() para evitar el error
    if (length(sig2obs) == 1) {
      sig2obs.z.vec <- rep(sig2obs, n)
    } else if (length(sig2obs) == n) {
      sig2obs.z.vec <- sig2obs
    } else {
      stop("The length of sig2obs is not correct.")
    }
  } else {
    if (length(sig2obs.z) == 1) {
      sig2obs.z.vec <- rep(sig2obs.z, n)
    } else if (length(sig2obs.z) == n) {
      sig2obs.z.vec <- sig2obs.z
    } else {
      stop("The length of sig2obs.z is not correct.")
    }
  }
  
  # Tiempo máximo entre observaciones
  too.much.time <- c((diff(t.obs) > t.max), TRUE)
  
  # Construcción de la lista de datos de movimiento
  move.dat <- list(
    dimension = dimension, 
    t.obs = t.obs, 
    x.obs = x.obs,
    y.obs = y.obs, 
    z.obs = z.obs, 
    a.obs = a.obs,
    t.max = t.max,
    sig2xy = rep(NA, n-1), 
    sig2z = rep(NA, n-1),
    sig2obs = sig2obs.vec, 
    sig2obs.z = sig2obs.z.vec, 
    n.excl.time = too.much.time,       # step-based (pre-computed)
    n.excl.bound = rep(FALSE, n),      # location-based
    n.excl.nomove = rep(FALSE, n),     # step-based
    use.obs = (!too.much.time)         # overall indicator for each step
  )
  
  return(move.dat)
}








#---------------------------------------------------------
# Define the spatial extent and resolution of a 3D MKDE
#---------------------------------------------------------

#to give a buffer around the mkde3d object:
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

# Define the spatial extent and resolution of a 3D MKDE
mkde.obj <- initializeMKDE3D(xLL = xmin, xCellSize = voxel.xsize, nX = nx, yLL = ymin, yCellSize = voxel.ysize,
                             nY = ny, zLL = zll, zCellSize = voxel.zsize, nZ = nz)


#---------------------------------------------------------
# Calculate raster of density values for MKDE
#---------------------------------------------------------

# Calculate raster density for 3D
dens.res <- initializeDensity(mkde.obj, mv.dat, integration.step)
mkde.obj <- dens.res$mkde.obj  # updated MKDE object 
mv.dat <- dens.res$move.dat  # updated move data object

#---------------------------------------------------------
# Calculate volumes for selected contours
#---------------------------------------------------------

# Set contours
my.quantiles <- contours

# Calculate density thresholds for select contours
res <- computeContourValues(mkde.obj, my.quantiles)

# Calculate volumes of 3d home ranges
res$volume <- computeSizeMKDE(mkde.obj, my.quantiles)

# Add variables to include in parameter table:
# note: column names are different for z error in ttdr vs. ssm
mean.xy.error <- mean(night$xy.error)
mean.z.error <- mean(night$z.error)
avgdepth <- mean(night$depth_adjusted, na.rm = T)
# Extract information from the updated move data object 
use.obs <- sum(mv.dat$use.obs == TRUE)  # Used observations for the mkde

#---------------------------------------------------------
# Create data frame to store results
#---------------------------------------------------------

newnight3d <- data.frame(ptt,
                         day.night = "night",
                         volume.50 = res$volume[res$prob == 0.50],
                         volume.75 = res$volume[res$prob == 0.75],
                         volume.95 = res$volume[res$prob == 0.95],
                         threshold.50 = res$threshold[res$prob == 0.50],
                         threshold.75 = res$threshold[res$prob == 0.75],
                         threshold.95 = res$threshold[res$prob == 0.95],
                         mean.xy.error = mean.xy.error,
                         mean.z.error = mean.z.error, 
                         avgdepth = avgdepth,
                         use.obs.mkde = use.obs)

daynight3d <- rbind(daynight3d, newnight3d)

##------------------------------------------------------------------------------------------------------------------------------##
## Save objects for further plotting / analysis
##------------------------------------------------------------------------------------------------------------------------------##

# export mkde object and results rdaya and csv files 

output_data <- paste0(output_dir,"/","01_kde_3d")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# create output folder for ptt
kde_folder <- paste0(output_data,"/",ptt)
if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)

# export files as .rdata and csv format
mkdeobjfile <- paste0(kde_folder,"/",ptt,"_3dmkde_obj_night.rdata")
resfile <- paste0(kde_folder,"/",ptt,"_3d_res_night.rdata")
finalttdrfile <- paste0(kde_folder,"/",ptt,"_3d_ttdr_night.rdata")


save(mkde.obj, file = mkdeobjfile)
save(day, file = finalttdrfile)
save(newnight3d, file = resfile)
write.csv(newnight3d, paste0(kde_folder,"/",ptt,"_3d_res_night",".csv"), row.names = TRUE)


##------------------------------------------------------------------------------------------------------------------------------##








#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# script 6 --------------------




##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
##------------------------------------------------------------------------------------------------------------------------------##



##------------------------------------------------------------------------------------------------------------------------------##
## 6. Calculate DAY vs. NIGHT 2D home ranges
##------------------------------------------------------------------------------------------------------------------------------##

# select 
day <- ttdr[ttdr$daynight=="day",]
night <- ttdr[ttdr$daynight=="night",]



##------------------------------------------------------------------------------------------------------------------------------##
## DAY
##------------------------------------------------------------------------------------------------------------------------------##


x = day$x
y = day$y
z = day$depth_adjusted
date = day$time
xy_error = day$xy.error ####renamed so not to overwrite the name of the function
t.max = 250
integration.step = 5
voxel.xsize = 10000
voxel.ysize = 10000
voxel.zsize = 10
extend.raster = 10000
zll = 0
crs = "+init=epsg:3035"
contours = c(0.50, 0.75, 0.95)

# Location error variance in the xy dimension
sig2obs <- xy_error^2

# Convert time stamps to elapsed minutes from first time
time <- as.numeric(difftime(date, date[1], units="mins"))

# Set up movement data
mv.dat <- initializeMovementData(t.obs = time, x.obs = x, y.obs = y,
                                 sig2obs= sig2obs, t.max = t.max)

#---------------------------------------------------------
# Define the spatial extent and resolution of a 2D MKDE
#---------------------------------------------------------

#to give a buffer around the mkde2d object:
xmin <- min(x) - extend.raster
xmax <- max(x) + extend.raster
ymin <- min(y) - extend.raster
ymax <- max(y) + extend.raster

#to set up raster for the mkde2d object:
r <- raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax,
            crs=CRS(crs), resolution=c(voxel.xsize, voxel.ysize), vals=NA)

#to set up parameters for the initialize mkde2d:
nx <- ncol(r)
ny <- nrow(r)

##initialzie mkde2d
mkde.obj <-  initializeMKDE2D(xLL = xmin, xCellSize = voxel.xsize, nX = nx, yLL= ymin, yCellSize = voxel.ysize, nY = ny)

#---------------------------------------------------------
# Calculate raster of density values for MKDE
#---------------------------------------------------------

# Calculate raster density for 2D
dens.res <- initializeDensity(mkde.obj, mv.dat, integration.step)

#to update mkde and move data objects
mkde.obj <- dens.res$mkde.obj
mv.dat <- dens.res$move.dat

#---------------------------------------------------------
# Calculate areas for selected contours
#---------------------------------------------------------

# Set contours
my.quantiles <- contours

# Calculate density thresholds for select contours
res <- computeContourValues(mkde.obj, my.quantiles)

# Calculate areas of 2d home ranges
res$volume <- computeSizeMKDE(mkde.obj, my.quantiles)

#add variables to include in parameter table
use.obs <- sum(mv.dat$use.obs == TRUE)  # Used observations for the mkde
#note: column names are different for z error in ttdr vs. ssm
mean.xy.error <- mean(day$xy.error)

#---------------------------------------------------------
# Create data frame to store results
#---------------------------------------------------------

newtwodday <- data.frame(ptt, day.night = "day",
                         area.50 = res$volume[res$prob == 0.50],
                         area.75 = res$volume[res$prob == 0.75],
                         area.95 = res$volume[res$prob == 0.95],
                         threshold.50 = res$threshold[res$prob == 0.50],
                         threshold.75 = res$threshold[res$prob == 0.75],
                         threshold.95 = res$threshold[res$prob == 0.95],
                         mean.xy.error = mean.xy.error,
                         use.obs.mkde = use.obs)

daynight2d <- rbind(daynight2d, newtwodday)





##------------------------------------------------------------------------------------------------------------------------------##
## Save objects for further plotting / analysis
##------------------------------------------------------------------------------------------------------------------------------##

# export mkde object and results rdaya and csv files 

output_data <- paste0(output_dir,"/","02_kde_2d")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# create output folder for ptt
kde_folder <- paste0(output_data,"/",ptt)
if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)

# export files as .rdata and csv format
mkdeobjfile <- paste0(kde_folder,"/",ptt,"_2dmkde_obj_day.rdata")
resfile <- paste0(kde_folder,"/",ptt,"_2d_res_day.rdata")


save(mkde.obj, file = mkdeobjfile)
save(newnight3d, file = resfile)
write.csv(newnight3d, paste0(kde_folder,"/",ptt,"_2d_res_day",".csv"), row.names = TRUE)






####------------------------------------------------------------------------------------------------------------------------------##
## NIGHT
##------------------------------------------------------------------------------------------------------------------------------##

x = night$x
y = night$y
z = night$depth_adjusted
date = night$time
xy_error = night$xy.error ####renamed so not to overwrite the name of the function
t.max = 250
integration.step = 5
voxel.xsize = 10000
voxel.ysize = 10000
voxel.zsize = 10
extend.raster = 10000
zll = 0
crs = "+init=epsg:3035"
contours = c(0.50, 0.75, 0.95)

# Location error variance in the xy dimension
sig2obs <- xy_error^2

# Convert time stamps to elapsed minutes from first time
time <- as.numeric(difftime(date, date[1], units="mins"))

# Set up movement data
mv.dat <- initializeMovementData(t.obs = time, x.obs = x, y.obs = y,
                                 sig2obs= sig2obs, t.max = t.max)

#---------------------------------------------------------
# Define the spatial extent and resolution of a 2D MKDE
#---------------------------------------------------------

#to give a buffer around the mkde2d object:
xmin <- min(x) - extend.raster
xmax <- max(x) + extend.raster
ymin <- min(y) - extend.raster
ymax <- max(y) + extend.raster

#to set up raster for the mkde2d object:
r <- raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax,
            crs=CRS(crs), resolution=c(voxel.xsize, voxel.ysize), vals=NA)

#to set up parameters for the initialize mkde2d:
nx <- ncol(r)
ny <- nrow(r)

##initialzie mkde2d
mkde.obj <-  initializeMKDE2D(xLL = xmin, xCellSize = voxel.xsize, nX = nx, yLL= ymin, yCellSize = voxel.ysize, nY = ny)

#---------------------------------------------------------
# Calculate raster of density values for MKDE
#---------------------------------------------------------

# Calculate raster density for 2D
dens.res <- initializeDensity(mkde.obj, mv.dat, integration.step)

#to update mkde and move data objects
mkde.obj <- dens.res$mkde.obj
mv.dat <- dens.res$move.dat

#---------------------------------------------------------
# Calculate areas for selected contours
#---------------------------------------------------------

# Set contours
my.quantiles <- contours

# Calculate density thresholds for select contours
res <- computeContourValues(mkde.obj, my.quantiles)

# Calculate areas of 2d home ranges
res$volume <- computeSizeMKDE(mkde.obj, my.quantiles)

#add variables to include in parameter table:
use.obs <- sum(mv.dat$use.obs == TRUE)  # Used observations for the mkde
#note: column names are different for z error in ttdr vs. ssm
mean.xy.error <- mean(night$xy.error)


#---------------------------------------------------------
# Create data frame to store results
#---------------------------------------------------------

newtwodnight <- data.frame(ptt, day.night = "night",
                           area.50 = res$volume[res$prob == 0.50],
                           area.75 = res$volume[res$prob == 0.75],
                           area.95 = res$volume[res$prob == 0.95],
                           threshold.50 = res$threshold[res$prob == 0.50],
                           threshold.75 = res$threshold[res$prob == 0.75],
                           threshold.95 = res$threshold[res$prob == 0.95],
                           mean.xy.error = mean.xy.error,
                           use.obs.mkde = use.obs)

daynight2d <- rbind(daynight2d, newtwodnight)


##------------------------------------------------------------------------------------------------------------------------------##
## Save objects for further plotting / analysis
##------------------------------------------------------------------------------------------------------------------------------##

# export mkde object and results rdaya and csv files 

output_data <- paste0(output_dir,"/","02_kde_2d")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# create output folder for ptt
kde_folder <- paste0(output_data,"/",ptt)
if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)

# export files as .rdata and csv format
mkdeobjfile <- paste0(kde_folder,"/",ptt,"_2dmkde_obj_night.rdata")
resfile <- paste0(kde_folder,"/",ptt,"_2d_res_night.rdata")


save(mkde.obj, file = mkdeobjfile)
save(newnight3d, file = resfile)
write.csv(newnight3d, paste0(kde_folder,"/",ptt,"_2d_res_night",".csv"), row.names = TRUE)








