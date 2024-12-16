
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
write.csv(kde_3d_res, paste0(kde_folder,"/",ptt,"_3d_res",".csv"), row.names = TRUE)


# append to global results

kde_3d_res_all <- rbind(kde_3d_res_all, kde_3d_res)
##---------------------------------------------------



Sys.time() - t

