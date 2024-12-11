

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








##------------------------------------------------------------------------------------------------------------------------------##
##   ##NIGHT:
##------------------------------------------------------------------------------------------------------------------------------##

## Inputs to 3dmkde
## Dataframe is named "night"

x = night$x
y = night$y
z = night$depth_adjusted
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



