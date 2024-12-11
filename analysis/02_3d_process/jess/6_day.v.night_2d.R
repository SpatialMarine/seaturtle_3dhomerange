
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