
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



ptt <- ssm$organismID[1]


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

new <- data.frame(ptt = ptt,
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

# combine resuls

kde_2d_res_all <- rbind(kde_2d_res_all, twod)