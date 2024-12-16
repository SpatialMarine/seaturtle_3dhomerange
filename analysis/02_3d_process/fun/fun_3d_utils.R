# utils.R
# Suite of custom functions:
#
# ks3d         Calculate 3D volumnes using ks package
# mkde3d         Calculate 3D volumnes using mkde package
# reproject       Reproject coordinates to metric system
# resampTTDR         Resample TTDR data at larger time intervals (6H)
# xy.error         Calculate the horizontal error from SSM data
# z.error         Calculate the vertical error from TTDR data

# # initializeMovementData modify from "mkde" R package. Note: this function it
#                          wasn't works properlly (2024). There was and error in 
#                          the logic evaluation of NA. Modified to correct this.
#                          After the modification, the functions works well and
#                          provides same results than those obtained by Jess Ruff (2021)






#--------------------------------------------------------------------------------
# ks3d         Calculate 3D volumes using ks package
#--------------------------------------------------------------------------------
ks3d <- function(x, y, z, multiplier=NULL, zll, voxel.zsize, voxel.xsize, voxel.ysize, extend.raster, crs){
  
  # Load library
  library(ks)
  
  #convert from data frame to matrix
  df <- data.frame(x, y, z)
  
  ## call the plug-in bandwidth estimator 
  ##They decided to multiply this by 3, by a long process in Simpfendorfer
  #with multiplier
  if (is.null(multiplier))  H.pi <- Hpi(df, binned=TRUE)
  if (!is.null(multiplier))  H.pi <- Hpi(df, binned=TRUE) * multiplier
  
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
  zmax <- nz * voxel.zsize
  #------------------------------------
  
  
  ## calculate the kernel densities
  density <- kde(x = df, H = H.pi, binned = FALSE, xmin = c(xmin, ymin, zll), xmax = c(xmax, ymax, zmax), gridsize = c(50))
  # density <- kde(x = df, H = H.pi, binned = FALSE, xmin = c(xmin, ymin, zll), xmax = c(xmax, ymax, zmax), gridsize = c(nx, ny, nz))
  # density <- kde(df, H = H.pi)
  
  ## Calculate 50% volume  
  contour50 <- contourLevels(density, cont=50, approx=FALSE)
  vol50 <- contourSizes(density, cont=50)
  
  # calculate the 95% kernel volume (vol95)
  contour95 <- contourLevels(density, cont=95, approx=FALSE)
  vol95 <- contourSizes(density, cont=95)
  
  # Combine volume data
  voldata <- data.frame(prob=c(0.50, 0.95), threshold = c(contour50, contour95), volume = c(vol50, vol95))
  
  #---------------------------------------------------------
  # Store parameters information
  #---------------------------------------------------------
  
  # Create a list with all output information
  #params <- data.frame(multiplier)
  out <- list(volumnes = voldata)
  return(out)
  
  ## Convert to raster (not working yet)
  # b <- brick(density$estimate)
  
  ## 3d plot
  # plot(density,cont=c(50,95),colors=c("purple","green"),drawpoints=TRUE,
  #      xlab="easting (m)", ylab="northing (m)", zlab="depth (m)",size=2, ptcol="black")
  
}
#--------------------------------------------------------------------------------










#--------------------------------------------------------------------------------
# mkde3d         Calculate 3D volumnes using mkde package
#--------------------------------------------------------------------------------
mkde3d <- function(date, x, y, z, z.error, xy.error, t.max, integration.step, voxel.xsize, voxel.ysize, voxel.zsize, zlevels, zll, extend.raster, crs, contours, rasterfile){
  # Arguments
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
  # rasterfile
  #
  # Value
  # Reuturns a list with parameters and volumes, and save the 3d object into a multiband raster
  
  # Load libraries
  library(mkde)
  library(raster)
  
  #---------------------------------------
  # Initialize a movement data list
  #---------------------------------------
  
  # Location error variance in the xy dimension
  sig2obs <- xy.error^2
  sig2obs.z <- z.error^2
  
  # Convert time stamps to elapsed minutes from first time
  time <- as.numeric(difftime(date, date[1], units="mins")) 
  
  # Set up movement data
  # warning message is generated when a vector is used for the z dimension error, but still works...
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
  
  # Convert mkde object to raster
  mkde.rst <- mkdeToRaster(mkde.obj)
  writeRaster(mkde.rst, filename=rasterfile, bandorder='BIL', overwrite=TRUE)
  
  
  #---------------------------------------------------------
  # Calculate volumes for selected contours
  #---------------------------------------------------------
  
  # Set contours
  my.quantiles <- contours
  
  # Calculate density thresholds for select contours
  res <- computeContourValues(mkde.obj, my.quantiles)
  
  # Calculate volumes of 3d home ranges
  res$volume <- computeSizeMKDE(mkde.obj, my.quantiles)
  
  
  
  #---------------------------------------------------------
  # Store parameters information
  #---------------------------------------------------------
  
  # Extract information from the updated move data object
  use.obs <- sum(mv.dat$use.obs == TRUE)  # Used observations for the MKDE
  nouse.obs <- sum(!mv.dat$use.obs == TRUE) # Discarded observations due to t.max
  sig2xy <- unique(mv.dat$sig2xy)  # Movement variance in xy
  sig2z <- unique(mv.dat$sig2z)  # Movement variance in z
  
  # Create a list with all output information
  params <- data.frame(t.max, integration.step, use.obs, nouse.obs, sig2xy, sig2z, rasterfile)
  out <- list(parameters = params, volumnes = res)
  return(out)
}
#--------------------------------------------------------------------------------







#--------------------------------------------------------------------------------
# reproject         RReproject coordinates to metric system
#--------------------------------------------------------------------------------
reproject <- function(lon, lat, crs = "+init=epsg:3035"){
  # Arguments
  # lon         vector of longitudes
  # lat         vector of latitudes
  # crs       coordinate reference system
  
  df <- data.frame(lon, lat)
  coordinates(df)= ~ lon + lat   # convert to class spatial
  proj4string(df) <- CRS("+init=epsg:4326")    # define coordinate system
  df.proj <- spTransform(df, CRS=CRS(crs))  # transform to other CRS
  xy <- data.frame(df.proj)  # convert to data.frame
  names(xy) <- c("x", "y")  # rename variables
  
  return(xy)
}
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# resampTTDR         Resample TTDR data at larger time intervals (6H)
#--------------------------------------------------------------------------------
resampTTDR <- function(ttdr, ssm, timelap = 60*60*3){
  # Arguments:
  # ttdr        Data frame with TTDR data with required columns: date, depth, z.error
  # ssm         Data frame of SSM data with required columns: date
  # timelap     Time lap to average depth registers (in seconds). Default is 3hours
  #
  # Value:
  # Data frame with mean and median values of depth and z.error
  
  # Create empty data.frame
  df <- data.frame(time = ssm$time, depth_mean = NA, z.error_mean = NA, depth_median = NA, z.error_median = NA)
  
  # Loop to calculate mean and median values for defined time lap for each location stimated location
  for (i in 1:nrow(df)){
    
    # print(i)
    
    # get ssm location info
    ssm_time <- df$time[i]
    
    # filter time series data by time
    ts <- filter(ttdr, time > ssm_time - timelap & time < ssm_time + timelap)
    
    if(nrow(ts) > 0 & any(!is.na(ts$depth))){
      
      # Calculate mean values
      df$depth_mean[i] <- mean(ts$depth, na.rm=TRUE)
      df$z.error_mean[i] <- mean(ts$z.error, na.rm=TRUE)
      
      # Calculate median values
      df$depth_median[i] <- median(ts$depth, na.rm=TRUE)
      df$z.error_median[i] <- median(ts$z.error, na.rm=TRUE)
    }
  } 
  
  return(df)
}
#--------------------------------------------------------------------------------








#--------------------------------------------------------------------------------
# xy.error         Calculate the horizontal error from SSM data
#--------------------------------------------------------------------------------
xy.error <- function(ssm){
  # Arguments
  # ssmDF       Data.frame from a SSM with columns: lon, lat, lon.025, lon.975, lat.025, lat.975
  #
  # Description
  # Return the xy error in meters
  
  ## Load libraries
  library(dplyr)
  library(geosphere)
  
  ## selecting the sets of points to substract:
  west <- dplyr::select(ssm, lon.025, latitude)
  east <- dplyr::select(ssm, lon.975, latitude)
  north <- dplyr::select(ssm, longitude, lat.975)
  south <- dplyr::select(ssm, longitude, lat.025)
  
  ## Calculate distance between W-E and S-N points
  # distence in meters (m)
  londist <- distGeo(west, east)/2
  latdist <- distGeo(south, north)/2
  er <-  rowMeans(cbind(londist, latdist), na.rm=T)
  return(er)
}
#--------------------------------------------------------------------------------







#--------------------------------------------------------------------------------
# z.error         Calculate the vertical error from TTDR data
#--------------------------------------------------------------------------------
z.error <- function(depth.upper, depth.lower){
  # Arguments:
  # depth.upper       Upper estimate of the depth (deepest estimate)
  # depth.lower       Lower estimate of the depth (shallowest estimate)
  # 
  # Description:
  # Calculate the average of the depth errors
  
  er <- abs(depth.upper - depth.lower)/2
  return(er)
}
#--------------------------------------------------------------------------------








#--------------------------------------------------------------------------------
# initializeMovementData         From mkde R package. Create a mov.data object 
                                 # for further analisys
#--------------------------------------------------------------------------------

# original error in (is.na(sig2obs.z))


# Sets up the list with movement data
initializeMovementData <- function(t.obs, x.obs, y.obs, z.obs=NULL, 
                                   sig2obs=0.0, sig2obs.z=NA, t.max=max(diff(t.obs), na.rm=TRUE)) {
  # CHECK LENGTHS
  if (is.null(z.obs)) {
    dimension=2
  } else {
    dimension=3
  }
  n <- length(t.obs)
  a.obs <- rep(NA, n)
  if (length(sig2obs) == 1) {
    sig2obs.vec <- rep(sig2obs, n)
  } else if (length(sig2obs) == n) {
    sig2obs.vec <- sig2obs
  } else {
    stop("The length of sig2obs is not correct.")
  }
  if (all(is.na(sig2obs.z))) {
    if (length(sig2obs) == 1) {
      sig2obs.z.vec <- rep(sig2obs, n)
    } else if (length(sig2obs) == n) {
      sig2obs.z.vec <- sig2obs
    } else {
      stop("The length of sig2obs is not correct.")
    }
  } else {
    if (length(sig2obs) == 1) {
      sig2obs.z.vec <- rep(sig2obs.z, n)
    } else if (length(sig2obs) == n) {
      sig2obs.z.vec <- sig2obs.z
    } else {
      stop("The length of sig2obs is not correct.")
    }
  }
  too.much.time <- c((diff(t.obs) > t.max), TRUE)
  move.dat <- list(dimension=dimension, 
                   t.obs=t.obs, 
                   x.obs=x.obs,
                   y.obs=y.obs, 
                   z.obs=z.obs, 
                   a.obs=a.obs,
                   t.max=t.max,
                   sig2xy=rep(NA, n-1), 
                   sig2z=rep(NA, n-1),
                   sig2obs=sig2obs.vec, 
                   sig2obs.z=sig2obs.z.vec, 
                   n.excl.time=too.much.time,       # step-based (pre-computed)
                   n.excl.bound=rep(FALSE, n),      # location-based
                   n.excl.nomove=rep(FALSE, n),     # step-based
                   use.obs=(!too.much.time)         # overall indicator for each step
  )
  return(move.dat)
}










