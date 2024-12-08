

x = ssm$x
y = ssm$y
z = ssm$depth_mean
date = ssm$date
z.error = ssm$z.error_mean
xy.error = ssm$xy.error
t.max = 390
integration.step = 10
voxel.xsize = 10000
voxel.ysize = 10000
voxel.zsize = 10
extend.raster = 10000
zll = 0
crs = "+init=epsg:3035"
contours = c(0.50, 0.95)
rasterfile = rasterfile



zll = 0
voxel.zsize = 10
voxel.xsize = 10000
voxel.ysize = 10000
extend.raster = 100000
contours = c(50, 95)
multiplier = 3




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
  




