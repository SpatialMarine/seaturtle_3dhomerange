


#------------------------------------------------------------------------------
## 3D Habitat Use of Loggerhead Turtles

## Created by Jessica Ruff and David March (2021)

# Update package and standarized field names following Sequeria et al., 2021
# by Javier Menéndez-Blázquez | @jmenblaz


## 5. Calculate DAY vs. NIGHT 2D home 

## Compute home range volumes for all turtles -- compute separate home ranges for day and night
# Method: mkde package
#time interval: ttdr, 5 minutes

# extract day and night records from ttdr 3d used in mkde 2D


# path
source("setup.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # custom functions for 3d process

# Load libraries
library(mkde)
library(raster)



# load ttdr lives

# 1) list ttdr L3 files
ttdr_files <- list.files(paste0(main_dir,"/input/tracking/ttdr/L3"), full.names = TRUE, pattern = "L3_ttdr.csv")



# ------------------------------------------------------------------------------
# 2) process 3D Kernel Density Stimation (3d kde) using mkde package from animal movement
# mkde3d         Calculate 3D volumnes using mkde package

#' commons inputs @params for mkde::functions()

t.max = 250
integration.step = 5
voxel.xsize = 10000
voxel.ysize = 10000
voxel.zsize = 10
extend.raster = 10000
zll = 0
crs = "+init=epsg:3035"
contours = c(0.50, 0.75, 0.95)


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



# t <- Sys.time()
# 
# cores <- detectCores() - 2
# cl <- makeCluster(cores)
# registerDoParallel(cl)
# 
# getDoParWorkers() # backend information

t <- Sys.time()

for (i in 1:length(ttdr_files)) {
  
  ttdr <- ttdr_files[i]
  # extract organismID from L3_ttdr fiel name
  organismID <- sub("_L3_ttdr\\.csv$", "", basename(ttdr))
  
  # info 
  cat("Processing individual:", i,"/",length(ttdr_files))
  cat(" · organismID:", organismID, "\n")
  
  # import locs and ttdr data for this organismID or ptt ----------------------
  ttdr <- paste0(main_dir,"/input/tracking/ttdr/L3/",organismID,"_L3_ttdr.csv")
  ttdr <- read.csv(ttdr, dec=",", head=TRUE)
  # parse / format time date for ttdr data  and convert numeric fields:
  ttdr$time <- lubridate::parse_date_time(ttdr$time, "Ymd HMS")
  ttdr <- ttdr |> mutate(across(c(latitude, longitude, x, y,
                                  depth_upper_error, depth_lower_error, 
                                  depth, depth_adjusted, 
                                  drange, 
                                  xy.error, z.error), as.numeric))
  
  # select subset of day and night by organismID
  day <- ttdr[ttdr$daynight=="day",]
  night <- ttdr[ttdr$daynight=="night",]
  

  
  # ----------------------------------------------------------------------------
  # DAY
  # ----------------------------------------------------------------------------
  
  x = day$x
  y = day$y
  z = day$depth_adjusted
  date = day$time
  xy_error = day$xy.error ####renamed so not to overwrite the name of the function

  
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

  
# Create data frame to store results

  kde_2d_res <- data.frame(organismID = organismID,
                           day.night = "day",
                           area.50 = res$volume[res$prob == 0.50],
                           area.75 = res$volume[res$prob == 0.75],
                           area.95 = res$volume[res$prob == 0.95],
                           threshold.50 = res$threshold[res$prob == 0.50],
                           threshold.75 = res$threshold[res$prob == 0.75],
                           threshold.95 = res$threshold[res$prob == 0.95],
                           mean.xy.error = mean.xy.error,
                           days.tracked = days.tracked,
                           use.obs.mkde = use.obs)


  
  #  export mkde object and results rdaya and csv files -----------------------
  output_data <- paste0(output_dir,"/","02_kde_2d")
  if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)
  
  # create output folder for ptt
  kde_folder <- paste0(output_data,"/",organismID)
  if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)
  
  # export files as .rdata and csv format
  mkdeobjfile <- paste0(kde_folder,"/",organismID,"_2dmkde_obj_day.rdata")
  resfile <- paste0(kde_folder,"/",organismID,"_2d_res_day.rdata")
  finalttdrfile <- paste0(kde_folder,"/",organismID,"_2d_ttdr_day.rdata")
  
  save(mkde.obj, file = mkdeobjfile)
  save(ttdr, file = finalttdrfile)
  save(kde_2d_res, file = resfile)
  write.csv(kde_2d_res, paste0(output_data,"/",organismID,"_2d_res_day",".csv"), row.names = TRUE)



  # ----------------------------------------------------------------------------
  # NIGHT
  # ----------------------------------------------------------------------------
  
  x = night$x
  y = night$y
  z = night$depth_adjusted
  date = night$time
  xy_error = night$xy.error ####renamed so not to overwrite the name of the function
  
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
  kde_2d_res <- data.frame(organismID = organismID,
                           day.night = "night",
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
  ## Save objects for further plotting / analysis
  ##------------------------------------------------------------------------------------------------------------------------------##
  
  # export mkde object and results rdaya and csv files 
  
  output_data <- paste0(output_dir,"/","02_kde_2d")
  if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)
  
  # create output folder for ptt
  kde_folder <- paste0(output_data,"/",organismID)
  if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)
  
  # export files as .rdata and csv format
  mkdeobjfile <- paste0(kde_folder,"/",organismID,"_2dmkde_obj_night.rdata")
  resfile <- paste0(kde_folder,"/",organismID,"_2d_res_night.rdata")
  finalttdrfile <- paste0(kde_folder,"/",organismID,"_2d_ttdr_night.rdata")
  
  save(mkde.obj, file = mkdeobjfile)
  save(ttdr, file = finalttdrfile)
  save(kde_2d_res, file = resfile)
  write.csv(kde_2d_res, paste0(output_data,"/",organismID,"_2d_res_night",".csv"), row.names = TRUE)
  

}

Sys.time() - t # 1 min



# -----------------------------------------------------------------------------
# 3) Combine results and export              ---------------------------------

# list results for all individuals
files <- list.files(output_data, pattern = "_2d_res_day.csv", recursive = TRUE, full.names = TRUE)
# combine csv into single one
df <- files %>% 
  purrr::map_df(read.csv)
# save / export combined result for 3D kernel density estimation
write.csv(df, paste0(output_data,"/kde_2d_res_day.csv"), row.names = TRUE)


# list results for all individuals
files <- list.files(output_data, pattern = "_2d_res_night.csv", recursive = TRUE, full.names = TRUE)
# combine csv into single one
df <- files %>% 
  purrr::map_df(read.csv)
# save / export combined result for 2d kernel density estimation
write.csv(df, paste0(output_data,"/kde_2d_res_night.csv"), row.names = TRUE)






# -----------------------------------------------------------------------------
# 4) export VTK and ASCII 2d files from 2d mkde.obt         ----------------

# list results for all individuals for day
files <- list.files(output_data, pattern = "_2dmkde_obj_day.rdata", recursive = TRUE, full.names = TRUE)

for (f in files) {
  # load 2D mdke.obj
  load(f)
  # extract id from file
  organismID <- sub("_2dmkde_obj_day\\.rdata$", "", basename(f))
  
  # export to raster using mkde.raster function from last version of mkde R pakcage
  rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_2dmkde_obj_raster_day.tif")
  mkde.rst <- mkde::mkdeToTerra(mkde.obj)
  # plot(mkde.rst)
  writeRaster(mkde.rst, rst_file, overwrite = TRUE)
  
  # # output ascii file
  # ascii_file <- paste0(output_data,"/",organismID,"/",organismID,"_2dmkde_obj_ascii.txt")
  # writeToGRASS(mkde.obj, ascii_file)
  #   
  # #output VTK file
  # vtk_file <- paste0(output_data,"/",organismID,"/",organismID,"_2dmkde_obj.vtk")
  # writeToVTK(mkde.obj, vtk_file,
  #            description=paste0(organismID," 2D MKDE"))
}



# list results for all individuals for night
files <- list.files(output_data, pattern = "_2dmkde_obj_night.rdata", recursive = TRUE, full.names = TRUE)

for (f in files) {
  # load 2D mdke.obj
  load(f)
  # extract id from file
  organismID <- sub("_2dmkde_obj_night\\.rdata$", "", basename(f))
  
  # export to raster using mkde.raster function from last version of mkde R pakcage
  rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_2dmkde_obj_raster_night.tif")
  mkde.rst <- mkde::mkdeToTerra(mkde.obj)
  # plot(mkde.rst)
  writeRaster(mkde.rst, rst_file, overwrite = TRUE)
  
  # # output ascii file
  # ascii_file <- paste0(output_data,"/",organismID,"/",organismID,"_2dmkde_obj_ascii.txt")
  # writeToGRASS(mkde.obj, ascii_file)
  #   
  # #output VTK file
  # vtk_file <- paste0(output_data,"/",organismID,"/",organismID,"_2dmkde_obj.vtk")
  # writeToVTK(mkde.obj, vtk_file,
  #            description=paste0(organismID," 2D MKDE"))
}










