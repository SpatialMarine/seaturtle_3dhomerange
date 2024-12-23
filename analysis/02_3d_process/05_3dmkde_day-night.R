
#------------------------------------------------------------------------------
## 3D Habitat Use of Loggerhead Turtles

## Created by Jessica Ruff and David March (2021)

# Update package and standarized field names following Sequeria et al., 2021
# by Javier Menéndez-Blázquez | @jmenblaz


## 5. Calculate DAY vs. NIGHT 3D home 

## Compute home range volumes for all turtles -- compute separate home ranges for day and night
# Method: mkde package
#time interval: ttdr, 5 minutes

# extract day and night records from ttdr 3d used in mkde 3D



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
  
  
  # ------------------------------------------------------------------------------
  ## Processing DAY data   --------------------------------------------------------
  message("Processing DAY ttdr data")

  x = day$x
  y = day$y
  z = as.numeric(day$depth_adjusted)
  date = day$time
  z_error = day$z.error ## changed to avoid overwriting function name
  xy_error = day$xy.error ##changed to avoid overwriting function name


  ##  variables to include in results parameter table --------------------------
  # Note: column names are different for z error in ttdr vs. ssm
  # general information about the diving data
  day.mean.xy.error <- mean(day$xy.error)
  mean.z.error <- mean(day$z.error)
  day$depth_adjusted <- as.numeric(day$depth_adjusted) # convert to numeric
  avgdepth <- mean(day$depth_adjusted, na.rm = T) 
  day$yday <- yday(day$time) # number of day for a year
  days.tracked <- length(unique(day$yday)) 


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
  
  kde_3d_res <- data.frame(organismID = organismID,
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
                         days.tracked = days.tracked,
                         use.obs.mkde = use.obs)
  
  ##------------------------------------------------------------------------------------------------------------------------------##
  ## Save objects for further plotting / analysis
  ##------------------------------------------------------------------------------------------------------------------------------##
  
  # export mkde object and results rdaya and csv files 
  
  # export mkde object and results rdaya and csv files 
  
  output_data <- paste0(output_dir,"/","01_kde_3d")
  if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)
  
  # create output folder for ptt or organismID
  kde_folder <- paste0(output_data,"/",organismID)
  if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)
  
  # export files as .rdata and csv format
  mkdeobjfile <- paste0(kde_folder,"/",organismID,"_3dmkde_obj_day.rdata")
  finalttdrfile <- paste0(kde_folder,"/",organismID,"_3d_ttdr_day.rdata")
  resfile <- paste0(kde_folder,"/",organismID,"_3d_res_day.rdata")
  
  
  save(mkde.obj, file = mkdeobjfile)
  save(day, file = finalttdrfile)
  save(kde_3d_res, file = resfile)
  write.csv(kde_3d_res, paste0(kde_folder,"/",organismID,"_3d_res_day",".csv"), row.names = TRUE)



  
  
  # ---------------------------------------------------------------------------------
  ## Processnig NIGHT data   --------------------------------------------------------
  message("Processing NIGHT ttdr data")
  


  night$depth_adjusted <- as.numeric(night$depth_adjusted)

  x = night$x
  y = night$y
  z = as.numeric(night$depth_adjusted)
  date = night$time
  z_error = night$z.error ## changed to not overwrite function
  xy_error = night$xy.error  ## changed to not overwrite function

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
  
  kde_3d_res <- data.frame(organismID = organismID,
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
                           days.tracked = days.tracked,
                           use.obs.mkde = use.obs)
  
  ##------------------------------------------------------------------------------------------------------------------------------##
  ## Save objects for further plotting / analysis
  ##------------------------------------------------------------------------------------------------------------------------------##
  
  # export mkde object and results rdaya and csv files 
  
  output_data <- paste0(output_dir,"/","01_kde_3d")
  if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)
  
  # create output folder for ptt or organismID
  kde_folder <- paste0(output_data,"/",organismID)
  if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)
  
  # export files as .rdata and csv format
  mkdeobjfile <- paste0(kde_folder,"/",organismID,"_3dmkde_obj_night.rdata")
  finalttdrfile <- paste0(kde_folder,"/",organismID,"_3d_ttdr_night.rdata")
  resfile <- paste0(kde_folder,"/",organismID,"_3d_res_night.rdata")
  
  
  save(mkde.obj, file = mkdeobjfile)
  save(day, file = finalttdrfile)
  save(kde_3d_res, file = resfile)
  write.csv(kde_3d_res, paste0(kde_folder,"/",organismID,"_3d_res_night",".csv"), row.names = TRUE)
 
 }

Sys.time() - t # 8 mins

  
  

  
  
  
  
  
  
  
  
  
  
  # -----------------------------------------------------------------------------
  # 3) Combine results and export              ---------------------------------
  
  # list results for all individuals
  files <- list.files(output_data, pattern = "_3d_res_day.csv", recursive = TRUE, full.names = TRUE)
  # combine csv into single one
  df <- files %>% 
    purrr::map_df(read.csv)
  # save / export combined result for 3D kernel density estimation
  write.csv(df, paste0(output_data,"/kde_3d_res_day.csv"), row.names = TRUE)
  
  
  # list results for all individuals
  files <- list.files(output_data, pattern = "_3d_res_night.csv", recursive = TRUE, full.names = TRUE)
  # combine csv into single one
  df <- files %>% 
    purrr::map_df(read.csv)
  # save / export combined result for 3D kernel density estimation
  write.csv(df, paste0(output_data,"/kde_3d_res_night.csv"), row.names = TRUE)
  
  
  
  
  # -----------------------------------------------------------------------------
  # 4) export VTK and ASCII 3D files from 3D mkde.obt         ----------------
  
  # list results for all individuals for day
  files <- list.files(output_data, pattern = "_3dmkde_obj_day.rdata", recursive = TRUE, full.names = TRUE)
  
  for (f in files) {
    # load 3D mdke.obj
    load(f)
    # extract id from file
    organismID <- sub("_3dmkde_obj_day\\.rdata$", "", basename(f))
    
    # output ascii file
    ascii_file <- paste0(output_data,"/",organismID,"/",organismID,"_3dmkde_obj_day_ascii.txt")
    writeToGRASS(mkde.obj, ascii_file)
    
    #output VTK file
    vtk_file <- paste0(output_data,"/",organismID,"/",organismID,"_3dmkde_obj_day.vtk")
    writeToVTK(mkde.obj, vtk_file,
               description=paste0(organismID," 3D MKDE day"))
  }
  
  
  
  # list results for all individuals for night
  files <- list.files(output_data, pattern = "_3dmkde_obj_night.rdata", recursive = TRUE, full.names = TRUE)
  
  for (f in files) {
    # load 3D mdke.obj
    load(f)
    # extract id from file
    organismID <- sub("_3dmkde_obj_night\\.rdata$", "", basename(f))
    
    # output ascii file
    ascii_file <- paste0(output_data,"/",organismID,"/",organismID,"_3dmkde_obj_night_ascii.txt")
    writeToGRASS(mkde.obj, ascii_file)
    
    #output VTK file
    vtk_file <- paste0(output_data,"/",organismID,"/",organismID,"_3dmkde_obj_night.vtk")
    writeToVTK(mkde.obj, vtk_file,
               description=paste0(organismID," 3D MKDE night"))
  }
  
  



