
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


# path
source("setup.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # custom functions for 3d process
source("analysis/02_3d_process/fun/fun_fishtrack3d.R") # functions for fishtrack3D R package
source("analysis/02_3d_process/fun/fun_ks3d.R") 

# Load libraries
library(mkde)
library(raster)



# load ttdr lives

# 1) list ttdr L3 files
ttdr_files <- list.files(paste0(main_dir,"/input/tracking/ttdr/L3"), full.names = TRUE, pattern = "L3_ttdr.csv")


# raster/voxel area size (meters) 10000 = 10x10 km
# use 5x5 pixel to more accuracy results in fishing overlap
size = 5000

# ------------------------------------------------------------------------------
# 2) process 3D Kernel Density Stimation (3d kde) using mkde package from animal movement
# mkde3d         Calculate 3D volumnes using mkde package

#' commons inputs @params for mkde::functions()
t.max = 250
integration.step = 5
voxel.xsize = size
voxel.ysize = size
voxel.zsize = 10
extend.raster = size
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
  
  # Note: filtering locations for organismIDs 151934 and 200045
  # within the study area in western mediterranean
  if (organismID == "151934" | organismID == "200045") {
    # load study area
    area <- st_read(paste0(input_dir,"/gis/study_area.geojson"))
    # bounding box
    bbox <- st_bbox(area)
    # filter ttdr locations
    ttdr <- ttdr %>% filter(latitude >= bbox["ymin"], latitude <= bbox["ymax"],
                            longitude >= bbox["xmin"], longitude <= bbox["xmax"])
    # info
    cat("   · Filtered position by study area (OrganismID:",organismID,") \n")
  }
  
  
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
  write.csv(kde_3d_res, paste0(kde_folder,"/",organismID,"_3d_res_day",".csv"), row.names = FALSE)
  
  
  
  
  
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
  write.csv(kde_3d_res, paste0(kde_folder,"/",organismID,"_3d_res_night",".csv"), row.names = FALSE)
  
}

Sys.time() - t # 8 mins














# -----------------------------------------------------------------------------
# 3) Combine results and export              ---------------------------------

# list results for all individuals
files <- list.files(output_data, pattern = "_3d_res_day.csv", recursive = TRUE, full.names = TRUE)
files <- files[grepl("/\\d+_3d_res_day\\.csv$", files)] # select only .csv with organismID in the name (for future changes)
# combine csv into single one
df <- files %>% 
  purrr::map_df(read.csv)
# save / export combined result for 3D kernel density estimation
write.csv(df, paste0(output_data,"/kde_3d_res_day.csv"), row.names = FALSE)


# list results for all individuals
files <- list.files(output_data, pattern = "_3d_res_night.csv", recursive = TRUE, full.names = TRUE)
files <- files[grepl("/\\d+_3d_res_night\\.csv$", files)] # select only .csv with organismID in the name (for future changes)
# combine csv into single one
df <- files %>% 
  purrr::map_df(read.csv)
# save / export combined result for 3D kernel density estimation
write.csv(df, paste0(output_data,"/kde_3d_res_night.csv"), row.names = FALSE)






# -------------------------------------------------------------------------------
# 4) Create Rasterbrick (or RasterStack) from mkde.objt and kde values

# output data - create before
output_data <- paste0(output_dir,"/","01_kde_3d")

# load world land mask to delimited 3d kernel densities "medium scale = 50m"
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


# list results for all individuals for day ------------------------------------
files <- list.files(output_data, pattern = "_3dmkde_obj_day.rdata", recursive = TRUE, full.names = TRUE)

for (f in files) {
  # load 3D mdke.obj
  load(f)
  # extract id from file
  organismID <- sub("_3dmkde_obj_day\\.rdata$", "", basename(f))
  
  # info
  cat("Calculate UD volume / processing mkde.obj to RasterBrick (DAY)")
  cat("\n")
  cat("Processing organism ID:", organismID)
  
  # export to raster using mkde.raster function from last version of mkde R pakcage
  # rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_3dmkde_obj_rbrick.tif")
  # mkde.rst <- mkde::mkdeToTerra(mkde.obj)
  # plot(mkde.rst)
  # Not work for 3D only for 2D and 2.5D...
  
  # Crear una lista vacía para almacenar los RasterLayer
  raster_layers <- list()
  
  # Iterar sobre los niveles de z (de 1 a nz) para crear un RasterLayer para cada uno
  for (i in 1:mkde.obj$nz) {
    # Extraer los valores para el nivel i
    d_layer <- mkde.obj$d[, , i]
    
    # Transpond and flip the layer - Key step to convert mdke.obj into RasterStack
    # Transponer y luego voltear verticalmente (flip) la capa
    d_matrix <- base::t(d_layer)  # Transponer la capa
    d_flipped <- d_matrix[base::nrow(d_matrix):1, ]  # Voltear verticalmente
    
    # Crear un RasterLayer para ese nivel (usando x, y como coordenadas y d_flipped como valores)
    r_layer <- raster(d_flipped, xmn = min(mkde.obj$x), xmx = max(mkde.obj$x), 
                      ymn = min(mkde.obj$y), ymx = max(mkde.obj$y))
    
    # Añadir el RasterLayer a la lista
    raster_layers[[i]] <- r_layer
  }
  
  # Convert list of raser layers into RasterStack object
  # raster_brick <- brick(raster_layers)
  
  raster_stack <- stack(raster_layers)
  
  # add CRS to raster brick
  crs(raster_stack) <- CRS("EPSG:3035") # using newest version of assing CRS
  # change layers names
  names(raster_stack) <- paste("layer", 1:nlayers(raster_stack), sep = ".")
  #plot(raster_stack)
  
  # values 0 as NA
  raster_stack <- calc(raster_stack, fun = function(x) { 
    x[x == 0] <- NA
    return(x)
  })
  
  # note that the transformation from mkde.obj to raster stack modify 
  # the voxel / pixel resolution
  # it necessary apply a resample
  
  # # raster of reference of 10x10km = 10,000 m2
  # reference_raster <- raster(
  #   xmn = extent(raster_stack)@xmin,
  #   xmx = extent(raster_stack)@xmax,
  #   ymn = extent(raster_stack)@ymin,
  #   ymx = extent(raster_stack)@ymax,
  #   res = c(10000, 10000),  # Resolución deseada
  #   crs = crs(raster_stack)  # Mantener el CRS original
  # )
  
  
  # calculate ud volumes for raster stack
  # fishtrack3D::volumeUD()
  # see also fun/fun_fishtrack3d.R
  udvolume <- volumeUD(raster_stack, ind.layer = FALSE)
  # crs
  crs(udvolume) <- CRS("EPSG:3035")
  # rename rasterstack layers names
  names(udvolume) <- paste("layer", 1:nlayers(udvolume), sep = ".")
  
  # 0 values as NA
  udvolume <- calc(udvolume, fun = function(x) { 
    x[x == 0] <- NA
    return(x)
  })
  #plot(udvolume)
  
  
  # crop / clip by landmask
  # using mask 
  # Note that mask take into account that the cell's centroid is in the polygon. 
  # if the centroid doesn't intersect with the polygon, there will be not a masking
  # solution for other cases: https://gis.stackexchange.com/questions/255025/r-raster-masking-a-raster-by-polygon-also-remove-cells-partially-covered
  
  # for our analysis, using default raster::mask function is enough based on the raster kde resolution
  # land mask
  world <- sf::st_transform(world, raster::crs(raster_stack))
  # mask (inverse for marine enviroment)
  udvolume <- raster::mask(udvolume, world, inverse = TRUE)
  raster_stack <- raster::mask(raster_stack, world, inverse = TRUE)
  # plot(raster_stack)
  # plot(udvolume)
  
  
  # export raster brick -------------
  # raster_stack <- resample(raster_stack, reference_raster, method = "bilinear") # Note: used in the first version (no layer fliped)
  rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_3dmkde_obj_rstack_day.tif")
  writeRaster(raster_stack, rst_file, overwrite=TRUE)
  
  # udvolume <- resample(udvolume, reference_raster, method = "bilinear") # Note: used in the first version (no layer fliped)
  rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_3d_UD_volume_rstack_day.tif")
  writeRaster(udvolume, rst_file, overwrite=TRUE)
  

  Sys.sleep(2)
  cat("\n")
  cat("\n")
}



# list results for all individuals for night -----------------------------------
files <- list.files(output_data, pattern = "_3dmkde_obj_night.rdata", recursive = TRUE, full.names = TRUE)

for (f in files) {
  # load 3D mdke.obj
  load(f)
  # extract id from file
  organismID <- sub("_3dmkde_obj_night\\.rdata$", "", basename(f))
  
  # info
  cat("Calculate UD volume / processing mkde.obj to RasterBrick (NIGHT)")
  cat("\n")
  cat("Processing organism ID:", organismID)
  
  # export to raster using mkde.raster function from last version of mkde R pakcage
  # rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_3dmkde_obj_rbrick.tif")
  # mkde.rst <- mkde::mkdeToTerra(mkde.obj)
  # plot(mkde.rst)
  # Not work for 3D only for 2D and 2.5D...
  
  # Crear una lista vacía para almacenar los RasterLayer
  raster_layers <- list()
  
  # Iterar sobre los niveles de z (de 1 a nz) para crear un RasterLayer para cada uno
  for (i in 1:mkde.obj$nz) {
    # Extraer los valores para el nivel i
    d_layer <- mkde.obj$d[, , i]
    
    # Transpond and flip the layer - Key step to convert mdke.obj into RasterStack
    # Transponer y luego voltear verticalmente (flip) la capa
    d_matrix <- base::t(d_layer)  # Transponer la capa
    d_flipped <- d_matrix[base::nrow(d_matrix):1, ]  # Voltear verticalmente
    
    # Crear un RasterLayer para ese nivel (usando x, y como coordenadas y d_flipped como valores)
    r_layer <- raster(d_flipped, xmn = min(mkde.obj$x), xmx = max(mkde.obj$x), 
                      ymn = min(mkde.obj$y), ymx = max(mkde.obj$y))
    
    # Añadir el RasterLayer a la lista
    raster_layers[[i]] <- r_layer
  }
  
  # Convert list of raser layers into RasterStack object
  # raster_brick <- brick(raster_layers)
  
  raster_stack <- stack(raster_layers)
  
  # add CRS to raster brick
  crs(raster_stack) <- CRS("EPSG:3035") # using newest version of assing CRS
  # change layers names
  names(raster_stack) <- paste("layer", 1:nlayers(raster_stack), sep = ".")
  #plot(raster_stack)
  
  # values 0 as NA
  raster_stack <- calc(raster_stack, fun = function(x) { 
    x[x == 0] <- NA
    return(x)
  })
  
  # note that the transformation from mkde.obj to raster stack modify 
  # the voxel / pixel resolution
  # it necessary apply a resample
  
  # # raster of reference of 10x10km = 10,000 m2
  # reference_raster <- raster(
  #   xmn = extent(raster_stack)@xmin,
  #   xmx = extent(raster_stack)@xmax,
  #   ymn = extent(raster_stack)@ymin,
  #   ymx = extent(raster_stack)@ymax,
  #   res = c(10000, 10000),  # Resolución deseada
  #   crs = crs(raster_stack)  # Mantener el CRS original
  # )
  
  
  # calculate ud volumes for raster stack
  # fishtrack3D::volumeUD()
  # see also fun/fun_fishtrack3d.R
  udvolume <- volumeUD(raster_stack, ind.layer = FALSE)
  # crs
  crs(udvolume) <- CRS("EPSG:3035")
  # rename rasterstack layers names
  names(udvolume) <- paste("layer", 1:nlayers(udvolume), sep = ".")
  
  # 0 values as NA
  udvolume <- calc(udvolume, fun = function(x) { 
    x[x == 0] <- NA
    return(x)
  })
  #plot(udvolume)
  
  
  # crop / clip by landmask
  # using mask 
  # Note that mask take into account that the cell's centroid is in the polygon. 
  # if the centroid doesn't intersect with the polygon, there will be not a masking
  # solution for other cases: https://gis.stackexchange.com/questions/255025/r-raster-masking-a-raster-by-polygon-also-remove-cells-partially-covered
  
  # for our analysis, using default raster::mask function is enough based on the raster kde resolution
  # land mask
  world <- sf::st_transform(world, raster::crs(raster_stack))
  # mask (inverse for marine enviroment)
  udvolume <- raster::mask(udvolume, world, inverse = TRUE)
  raster_stack <- raster::mask(raster_stack, world, inverse = TRUE)
  # plot(raster_stack)
  # plot(udvolume)
  
  
  # export raster brick -------------
  # raster_stack <- resample(raster_stack, reference_raster, method = "bilinear") # Note: used in the first version (no layer fliped)
  rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_3dmkde_obj_rstack_night.tif")
  writeRaster(raster_stack, rst_file, overwrite=TRUE)
  
  # udvolume <- resample(udvolume, reference_raster, method = "bilinear") # Note: used in the first version (no layer fliped)
  rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_3d_UD_volume_rstack_night.tif")
  writeRaster(udvolume, rst_file, overwrite=TRUE)
  
  
  Sys.sleep(2)
  cat("\n")
  cat("\n")
}

Sys.time() - t # 6 min --- 18 min all process











# -----------------------------------------------------------------------------
# 5) export VTK and ASCII 3D files from 3D mkde.obt         ----------------

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
  
  



