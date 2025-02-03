
#------------------------------------------------------------------------------
## 3D Habitat Use of Loggerhead Turtles

## Created by Jessica Ruff and David March (2021)

# Update package and standarized field names following Sequeria et al., 2021
# by Javier Menéndez-Blázquez | @jmenblaz

# Calculate 2d mkde steps:

# ----------------------------------------------------------------------------

# 0) Load libraries -----
library(mkde)
library(raster)

source("setup.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # custom functions for 3d process
source("analysis/02_3d_process/fun/fun_fishtrack3d.R") # functions for fishtrack3D R package

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
extend.raster = size
zll = 0
crs = "+init=epsg:3035"
contours = c(0.50, 0.75, 0.95)




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
  cat("Processing 2D KDE for individual:", i,"/",length(ttdr_files))
  cat(" · organismID:", organismID, "\n")
  
  # import locs and ttdr data for this organismID or ptt ----------------------
  ttdr <- paste0(main_dir,"/input/tracking/ttdr/L3/",organismID,"_L3_ttdr.csv")
  ttdr <- read.csv(ttdr, dec=",", head=TRUE)
  
      # Note: filtering locations for organismIDs 151934 and 200045
      # within the study area in the Western Mediterranean
  
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
  
  
  
  
  
  
  x = ttdr$x
  y = ttdr$y
  date = ttdr$time
  xy_error = ttdr$xy.error  # renamed so not to overwrite the name of the function


  # Location error variance in the xy dimension
  sig2obs <- xy_error^2

  # Convert time stamps to elapsed minutes from first time
  time <- as.numeric(difftime(date, date[1], units="mins"))

  # Set up movement data
  # doens't call the function directly from the package: stored in 3d_fun_utils.R
  mv.dat <- mkde::initializeMovementData(t.obs = time, x.obs = x, y.obs = y,
                                         sig2obs= sig2obs, t.max = t.max)
    
  #---------------------------------------------------------
  # Define the spatial extent and resolution of a 2D MKDE
  #---------------------------------------------------------
  
  # to give a buffer around the mkde2d object:
  xmin <- min(x) - extend.raster
  xmax <- max(x) + extend.raster
  ymin <- min(y) - extend.raster
  ymax <- max(y) + extend.raster
  

  # Define the spatial extent and resolution of a 2D MKDE
  # Crate a 2D raster
  # $dimension 2
  r <- raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax,
              crs=CRS(crs), resolution=c(voxel.xsize, voxel.ysize), vals=NA)

  # to set up parameters for the initialize mkde2d:
  nx <- ncol(r)
  ny <- nrow(r)
  
  # initialzie mkde2d
  mkde.obj <-  mkde::initializeMKDE2D(xLL = xmin, xCellSize = voxel.xsize, 
                                      nX = nx, yLL= ymin, yCellSize = voxel.ysize, nY = ny)

  #---------------------------------------------------------
  # Calculate raster of density values for MKDE
  #---------------------------------------------------------
  
  # Calculate raster density for 2D
  dens.res <- mkde::initializeDensity(mkde.obj, mv.dat, integration.step)
  
  mkde.obj <- dens.res$mkde.obj # updated MKDE object
  mv.dat <- dens.res$move.dat # updated move data object

  #---------------------------------------------------------
  # Calculate areas for selected contours
  #---------------------------------------------------------
  
  # Set contours
  my.quantiles <- contours
  
  # Calculate density thresholds for select contours
  res <- mkde::computeContourValues(mkde.obj, my.quantiles)
  
  # Calculate areas of 2d home ranges
  # for 2D densities estimate calculate areas (m2)
  res$volume <- computeSizeMKDE(mkde.obj, my.quantiles)

  # Save parameters
  use.obs <- sum(mv.dat$use.obs == TRUE)  # Used observations for the mkde
  mean.xy.error <- mean(ttdr$xy.error)

  #---------------------------------------------------------
  # Create data frame to store results
  #---------------------------------------------------------
  
  kde_2d_res <- data.frame(organismID = organismID,
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
  kde_folder <- paste0(output_data,"/",organismID)
  if(!dir.exists(kde_folder)) dir.create(kde_folder, recursive = TRUE)
  
  # export files as .rdata and csv format
  mkdeobjfile <- paste0(kde_folder,"/",organismID,"_2dmkde_obj.rdata")
  resfile <- paste0(kde_folder,"/",organismID,"_2d_res.rdata")

  
  save(mkde.obj, file = mkdeobjfile)
  save(kde_2d_res, file = resfile)
  write.csv(kde_2d_res, paste0(kde_folder,"/",organismID,"_2d_res",".csv"), row.names = FALSE)
  
  # save(ttdr, file = finalttdrfile)
  # finalttdrfile <- paste0(kde_folder,"/",organismID,"_2d_ttdr.rdata")
  
}

Sys.time() - t # 1min

  
  
# -----------------------------------------------------------------------------
# 3) Combine results and export              ---------------------------------
  
# list results for all individuals
files <- list.files(output_data, pattern = "_2d_res.csv", recursive = TRUE, full.names = TRUE)
files <- files[grepl("/\\d+_2d_res\\.csv$", files)] # select only .csv with organismID in the name (for future changes)

# combine csv into single one
df <- files %>% 
  purrr::map_df(read.csv)
  
# save / export combined result for 3D kernel density estimation
write.csv(df, paste0(output_data,"/kde_2d_res.csv"), row.names = FALSE)
  
  
  
# -----------------------------------------------------------------------------
# 4) export VTK and ASCII 2D files from 2D mkde.obt         ----------------
  
# list results for all individuals
files <- list.files(output_data, pattern = "_2dmkde_obj.rdata", recursive = TRUE, full.names = TRUE)
  
for (f in files) {
  # load 2D mdke.obj
  load(f)
  # extract id from file
  organismID <- sub("_2dmkde_obj\\.rdata$", "", basename(f))
  
  # export to raster using mkde.raster function from last version of mkde R pakcage
  rst_file <- paste0(output_data,"/",organismID,"/",organismID,"_2dmkde_obj_raster.tif")
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




  