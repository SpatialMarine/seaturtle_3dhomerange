


# Fishing effort volume
#calculate fishing effort volume for each turtle


#------------------------------------------------------------------------------
## 3D Habitat Use of Loggerhead Turtles

# Update package and standarized field names following Sequeria et al., 2021
# by Javier Menéndez-Blázquez | @jmenblaz

# 1) Extract fishing effort for extent of seaturtle kde estimate
  # 1.1) Resample fishing effort to 10x10km2 

# 2) Create a raster stack following the different depths of fishing
  # 2.1) depth with no fishing values 0
# 3) Transform fishing effort metric (hours of navigation into proporcional 0-1 values regardign the area) 
# 4) Calculate the volume of fishing effort





source("setup.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # custom functions for 3d process


# 0) --------------------------------------------------------------------------

output_data <- paste0(output_dir,"/","03_fishing_3d")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

library(sp)
library(sf)
library(raster)


# 1) espicify fishing features:

# Specify fishing gear and depth (specify fishing depth in meters)
fishing_gear = "sup_longline" # trawler, etc, etc)
fishing_depth = 30


# kde 3D results
kde_files <- list.files(paste0(main_dir,"/output/01_kde_3d/"), full.names = TRUE, pattern = "_3dmkde_obj_rstack.tif", recursive = TRUE)



# ------------------------------------------------------------------------------
# 2) Obtain fishinf effort for same area of tracking data --------------------- 

# t <- Sys.time()
# 
# cores <- detectCores() - 2
# cl <- makeCluster(cores)
# registerDoParallel(cl)
# 
# getDoParWorkers() # backend information


t <- Sys.time()

for (i in 1:length(kde_files)) { }

  # 1) load  kde data from mkde.obj (as rasterstack)   -------------------------
  kde <- kde_files[i]
  # extract organismID from L3_ttdr fiel name
  organismID <- sub("_3dmkde_obj_rstack\\.tif$", "", basename(kde))
  
  # info 
  cat("Processing 3D overlap with fisheries:", i,"/",length(kde_files))
  cat(" · organismID:", organismID, "\n")

  # load 3D kde (rasterstack)
  kde <- raster::stack(kde)
  crs(kde) <- CRS("EPSG:3035")  # add CRS
  names(kde) <- paste("layer", 1:nlayers(kde), sep = ".")  # rename layers
  
  # # values 0 as NA
  # kde <- calc(kde, fun = function(x) { 
  #   x[x == 0] <- NA
  #   return(x)
  # })

  # extract raster extension as bounding box to delimited the traking area of organism ID
  bb <- extent(kde)
  
  xmax <- xmax(bb)
  xmin <- xmin(bb)
  ymax <- ymax(bb)
  ymin <- ymin(bb)
  
  # bounding box of organismID track extension as a sfc object
  bb <- st_as_sfc(st_bbox(c(
        xmin = xmin,
        ymin = ymin,
        xmax = xmax,
        ymax = ymax
      ), crs = st_crs(3035)))
  
  # convert or transform to from EPSG:3035 to EPSG:4326 (WGS84) bounding box
  bb <- st_transform(bb, crs = 4326)
  bb <- as(bb, "Spatial")  # transfor, from sfc to sp class
  
  
  # ---------------------------------------------------------------------------
  # 2)  import fishing effort data (Global fishing Watch, Global Marine Traffic, etc)
  
  # Global Marine Traffic Data 
  file <- list.files(paste0(input_dir,"/fishing/"), full.names = TRUE) #MODIFY.....
  fishing <- raster(file)
  
  # crop / mask fishing effort to bounding-box of tracking data 
  # (kernel density estimate extent)
  
  # for large raster (global datasets), first use crop to delimited minimum convex polygon to bb
  # and then masked the less size raster with the bb
  fishing <- crop(fishing, bb)  # crop raster
  fishing <- raster::mask(fishing, bb)  # mask raster
  
  # plot(fishing)
  # plot(bb, add = TRUE)
   
  
  # transform or reproject to CRS of study
  fishing <- projectRaster(fishing, crs = CRS("EPSG:3035"))

  # create references raster for resample fishing effort to dimension and exent of tracking data
  # same extension, dimension and resolution
  reference_raster <- raster(extent(kde), res = res(kde), crs = crs(kde))

  # resample raster - from 1x1 to 10x10 km2
  fishing <- resample(fishing, reference_raster, method = "bilinear")


  
  # ------------------------------------------------------------------------------
  # 3) Create a raster stack following the different depths of fishing -----------
  
  # convert from 2D to 3D raster stack fishing effort depends of the fishing gear depth
  # example: trawler, etc
  
  # extract number of layers from 
  depths <- nlayers(kde)
  
  # create a RasterStack from RasterLayer repeating the original layer
  # repeat raster layer
  fishing <- stack(lapply(1:depths, function(x) fishing))

  # modify layers names
  names(fishing) <- paste("layer", 1:nlayers(fishing), sep = ".")
  # plot(fishing)
  
  # specify the fishing effort in each layer following the "metier" or 
  # fishing art/gear features
  # each layer 10 meters
  
  fsh_layer = (fishing_depth/10)
  
  # For each Rasterstack layer, except the objective fishing layer and the top layers,
  # change values for 0
  
  for (i in (fsh_layer + 1):nlayers(fishing)) {
    # change layer values by 0 (no fishing effort == no fishing volume)
    fishing[[i]][] <- 0
  }
  
  # check
  # plot(fishing)
  
  # convert all 0 values in the stack as NA (== NO fishing activity)
  fishing <- calc(fishing, fun = function(x) { 
    x[x == 0] <- NA
    return(x)
  })
  
  plot(fishing)
  
  # save/export 3D fishing effort by spatial extent of organismID
  # same extent and spatial resolution
  # same CRS
  # resampled to 10x10 km2 (z = 10 m) -> mkde resolution
  rst_file <- paste0(output_data,"/",organismID,"_3d_fishing-effort.tif")
  writeRaster(fishing, rst_file, overwrite=TRUE)


























 

