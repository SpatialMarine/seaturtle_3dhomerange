
#------------------------------------------------------------------------------
## 3D Habitat Use of Loggerhead Turtles

# Update package and standarized field names following Sequeria et al., 2021
# by Javier Menéndez-Blázquez | @jmenblaz


# Fishing effort volume
# calculate fishing effort volume for each tagged sea-turtle

# 1) Extract fishing effort for extent of organismID kde estimate
  # 1.1) Resample fishing effort to kde resolution (e.,g 5x5 km2) and same extension


# Note: different script for 2D and 3D process due potential differences in the 2D/3D process
#       function in mkde R package


source("setup.R")


# -------------------------------------------------------------------------------

output_data <- paste0(output_dir,"/","03_fishing_2d_daynight")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

library(sp)
library(sf)
library(raster)

# day and night process
daynight_pattern <- c("day", "night")



# Not problem that at the end we will use 10x10 km
# For trawlers load bathymetry information (GEBCO, 2024)
# see setup.R for path
bath_gebco <- paste0(input_dir, "/gis/gebco/mediterranean_sea_gebco_2024.tif")
bath_gebco <- raster(bath_gebco)
# select values < 0 for sea areas (>= 0 as NA)
bath_gebco[bath_gebco >= 0] <- NA 

# Reproject bathymetry to same CRS
bath_gebco <- projectRaster(bath_gebco, crs = CRS("EPSG:3035"))

# NOTE*** : apply filter for bathymetry <50 meters as NA, 
# no fishing trawlers (at least in Spanish waters)
# Most of the turtles are in spanish waters
bath_gebco[bath_gebco >= -50] <- NA 






t <- Sys.time()

# processing for day and night
for (dnp in 1:length(daynight_pattern)) {
  
  # select day night period
  dn <- daynight_pattern[dnp]
  
  # --------------------------------------------
  # 2D three-dimensional fishing effort
  
  # 2) Obtain fishing effort for same area of tracking data ------------
  # load previously data for kde 3D results
  kde_files <- list.files(paste0(main_dir,"/output/02_kde_2d/"), full.names = TRUE, pattern = paste0("_2dmkde_obj_raster_",dn,".tif"), recursive = TRUE)
  
  
  # ----------------------------------------------------
  # 2.1) Fishing effort and fishing gears features 
  
  # LL = Drifting longline
  # TW = Trawlers
  fishing_gears <- c("LL","TW")  # create a chr chain
  
  
  # ----------------------------------------------------
  # 3) Process 2D fishing effort following the kde extension and resolution
  

  
  # for fishing gear in fishing_gears
  # fishing_gear = "drifting_longlines" # drifting_longlines, trawler, etc, etc)
  
  for (g in 1:length(fishing_gears)) {
    
    # extract differenrts fishing_gear to process
    fishing_gear <- fishing_gears[g]
    # info
    message("Processing fishing gear type: ", fishing_gear, "\n")
    
    # process each kde file
    for (i in 1:length(kde_files)) { 
      
      # 1) load  kde data from mkde.obj (as rasterstack)   ---------
      kde <- kde_files[i]
      # extract organismID from L3_ttdr fiel name
      organismID <- sub(paste0("_2dmkde_obj_raster_",dn,"\\.tif$"), "", basename(kde))
      
      # info 
      cat("Processing 2D fishing effort:", i,"/",length(kde_files))
      cat(" · organismID:", organismID, "\n")
      cat(" · gear type:", fishing_gear,"\n")
      cat(" · daynight period: ", dn, "\n")
      
      # load 2D kde created previously
      kde <- raster(kde)
      crs(kde) <- CRS("EPSG:3035")  # add CRS
      # plot(kde)
      
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
      bb <- sf::st_as_sfc(st_bbox(c(
        xmin = xmin,
        ymin = ymin,
        xmax = xmax,
        ymax = ymax
      ), crs = st_crs(3035)))
      
      # convert or transform to from EPSG:3035 to EPSG:4326 (WGS84) bounding box
      # bb <- st_transform(bb, crs = 4326)
      bb <- as(bb, "Spatial")  # transform from sfc to sp class
      
      
      # ---------------------------------------------------------------------------
      # 2)  import fishing effort data
      
      # Global Marine Traffic Data for specific fishing gear (see 00_global_fishing_watch_gfw_api_data.R)
      if (fishing_gear == "LL") fg <- "drifting_longlines"
      if (fishing_gear == "TW") fg <- "trawlers"
      
      file <- list.files(paste0(input_dir,"/gfw/"), pattern = paste0(fg,"_fishing_effort_",dn,".tif"), full.names = TRUE)
      fishing <- raster(file)
      # plot(fishing)
      
      # transform or reproject to CRS of study
      # EPSG:3035 same used for kde 3D
      fishing <- projectRaster(fishing, crs = CRS("EPSG:3035"))
      
      # Global Fishing Watch high spatial resolution = 0.01° (1km2 aprox)
      
      # crop / mask fishing effort to bounding-box of tracking data 
      # (kernel density estimate extent)
      # for large raster (global datasets), first use crop to delimited minimum convex polygon to bb
      # and then masked the less size raster with the bb
      fishing <- crop(fishing, bb)  # crop raster
      fishing <- raster::mask(fishing, bb)  # mask raster
      
      # plot(fishing)
      # plot(bb, add = TRUE)
       
      
      # crop by bathymetry > 50 for trawlers
      if (fishing_gear == "TW") {
        # crop and mask bathymety by bb
        bath_mask <- crop(bath_gebco, bb)
        bath_mask <- mask(bath_mask, bb)
        # resample bath_mask to equal extension
        bath_mask <- resample(bath_mask, fishing, method = "bilinear")  # 'bilinear' para continuo
        # apply mask of 50m bath to Trawlers fishing effort (events) detected
        fishing <- mask(fishing, bath_mask)
      }
      
      # create references raster for resample fishing effort to dimension and exent of tracking data
      # same extension, dimension and resolution
      reference_raster <- raster(extent(kde), res = res(kde), crs = crs(kde))
      
      # use kde raster as base for resample (Note: not use the values)
      # reference_raster <- kde
      
      # resample raster - from 1x1 km2 (GFW resolution) to 10x10 km2
      fishing <- resample(fishing, reference_raster, method = "bilinear")
      # plot(fishing)
      
      # export as raster file
      rst_file <- paste0(output_data,"/",organismID,"_2d_fishing-effort_",fishing_gear,"_",dn,".tif")
      writeRaster(fishing, rst_file, overwrite = TRUE)
      
    }
  }
}




Sys.time() - t # 21:30 min

cat("2D Fishing effort processed for all organism IDs")



