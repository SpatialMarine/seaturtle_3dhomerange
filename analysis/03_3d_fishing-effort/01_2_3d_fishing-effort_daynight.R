
#------------------------------------------------------------------------------
## 3D Habitat Use of Loggerhead Turtles

# Update package and standarized field names following Sequeria et al., 2021
# by Javier Menéndez-Blázquez | @jmenblaz


# Fishing effort volume
# calculate apparent fishing effort volume for each tagged sea-turtle by day and night
#  (see data processing scripts for how to obtain day and night night from GFW API - fishing events)

# Fishing event GFW information:
# https://globalfishingwatch.org/data-documentation/apparent-fishing-events-ais/

# ---------------------------------------------------------------------------

# same steps than for the global (day + night fishing effort)

# 1) Extract fishing effort activty for extent of seaturtle day and night kde estimates
  # 1.1) Resample fishing effort to kde resolution (e.,g 5x5 km2) and same extension

# 2) Create a raster stack following the different depths of fishing
  # 2.1) depth with no fishing values 0

# 3) For trawlers fisheries,
#   3.1) Create a rasterbrick per depth range
#        using a similar raster reference that fishing 2D
#        filter trawler fishing effort by bathymetry ranges






source("setup.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # custom functions for 3d process


# -------------------------------------------------------------------------------

output_data <- paste0(output_dir,"/","03_fishing_3d_daynight")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

library(sp)
library(sf)
library(raster)


# ------------------------------------------------------------------------------
# 0) Calculate mean drifting longlines set depths based on literature review
# references in supplementary information: (Báez, et al 2019; Camiñas et al., 2016; García-Barcelona et al., 2010).
# Differenrt class of drifting longline classificed into one class

# For these reference that provide only a set depth, these were used as max depth (NA as min)
data <- data.frame(
  source = c("Baez_2019", "Baez_2019", "Baez_2019", "Baez_2019", 
             "Garcia_2010", "Garcia_2010", "Garcia_2010", "Garcia_2010",
             "Caminas_2016", "Caminas_2016"),
  min_depth = c(20, 50, 40, 50, 
                NA, NA, 30, NA, 
                30, NA),  # NA for set depth instead range depths (used as max)
  max_depth = c(50, 90, 70, 90, 
                12, 70, 50, 30, 
                90, 90))

# Caculare min, max values and range
overall_stats <- data %>%
  summarise(
    min = min(min_depth, na.rm = TRUE),  # Media de las mínimas
    max = max(max_depth, na.rm = TRUE),
    mean_min = mean(min_depth, na.rm = TRUE),
    mean_max = mean(max_depth, na.rm = TRUE) # Media de las máximas
  ) %>%
  mutate(final_range = paste0(round(min, 1), " - ", round(max, 1)),
         final_range_mean = paste0(round(mean_min, 1), " - ", round(mean_max, 1)))
# info
cat("Drifting longline range determined (min - max: ", overall_stats$final_range) # 20 - 90 m
cat("Drifting longline range determined (min - max; mean): ", overall_stats$final_range_mean) # 30 - 70 m





# ------------------------------------------------------------------------------
# 3D three-dimensional day/night fishing effort

# 2) Obtain fishing effort for same area of tracking data by day and night  ---- 

daynight_pattern <- c("day", "night")



# ------------------------------------------------------------------------------
# 2.1) Fishing effort and fishing gears features

# Fishing effort data for Global Fishing Watch (see 00_global_fishing_watch_gfw_api_data.R)
# names provide for Global Fishing Watch
# for gear type: "drifting_longlines", "trawelers".

# specify fishing gear. Names based on ICCAT classification:

# LL = Drifting longline
# TW = Trawlers
fishing_gears <- c("LL","TW")  # create a chr chain

# Specify fishing gear and depth (specify fishing depth in meters)
# same names that those provide for Global Fishing Watch

# specify single or ranfe depths for different gear type
# LL or Drifintg lonline
LL_fishing_depth = c(30 , 70) # note depths are divided into 10 meters per depth range


# For trawlers load bathymetry information (GEBCO, 2024)
# see setup.R for path
bath_gebco <- paste0(input_dir, "/gis/gebco/mediterranean_sea_gebco_2024.tif")
bath_gebco <- raster(bath_gebco)
# select values < 0 for sea areas (>= 0 as NA)
bath_gebco[bath_gebco >= 0] <- NA 

# NOTE*** : apply filter for bathymetry <50 meters as NA, 
# no fishing trawlers (at least in Spanish waters)
# Most of the turtles are in spanish waters

bath_gebco[bath_gebco >= -50] <- NA 


# ------------------------------------------------------------------------------
# 3) Process 3D three dimensional fishing effort following the kde extension ---

# cores <- detectCores() - 2
# cl <- makeCluster(cores)
# registerDoParallel(cl)
# 
# getDoParWorkers() # backend information


t <- Sys.time()

# for fishing gear in fishing_gears
# fishing_gear = "drifting_longlines" # drifting_longlines, trawler, etc, etc)


# processing for day and night
for (dnp in 1:length(daynight_pattern)) {
  # select day night period
  dn <- daynight_pattern[dnp]
  
  # load previously data for kde 3D results by daynight period
  kde_files <- list.files(paste0(main_dir,"/output/01_kde_3d/"), full.names = TRUE, pattern = paste0("_3dmkde_obj_rstack_",dn,".tif"), recursive = TRUE)
  
  
  for (g in 1:length(fishing_gears)) {
    
    # extract differenrts fishing_gear to process
    fishing_gear <- fishing_gears[g]
    # info
    message("Processing fishing gear type: ", fishing_gear, "\n")
    message("· period time: ",dn,"\n")
    
    # process each kde file
    for (i in 1:length(kde_files)) { 
      
      # 1) load  kde data from mkde.obj (as rasterstack)   -------------------------
      kde <- kde_files[i]
      
      # extract organismID from L3_ttdr fiel name
      organismID <- sub(paste0("_3dmkde_obj_rstack_",dn,"\\.tif$"), "", basename(kde))
      
      # info 
      cat("Processing 3D fishing effort:", i,"/",length(kde_files))
      cat(" · organismID:", organismID, "\n")
      cat(" · Gear type:", fishing_gear,"\n")
      
      # load 3D kde (rasterstack) created previously
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
      bb <- sf::st_as_sfc(st_bbox(c(
        xmin = xmin,
        ymin = ymin,
        xmax = xmax,
        ymax = ymax
      ), crs = st_crs(3035)))
      
      # convert or transform to from EPSG:3035 to EPSG:4326 (WGS84) bounding box
      bb <- st_transform(bb, crs = 4326)
      bb <- as(bb, "Spatial")  # transform from sfc to sp class
      
      
      # ---------------------------------------------------------------------------
      # 2)  import fishing effort data
      
      # Global Marine Traffic Data for specific fishing gear (see 00_global_fishing_watch_gfw_api_data.R)
      if (fishing_gear == "LL") fg <- "drifting_longlines"
      if (fishing_gear == "TW") fg <- "trawlers"
      
      file <- list.files(paste0(input_dir,"/gfw/"), pattern = paste0(fg,"_fishing_effort_",dn,".tif"), full.names = TRUE)
      fishing <- raster(file)
      
      # Global Fishing Watch high spatial resolution = 0.01º (1km2 aprox)
      
      # crop / mask fishing effort to bounding-box of tracking data 
      # (kernel density estimate extent)
      # for large raster (global datasets), first use crop to delimited minimum convex polygon to bb
      # and then masked the less size raster with the bb
      fishing <- crop(fishing, bb)  # crop raster
      fishing <- raster::mask(fishing, bb)  # mask raster
      
      # plot(fishing)
      # plot(bb, add = TRUE)
      
      # transform or reproject to CRS of study
      # EPSG:3035 same used for kde 3D
      fishing <- projectRaster(fishing, crs = CRS("EPSG:3035"))
      
      # create references raster for resample fishing effort to dimension and exent of tracking data
      # same extension, dimension and resolution
      reference_raster <- raster(extent(kde), res = res(kde), crs = crs(kde))
      
      # resample raster - from 1x1 km2 (GFW resolution) to 10x10 km2
      # same extension than KDE raster
      fishing <- resample(fishing, reference_raster, method = "bilinear")
      
      
      
      # ------------------------------------------------------------------------------
      # 3) Create a raster stack following the different depths of fishing -----------
      
      # convert from 2D to 3D raster stack fishing effort depends of the fishing gear depth
      # example: trawler, drifting longline, etc
      
      # extract number of layers from (each layer 10 m depth range)
      depths <- nlayers(kde)
      
      # create a RasterStack from single RasterLayer repeating the original layer of fishing effort
      # repeat raster layer
      fishing <- stack(lapply(1:depths, function(x) fishing))
      
      # modify layers names
      names(fishing) <- paste("layer", 1:nlayers(fishing), sep = ".")
      # plot(fishing)  # check
      
      
      
      # ----------------------------------------------------------------------------
      # 4) Process 3D fishing effort by feature of fishing gear of interest
      
      # Names based on ICCAT classification
      # LL = Drifting longline
      # TW = Trawlers
      
      # 4.1) LL - Drifting longline with ranges depth ---------------------------
      # delimited range of depths
      if (fishing_gear == "LL") {
        
        # specify the fishing effort in each layer following the "metier" or 
        # fishing art/gear features
        # each layer = 10 meters
        fsh_layer = (LL_fishing_depth/10) # specify number of depth following fishing gear type set depth as number (e.i., 20m = layer 2, 40m == layer 4)
        
        # For each Rasterstack layer, except the objective fishing layer and the top layers,
        # change values for 0
        
        # a) Only set depth provided:
        #      fishing effort from sea surface to set depth
        if (length(fsh_layer) == 1) {
          for (f in (fsh_layer + 1):nlayers(fishing)) {
            # change layer values by 0 (no fishing effort == no fishing volume)
            fishing[[f]][] <- 0
          }
        }
        
        # b) using a set depth range (30-70m)
        if (length(fsh_layer) > 1) {
          for (f in 1:nlayers(fishing)) {
            if (f < fsh_layer[1] || f > fsh_layer[2]) {
              # Establecer en 0 las capas fuera del rango de fsh_layer
              fishing[[f]][] <- 0
            }
          }
        }
        
        # check
        # plot(fishing)
        
        # convert all 0 values in the raster stack as NA (== NO fishing activity)
        fishing <- calc(fishing, fun = function(x) { 
          x[x == 0] <- NA
          return(x)
        })
        
        # check 
        plot(fishing)
        
        # save/export 3D fishing effort by spatial extent of organismID
        # same extent and spatial resolution
        # same CRS
        # resampled to 10x10 km2 (z = 10 m) -> mkde resolution
        rst_file <- paste0(output_data,"/",organismID,"_3d_fishing-effort_",fishing_gear,"_",dn,".tif")
        writeRaster(fishing, rst_file, overwrite=TRUE)
        
      }
      
      
      # 4.2) TW - Trawlers   ---------------------------------------------------
      # delimited fishing effort based on bathymetry ranges
      if (fishing_gear == "TW") { 
        
        # mask bathymetry in spatial range of the organismID
        bath <- crop(bath_gebco, bb) # crop raster by bb
        bath <- raster::mask(bath, bb)  # mask raster (values 0 delimited)
        # reproject 
        bath <- projectRaster(bath, crs = CRS("EPSG:3035"))
        # plot(bath)  # check
        
        # maximun depth for study case (m)
        maxd <- (depths*10)*-1
        
        # max bathymetry (minimum depth bottom) 
        # note bathymetry has negative values (see GEBCO, 2024)
        min_bath <- cellStats(bath, stat = "max", na.rm = TRUE)
        
        # if maximum bathymetry value is higher than depth reached by organism
        # there is not interaction between trawlers and volume occupied by organism
        if (min_bath >= maxd) {
          
          # delimited bathymetry areas based into max depth reached by organism
          # filter raster values or assign NA values to less or equal to maximum depth
          values(bath)[values(bath) < maxd] <- NA
          
          
          # create a rasterbrick bathimetry areas by depth ranges (10 meters)
          # separate the bathymetry from one single raster into rasterstack
          
          # create a RasterStack from single RasterLayer repeating the original layer of fishing effort
          # repeat raster layer
          bath <- stack(lapply(1:depths, function(x) bath))
          # modify layers names
          names(bath) <- paste("layer", 1:nlayers(bath), sep = ".")
          # plot(bath) # check
          
          # create bathymetry ranges 
          depth_range <- seq(0, maxd, by = -10)
          
          # filter by bathymetry values per depth ranges of 10 meters
          # process range by range
          for (d in 1:(length(depth_range) - 1)) {
            upper <- depth_range[d]     # up depth in the range
            lower <- depth_range[d + 1] # bottom depth in range
            # select rasterstack layer in the range depth
            temp_layer <- bath[[d]]  # select rasterstack layer
            # filter values outside the depth range
            # == change as NA values out depth range assigned
            values(temp_layer)[values(temp_layer) < lower | values(temp_layer) >= upper] <- NA
            # update layer
            bath[[d]] <- temp_layer
          }
          
          # plot(bath)  # check  
          
          # create references raster for resample fishing effort to dimension and extent of tracking data
          # same extension, dimension and resolution
          reference_raster <- raster(extent(fishing), res = res(fishing), crs = crs(fishing))
          
          # resample raster - from 0.5x0.5 km2 to 10x10 km2
          bath <- resample(bath, reference_raster, method = "bilinear")
          # plot(bath)  # check
          
          # delimited fishing effort by depth range (from 2D Trawler fishing effort to 3D in raster stack)
          fishing <- raster::mask(fishing, bath)
          # plot(fishing)
          
          # save/export 3D fishing effort by spatial extent of organismID
          # same extent and spatial resolution
          # same CRS
          # resampled to 10x10 km2 (z = 10 m) -> mkde resolution
          rst_file <- paste0(output_data,"/",organismID,"_3d_fishing-effort_",fishing_gear,"_",dn,".tif")
          writeRaster(fishing, rst_file, overwrite = TRUE)
        } 
        
        if (min_bath < maxd) { 
          #  there is not interaction between trawlers and volume occupied by organism
          # maximum depth reached by organism < minum depth for the movement extension
          
          # some cases for small extension and/or high pelagic areas
          
          # resampled to 10x10 km2 (z = 10 m) -> mkde resolution
          # NO fishing effort for this raster at differenrent depth
          values(fishing)[] <- NA
          
          # export as raster stack, with different depths, but without values (only NA) for further overlap
          rst_file <- paste0(output_data,"/",organismID,"_3d_fishing-effort_",fishing_gear,"_",dn,".tif")
          writeRaster(fishing, rst_file, overwrite = TRUE)
        }
      }
    }
  }
}



Sys.time() - t # 21 min

cat("3D Fishing effort processed for all organism IDs")





