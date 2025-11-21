#------------------------------------------------------------------------------
## 3D Habitat Use of Loggerhead Turtles

## Created by Javier Menéndez-Blázquez | @jmenblaz

# Update package and standarized field names following Sequeria et al., 2021


# 1) load / import data 
# 2) process 2D overlap between fishing effort data and 2D kde organism ID processed 
#   - 2.1 Load data for processing - kde and fishing effort 
#   - 2.2 Calculate the 2D overlap 
#   - 2.3 Calculate the 2D overlap areas




# 0) Load libraries and create paths -----
library(raster)
library(dplyr)

source("setup.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # custom functions for 3d process

# create ouput dir
output_data <- paste0(output_dir,"/03_fishing_2d_overlap_daynight")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)




# day and night process
daynight_pattern <- c("day", "night")

# different fishing gears (see 01_3d_fishing-overlap.R)
fishing_gears <- c("LL","TW")  # create a chr chain



# empty dataframe to add results
results <- data.frame(organismID                 = character(),
                      daynight                   = character(),
                      area.50.km2                = numeric(),
                      area.75.km2                = numeric(),
                      area.95.km2                = numeric(),
                      area.total.km2             = numeric(),
                      fishing_gear                 = character(),
                      udarea50_no_fishing_km2       = numeric(),
                      udarea50_intersect_fishing    = numeric(),
                      udarea50_intersect_percentage = numeric(),
                      udarea75_no_fishing_km2       = numeric(),
                      udarea75_intersect_fishing    = numeric(),
                      udarea75_intersect_percentage = numeric(),
                      udarea95_no_fishing_km2       = numeric(),
                      udarea95_intersect_fishing    = numeric(),
                      udarea95_intersect_percentage = numeric(),
                      stringsAsFactors = FALSE)



t <- Sys.time()

# processing for day and night
for (dnp in 1:length(daynight_pattern)) {
  
  # select day night period
  dn <- daynight_pattern[dnp]
 
  # ---------------------------------
  # 1) load import data
  
  # kde 2D results
  kde_files <- list.files(paste0(main_dir,"/output/02_kde_2d/"), full.names = TRUE, pattern = paste0("_2dmkde_obj_raster_",dn,".tif"), recursive = TRUE)
  # note: areas in m2
  
  kde_res <- read.csv(paste0(main_dir,"/output/02_kde_2d/kde_2d_res_",dn,".csv"))
  
  # transform to km2
  kde_res$area.50.km2 <- kde_res$area.50/1000000
  kde_res$area.75.km2 <- kde_res$area.75/1000000
  kde_res$area.95.km2 <- kde_res$area.95/1000000  
  
  
  # ------------------------------------------------------------------------------
  # 2) process 2D overlap between fishing effort data and kde organism ID processed 
  
  # t <- Sys.time()
  # 
  # cores <- detectCores() - 2
  # cl <- makeCluster(cores)
  # registerDoParallel(cl)
  # 
  # getDoParWorkers() # backend information
  
  
  # process overlap and areas for organism IDs (kde files)
  
  for (i in 1:length(kde_files)) {
    
    # 2.1) load data for processing - kde and fishing effort --------------
    kde <- kde_files[i]
    # extract organismID from L3_ttdr fiel name
    organismID <- sub(paste0("_2dmkde_obj_raster_",dn,"\\.tif$"), "", basename(kde))
    
    # info 
    cat("Processing 2D overlap with fisheries:", i,"/",length(kde_files))
    cat(" · organismID:", organismID, "\n")
    cat(" · daynight period: ", dn, "\n")
    
    # load kde result from mkde 3d functions process
    # note: volumes in km3, transfortmed previously
    kde_res_id <- kde_res %>% filter(kde_res$organismID == !!organismID)
    
    # load 2D kde (raster layer)
    kde <- raster(kde)
    crs(kde) <- CRS("EPSG:3035")  # add CRS
    
    # values 0 as NA in RasterStack
    # double check, implemented before
    kde <- calc(kde, fun = function(x) {
      x[x == 0] <- NA
      return(x)
    })
    
    # min(values(kde), na.rm = TRUE)
    # plot(kde)
    
    for (g in 1:length(fishing_gears)) {
      
      # select fishing gear
      fishing_gear <- fishing_gears[g]
      
      # info
      message("Processing fishing gear type: ", fishing_gear, "\n")
      
      # load fishing data into spatial area of the KDE
      fishing <- raster(paste0(main_dir,"/output/03_fishing_2d_daynight/",organismID,"_2d_fishing-effort_",fishing_gear,"_",dn,".tif"))
      crs(fishing) <- CRS("EPSG:3035")  # add CRS
      # plot(fishing)
      
      # QC of objects extents
        # resample raster - avoid some isuuses of extent
        # fishing <- resample(fishing, kde, method = "bilinear")
      
      # check min values without NA
      # min(values(fishing), na.rm = TRUE)
      


      # 2) calculate the 2D overlap ------------------------------------------
      
      # 2.1) calculate interaction between kde and fishing effort
      # multiply kde and fishing effort values to identify areas with high impact
      # high impact = high kde values and high fishing effort
      fishing_interact <- kde * fishing
      # plot(fishing_interact, col = magma(100))
      
      # 2.2) fishing used as mask in raster::mask()
      # logic: remove pixel where there are fishing impact in order to calculate 
      #   - result: is the volumen affects by fishing activities 
      # (is necessary to calculate the difference between total volume or with threshold)
      
      kde_fishing_intersect <- raster::mask(kde, fishing)  # provide the UD volume of the impact
      # plot(kde_fishing_intersect)
      
      # kde without fishing area of impact(symmetrical difference)
      kde_fishing_simdif <- raster::mask(kde, fishing,  inverse = TRUE) # provide the UD area without impact
      # plot(kde_fishing_simdif)
      
      # 2.3) calculate the 2D overlap volumes  ---------------------------------
      threshold.95 <- kde_res_id$threshold.95  
      threshold.75 <- kde_res_id$threshold.75
      threshold.50 <- kde_res_id$threshold.50
      
      # calculate UD areas after difference with fishing effort (free area without fishing effort)
      # areas in m2
      
      # raster cell area
      cell_area <- raster::area(kde_fishing_simdif)
      # calculate areas
      area.95 <- sum(cell_area[kde_fishing_simdif >= threshold.95], na.rm = TRUE)
      area.75 <- sum(cell_area[kde_fishing_simdif >= threshold.75], na.rm = TRUE)
      area.50 <- sum(cell_area[kde_fishing_simdif >= threshold.50], na.rm = TRUE)
      
      # m2 -> km2
      area.95 <- area.95/1000000  
      area.75 <- area.75/1000000
      area.50 <- area.50/1000000
      
      # UD  area without intersect between fishing effort (kde free of interaction)
      ud95_intersect <- (kde_res_id$area.95.km2) - area.95  # km2
      ud95_intersect_percentage <- ((kde_res_id$area.95.km2 - area.95) / kde_res_id$area.95.km2) * 100
      
      ud75_intersect <- (kde_res_id$area.75.km2) - area.75  # km2
      ud75_intersect_percentage <- ((kde_res_id$area.75.km2 - area.75) / kde_res_id$area.75.km2) * 100
      
      ud50_intersect <- (kde_res_id$area.50.km2) - area.50  # km2
      ud50_intersect_percentage <- ((kde_res_id$area.50.km2 - area.50) / kde_res_id$area.50.km2) * 100
      
      # for total volume (m2)
      # important -> cell_area <- raster::area(kde) # created previously
      area_total <- sum(cell_area[kde], na.rm = TRUE)
      area_intersect  <- sum(cell_area[kde_fishing_intersect], na.rm = TRUE)  # area with fishing interaction
      area_no_fishing <- area_total - area_intersect  # area witouth fishing interaction
      area_intersect_percentage <- (area_intersect / area_total) * 100   # percentage over total area
      
      # m3 -> km3
      area_total      <- area_total/1000000
      area_intersect  <- area_intersect/1000000
      area_no_fishing <- area_no_fishing/1000000 
      
      
      # 2.4) save / export results ----------------------------------------------
      
      # append organismID result into summary dataframe
      results_id <- data.frame(organismID = organismID,
                               # daynight
                               daynight   = dn,
                               # UD results
                               area.50.km2 = kde_res_id$area.50.km2,
                               area.75.km2 = kde_res_id$area.75.km2,
                               area.95.km2 = kde_res_id$area.95.km2,
                               area.total.km2 = area_total,
                               # Fishing overlap results
                               fishing_gear = fishing_gear,
                               # UD 50
                               ud50_no_fishing_km2 = area.50,
                               ud50_intersect_fishing = ud50_intersect, 
                               ud50_intersect_percentage = ud50_intersect_percentage,
                               # UD 75
                               ud75_no_fishing_km2 = area.75,
                               ud75_intersect_fishing = ud75_intersect, 
                               ud75_intersect_percentage = ud75_intersect_percentage,
                               # UD 95
                               ud95_no_fishing_km2 = area.95,
                               ud95_intersect_fishing = ud95_intersect, 
                               ud95_intersect_percentage = ud95_intersect_percentage
      )
      
      # append into general summary results
      results <- rbind(results, results_id)
      
      # export raster files ------ 
      # 1) fishing interact
      rst_file <- paste0(output_data,"/",organismID,"_2d_kde_fishing_interact_",fishing_gear,"_",dn,".tif")
      writeRaster(fishing_interact, rst_file, overwrite = TRUE)
      
      # 2) kde fishing intersect
      rst_file <- paste0(output_data,"/",organismID,"_2d_kde_fishing_intersect_",fishing_gear,"_",dn,".tif")
      writeRaster(kde_fishing_intersect , rst_file, overwrite = TRUE)
      
      # 3) kde fishing difference
      rst_file <- paste0(output_data,"/",organismID,"_2d_kde_fishing_difference_",fishing_gear,"_",dn,".tif")
      writeRaster(kde_fishing_simdif, rst_file, overwrite = TRUE)
      
    }
  }   
  
}




# -----------------------------------------------------------------------------
# 3) export summary results
#    incorporate that as  supplementary material
file <- paste0(output_data,"/2d_kde_fishing_overlap_results_daynight.csv")
write.csv(results, file, row.names = FALSE)
View(results)

Sys.time() - t  # 11 min


