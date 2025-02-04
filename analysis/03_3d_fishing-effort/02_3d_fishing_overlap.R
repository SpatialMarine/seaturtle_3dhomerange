#------------------------------------------------------------------------------
## 3D Habitat Use of Loggerhead Turtles

## Created by Javier Menéndez-Blázquez | @jmenblaz

# Update package and standarized field names following Sequeria et al., 2021


# 1) load / import data 
# 2) process 3D overlap between fishing effort data and kde organism ID processed 
#   - 2.1 Load data for processing - kde and fishing effort 
#   - 2.2 Calculate the 3D overlap
#   - 2.3 Calculate the 3D overlap volumes



# ------------------------------------------------------------

# 0) Load libraries and create paths -----
library(mkde)
library(raster)

source("setup.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # custom functions for 3d process

# create ouput dir
output_data <- paste0(output_dir,"/03_fishing_3d_overlap")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


# ------------------------------------------------------------------------------
# 1) load import data                    ----------------------------------

# kde 3D results
kde_files <- list.files(paste0(main_dir,"/output/01_kde_3d/"), full.names = TRUE, pattern = "_3dmkde_obj_rstack.tif", recursive = TRUE)
# note: volumes in m3
kde_res <- read.csv(paste0(main_dir,"/output/01_kde_3d/kde_3d_res.csv"))
# transform to km3
kde_res$volume.50.km3 <- kde_res$volume.50/1000000000
kde_res$volume.75.km3 <- kde_res$volume.75/1000000000
kde_res$volume.95.km3 <- kde_res$volume.95/1000000000  

# different fishing gears (see 01_3d_fishing-overlap.R)
fishing_gears <- c("LL","TW")  # create a chr chain


# ------------------------------------------------------------------------------
# 2) process 3D overlap between fishing effort data and kde organism ID processed 

# t <- Sys.time()
# 
# cores <- detectCores() - 2
# cl <- makeCluster(cores)
# registerDoParallel(cl)
# 
# getDoParWorkers() # backend information

# empty dataframe to add results
results <- data.frame(organismID                   = character(),  
                      volume.50.km3                = numeric(),
                      volume.75.km3                = numeric(),
                      volume.95.km3                = numeric(),
                      volume.total.km3             = numeric(),
                      fishing_gear                 = character(),
                      udvol50_no_fishing_km3       = numeric(),
                      udvol50_intersect_fishing    = numeric(),
                      udvol50_intersect_percentage = numeric(),
                      udvol75_no_fishing_km3       = numeric(),
                      udvol75_intersect_fishing    = numeric(),
                      udvol75_intersect_percentage = numeric(),
                      udvol95_no_fishing_km3       = numeric(),
                      udvol95_intersect_fishing    = numeric(),
                      udvol95_intersect_percentage = numeric(),
                      stringsAsFactors = FALSE)



# proces overlap and calculate volumes for organism IDs (KDE files)

t <- Sys.time()

for (i in 1:length(kde_files)) {
  
  # 2.1) load data for processing - kde and fishing effort --------------
  kde <- kde_files[i]
  # extract organismID from L3_ttdr fiel name
  organismID <- sub("_3dmkde_obj_rstack\\.tif$", "", basename(kde))
  
  # info 
  cat("Processing 3D overlap with fisheries:", i,"/",length(kde_files))
  cat(" · organismID:", organismID, "\n")
  
  # load kde result from mkde 3d functions process
  # note: volumes in km3, transfortmed previously
  kde_res_id <- kde_res %>% filter(kde_res$organismID == !!organismID)
  
  # load 3D kde (rasterstack)
  kde <- raster::stack(kde)
  crs(kde) <- CRS("EPSG:3035")  # add CRS
  names(kde) <- paste("layer", 1:nlayers(kde), sep = ".")  # rename layers
  
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
        
    # load fishing data
    fishing <- raster::stack(paste0(main_dir,"/output/03_fishing_3d/",organismID,"_3d_fishing-effort_",fishing_gear,".tif"))
    crs(fishing) <- CRS("EPSG:3035")  # add CRS
    names(fishing) <- paste("layer", 1:nlayers(kde), sep = ".")  # rename layers
    # plot(fishing)
    
    # check min values without NA
    # min(values(fishing), na.rm = TRUE)
    
    
    # 2) calculate the 3D overlap ------------------------------------------
    
    # 2.1) calculate interaction between kde and fishing effort
    # multiply kde and fishing effort values to identify areas with high impact
    # high impact = high kde values and high fishing effort
    fishing_interact <- kde * fishing
      # plot(fishing_interact)
    
    # 2.2) fishing used as mask in raster::mask()
    # logic: remove voxels where there are fishing impact in order to calculate 
    #   - result: is the volumen affects by fishing activities 
    # (is necessary to calculate the difference between total volume or with threshold)
  
    kde_fishing_intersect <- raster::mask(kde, fishing)  # provide the UD volume of the impact
      # plot(kde_fishing_intersect)
      
    # kde without fishing area of impact(symmetrical difference)
    kde_fishing_simdif <- raster::mask(kde, fishing,  inverse=TRUE) # provide the UD volume without impact
      # plot(kde_fishing_simdif)
    
      
    # 2.3) calculate the 3D overlap volumes  ---------------------------------
    threshold.95 <- kde_res_id$threshold.95  
    threshold.75 <- kde_res_id$threshold.75
    threshold.50 <- kde_res_id$threshold.50
    
    z = 10 # depth meters per layer
    # calculate UD volumes after difference with fishing effort (freevolumen without fishing effort)
    # calculate_vol_stack (custom function in: fun_3d_utils.R)
    # volumes in m3
    volume.95 <- calculate_vol_stack(kde_fishing_simdif, z = z, threshold.95) 
    volume.75 <- calculate_vol_stack(kde_fishing_simdif, z = z, threshold.75)
    volume.50 <- calculate_vol_stack(kde_fishing_simdif, z = z, threshold.50)
    
    # m3 -> km3
    volume.95 <- volume.95/1000000000  
    volume.75 <- volume.75/1000000000
    volume.50 <- volume.50/1000000000
    
    # UD volume without intersect between fishing effort (kde free of interaction)
    udvol95_intersect <- (kde_res_id$volume.95.km3) - volume.95
    udvol95_intersect_percentage <- ((kde_res_id$volume.95.km3 - volume.95) / kde_res_id$volume.95.km3) * 100
    
    udvol75_intersect <- (kde_res_id$volume.75.km3) - volume.75
    udvol75_intersect_percentage <- ((kde_res_id$volume.75.km3 - volume.75) / kde_res_id$volume.75.km3) * 100
    
    udvol50_intersect <- (kde_res_id$volume.50.km3) - volume.50
    udvol50_intersect_percentage <- ((kde_res_id$volume.50.km3 - volume.50) / kde_res_id$volume.50.km3) * 100
    
    # for total volume (m3)
    volume_total      <- calculate_vol_stack(kde, z = z)
    volume_intersect  <- calculate_vol_stack(kde_fishing_intersect, z = z) # volume with fishing interaction
    volume_no_fishing <- volume_total - volume_intersect # volume witouth fishing interaction
    volume_intersect_percentage <- (volume_intersect / volume_total) * 100 # percentage over total vol
    
    # m3 -> km3
    volume_total      <- volume_total/1000000000
    volume_intersect  <- volume_intersect/1000000000
    volume_no_fishing <- volume_no_fishing/1000000000 
    
  
    # 2.4) save / export results ----------------------------------------------
    
    # append organismID result into summary dataframe
    results_id <- data.frame(organismID,
                             volume.50.km3 = kde_res_id$volume.50.km3,
                             volume.75.km3 = kde_res_id$volume.75.km3,
                             volume.95.km3 = kde_res_id$volume.95.km3,
                             volume.total.km3 = volume_total,
                             # overlap fishing results
                             fishing_gear = fishing_gear,
                             # UD 50
                             udvol50_no_fishing_km3 = volume.50,
                             udvol50_intersect_fishing = udvol50_intersect, 
                             udvol50_intersect_percentage = udvol50_intersect_percentage,
                             # UD 75
                             udvol75_no_fishing_km3 = volume.75,
                             udvol75_intersect_fishing = udvol75_intersect, 
                             udvol75_intersect_percentage = udvol75_intersect_percentage,
                             # UD 95
                             udvol95_no_fishing_km3 = volume.95,
                             udvol95_intersect_fishing = udvol95_intersect, 
                             udvol95_intersect_percentage = udvol95_intersect_percentage
                            )
    
    # append into general summary results
    results <- rbind(results, results_id)
    
    # export raster files ------ 
    # 1) fishing interact
    rst_file <- paste0(output_data,"/",organismID,"_3d_kde_fishing_interact_",fishing_gear,".tif")
    writeRaster(fishing_interact, rst_file, overwrite=TRUE)
    
    # 2) kde fishing intersect
    rst_file <- paste0(output_data,"/",organismID,"_3d_kde_fishing_intersect_",fishing_gear,".tif")
    writeRaster(kde_fishing_intersect , rst_file, overwrite=TRUE)
    
    # 3) kde fishing difference
    rst_file <- paste0(output_data,"/",organismID,"_3d_kde_fishing_difference_",fishing_gear,".tif")
    writeRaster(kde_fishing_simdif, rst_file, overwrite=TRUE)
    
  }
}   


Sys.time() - t # 20 mins

# -----------------------------------------------------------------------------
# 3) export summary results
#    incorported as supplementary material
file <- paste0(output_data,"/3d_kde_fishing_overlap_results.csv")
write.csv(results, file, row.names = FALSE)
View(results)





 
