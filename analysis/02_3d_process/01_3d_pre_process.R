

#-------------------------------------------------------------------------------------
# 01_3d_pre-process  3D Kernel density

# By: Jessica Ruff and David March (2021) and,
# update and standarized by Javier Menéndez-Blázquez | @jmenblaz


#-------------------------------------------------------------------------------------
# This script pre processes the output of tracking analysis
# L1 ttdr and L2 locs files for further analysis


# 1) load data
# 2) pre Process data steps:

# · calculate vertical error in ttdr data
# · calculate horizontal error in locs data
# · reproject coordinates to metric system (from study area)
# · filter out data with NA depth data



# load libraries and functions

source("setup.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # custom functions for 3d process
# source("analysis/02_3d_process/fun/fun_ks3d.R")



# 1) Import processed tracking data --------------------------------------------
# Use L1 for ttdr files and L2 locs files (post-processed tracking data)

ttdr_files <- list.files(paste0(main_dir,"/input/tracking/ttdr/L1"), full.names = TRUE, pattern = "L1_ttdr.csv")
locs_files <- list.files(paste0(main_dir,"/input/tracking/loc/L2"), full.names = TRUE, pattern = "L2_loc.csv")

# extract ids for ptt from ttdr processed files (individual for sea-turtles)
organismIDs <- sub("_L1_ttdr\\.csv$", "", basename(ttdr_files))



# 2)  Pre-process tracking data for 3D analysis --------------------------------

t <- Sys.time()

cores <- detectCores() - 2
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i=1:length(organismIDs), .packages=c("dplyr", "lubridate", "akima", "sp")) %dopar% {  

  # info 
  cat("Processing individual:", i,"/",length(organismIDs))
  cat("\n")
  # organism ID
  organismID <- organismIDs[i]
  cat(" · organismID:", organismID)
  cat("\n")
  cat("\n")

  
  # import locs and ttdr data for this organismID or ptt --------
  ttdr <- paste0(main_dir,"/input/tracking/ttdr/L2/",organismID,"_L2_ttdr.csv")
  ttdr <- read.csv(ttdr, dec=",", head=TRUE)
  # parse / format time date for ttdr data
  ttdr$time <- lubridate::parse_date_time(ttdr$time, "Ymd HMS")
  
  # convert to numeric fields:
  ttdr <- ttdr |> mutate(across(c(depth_upper_error, depth_lower_error, depth, drange), as.numeric))
  
  
  ssm <- paste0(main_dir,"/input/tracking/loc/L2/",organismID,"_L2_loc.csv")
  ssm <- read.csv(ssm, dec=",", head=TRUE)
  # parse / format time date for L2 loc data
  ssm$time <- parse_date_time(ssm$time, "Ymd HMS")
  
  # convert to numeric fields
  # reduce the decimals number in the View(df), not affects to number stored
  ssm <- ssm |> mutate(across(c(longitude, latitude, lon.025, lon.975, lat.025, lat.975), as.numeric))
  
  
  # 2.1 Calculate vertical error from TTDR ----------------------------------
  # errors in meters (m)
  ttdr$z.error <- z.error(depth.upper = ttdr$depth_upper_error, depth.lower = ttdr$depth_lower_error)
  
  ## Calculate horizontal error from SSM data
  # errors in meters (m)
  ssm$xy.error <- xy.error(ssm)
  
  # Interpolate horizontal errors to TTDR data
  # TTDR with NA in timestamps produce erros in aspline function
  ttdr$xy.error <- aspline(x=(as.numeric(ssm$time)), y=(ssm$xy.error), xout=(as.numeric(ttdr$time)))$y
  
  # Resample TTDR data from 5 min to interpolated timesteps SSM (hours) 
  # (review parameter in 03_regularize_ssm.R and main_tracking_process.R)
  resamp <- resampTTDR(ttdr, ssm)
  ssm <- merge(ssm, resamp, by="time")
  

  # use planar projection for europe
  xy <- reproject(lon = ssm$longitude, lat = ssm$latitude, crs = "+init=epsg:3035")
  xy <- xy[,-3] # remove logi column # TRUE == reprojected
  
  # rename "x" and "y" columns from ssm file (L2)
  # reminder: Mercator estimated coordinates from apply State Space Models: fit_ssm()
  ssm <- ssm %>% rename(x_ssm = x,
                        y_ssm = y)
  
  # combine reproject coordinates with ssm data
  ssm <- cbind(ssm, xy)
  
  # Reproject to metric system -- 5 min ttdr
  ttdr <- ttdr |> mutate(across(c(longitude, latitude), as.numeric))
  xy <- reproject(ttdr$longitude, ttdr$latitude, crs = "+init=epsg:3035")
  xy <- xy[,-3] # remove logi column # TRUE == reprojected
  ttdr <- cbind(ttdr, xy)
  
  

  # Filter out NA data in depth
  ssm <- dplyr::filter(ssm, !is.na(depth_mean))
  ttdr <- dplyr::filter(ttdr, !is.na(depth)) # also filtered in L3 ttdr data
  
  # Remove extra objects not needed for enviroment
  rm(resamp, xy)
  
  
  # export pre-processed data loc and ttdr L3 level ------------------------
  
  # ssm or loc ---------
  output_data <- paste0(input_dir,"/","tracking/loc/L3")
  if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)
  
  L3_loc <- paste0(output_data, "/",organismID,"_L3_loc.csv")
  write.csv(ssm, L3_loc, row.names = FALSE)
 

  # ttdr -------------
  output_data <- paste0(input_dir,"/","tracking/ttdr/L3")
  if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)
  
  L3_ttdr <- paste0(output_data, "/",organismID,"_L3_ttdr.csv")
  write.csv(ttdr, L3_ttdr, row.names = FALSE)
  
  # info 
  message("Processing individual:", i,"/",length(organismIDs), " -- Finished -- \n")
  message(" · organismID:", organismID)
  message("\n")
  message("L3 TTDR and Locations data processed for 3D analysis")
  message("\n")
  message("\n")
  
}

Sys.time() - t # 4 min (6 cores)
stopCluster(cl)

# info 
cat("- 3D pre-process completed - \n
         -- L3 TTDR and Locations data processed")
message("\n")
message("\n")

