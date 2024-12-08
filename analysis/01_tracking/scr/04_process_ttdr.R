#--------------------------------------------------------------------------------
# 04_process_ttdr.R
#--------------------------------------------------------------------------------

# Process pressure and temperature data from WC tags configured using time series

# TTDR processing includes the following steps:
# 1. Data standardization
# 2. Zero offset correction + Identification of ascent and descent phases
      # Luque & Fried (2011) Zero Offset Correction of Diving Depth Time (R Package diveMove)
# 3. Estimation of depth and temperature errors
# 4. Temperature QC (regional test)
# 5. Interpolate locations from SSM 
# 6. Day and night information

# Then, other scripts can be followed:
# 2. SST product and QC to detected insolation events.
# 3. Diving analysis
# 4. Derive MLD product
# 5. Compare SST and MLD with other products
# Custom functions are found in "scr/fun_ttdr"



# Original scripts from D.March (@dmarch)
# Update and stadarization made it by J.Menéndez-Blázquez (@jmenblaz) 
# based in Sequeira et al., 2021



#---------------------------------------------------------------
# 1. Set data repositories
 
location_data <- paste0(input_dir,"/tracking/loc/L2")
output_data <- paste0(input_dir,"/tracking/ttdr/L0")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)



#---------------------------------------------------------------
# 2. Process metadata

# import metadata (no L2 metadata created)
# same metadata used for created L2 locs files
metadata <- read.csv(paste0(input_dir,"/tracking/loc/L1/metadataL1.csv"))

# El origen para el formato de fecha de Excel es 1899-12-30, no 1900-01-01, 
# ya que Excel tiene un pequeño error en su cálculo de fechas
# (considera el 1900 como un año bisiesto cuando no lo es).

metadata$deploymentDateTime <- as.Date(metadata$deploymentDateTime, origin="1899-12-30")


# avoid ids with processing issues
ids <- c(200043, 200045, 235396)
# filter data 
metadata <- metadata[!(metadata$organismID %in% ids), ]


#---------------------------------------------------------------
# 3. Process TTDR


# cores <- detectCores() - 2
# cl <- makeCluster(cores)
# registerDoParallel(cl)


## Process delayed mode (location data)

t <- Sys.time()

for (i in 1:nrow(metadata)){
  #foreach(i=1:nrow(db), .packages=c("dplyr", "stringr", "lubridate", "diveMove", "move", "aniMotum")) %dopar% {
  
  # info
  cat("Processing tag", i, "of", nrow(metadata))
  cat("\n")
  # organism ID
  organismID <- metadata$organismID[i]
  cat(" · organismID:", organismID)
  cat("\n")
  cat("\n")
  
  
  # Import ttdr raw data from Argos data by organismID (input_dir from setup.R)
  ttdr_file <- list.files(input_dir, recursive=TRUE, full.names=TRUE, pattern = sprintf("%s-Series.csv", organismID))
  data <- read.csv(ttdr_file)
  
  #-----------------------------------------------
  # Step 1. Data standardization
  
  # Standardize data
  # Need to keep the file ttdr for diveMove R package
  # Use custom function wc2ttdr (fun_ttdr.R in tracking/fun)
  ttdr <- wc2ttdr(data, locale = locale, date_deploy = metadata$deploymentDateTime[i], tfreq = "5 min")
  
  # export raw or L0_ttdr.csv
  ttdr_file <- paste0(output_data, "/", organismID, "_L0_ttdr.csv")
  write.csv(ttdr, ttdr_file, row.names = FALSE)
  
  
  #-------------------------------------------------
  # Step 2. Zero offset correction
  
  # remove duplicates
  # priority to registers with [depth & temp] > [depth] > [temp]

  # Detect duplicates in the time column.
  # Identify those duplicates where depth is NA.
  # Remove those specific duplicate rows while keeping the others.
  # Restore the data.frame without the auxiliary idx column.
  
  if (any(duplicated(ttdr$time))){
    ttdr$idx <- 1:nrow(ttdr)
    dups <- ttdr %>% group_by(time) %>% filter(n()>1) %>% filter(is.na(depth))
    if (nrow(dups) > 0) {
      ttdr <- ttdr[-dups$idx,]
      ttdr <- dplyr::select(ttdr, -idx)
    } else { # it could be that the first filter gives 0 obs (no NA, in duplicates)
      dups <- ttdr %>% group_by(time) %>% filter(n()>1) %>% filter(!is.na(depth))
      ttdr <- ttdr[-dups$idx,]
      ttdr <- dplyr::select(ttdr, -idx)  
    }
  }

  # Create a TDR class ------

  # TDR is the simplest class of objects used to represent Time-Depth recorders data in diveMove.
  tdrdata <- diveMove::createTDR(time = ttdr$time, depth = ttdr$depth,
                                 dtime = 300,  # sampling interval (in seconds)
                                 file = ttdr_file)  # path to the file L0 ttdr
  
  ## Calibrate with Zero Offset Correction (ZOC) using filter method
  # The method consists of recursively smoothing and filtering the input time series 
  # using moving quantiles. It uses a sequence of window widths and quantiles, and starts
  #by filtering the time series using the first window width and quantile in the specified
  #sequences 
  
  dcalib <- diveMove::calibrateDepth(tdrdata,
                                    wet.thr = 3610,   # (seconds) At-sea phases shorter than this threshold will be considered as trivial wet.Delete periods of wet activity that are too short to be compared with other wet periods.
                                    dive.thr = 3,     # (meters) threshold depth below which an underwater phase should be considered a dive.
                                    zoc.method = "filter",   # see Luque and Fried (2011)
                                    k = c(12, 240),   # (60 and 1200 minutes) Vector of moving window width integers to be applied sequentially.
                                    probs = c(0.5, 0.05),    # Vector of quantiles to extract at each step indicated by k (so it must be as long as k)
                                    depth.bounds = c(-5, 15),   # minimum and maximum depth to bound the search for the surface.
                                    na.rm = TRUE)

  # save dcalib object for further analysis (export .rds)
  output_data <- paste0(input_dir,"/tracking/ttdr/L1")
  if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)
  
  saveRDS(dcalib, paste0(output_data,"/",organismID,"_dcalib.rds"))
  
  # Incorporate adjusted depth and Zero Offset Correction
  ttdr <- cbind(ttdr, depth_adjusted = dcalib@tdr@depth, depth_offset = ttdr$depth - dcalib@tdr@depth)
  
  
  # ----------------------------------------------------------------------
  # Quality control for depth and temperature records
  
  # -------------------------------
  # Step 3. QC for depth

  # Calculate depth error (upper and lower)
  d_error <- depth_error(depth = ttdr$depth_adjusted, drange = ttdr$drange)
  ttdr$depth_upper_error <- round(d_error$upper.error, 2)
  ttdr$depth_lower_error <- round(d_error$lower.error, 2)
  
  
  #-------------------------------
  # Step 4. QC for temperature

  # Calculate temperature error
  ttdr$temp_error <- temp_error(ttdr$trange)
  
  # Temperature regional range test (Mediterranean Argo)
  # 1: good data; 4: bad data
  ttdr$temperature_qc1 <- trange_test(ttdr$temperature, ttdr$temp_error, tmin=10, tmax=40)
  
  #-------------------------------
  # Step 5. Derive diving metrics

  ## dive.activity: data frame with details about all dive and postdive periods found"dive.id",
  #"dive.activity", and "postdive.id"
  #L:dry, W:wet, U:underwater, D:diving, Z:brief wet 
  dive_act <- getDAct(dcalib)
  
  #dive.phases: This identifies each reading with a particular dive phase. Thus, each reading belongs to one
  #of descent (D), descent/bottom (DB), bottom (B), bottom/ascent (BA), and ascent (A) phases.
  dive_phases <- getDPhaseLab (dcalib)
  
  ## Merge diving data with Time Series ttdr data
  ttdr <- cbind(ttdr, dive_act, dive_phases)
  
  
  #-------------------------------
  # Step 6. QC for SST

  #-------------------------------
  # 6.1 - Generate SST product
  #-------------------------------
  ### Generate SST product
  ### Select first records for each post dive.
  ### We asume that for these records there is no insolation effect
  
  ### This function remove first 3 dives and select data
  sst <- getSST(ttdr)  
  
  ### Interpolate SST to TimeSeries
  ttdr$sst <- approx(x = sst$time, y = sst$temperature, xout = ttdr$time, method="linear", rule=2)$y
  ttdr$sst <- round(ttdr$sst, digits=1)
  ttdr$sst_qc <- 8  # 8 values means == interpolated data 
  ttdr$sst_qc[ttdr$time %in% sst$time] <- 1  # good data, no interpolated == 1
  
  ### SST product
  #L1_sst <- select(data, ptt, date, sst, sst_qc)
  
  #-------------------------------
  # 6.2 - Temperature above SST test
  #-------------------------------
  ### This test identifies temperature records that are above the estimated SST, given a temperature threshold
  ### We define a temperature threshold of 2 degrees
  ### We do not select points on the surface because there may be no depth available when having temperature data
  ### 4: bad data; 1: good data
  ttdr$temp_above_sst_qc <- aboveSST(temp = ttdr$temperature, temp.er = ttdr$temp_error,
                            sst = ttdr$sst, temp.thr = 1)
  
  
  
  #-------------------------------
  # Step 7. Add locations
  #-------------------------------
  loc_file <- paste0(location_data, "/", organismID, "_L2_loc.csv")
  loc_filt <- read.csv(loc_file)
  loc_filt$time <- parse_date_time(loc_filt$time, "Ymd HMS") # parse time

  # Calculate 95% confidence interval of location error
  # SSM provides standard errors in km
  # Calculate SE average between lon and lat
  
  # Estimate 95% confidence interval
  loc_filt$xy_error <- 1.96*1000*((loc_filt$x.se +  loc_filt$y.se)/2)  # return in m
  
  
  # Interpolate -----------------------------------------
  
  # Interpolate L2 data to each ttdr timestamp record
  # == create a new location position by each 5 min (ttdr series setting)
  
  # No extrapolation is performed. So check that timestamps in series are within time domain of loc
  ttdr <- dplyr::filter(ttdr, time >= min(loc_filt$time) & time <= max(loc_filt$time))
  
  # linear interpolation
  # convert location data to move class
  loc_filt_move <- move(x=loc_filt$longitude, y=loc_filt$latitude, time=loc_filt$time, data=loc_filt,
                        proj=CRS("+proj=longlat +ellps=WGS84"), animal=loc_filt$organismID)
  # interpolation
  lint <- interpolateTime(loc_filt_move, time = ttdr$time, spaceMethod = "greatcircle")
  
  # incorporate coordinates to TTDR data.frame
  ttdr$longitude <- lint@coords[,1]
  ttdr$latitude <- lint@coords[,2]
  
  # Interpolate movement persistence metric to TTDR data
  # use akima R package for interpolate movement persintence metric (mpm)
  ttdr$mpm <- akima::aspline(x=(as.numeric(loc_filt$time)), y=(loc_filt$g), xout=(as.numeric(ttdr$time)))$y
  
  
  #-------------------------------
  # Step 8. Add day/night information per position and timestamp
  ttdr$daynight <- daynight(lon = ttdr$longitude, lat = ttdr$latitude, time = ttdr$time)
  
  ttdr <- ttdr %>% rename(organismID = id)
  #-------------------------------
  # save / export data
  output_data <- paste0(input_dir,"/tracking/ttdr/L1")
  if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)
  
  ttdr_file <- paste0(output_data, "/", organismID, "_L1_ttdr.csv")
  write.csv(ttdr, ttdr_file, row.names = FALSE)
}




#---------------------------------------------------------------
# 4. report errors in depth and temperature measure by tag
#    clean records with no time(date) and depth records

# Import all L1 ttdr (readTrack function)
output_data <- paste0(input_dir,"/tracking/ttdr/L1")
ttdr_files <- list.files(output_data, full.names=TRUE, pattern = "_L1_ttdr.csv")

ttdr_errors <- list()

for (i in 1:length(ttdr_files)) {
  # import L1 ttdr data
  L1 <- readTrack(ttdr_files[i])
  organismID <- sub("_L1_ttdr.csv$", "", basename(ttdr_files[i]))
  
  # Number of records without time (date) info
  time_na_count <- sum(is.na(L1$time))
  depth_na_count <- sum(is.na(L1$depth)) 
  temp_na_count <- sum(is.na(L1$temperature))
  depth_temp_na_count <- sum(is.na(L1$depth) & is.na(L1$temperature))
  # % respect total records
  time_na_percentage <- (time_na_count*100)/nrow(L1)
  depth_na_percentage <- (depth_na_count*100)/nrow(L1)
  temp_na_percentage <- (temp_na_count*100)/nrow(L1)
  depth_temp_na_percentage <- (depth_temp_na_count*100)/nrow(L1)
  
  # L2 ttdr process, remove records with NA in time and depth
  L2 <- L1 %>% filter(!is.na(time))
  L2 <- L2 %>% filter(!is.na(depth))
                    
  ttdr_error_organism <- data.frame(organismID = organismID,
                                    ttdr_L1_records = nrow(L1),
                                    ttdr_L2_records = nrow(L2),
                                    time_na_count = time_na_count,
                                    time_na_percentage = time_na_percentage,
                                    depth_na_count = depth_na_count,
                                    depth_na_percentage = depth_na_percentage,
                                    temp_na_count = temp_na_count,
                                    temp_na_percentage = temp_na_percentage,
                                    depth_temp_na_count = depth_temp_na_count,
                                    depth_temp_na_percentage = depth_temp_na_percentage)
  
  # append to ttdr erros 
  ttdr_errors[[i]] <- ttdr_error_organism 
  
  # export save L2 ttdr files with NA records:
  output_data <- paste0(input_dir,"/tracking/ttdr/L2")
  if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)
  
  ttdr_file <- paste0(output_data, "/",organismID,"_L2_ttdr.csv")
  write.csv(L2, ttdr_file, row.names = FALSE)
  
}

ttdr_errors <- do.call(rbind, ttdr_errors)

ttdr_file <- paste0(output_data, "/","L1_summary_ttdr_errors.csv")
write.csv(ttdr_errors, ttdr_file, row.names = FALSE)




#---------------------------------------------------------------
# 5. Summarize TTDR

# metadata imported previously

# Import all ttdr. readTrack
output_data <- paste0(input_dir,"/tracking/ttdr/L2")
ttdr_files <- list.files(output_data, full.names=TRUE, pattern = "_L2_ttdr.csv")
data <- readTrack(ttdr_files)

# Summarize TTDR data
df <- data %>%
  arrange(time) %>%  # order by date
  group_by(organismID) %>%  # select group info
  summarize(date_deploy = first(time),
            lon_deploy = first(longitude),
            lat_deploy = first(latitude),
            date_last_uplink = last(time),
            duration_d = round(difftime(date_last_uplink, date_deploy, units="days")),
            distance_km = round(sum(distGeo(p1 = cbind(longitude, latitude)), na.rm=TRUE)/1000),
            max_depth = max(depth_adjusted, na.rm=TRUE),
            n_dives = length(unique(dive.id))-1)  # removes 1 dive that corresponds to surface

# Combine with metadata
# fields standarized following Sequeira et al. (2021)
metadata <- metadata %>%
  dplyr::select(organismID, scientificName, organismSize1, organismSizeMeasurementType1, 
                organismSex, instrumentType, instrumentModel)

df <- merge(metadata, df, by="organismID")

# export table
out_file <- paste0(output_data, "/","L2_summary_ttdr.csv")
write.csv(df, out_file, row.names = FALSE)



# -----------------------------------------------------------------------------

t - Sys.time() # 2:20 hours apróx

# stopCluster(cl) # Stop cluster
print("Process TTDR data finished - L0, L1 and L2 processing levels")






