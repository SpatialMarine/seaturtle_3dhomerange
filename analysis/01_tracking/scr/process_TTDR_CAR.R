#--------------------------------------------------------------------------------
# process_TTDR_CAR.R
#--------------------------------------------------------------------------------
# Process pressure and temperature data from WC tags configured using time seies
#
# TTDR processing includes the following steps:
# 1. Data standardization
# 2. Zero offset correction. + Identfication of ascent and descent phases
# 3. Estimation of depth and temperature errors
# 4. Temperature QC (regional test)
# 5. Interpolate locations from SSM
#
# Then, other scripts can be followed:
# 2. SST product and QC to detected insolation events.
# 3. Diving analysis
# 4. Derive MLD product
# 5. Compare SST and MLD with other products
# Custom functions are found in "scr/fun_ttdr"


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
input_data <- paste0(input_dir, "/tracking/", sp_code)
location_data <-  paste0(output_dir, "/tracking/", sp_code, "/L2_locations")
output_data <- paste0(output_dir, "/tracking/", sp_code, "/TTDR")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)




#---------------------------------------------------------------
# 2. Process metadata
#---------------------------------------------------------------

# import metadata
metafile <- paste(input_data, "TODB_2020-10-30.xlsx", sep="/")
meta <- readTurtleDB(metafile, endRow=95)

# rename variables and set deployment date to Date class
db <- meta %>% rename(id = ptt)

# select turtles with TTDR data
db <- filter(db, wc_time_series == "y")


#---------------------------------------------------------------
# 3. Process TTDR
#---------------------------------------------------------------

cl <- makeCluster(10)
registerDoParallel(cl)

## Process delayed mode (location data)
#for (i in 17:nrow(db)){
foreach(i=1:nrow(db), .packages=c("dplyr", "stringr", "lubridate", "diveMove", "move", "foieGras")) %dopar% {
  
  print(paste("Processing tag", i, "of", nrow(db)))
  
  ## get tag information
  id <- db$id[i]
  
  # Import data
  loc_file <- list.files(paste0(input_data, "/wc/", id), full.names=TRUE, pattern = "^\\w+-Series\\.csv$")
  data <- read.csv(loc_file)
  
  #-------------------------------
  # Step 1. Data standardization
  #-------------------------------
  
  # Standardize data
  # Need to keep the file for diveMove
  ttdr <- wc2ttdr(data, date_deploy = db$date_deploy[i], tfreq = "5 min")  
  ttdr_file <- paste0(output_data, "/", id, "_L0_ttdr.csv")
  write.csv(ttdr, ttdr_file, row.names = FALSE)
  
  
  #-------------------------------
  # Step 2. Zero offset correction
  #-------------------------------
  
  # remove duplicates
  # priority to registers with [depth & temp] > [depth] > [temp]
  if (any(duplicated(ttdr$date))){
    ttdr$idx <- 1:nrow(ttdr)
    dups <- ttdr %>% group_by(date) %>% filter(n()>1) %>% filter(is.na(depth))
    ttdr <- ttdr[-dups$idx,]
    ttdr <- dplyr::select(ttdr, -idx)
  }
  
  ##Create a TDR class##
  #TDR is the simplest class of objects used to represent Time-Depth recorders data in diveMove.
  tdrdata <- createTDR(time = ttdr$date, depth = ttdr$depth,
                       dtime = 300,  # sampling interval (in seconds)
                       file = ttdr_file)  # path to the file
  
  ## Calibrate with ZOC using filter method.
  #The method consists of recursively smoothing and filtering the input time series 
  #using moving quantiles.It uses a sequence of window widths and quantiles, and starts
  #by filtering the time series using the first window width and quantile in the specified
  #sequences 
  dcalib <-calibrateDepth(tdrdata,
                          wet.thr = 3610,  # (seconds) At-sea phases shorter than this threshold will be considered as trivial wet.Delete periods of wet activity that are too short to be compared with other wet periods.
                          dive.thr = 3,    # (meters) threshold depth below which an underwater phase should be considered a dive.
                          zoc.method ="filter",  # see Luque and Fried (2011)
                          k = c(12, 240),  # (60 and 1200 minutes) Vector of moving window width integers to be applied sequentially.
                          probs = c(0.5, 0.05),   # Vector of quantiles to extract at each step indicated by k (so it must be as long as k)
                          depth.bounds = c(-5, 15),  # minimum and maximum depth to bound the search for the surface.
                          na.rm = TRUE)
  
  # save dcalib object for further analysis
  #saveRDS(dcalib, paste0(output_data, "/", id, "_dcalib.rds"))
  
  ## Incorporate adjusted depth and offset
  ttdr <- cbind(ttdr, depth_adj = dcalib@tdr@depth, depth_offset = ttdr$depth - dcalib@tdr@depth)
  
  
  #-------------------------------
  # Step 3. QC for depth
  #-------------------------------
  
  # Calculate depth error
  d_error <- depth_error(depth = ttdr$depth_adj, drange = ttdr$drange)
  ttdr$depth_upper_error <- round(d_error$upper.error, 2)
  ttdr$depth_lower_error <- round(d_error$lower.error, 2)
  
  
  #-------------------------------
  # Step 4. QC for temperature
  #-------------------------------
  
  # Calculate temperature error
  ttdr$temp_error <- temp_error(ttdr$trange)
  
  # Temperature regional range test (Mediterranean Argo)
  # 1: good data; 4: bad data
  ttdr$temp_qc_rr <- trange_test(ttdr$temperature, ttdr$temp_error, tmin=10, tmax=40)
  
  
  #-------------------------------
  # Step 5. Derive diving metrics
  #-------------------------------

  ## dive.activity: data frame with details about all dive and postdive periods found"dive.id",
  #"dive.activity", and "postdive.id"
  #L:dry, W:wet, U:underwater, D:diving, Z:brief wet 
  dive_act <- getDAct(dcalib)
  
  #dive.phases: This identifies each reading with a particular dive phase. Thus, each reading belongs to one
  #of descent (D), descent/bottom (DB), bottom (B), bottom/ascent (BA), and ascent (A) phases.
  dive_phases <- getDPhaseLab (dcalib)
  
  ## Merge diving data with Time Series
  ttdr <- cbind(ttdr, dive_act, dive_phases)
  
  
  #-------------------------------
  # Step 6. QC for SST
  #-------------------------------
  
  #-------------------------------
  # 5.1. Generate SST product
  #-------------------------------
  ### Generate SST product
  ### Select first records for each postdive.
  ### We asume that for these records there is no insolation effect.
  ### This function remove first 3 dives and select data
  sst <- getSST(ttdr)  
  
  ### Interpolate SST to TimeSeries
  ttdr$sst <- approx(x = sst$date, y = sst$temperature, xout = ttdr$date, method="linear", rule=2)$y
  ttdr$sst <- round(ttdr$sst, digits=1)
  ttdr$sst_qc <- 8  # interpolated data
  ttdr$sst_qc[ttdr$date %in% sst$date] <- 1  # good data, no interpolated
  
  ### SST product
  #L1_sst <- select(data, ptt, date, sst, sst_qc)
  
  
  #-------------------------------
  # Temperature above SST test
  #-------------------------------
  ### This test identies temperature records that are above the estimated SST, given a temperature threshold
  ### We define a temperature threshold of 2 degrees
  ### We do not select points on the surface because there may be no depth available when having temperature data
  ### 4: bad data; 1: good data
  ttdr$above_sst_qc <- aboveSST(temp = ttdr$temperature, temp.er = ttdr$temp_error,
                                sst = ttdr$sst, temp.thr = 1)
  

  
  #-------------------------------
  # Step 7. Add locations
  #-------------------------------
  loc_file <- paste0(location_data, "/", id, "_L2_locations.csv")
  loc_filt <- readTrack(loc_file)
  
  # ### SSM at 2 hours -----------------------------------------
  # 
  # # convert to foieGras format
  # indata <- data %>%
  #   dplyr::select(id, date, lc, lon, lat, smaj, smin, eor) #%>%
  #   #rename(id = trip) 
  # 
  # # filter location class data with NA values
  # # very few cases, but creates an error in fit_ssm
  # indata <- dplyr::filter(indata, !is.na(lc))
  # 
  # # fit SSM
  # # we turn sdafilter off because we previously filtered data
  # # we run the model with multiple trips at once
  # fit <- fit_ssm(indata, model = "crw", time.step = 2, verbose = 0, spdf = FALSE)
  # 
  # # get fitted locations
  # # segments that did not converge were not consider
  # loc_filt <- data.frame(grab(fit, what = "predicted", as_sf = FALSE))
  # 
  
  ### Interpolate -----------------------------------------
  
  # No extrapolation is performed. So check that time stamps in series are within time domain of loc
  ttdr <- filter(ttdr, date >= min(loc_filt$date) & date <= max(loc_filt$date))
  
  # convert location data to move class
  loc_filt_move <- move(x=loc_filt$lon, y=loc_filt$lat, time=loc_filt$date, data=loc_filt,
                        proj=CRS("+proj=longlat +ellps=WGS84"), animal=loc_filt$id)
  
  # linear interpolation
  lint <- interpolateTime(loc_filt_move, time = ttdr$date, spaceMethod = "greatcircle")
  
  # incorporate coordinates to TTDR data.frame
  ttdr$lon <- lint@coords[,1]
  ttdr$lat <- lint@coords[,2]
  
  
  #-------------------------------
  # Step 8. Add day/night
  #-------------------------------
  ttdr$daynight <- daynight(lon = ttdr$lon, lat = ttdr$lat, time = ttdr$date)
  
  
  #-------------------------------
  # Export data
  #-------------------------------
  ttdr_file <- paste0(output_data, "/", id, "_L1_ttdr.csv")
  write.csv(ttdr, ttdr_file, row.names = FALSE)
}

stopCluster(cl)



#---------------------------------------------------------------
# 4. Summarize TTDR
#---------------------------------------------------------------

# Import all ttdr. readTrack
ttdr_files <- list.files(output_data, full.names=TRUE, pattern = "_L1_ttdr.csv")
data <- readTrack(ttdr_files)

# Summarize TTDR data
df <- data %>%
  arrange(date) %>%  # order by date
  group_by(id) %>%  # select group info
  summarize(date_deploy = first(date),
            lon_deploy = first(lon),
            lat_deploy = first(lat),
            date_last_uplink = last(date),
            duration_d = round(difftime(date_last_uplink, date_deploy, units="days")),
            distance_km = round(sum(distGeo(p1 = cbind(lon, lat)), na.rm=TRUE)/1000),
            max_depth = max(depth_adj, na.rm=TRUE),
            n_dives = length(unique(dive.id))-1)  # removes 1 dive that corresponds to surface


# Combine with metadata
db <- db %>%
          dplyr::select(id, scientific_name, ccl, life_stage, gender, type_capture,
                        tag_manufacturer, tag_type, gps_sensor)

df <- merge(db, df, by="id")


# export table
out_file <- paste0(output_data, "/", sp_code, "_summary_ttdr.csv")
write.csv(df, out_file, row.names = FALSE)


#---------------------------------------------------------------
# 5. Prepare static and dynamic plots
#---------------------------------------------------------------

