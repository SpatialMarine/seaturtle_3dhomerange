#-------------------------------------------------------------------------------------
# 02_filter_locs. R
#-------------------------------------------------------------------------------------
# These script processes animal tracking data following a common approach between
# different species


# Main steps are:
# - Selection of tracks given a defined criteria
# - Filter location data: Near-duplicate positions, filter, angle and point on land
# - Different processing for PTT/GPS (eg. time gaps, LC class)



# Common workflow based in Sequeira et al., 2021
# A standardisation framework for bio‐logging data to advance ecological research and conservation
# Stadarization made it by J.Menéndez-Blázquez - @jmenblaz


# Based in the original scripts of D.March (@dmarch), update by J.Menéndez-Blázquez (@jmenblaz)



#-------------------------------------------------------------------------------
# 1. Set data repository                 ------------------------------------

input_data  <- paste0(input_dir,"/tracking/loc/L0")
output_data <- paste0(input_dir,"/tracking/loc/L1")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)



#------------------------------------------------------------------------------
# 2. Import L0 locs and metadata          ---------------------------------

# import all location files
loc_files <- list.files(input_data, full.names = TRUE, pattern = "L0_loc.csv")
df <- readTrack(loc_files) #custom function fun_track_proc.R
# L0 metadata  processed in previously scritp (01_preproc_locs.R) 
metadata <- read.csv(paste0(input_data,"/metadataL0.csv"))




# ------------------------------------------------------------------------------
# 3. Filter locs                -------------------------------------------

# extract organismID from L0_loc.csv
organismIDs <- unique(df$organismID)

# cores <- detectCores() - 1
# cl <- makeCluster(cores)
# registerDoParallel(cl)

t <- Sys.time()

# foreach (i = 1:length(organismIDs), .packages = c("dplyr", "ggplot2", "gridExtra", "grid", 
#                                                  "data.table", "argosfilter", "stringr", "SDLfilter")) %dopar% {

for (i in 1:length(organismIDs)) {
  
  # info 
  cat("Processing tag", i,"of",length(organismIDs))
  cat("\n")
  id <- organismIDs[i]
  cat(" · organismID:", id)
  cat("\n")
  cat("\n")
  
  # filter organism metatatda
  organism_meta <- metadata %>% filter(organismID == id)
  # instrument or type (Argos, etc)
  instr <- organism_meta$instrumentType
  
  # import L0 location data
  infile <- sprintf(paste0(input_data,"/","%s_L0_loc.csv"), id)
  data <- read.csv(infile)
  
  # parse time - it should be parsed in preproc (L0)
  data$time <- parse_date_time(data$time, "Ymd HMS") # parse time
  

  # set filter parameters according to taxonomic group
  # trip_time_gap -- see "_main_tracking_proces.R"
  
  # ----------------------------------------------------------------------------
  # 3.1) Trim tracks into segments (trim tracks into different trips) 
  
  # Tracks with data gaps in excess of a given time are broken up for separate modeling
  data$tripID <- timedif.segment(data$time, thrs = trip_time_gap)
  
  # rename trips: organismID_tripID (e.g.: 138120_001)
  data$tripID <- paste(id, str_pad(data$trip, 3, pad = "0"), sep="_")
  
  # Select trips according to multiple criteria
  # summarize data per trip (custom function sumarizeTrips - fun_tracj_proc.R)
  trips <- animalsensor::summarizeTrips(data = data, id = "organismID", trip = "tripID", date ="time", lon = "longitude", lat = "latitude")

  # filter trips
  trips <- filter(trips,
                  duration_h >= sel_min_dur,
                  n_loc >= sel_min_loc,
                  distance_km >= sel_min_dist, 
                  !id %in% sel_exclude)
  
  # ----------------------------------------------------------------------------
  # 3.2) filter location L0 per trip identified        
  # select trips

  trip_list <- unique(trips$trip)
  if(length(trip_list) == 0) next   # next organismID (ptt, individual, tag...)
  
  # empty list to append data for filtered data per trip
  trip_data <- list()
  
  for (j in seq_along(trip_list)) {
    
    # filter L0 locs by trip identified
    sdata <- filter(data, tripID == trip_list[j])
    
    # 0). Common process for all instruments (Argos ptt, GPS ...) -----------
    #    for marine environment
    
    # Filter points on land
    # see main_tracking_process.R parameters
    # Note: usually keep all the position for have more points to recreated
    #       State Space Models (SSM; 03_regularize_ssm.R)
    # After applied the SSM using aniMotum R package the land position could be
    # reprocessed for aquatic or marine environments
    
    if(filt_land == TRUE){
      sdata$onland <- point_on_land(lat = sdata$lat, lon = sdata$lon, land = land)
      sdata <- filter(sdata, onland == FALSE)
    }
    
    # 3.2.1). filter locations by different class of instrument -------------
    # 3.2.1.1)  ARGOS geolocation 
    
      if(instr == "Argos"){
        
        # Set params: see main_tracking_process
            # filt_step_time 
            # filt_step_dist

        # Remove near-duplicate positions
        # default valus of filter_dup 
        #   - step.time = 2/60
        #   - step.dist = 0.001
        sdata <- filter_dup(sdata, step.time = filt_step_time, step.dist = filt_step_dist)
        
        # Filter out Z location classess
        sdata <- filter(sdata, argosLC != "Z")
        
        # Filter positions by speed and angle for PTT
        sdata$argosfilter <- sdafilter(lat = sdata$latitude,
                                       lon = sdata$longitude,
                                       dtime = sdata$time,
                                       lc = sdata$argosLC,
                                       vmax = filt_vmax, # in m/s
                                       ang = filt_ang, # No spikes are removed if ang=-1
                                       distlim = filt_distlim)
        
        # select position that are not removed by the filter
        sdata <- dplyr::filter(sdata, argosfilter == "not")
      }
    
   
      # 3.2.1.2)   GPS geolocation
      # Filter speed for GPS
      if (instr == "GPS"){
        sdata <- filter_speed(sdata, vmax = (filt_vmax * 3.6), method = 1)
      }
    
    # append to trips data list
    trip_data[[j]] <- sdata
  }
  
  
  # 3.3). Combine data from multiple trips -------------------------------------
  dataL1 <- rbindlist(trip_data)
  
  #  export / save track data into individual folder at output path
  out_file <- paste0(output_data, "/", id, "_L1_loc.csv")
  write.csv(dataL1, out_file, row.names = FALSE)
  
  # 3.4) plot map and histogram L1 info ----------------------------------------
  
  # plot map of the L1 track position showing the tiem difference between the 
  # beggining and the end of the track
  p1 <- mapL1(dataL1) 
  
  # plot histogram
  #'  The diffTimeHisto function generates a histogram that shows 
  #'  the distribution of time differences between observations in a dataset.

  if(instr == "Argos") p2 <- diffTimeHisto(dataL1, vline=24)
  if(instr == "GPS") p2 <- diffTimeHistoGPS(dataL1, vline=0.08)
  
  # combine plots
  lay <- rbind(c(1,1),
               c(1,1),
               c(2,2))
  
  p <- grid.arrange(p1, p2, layout_matrix = lay)
  
  # export multi-panel plot
  out_file <- paste0(output_data, "/", id, "_L1_loc.png")
  ggsave(out_file, p, width=30, height=15, units = "cm")
  
}




#------------------------------------------------------------------------------
# 4. Update L0 metadata

# L0 metadata previously 

# import all L1 location data
data <- list.files(output_data, full.names = TRUE, recursive = TRUE, pattern = "L1_loc.csv")
data <- rbindlist(lapply(data, fread), fill=TRUE)

# Find deployment from metadata that were not processed
sel <- which(metadata$organismID %in% data$organismID)
selected <- metadata[sel,]
discarded <- metadata[-sel,]

# Export L1 metadata
write.csv(selected, paste0(output_data, "/metadataL1.csv"), row.names=F)
write.csv(discarded, paste0(output_data, "/discardedL1.csv"), row.names=F)



# -----------------------------------------------------------------------------
Sys.time() - t
stopCluster(cl)

cat(" - L1 tracking process ready - ")







