
#--------------------------------------------------------------------------------
# 05_process_dives.R
#--------------------------------------------------------------------------------

#' This script use the postprocess L1 ttdr data to obtain differenrt dives for
#' seaturtle

# Stadarization made it by J.Menéndez-Blázquez (@jmenblaz) 
# based in Sequeira et al., 2021


#---------------------------------------------------------------
# 1. Set data repository

input_data <- paste0(input_dir,"/tracking/ttdr/L2")
output_data <- paste0(input_dir,"/tracking/dives")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Import data
# read all ttdr.files
ttdr_files <- list.files(input_data, full.names=TRUE, pattern = "_L2_ttdr.csv")


#---------------------------------------------------------------
# 3. Process  files

# cores <- detectCores() - 2
# cl <- makeCluster(cores)
# registerDoParallel(cl)

t <- Sys.time()


foreach(i=1:length(ttdr_files), .packages=c("dplyr", "stringr", "lubridate", "diveMove")) %dopar% {
  
  # import ttdr
  data <- readTrack(ttdr_files[i])
  organismID <- data$organismID[i]

  # import dcalib files (see 04_process_ttdr.R)
  path <- paste0(input_dir,"/tracking/ttdr/L1")
  dcalib <- readRDS(paste0(path,"/",organismID,"_dcalib.rds"))
  
  # dive summary
  dive_sum <- diveSummary(dcalib) %>%
    filter(begdesc >= min(data$time), endasc <= max(data$time))

  # calculate vertical speeds (m/s)
  dive_sum$desc.speed <- dive_sum$descdist/dive_sum$desctim
  dive_sum$asc.speed <- dive_sum$ascdist/dive_sum$asctim
  
  dive_sum$depth_upper_error <- NA
  dive_sum$depth_lower_error <- NA
  
  for (j in 1:nrow(dive_sum)){
    
    # subset TDR data during dive
    sdata <- data %>%
      filter(time >= dive_sum$begdesc[j], time <= dive_sum$endasc[j])
    
    # find max depth and get errors
    sel <- which(sdata$depth_adj == max(sdata$depth_adj, na.rm=T))
    dive_sum$depth_upper_error[j] <- sdata$depth_upper_error[sel]
    dive_sum$depth_lower_error[j] <- sdata$depth_lower_error[sel]
  }
  
  # Add latitute and longitude from TTDR dataset
  dive_sum <- dive_sum %>% inner_join(dplyr::select(data, time, longitude, latitude), by = c("begdesc" = "time"))
  
  # select variables
  df <- dplyr::select(dive_sum, dive.id, longitude, latitude, begdesc, endasc, divetim,
                      pdd, pdd_qc, maxdep, depth_upper_error, depth_lower_error, dtype, xy_error)
  
  # add id
  df <- cbind(organismID, df)
  
  # Export data
  outfile <- paste0(output_data, "/", organismID, "_dive.csv")
  write.csv(df, outfile, row.names = FALSE)
}


Sys.time()-t

stopCluster(cl)

cat("Processing dives finished")

