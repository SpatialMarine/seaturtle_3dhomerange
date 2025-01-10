
#--------------------------------------------------------------------------------
# 05_process_dives.R
#--------------------------------------------------------------------------------

#' This script use the postprocess L1 ttdr data to obtain differenrt dives for
#' seaturtle

#' First script draft by D.March 

# Standardization made it by J.Menéndez-Blázquez (@jmenblaz) 
# based in Sequeira et al., 2021




#---------------------------------------------------------------

# 0. load functions
# tracking functions paths
funs <- list.files("analysis/01_tracking/fun/", pattern = "\\.R$", full.names = TRUE)
# read .R scripts with source
sapply(funs, source)



# 1. Set data repository

input_data <- paste0(input_dir,"/tracking/ttdr/L3")
output_data <- paste0(input_dir,"/tracking/dives")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Import data
# read all ttdr.files
ttdr_files <- list.files(input_data, full.names=TRUE, pattern = "_L3_ttdr.csv")


#---------------------------------------------------------------
# 3. Process  files

#  cores <- detectCores() - 2
# cl <- makeCluster(cores)
# registerDoParallel(cl)

t <- Sys.time()

foreach(i=1:length(ttdr_files), .packages=c("dplyr", "stringr", "lubridate", "diveMove")) %do% {
  
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
  
  # Add latitute and longitude from TTDR dataset (and z.error, xy.error)
  dive_sum <- dive_sum %>% inner_join(dplyr::select(data, time, longitude, latitude, z.error, xy.error), by = c("begdesc" = "time"))
  
  # select variables
  df <- dplyr::select(dive_sum, dive.id, longitude, latitude, begdesc, endasc, divetim,
                      pdd, pdd_qc, maxdep, depth_upper_error, depth_lower_error, dtype, z.error, xy.error)
  # add id
  df <- cbind(organismID, df)
  
  # Export data
  outfile <- paste0(output_data, "/", organismID, "_dive.csv")
  write.csv(df, outfile, row.names = FALSE)
}


Sys.time()-t
cat("Processing dives finished")

stopCluster(cl)