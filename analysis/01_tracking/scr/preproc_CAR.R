#-----------------------------------------------------------------------------------------
# 01_preproc_CAR.R        Pre-process loggerhead tracking data
#-----------------------------------------------------------------------------------------
# This script pre-processes tracking data. The main goal is to standardize among multiple
# formats (tag manufacturers, custom pre-processing from different labs) and generate a
# common and standardized format to then follow a common workflow
#
# About loggerhead data:
# Loggerhead input data has been prepared by David March. It is a compilation of
# published and non-published tracks by Alnitak, SOCIB, Cardona, Eckert.
# Input data is found in several formats and files.
# - metadatatagging.csv: contains metadata for each tagged individual, including deployment date
# - indvidual folder per tag ID
#
# Pre-processing is structured by data source:
# - Cardona
# - Eckert
# - SMRU tags
# - STAT
# - Wildlife Computers
#
# All location data correspond to Argos


#---------------------------------------------------------------
# Prepare cluster
#---------------------------------------------------------------
cl <- makeCluster(cores)
registerDoParallel(cl)


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
input_data <- paste0(input_dir, "/tracking/", sp_code)
output_data <- paste0(output_dir, "/tracking/", sp_code, "/L0_locations")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#---------------------------------------------------------------
# 2. Process metadata
#---------------------------------------------------------------

# import metadata
metafile <- paste(input_data, "TODB_2020-10-30.xlsx", sep="/")
meta <- readTurtleDB(metafile, endRow=95)

# rename variables and set deployment date to Date class
db <- meta %>% rename(id = ptt)

# select turtles with TTDR
db <- dplyr::filter(db, wc_time_series == "y")


#---------------------------------------------------------------
# 3. Process individual data
#---------------------------------------------------------------

## Process delayed mode (location data)
#for (i in 1:nrow(db)){
foreach(i=1:nrow(db), .packages=c("dplyr", "ggplot2", "stringr", "lubridate")) %dopar% {
  
  print(paste("Processing tag", i, "of", nrow(db)))
  
  ## get tag information
  id <- db$id[i]

  # If data from Wildlife Computers -------------------------
  # One folder per individual
  if(db$raw_wc[i] == "y" & db$date_deploy[i] >= "2015-01-01"){
    # Import data
    loc_file <- list.files(paste0(input_data, "/wc/", id), full.names=TRUE, pattern = "^\\w+-Locations\\.csv$")
    data <- read.csv(loc_file)
    # Standardize data  
    dataL0 <- wc2L0(data, locale = "English", date_deploy = db$date_deploy[i])  
  }  
  
  # If data from STAT ----------------------------------------
  # One CSV per individual
  if(db$raw_stat[i] == "y" & db$date_deploy[i] < "2015-01-01"){
    # Import data
    loc_file <- list.files(paste0(input_data, "/stat"), full.names=TRUE, pattern = paste0(id, ".csv"))
    # Standardize data
    dataL0 <- stat2L0(stat_file = loc_file, date_deploy = db$date_deploy[i]) 
  }   
  
  # If data from Cardona -------------------------------------
  # Original data was transfered into a CSV file per individual
  if(db$raw_cardona[i] == "y"){
    # Import data
    loc_file <- list.files(paste0(input_data, "/cardona"), full.names=TRUE, pattern = paste0(id, ".csv"))
    data <- read.csv(loc_file, sep = ";", dec=",", header = TRUE, stringsAsFactors=F)
    # Standardize data
    dataL0 <- cardona2L0(data, date_deploy = db$date_deploy[i])  
  }
  
  # If data from Eckert -------------------------------------
  # All data is found in a unique CSV file
  if(db$raw_eckert[i] == "y"){
    # Import data
    loc_file <- "data/raw/tracking/CAR/eckert/MedTurtlesLocations.csv"
    data <- read.csv(loc_file, sep = ",", dec=".", header = TRUE, stringsAsFactors=F)
    # Standardize data
    dataL0 <- eckert2L0(data, ptt = id, date_deploy = db$date_deploy[i])
  }    
  
  # If data from SMRU -------------------------------------
  # Data is store in two Access databases. Because of incompatibility with Win-64, data
  # was previously extracted using smru2L0(). We use processed files as raw data.
  if(db$raw_smru[i] == "y"){
    # Import data
    loc_file <- list.files(paste0(input_data, "/smru"), full.names=TRUE, pattern = paste0(id, ".csv"))
    data <- read.csv(loc_file, sep = ";", dec=",", header = TRUE, stringsAsFactors=F)
    dataL0 <- rename(data, id = ptt)
  } 
  
  # define "trips"
  # We incorporate this new variable to use same concept as trip from central-place foragers
  # On a later step, trip will be the trimmed track after a segmentation procedure.
  # At this point, trip = id.
  # dataL0$trip <- timedif.segment(dataL0$date, thrs = trip_time_gap)
  # dataL0$trip <- paste(id, str_pad(dataL0$trip, 3, pad = "0"), sep="_") 
  dataL0$trip <- dataL0$id
  dataL0 <- dplyr::select(dataL0, id, trip, everything())
  
  # store into individual folder at output path
  out_file <- paste0(output_data, "/", id, "_L0_locations.csv")
  write.csv(dataL0, out_file, row.names = FALSE)
  
  # plot track
  p <- map_argos(dataL0)
  out_file <- paste0(output_data, "/", id, "_L0_locations.png")
  ggsave(out_file, p, width=30, height=15, units = "cm")
}


#---------------------------------------------------------------
# 4. Summarize processed data
#---------------------------------------------------------------

# import all location files
loc_files <- list.files(output_data, full.names = TRUE, pattern = "L0_locations.csv")
df <- readTrack(loc_files)

# summarize data per animal id
idstats <- summarizeId(df)

# export table
out_file <- paste0(output_data, "/", sp_code, "_summary_id.csv")
write.csv(idstats, out_file, row.names = FALSE)


#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)


print("Pre-processing ready")

