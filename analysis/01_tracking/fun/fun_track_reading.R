#------------------------------------------------------------------------------------
# reading_tools.R    Suite of function for reading and transforming data
#------------------------------------------------------------------------------------
# This script contains the following custom functions:
#
# batch2db      Convert batch file of track data from IFREMER
# batchUB       Convert batch file of track data from University of Barcelona
# cardona2L0    Processes data from Cardona and Hays 2018
# coloniesUB    Standardize colony data from UB
# eckert2L0     Processes data from Eckert et al 2008
# readTrack     Read standardized animal track data in csv
# readTurtleDB  Read Turtle Database
# stat2L0       Processes STAT locations csv files
# smru2L0       Processes data from SMRU tags (location data)
# wc2L0         Processes Wildlife Computers Locations csv files
# wc2L0_GPS     Processes Wildlife Computers Fastloc-GPS csv files
# wc2L0_SST     Processes Wildlife Computers SST csv files
# wc2L0_status  Processes Wildlife Computers status csv files
# wc2stat       Transform Locations CSV from Wildlife Computers to STAT format
# wcGetPttData  Download and extract Wildlife Computer CSVs from the Data Portal
#------------------------------------------------------------------------------------




# Functions created by D.March / GitHub: @dmarch

# Updated by J. Menéndez-Blázquez (@jmenblaz) for standarization of variables names
# following Sequeria et al., 2021





#--------------------------------------------------------------------------------------
# batchUB       Convert batch file of track data from University of Barcelona
#--------------------------------------------------------------------------------------
batchUB <- function(ubdata){
  # Description:
  # This function reads a batch file provided by UB with all animal tracks.
  # Then, extracts information for each animal, and generates a table with basic information for each
  # tagged animal.
  # Returns a data.frame
  #
  # Arguments:
  # batch_file      RData file provided by UB
  
  # load libraries
  library(dplyr)
  library(lubridate)
  library(geosphere)
  
  # read batch file
  df <- ubdata
  
  # rename variables
  df$date <- df$Date_Time
  df$lon <- df$Longitude
  df$lat <- df$Latitude
  
  # parse date time
  df$date <- parse_date_time(df$date, c("Ymd HMS"), tz="UTC")
  
  # generate table with deployment information
  df2 <- df %>%
    mutate(species_name = Species, ring = Ring, bphase = BPhase, trip = tracking_event) %>%  # change names
    arrange(date) %>%  # order by date
    group_by(ring, species_name, trip, Colony, bphase, complete) %>%  # select group info
    summarize(date_deploy = first(date),
              lon_deploy = first(lon),
              lat_deploy = first(lat),
              date_last = last(date),
              max_dist_km = round(max(distGeo(p1 = cbind(lon, lat), p2 = c(lon_deploy, lat_deploy)))/1000),
              time_interval_min = round(mean(as.numeric(difftime(tail(date, -1), head(date, -1), units="min")))),
              n_loc = n()) %>%  # get first and last observations
    mutate(duration_hour = difftime(date_last, date_deploy, units="days"))  # calculate duration of the track
  
  # return data.frame
  return(df2)
}
#--------------------------------------------------------------------------------------


#---------------------------------------------------------------------
# batch2db      Convert batch file of track data from IFREMER
#---------------------------------------------------------------------
batch2db <- function(batch_file){
  # Description:
  # This function reads a batch file provided by IFREMER with all animal tracks.
  # Then, extracts information for each animal, and generates a table with basic information for each
  # tagged animal.
  # Returns a data.frame
  #
  # Arguments:
  # batch_file      csv file provided by IFREMER
  
  # load libraries
  library(dplyr)
  library(lubridate)
  
  # read batch file
  df <- read.csv(batch_file)
  
  # parse date time
  df$date <- parse_date_time(df$date, c("ymd HM", "dmY HM", "Ymd HM"), tz="UTC")
  
  # generate table with deployment information
  df <- df %>%
    mutate(species_name = "blue_shark", id = Ptt, instr = Instr, size_m = size) %>%  # change names
    arrange(date) %>%  # order by date
    group_by(id, species_name, instr, sex, size_m, scat) %>%  # select group info
    summarize(date_deploy = first(date),
              lon_deploy = first(lon),
              lat_deploy = first(lat),
              date_last = last(date),
              time_interval_h = round(mean(as.numeric(difftime(tail(date, -1), head(date, -1), units="hours")))),
              n_loc = n()) %>%  # get first and last observations
    mutate(duration_d = round(difftime(date_last, date_deploy, units="days")))  # calculate duration of the track
  
  # return data.frame
  return(df)
}
#---------------------------------------------------------------------



#---------------------------------------------------------------------
# cardona2L0     Processes data from Cardona and Hays 2018
#---------------------------------------------------------------------
cardona2L0 <- function(data, date_deploy=NULL){
  
  require(dplyr)
  require(lubridate)
  
  #The date and time information are separated into individualcolumns 
  ##using the ymd() and ymd_hms()functions to take character and numeric vectors, and convert it to a POSIXct object. 
  data$date <- dmy_hms(paste(data$date, data$time), tz="UTC")
  
  # Filter data by removing NA registers
  # Causes found:
  # 1) empty rows at the end (due to format conversion)
  # 2) empty time stamps
  data <- filter(data, !is.na(date))
  
  # Select data collected from the date of deployment
  if (!is.null(date_deploy)) data <- filter(data, date >= date_deploy)
  
  ### Rename columns
  names(data)[names(data)=="longis"] <- "lon"
  names(data)[names(data)=="latgis"] <- "lat"
  names(data)[names(data)=="loc..class"] <- "lc"
  
  ### Add idcolumn
  data$id <- id
  
  ### Change lower case to upper case
  data$lc <- toupper(data$lc)
  
  # Reorder and select column names
  data <- dplyr::select(data, id, date, lon, lat, lc)
  return(data)
}
#---------------------------------------------------------------------


#---------------------------------------------------------------------
# coloniesUB     Standardize colony data from UB
#---------------------------------------------------------------------
coloniesUB <- function(data){
  # This function standardizes names
  
  # Rename columns
  names(data)[names(data)=="Species"] <- "sp_ub_code"
  names(data)[names(data)=="Colony"] <- "colony"
  names(data)[names(data)=="Sample.size"] <- "col_sample_size"
  names(data)[names(data)=="Pop.Min"] <- "col_pop_min"
  names(data)[names(data)=="Pop.Max"] <- "col_pop_max"
  names(data)[names(data)=="Longitude"] <- "lon_col"
  names(data)[names(data)=="Latitude"] <- "lat_col"
  
  return(data)
}
#---------------------------------------------------------------------

#---------------------------------------------------------------------
# eckert2L0     Processes data from Eckert et al 2008
#---------------------------------------------------------------------
eckert2L0 <- function(data, ptt, date_deploy=NULL){
  
  require(dplyr)
  require(lubridate)
  
  ## Filter data by platform and deploy date (if available)
  data <- filter(data, Ptt == ptt)
  
  #The date and time information are separated into individualcolumns 
  ##using the ymd() and ymd_hms()functions to take character and numeric vectors, and convert it to a POSIXct object. 
  data$date <- mdy_hm(data$ReceptionDateTime, tz="UTC")
  
  # Filter data by removing NA registers
  # Causes found:
  # 1) empty rows at the end (due to format conversion)
  # 2) empty time stamps
  data <- filter(data, !is.na(date))
  
  # Select data collected from the date of deployment
  if (is.null(date_deploy)) date_deploy <- mdy(data$DateDeploy[1])
  if (!is.null(date_deploy)) data <- filter(data, date >= date_deploy)
  
  ### Rename columns
  names(data)[names(data)=="Ptt"] <- "id"
  names(data)[names(data)=="Longitude"] <- "lon"
  names(data)[names(data)=="Latitude"] <- "lat"
  names(data)[names(data)=="LocationQuality"] <- "lc"
  
  ### Change lower case to upper case
  data$lc <- toupper(data$lc)
  
  # Reorder and select column names
  data <- dplyr::select(data, id, date, lon, lat, lc)
  return(data)
}
#---------------------------------------------------------------------


#---------------------------------------------------------------------
# readTrack     Read standardized animal track data in csv
#---------------------------------------------------------------------
readTrack <- function(csvfiles){
  # Description
  # Reads a standardized animal track data in csv.
  # Returns a data frame with parsed time
  # It allows the combination of multiple files
  # csvfiles: string with the location of 1 or more csv files
  
  library(lubridate)
  library(data.table)
  
  ## create empty list
  dt_list <- list()  
  
  ## process and append files
  for (i in 1:length(csvfiles)){
    data <- read.csv(csvfiles[i], header=TRUE)  # read csv
    data$time <- parse_date_time(data$time, "Ymd HMS") # parse time
    dt_list[[i]] <- data  # append to list
  }
  
  dt <- rbindlist(dt_list, fill=TRUE)  # combine data.frames
  return(dt)
}
#---------------------------------------------------------------------


#------------------------------------------------------------------------------------
# readTurtleDB    Read Turtle Database
#------------------------------------------------------------------------------------
readTurtleDB <- function (xlsx, endRow=94){
  
  # Load libraries
  library(xlsx)
  library(dplyr)
  
  # Import necessary tables for processing
  tbl.deployment <- read.xlsx(xlsx, sheetName = "deployment", header=TRUE, endRow=endRow)
  tbl.data <- read.xlsx(xlsx, sheetName = "data", header=TRUE, endRow=endRow)
  tbl.tag <- read.xlsx(xlsx, sheetName = "tag", header=TRUE, endRow=endRow)
  tbl.animal <- read.xlsx(xlsx, sheetName = "animal", header=TRUE, endRow=endRow)
  tbl.project <- read.xlsx(xlsx, sheetName = "project", header=TRUE, endRow=endRow)
  
  # Combine tables and select necessary information
  meta <- tbl.deployment %>%
    left_join(tbl.data, by = "ptt") %>%
    left_join(tbl.tag, by = "ptt") %>%
    left_join(tbl.animal, by = "ptt") %>%
    left_join(tbl.project, by = "project_id") %>%
    filter(!ptt %in% c(149564, 149572))  # IFREMER tags that we don't have access
  
  # Change time zone
  attr(meta$date_deploy, "tzone") <- "UTC"
  
  return(meta)
}
#------------------------------------------------------------------------------------


#---------------------------------------------------------------------
# smru2L0     Processes data from SMRU tags (location data)
#---------------------------------------------------------------------
smru2L0 <- function(ptt, date_deploy=NULL, datadir){
  
  require(RODBC)
  require(dplyr)
  require(lubridate)
  
  ## Select database based on PTT number
  ## There are two databases
  if(ptt %in% c(2840, 28484, 28483)) smrudb <- "tu48.mdb"
  if(ptt %in% c(28482, 28508)) smrudb <- "tu48b.mdb"
  
  ## Connect to database
  db <- paste(datadir, smrudb, sep="/")
  conn <- odbcConnectAccess2007(db)    # sqlTables(conn)

  ## Read diag table
  data <- sqlFetch(conn, "diag") 
  
  ## Filter data by platform and deploy date (if available)
  data <- filter(data, PTT == ptt)
  
  ## Rename columns
  names(data)[names(data)=="PTT"] <- "id"
  names(data)[names(data)=="D_DATE"] <- "date"
  names(data)[names(data)=="LON"] <- "lon"
  names(data)[names(data)=="LAT"] <- "lat"
  names(data)[names(data)=="LQ"] <- "lc"
  
  ## Select data collected from the date of deployment
  if (!is.null(date_deploy)) data <- filter(data, date >= date_deploy)
  
  ## Standardize Location clasess
  data$lc[data$lc == -1] <- "A"
  data$lc[data$lc == -2] <- "B"
  data$lc[data$lc == -9] <- "Z"
  
  # Reorder and select column names
  data <- dplyr::select(data, id, date, lon, lat, lc)
  return(data)
  
  ## Close connection to database
  close(conn) 
}
#---------------------------------------------------------------------


#--------------------------------------------------------------------------------------
# spot2L0           Function to convert SPOT data from IFREMER to L0
#--------------------------------------------------------------------------------------
spot2L0 <- function(ptt, date_deploy=NULL, rawdata){
  # Import and parses SPOT tag data from IFREMER
  # Returns data frame with parsed times and coordinates in lon/lat
  
  # load library
  library(lubridate)
  library(dplyr)
  
  # import rawdata in csv
  data <- read.csv(rawdata)
  
  # parse time
  data$date <- parse_date_time(data$date, c("ymd HM", "dmY HM", "Ymd HM"), tz="UTC")
  
  ## Filter data by platform and deploy date (if available)
  data <- filter(data, Ptt == ptt)
  
  # Rename columns
  names(data)[names(data)=="Ptt"] <- "id"
  
  # Create KF error ellipse information
  # Current version contains empty parameters
  data$smaj <- NA
  data$smin <- NA
  data$eor <- NA
  
  # Standardize Location clasess
  data$lc[data$lc == -1] <- "A"
  data$lc[data$lc == -2] <- "B"
  data$lc[data$lc == -9] <- "Z"
  
  # Order by ascending time stamps
  data <- arrange(data, date)
  
  # Reorder and select column names
  data <- dplyr::select(data, id, date, lon, lat, lc, smaj, smin, eor)
  return(data)
}
#--------------------------------------------------------------------------------------



#---------------------------------------------------------------------
# stat2L0     Processes STAT locations csv files
#---------------------------------------------------------------------
stat2L0 <- function(stat_file, date_deploy=NULL){
  
  require(dplyr)
  require(lubridate)
  
  #read CSV file
  #standard import (read.csv) does not work with headerless columns
  #so reading whole table then extract useful info
  data = read.table(stat_file, header=F, fill = TRUE, sep = ",", stringsAsFactors=F)
  
  #extract column names from first row
  Header_Names = data[1,]
  
  #apply column names to dataframe
  names(data) = Header_Names
  
  #remove first row (duplicate header row)
  data = data[-1,]
  
  #removes any rows that may have been errononeously added
  #by the read table function
  #remove rows where Tag ID value is not the proper value 
  #as reported by first ID cell
  data = data[data$tag_id == data$tag_id[1],]
  
  ### Rename columns
  names(data)[names(data)=="tag_id"] <- "id"
  names(data)[names(data)=="utc"] <- "date"
  names(data)[names(data)=="lon1"] <- "lon"
  names(data)[names(data)=="lat1"] <- "lat"
  names(data)[names(data)=="lc"] <- "lc"
  
  # Reorder column names
  data <- data[, !duplicated(colnames(data))]
  data <- dplyr::select(data, id, date, lon, lat, lc)
  
  # Convert character values to numeric and POSIXct
  data <- data %>% mutate_each(funs(as.numeric), id, lat, lon) %>%
    mutate_each(funs(parse_date_time(.,"Ymd HMS")), date) 
  
  # Select data collected from the date of deployment
  if (!is.null(date_deploy)) data <- filter(data, date >= date_deploy)
  
  # Reorder column names
  data <- dplyr::select(data, id, date, lon, lat, lc)
  return(data)
}
#---------------------------------------------------------------------



#----------------------------------------------------------------------------------------------
# ub2L0    Function to convert UB data to L0
#----------------------------------------------------------------------------------------------
ub2L0 <- function(id, data){
  # Import and parses tag data from University of Barcelona
  # Returns data frame with parsed times and coordinates in lon/lat
  
  # load library
  library(lubridate)
  library(dplyr)

  ## Filter data by trip
  data <- dplyr::filter(data, Ring == id)
  
  # Rename columns
  names(data)[names(data)=="Ring"] <- "id"
  names(data)[names(data)=="Colony"] <- "colony"
  names(data)[names(data)=="Date_Time"] <- "date"
  names(data)[names(data)=="Longitude"] <- "lon"
  names(data)[names(data)=="Latitude"] <- "lat"
  names(data)[names(data)=="tracking_event"] <- "trip"
  names(data)[names(data)=="BPhase"] <- "stage"
  
  
  ## Create new column of Location class
  data$lc <- "G"
  
  # parse time
  data$date <- parse_date_time(data$date, c("Ymd HMS"), tz="UTC")
  
  # Order by ascending time stamps
  data <- arrange(data, date)
  
  # Reorder and select column names
  data <- dplyr::select(data, id, colony, stage, trip, date, lon, lat, lc)
  
  return(data)
}
#----------------------------------------------------------------------------------------------




#---------------------------------------------------------------------
# wc2L0  Processes Wildlife Computers Locations csv files
#---------------------------------------------------------------------
wc2L0 <- function(data, locale = "English", date_deploy=NULL){
  
  require(dplyr)
  require(lubridate)

  ### Rename columns
  names(data)[names(data)=="Ptt"] <- "id"
  names(data)[names(data)=="Date"] <- "date"
  names(data)[names(data)=="Longitude"] <- "lon"
  names(data)[names(data)=="Latitude"] <- "lat"
  names(data)[names(data)=="Quality"] <- "lc"
  names(data)[names(data)=="Error.Semi.major.axis"] <- "smaj"
  names(data)[names(data)=="Error.Semi.minor.axis"] <- "smin"
  names(data)[names(data)=="Error.Ellipse.orientation"] <- "eor"
  
  # Convert to POSIXct
  data$date <- parse_date_time(data$date, c("HMS dbY", "Ymd HMS"), locale=locale, tz="UTC")
  
  # Select data collected from the date of deployment
  if (!is.null(date_deploy)) data <- filter(data, date >= date_deploy)
  
  # Reorder column names
  data <- dplyr::select(data, id, date, lon, lat, lc, smaj, smin, eor)
  return(data)
}
#---------------------------------------------------------------------

#---------------------------------------------------------------------
# wc2L0_GPS     Processes Wildlife Computers Fastloc-GPS csv files
#---------------------------------------------------------------------
wc2L0_GPS <- function(data, date_deploy=NULL){
  
  require(dplyr)
  require(lubridate)
  
  ### Select relevant columns
  data <- dplyr::select(data, Name, Day, Time, Latitude, Longitude, Residual, Satellites)
  
  # Convert to POSIXct
  data$date <- paste(data$Day, data$Time)
  data$date <- parse_date_time(data$date, c("HMS dbY", "dbY HMS", "Ymd HMS"), locale=Sys.setlocale("LC_TIME", "English"), tz="UTC")
  
  # Select data collected from the date of deployment
  if (!is.null(date_deploy)) data <- filter(data, date >= date_deploy)
  
  ### Rename columns
  names(data)[names(data)=="Name"] <- "id"
  names(data)[names(data)=="Longitude"] <- "lon"
  names(data)[names(data)=="Latitude"] <- "lat"
  names(data)[names(data)=="Residual"] <- "residual"
  names(data)[names(data)=="Satellites"] <- "satellites"
  
  # Filter data by removing NA registers
  data <- filter(data, !is.na(lon))
  
  # Add GPS QC assessment based on residual and number of satellites
  # 1: good data; 4: bad data
  # References:
  # Witt et al. 2010 Animal Behaviour, 80: 571-581.
  # Dujon et al. 2014 Methods in Ecology and Evolution, 5, 1162–1169
  ref_max_gps <- 30 # Witt et al. 2010 (Dujon uses 35)
  data$gps_qc <- ifelse(data$residual > ref_max_gps | data$satellites <= 5, 4, 1)
  
  # Reorder column names
  data <- dplyr::select(data, id, date, lon, lat, residual, satellites, gps_qc)
  return(data)
}
#---------------------------------------------------------------------


#---------------------------------------------------------------------
# wc2L0_SST     Processes Wildlife Computers SST csv files
#---------------------------------------------------------------------
wc2L0_SST <- function(ptt, date_deploy, datadir){
  
  source("R/database_tools.R")
  require(dplyr)
  require(lubridate)
  
  ### Folder to read data
  rawdir <- pttPath(ptt, "rawWC", datadir)
  loc_file <- list.files(rawdir,full.names=TRUE,pattern="\\w+-SST\\.csv$")  # previous pattern: "^\\w+-1-FastGPS\\.csv$"
  data <- read.csv(loc_file)
  
  ### Rename columns
  names(data)[names(data)=="Ptt"] <- "ptt"
  names(data)[names(data)=="Date"] <- "date"
  names(data)[names(data)=="Depth"] <- "depth"
  names(data)[names(data)=="Temperature"] <- "temperature"
  names(data)[names(data)=="Source"] <- "source"
  
  # Convert to POSIXct
  data$date <- parse_date_time(data$date, c("HMS dbY", "dbY HMS", "Ymd HMS"), locale=Sys.setlocale("LC_TIME", "English"), tz="UTC")
  
  # Select data collected from the date of deployment
  data <- filter(data, date >= date_deploy)
  
  # Select Reorder column names
  data <- dplyr::select(data, ptt, date, depth, temperature, source)
  
  ### Create output file
  exdir <- pttPath(ptt, "L0_TTDR", datadir)
  file <- paste0(exdir, paste0("/L0_SST_", ptt, ".csv"))
  write.table(data, file, row.names=FALSE, sep=";", dec=",")
}
#---------------------------------------------------------------------

#---------------------------------------------------------------------
# wc2L0_status  Processes Wildlife Computers status csv files
#---------------------------------------------------------------------
wc2L0_status <- function(ptt, date_deploy, datadir){
  
  source("R/database_tools.R")
  require(dplyr)
  require(lubridate)
  
  ### Folder to read data
  rawdir <- pttPath(ptt, "rawWC", datadir)
  loc_file <- list.files(rawdir,full.names=TRUE,pattern="^\\w+-Status\\.csv$")
  data <- read.csv(loc_file)
  
  ### Select relevant columns
  data <- dplyr::select(data, Ptt, Received, Latitude, Longitude, Time.Offset, Type,
                        BrokenThermistor, Transmits, BattVoltage, Depth, ZeroDepthOffset, MinWetDry, MaxWetDry, WetDryThreshold)
  
  # Convert to POSIXct
  data$date <- parse_date_time(data$Received, c("HMS dbY", "dbY HMS", "Ymd HMS"), locale=Sys.setlocale("LC_TIME", "English"), tz="UTC")
  
  # Select data collected from the date of deployment
  data <- filter(data, date >= date_deploy)
  
  ### Rename columns
  names(data)[names(data)=="Ptt"] <- "ptt"
  names(data)[names(data)=="Latitude"] <- "lat"
  names(data)[names(data)=="Longitude"] <- "lon"
  names(data)[names(data)=="Time.Offset"] <- "time_offset"
  names(data)[names(data)=="Type"] <- "type"
  names(data)[names(data)=="BrokenThermistor"] <- "broken_thermistor"
  names(data)[names(data)=="Transmits"] <- "transmits"
  names(data)[names(data)=="BattVoltage"] <- "batt_voltage"
  names(data)[names(data)=="Depth"] <- "depth"
  names(data)[names(data)=="ZeroDepthOffset"] <- "zoffset"
  names(data)[names(data)=="MinWetDry"] <- "min_wetdry"
  names(data)[names(data)=="MaxWetDry"] <- "max_wetdry"
  names(data)[names(data)=="WetDryThreshold"] <- "wetdry_threshold"
  
  # Reorder column names
  data <- dplyr::select(data, ptt, date, lat, lon, time_offset, type, broken_thermistor, transmits,
                        batt_voltage, depth, zoffset, min_wetdry, max_wetdry, wetdry_threshold)
  
  ### Create output file
  exdir <- pttPath(ptt, "L0_status", datadir)
  file <- paste0(exdir, paste0("/L0_status_", ptt, ".csv"))
  write.table(data, file, row.names=FALSE, sep=";", dec=",")
}
#---------------------------------------------------------------------


#------------------------------------------------------------------------------------
# wc2stat         Transform Locations CSV from Wildlife Computers to STAT format
#------------------------------------------------------------------------------------
wc2stat <- function(wcfile, statfile){
  # Description:
  # Transform Locations CSV from Wildlife Computers to STAT format
  #
  # Arguments:
  # wcfile      path to a json formatted keyfile with wcAccessKey and wcSecretKey
  # statfile    owner id from WC portal
  #
  # Value:
  # Comma-separated file (CSV) in STAT format
  #
  # Notes:
  # Only data obtained from WC Locations file is transfered. Note that all other fields are blank or with fictitious data
  
  # Load dependencies
  require(lubridate)
  
  # read wc csv
  data <- read.csv(wcfile)
  
  # Convert to POSIXct
  data$Date <- parse_date_time(data$Date, c("HMS dbY", "Ymd HMS"), locale=Sys.setlocale("LC_TIME", "English"), tz="UTC")
  
  # create stat csv structure
  h <- c("uid","uuid","prognum","tag_id","utc","lc","iq","lat1","dir1","lon1","dir2","lat2","dir3","lon2","dir4","nb_mes","big_nb_mes","best_level","pass_duration","nopc","calcul_freq","altitude","sensors","error_radius","semi_major_axis","semi_minor_axis","ellipse_orientation","GDOP","POSIX","local_time","Swapped","POSIX","Local Time","Day of Year","Depth (m)","SST daily (C)","SST weekly (C)","SST monthly (C)","Current U (cm/s)","Current V (cm s-1)","Current Mag (cm s-1)","Current Dir (deg)","Wind U (m s-1)","Wind V (m s-1)","Wind Mag (m s-1)","Wind Dir (deg)","CHL daily (mg m-3)","CHL weekly (mg m-3)","CHL monthly (mg m-3)","Productivity weekly (mg C/m^2/Day)","Productivity monthly (mg C/m^2/Day)","From Shore (km)","Wet/Dry","Country","Displacement (km)","Distance (km)","Cummulative (km)","Hours","Cummulative (days)","Speed (kph)","Bearing (deg)","Azimuth (deg)","sensors")
  nrow <- nrow(data)
  ncol <- length(h)
  m <- matrix("", nrow, ncol)
  df <- data.frame(m)
  colnames(df) <- h
  
  # populate csv from wc data
  df$tag_id <- data$Ptt
  df$prognum <- 02286
  df$utc <- data$Date
  df$lc <- data$Quality
  df$lat1 <- data$Latitude
  df$dir1 <- ifelse(data$Latitude >= 0, "N", "S")
  df$lon1 <- data$Longitude
  df$dir2 <- ifelse(data$Longitude >= 0, "E", "w")
  df$error_radius <- ifelse(is.na(data$Error.radius) == TRUE, "", data$Error.radius)
  df$semi_major_axis <- ifelse(is.na(data$Error.Semi.major.axis) == TRUE, "", data$Error.Semi.major.axis)
  df$semi_minor_axis <- ifelse(is.na(data$Error.Semi.minor.axis) == TRUE, "", data$Error.Semi.minor.axis)
  df$ellipse_orientation <- ifelse(is.na(data$Error.Ellipse.orientation) == TRUE, "", data$Error.Ellipse.orientation)
  
  # populate with fictitious data
  df$uid <- seq(1:nrow)
  df$uuid <- ""
  df$iq <- "50"
  df$lat2 <- df$lat1
  df$dir3 <- df$dir1
  df$lon2 <- df$lon1
  df$dir4 <- df$dir2
  df$nb_mes <- 10
  df$big_nb_mes <- 0
  df$best_level <- -122
  df$pass_duration <- 600
  df$nopc <- 3
  df$calcul_freq <- "401 677557.1"
  df$altitude <- 0
  df$sensors <- "26 125 213"
  df$GDOP <- ""
  df$POSIX <- as.double(df$utc,  origin = "1970-01-01", tz="utc")
  df$local_time <- df$utc
  df$Swapped <- ""
  df$POSIX <- as.double(df$utc,  origin = "1970-01-01", tz="utc")
  df$"Local Time" <- ""
  df$"Day of Year" <- ""
  df$"Depth (m)" <- ""
  df$"SST daily (C)" <- ""
  df$"SST weekly (C)" <- ""
  df$"SST monthly (C)" <- ""
  df$"Current U (cm/s)" <- ""
  df$"Current V (cm s-1)" <- ""
  df$"Current Mag (cm s-1)" <- ""
  df$"Current Dir (deg)" <- ""
  df$"Wind U (m s-1)" <- ""
  df$"Wind V (m s-1)" <- ""
  df$"Wind Mag (m s-1)" <- ""
  df$"Wind Dir (deg)" <- ""
  df$"CHL daily (mg m-3)" <- ""
  df$"CHL weekly (mg m-3)" <- ""
  df$"CHL monthly (mg m-3)" <- ""
  df$"Productivity weekly (mg C/m^2/Day)"  <- ""
  df$"Productivity monthly (mg C/m^2/Day)" <- ""
  df$"From Shore (km)" <- ""
  df$"Wet/Dry" <- ""
  df$"Country" <- ""
  df$"Displacement (km)"  <- ""
  df$"Distance (km)" <- ""
  df$"Cummulative (km)" <- ""
  df$"Hours" <- ""
  df$"Cummulative (days)"  <- ""
  df$"Speed (kph)" <- ""
  df$"Bearing (deg)" <- ""
  df$"Azimuth (deg)" <- ""
  df$"sensors" <- ""
  
  # convert data to character
  for(n in names(df)[1:ncol(df)]){
    df[,n]<-as.character(df[,n])}
  
  # Export to csv
  write.table(df, statfile, sep=",", dec=".", quote=TRUE, row.names=FALSE)
}
#------------------------------------------------------------------------------------

