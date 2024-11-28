#-------------------------------------------------------------------------------------
# 03_regularize_ssm    Interpolate tracks into regular time steps using aniMotum (new version of foiegras)
#-------------------------------------------------------------------------------------
# This script processes animal tracking data following a common approach between
# different species


# Main steps are:
# - Regularize tracks
# - New version use "aniMotum" instead "foiegras" R packages

# Note: variables or fields names are standardized following Sequeria et al., 2021

#' A standardization framework for bio‐logging data to advance ecological research and conservation
#' standardization and use aniMotum for regularize track position 
#' by J.Menéndez-Blázquez - @jmenblaz



# tracking functions paths
funs <- list.files("analysis/01_tracking/fun/", pattern = "\\.R$", full.names = TRUE)
# read .R scripts with source
sapply(funs, source)

source("setup.R")

#-------------------------------------------------------------------------------
# 1. Set data repository                 ------------------------------------

# input_data for L1 locations processed previously (02_filter_locs.R)

input_data  <- paste0(input_dir,"/tracking/loc/L1")
output_data <- paste0(input_dir,"/tracking/loc/L2")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


#-------------------------------------------------------------------------------
# 2. Import L1 metadata                   ------------------------------------

# basic data for further processing: instrumentType, organismID
metadata <- read.csv(paste0(input_data,"/metadataL1.csv"))

# summarize metadata per organismID
organism_meta <- metadata %>%
  group_by(organismID, instrumentType) %>%
  summarize()

#------------------------------------------------------------------------------
# 3. Regularize each track using a SSM      ---------------------------------
t <- Sys.time()

cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)


foreach(i=1:nrow(organism_meta), .packages=c("dplyr", "ggplot2", "aniMotum", "stringr", "lubridate", "animalsensor")) %dopar% {  
  
  # info 
  cat("Processing tag", i,"of",length(organismIDs))
  cat("\n")
  # organism ID
  organismID <- organism_meta$organismID[i]
  cat(" · organismID:", organismID)
  cat("\n")
  cat("\n")
  
  # import L1 location data
  infile <- sprintf(paste0(input_data,"/","%s_L1_loc.csv"), organismID)
  data <- read.csv(infile)
  data$time <- parse_date_time(data$time, "Ymd HMS") # parse time
  
  # summarize organismID L1 data per trip
  trips <- animalsensor::summarizeTrips(data)

  trips <- filter(trips,
                  duration_h >= sel_min_dur,
                  n_loc >= sel_min_loc,
                  distance_km >= sel_min_dist, 
                  !id %in% sel_exclude)
  
  # subset data
  # filter by organismID and selected trips
  data <- filter(data, tripID %in% trips$trip)
  
  
  # State-Space Model (SSM) from aniMotum package, Jonsen et al., 2023 -------
  # https://doi.org/10.1111/2041-210X.14060
  
  # convert to aniMotum format
  # Note: use each trip as a id for SSM
  indata <- data %>%
    rename(id = tripID,
           date = time,
           lc = argosLC,
           lon = longitude,
           lat = latitude,
           smaj = argosSemiMajor,
           smin = argosSemiMinor,
           eor = argosOrientation) %>%
    dplyr::select(id, date, lc, lon, lat, smaj, smin, eor)
  
  # filter location class data with NA values
  # very few cases, but creates an error for fit_ssm
  indata <- dplyr::filter(indata, !is.na(lc))
  
  
  # fit SSM  ------------------------------------------

  # we turn sdafilter off because we previously filtered data 
  #   (see 02_filter_locs.R)
  
  # we run the model with multiple trips at once
  
  # ssm models: "crw" - correlated random walk: Movements are random and correlated in direction and magnitude
  #              "rw" - random walk: Movements are random in direction and magnitude.
  #              "mp" - Move persistence: Movements are random with correlation in direction and magnitude that varies in time
  

  
  fit <- fit_ssm(indata, model = "crw", time.step = reg_time_step,
                 control = ssm_control(verbose = 0), spdf = FALSE,
                 map = list(psi = factor(NA)))
  
  # Note: SSMs implemented in aniMotum have no information about potential 
  # barriers to animal movement,for example land for marine species
  # aniMotum makes use of the pathroutr R package route_path() function (see ?route_path() for details)
  # Josh M. London. (2020)
  # https://zenodo.org/records/5522909#.YnPxEy_b1qs
  
  
  fit_rerouted <- route_path(fit, what = "predicted", map_scale = 10)
  
  # Fit Time-varying move persistence
  # When the data have minimal measurement error (e.g. GPS locations)
  fmp <- fit_mpm(fit, what = "predicted", model = "jmpm", control = mpm_control(verbose = 0))
  
  # plot(fmp, pages = 1, ncol = 3, pal = "Cividis", rev = TRUE)
  # m <- fmap(fit, fmp, what = "predicted", pal = "Cividis")
  
  # mapping or plot ssm model
  # map(fit, what = "predicted")
  

  # get or extract locations for SSM and MPM
  # grab() - Extract fitted/predicted/observed locations from a aniMotum
  #          model, with or without projection information
  
  # get fitted locations (SSM)
  data <- data.frame(grab(fit, what = "predicted", as_sf = FALSE))

  data <- data.frame(grab(fit, what = "rerouted", as_sf = FALSE))
  
  # get fitted behavioral state (MPM) - Time-varying move persistence
  datafmp <- data.frame(grab(fmp, what = "fitted", as_sf = FALSE))

  # combine and arrange results data fpr SSM and MPM
  data <- data %>%
    # join datasets
    left_join(datafmp, by=c("id", "date")) %>%
    # rename and prepare data for further steps
    rename(tripID = id, time = date, longitude = lon, latitude = lat) %>%
    arrange(time) %>%
    mutate(organismID = organismID) %>%
    dplyr::select(organismID, everything())
  
  
  
  # export/save L2 locations results (SSM preducted models and MPM information)
  output_data <- paste0(output_dir, "/tracking/locdata/L2_loc/")
  if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)
  outfile <- sprintf(paste0(output_data, "%s_L2_loc.csv"), organismID)
  write.csv(data, outfile, row.names = F)
  
  # export convergence status
  convergence <- data.frame(organismID = organismID, tripID = fit$id, converged_fit = fit$converged, converged_fmp = fmp$converged)
  outfile <- sprintf(paste0(output_data, "%s_L2_convergence.csv"), organismID)
  write.csv(convergence, outfile, row.names = FALSE)
  
  
  # plot figures
  p <- mapL1(data = data)
  out_file <- paste0(output_data, "/", i, "_L2_locations.png")
  ggsave(out_file, p, width=30, height=15, units = "cm")  # plot figures
  
  
  
}


stopCluster(cl) # Stop cluster
print("Regularization ready")




























foreach(i=tags, .packages=c("dplyr", "ggplot2", "aniMotum", "stringr")) %dopar% {
#for (i in tags){
  
  print(paste("Processing tag", i))
  # 
  # # import data
  # loc_file <- paste0(input_data, "/", i, "_L1_locations.csv")
  # data <- readTrack(loc_file)
  
  # subset data
  # filter by id and selected trips
  data <- filter(df, id == i, trip %in% trips$trip)

  ###### State-Space Model
  
  # convert to foieGras format
  if(tag_type == "GPS") indata <- data %>% dplyr::select(trip, date, lc, lon, lat) %>% rename(id = trip) 
  if(tag_type == "PTT") indata <- data %>% dplyr::select(trip, date, lc, lon, lat, smaj, smin, eor) %>% rename(id = trip) 
  
  # filter location class data with NA values
  # very few cases, but creates an error in fit_ssm
  indata <- dplyr::filter(indata, !is.na(lc))
  
  # fit SSM
  # we turn sdafilter off because we previously filtered data
  # we run the model with multiple trips at once
  fit <- fit_ssm(indata, model = "crw", time.step = reg_time_step, verbose = 0, spdf = FALSE)
  
  # get fitted locations
  # segments that did not converge were not consider
  data <- data.frame(grab(fit, what = "predicted", as_sf = FALSE))
  data <- data %>% rename(trip = id) %>% arrange(date)
  data <- cbind(id = i, data)
  
  # check if points on land
  #data$onland <- point_on_land(lat = data$lat, lon = data$lon, land = land)
  
  # export track data into individual folder at output path
  out_file <- paste0(output_data, "/", i, "_L2_locations.csv")
  write.csv(data, out_file, row.names = FALSE)
  
  # export convergence status
  convergence <- data.frame(id = i, trip = fit$id, converged = fit$converged)
  out_file <- paste0(output_data, "/", i, "_L2_convergence.csv")
  write.csv(convergence, out_file, row.names = FALSE)
  
  # plot figures
  p <- mapL1(data = data)
  out_file <- paste0(output_data, "/", i, "_L2_locations.png")
  ggsave(out_file, p, width=30, height=15, units = "cm")
}
  













































#---------------------------------------------------------------
# 4. Summarize processed data
#---------------------------------------------------------------

# import all location files
loc_files <- list.files(output_data, full.names = TRUE, pattern = "L2_locations.csv")
df <- readTrack(loc_files)

# import convergence files
loc_files <- list.files(output_data, full.names = TRUE, pattern = "L2_convergence.csv")
data_proc <- lapply(loc_files, read.csv) %>% rbindlist

# summarize data per trip
tripstats <- summarizeTrips(df)

# combine track data summary and convergence status
comb <- merge(tripstats, data_proc, by=c("id", "trip"))

# export table
out_file <- paste0(output_data, "/", sp_code, "_summary_ssm.csv")
write.csv(comb, out_file, row.names = FALSE)


#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)


print("Regularization ready")