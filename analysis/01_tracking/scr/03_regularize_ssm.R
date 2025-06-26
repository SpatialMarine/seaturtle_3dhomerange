#-------------------------------------------------------------------------------------
# 03_regularize_ssm    Interpolate tracks into regular time steps using aniMotum (new version of foiegras)
#-------------------------------------------------------------------------------------
# This script processes animal tracking data following a common approach between
# different species


# Main steps are:
# - Regularize tracks
# - New version use "aniMotum" instead "foiegras" R packages

# Note: variables or fields names are standardized following Sequeria et al., 2021

# A standardization framework for bio‐logging data to advance ecological research and conservation
# standardization and use aniMotum for regularize track position 
# by J.Menéndez-Blázquez - @jmenblaz



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

# summarize metadata per OrganismID
organism_meta <- metadata %>%
  group_by(organismID, instrumentType) %>%
  summarize()

organismIDs <- unique(organism_meta$organismID)

#------------------------------------------------------------------------------
# 3. Regularize each track using a SSM      ---------------------------------
t <- Sys.time()

cores <- detectCores() - 2
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
  
  
  # fit State Space Model (SSM)  ------------------------------------------

  # we turn sdafilter off because we previously filtered data 
  #   (see 02_filter_locs.R)
  
  # we run the model with multiple trips at once
  
  # ssm models: "crw" - correlated random walk: Movements are random and correlated in direction and magnitude
  #              "rw" - random walk: Movements are random in direction and magnitude.
  #              "mp" - Move persistence: Movements are random with correlation in direction and magnitude that varies in time
  
  
  fit <- fit_ssm(indata, model = "crw", time.step = reg_time_step,
                 control = ssm_control(verbose = 1), spdf = FALSE,
                 map = list(psi = factor(NA)))
  
  
  # after applied the fit_ssm() fit$x and fit$y
  # x and y values are coordinates from Mercator projection
  
  # standard errors:
  # their standard errors (`x.se`, `y.se` in KM**)
  
  # `u`, `v` (and their standard errors, `u.se`, `v.se` in km/h) 
  # are estimates of signed velocity in the x and y directions. 
  # The `u`, `v` velocities should generally be ignored as their estimation 
  # uses time intervals between consecutive locations, whether they are observation times or prediction times. 
  
  # The columns `s` and `s.se` provide a more reliable 2-D velocity estimate, 
  # although standard error estimation is turned off by default as this generally 
  # increases computation time for the `crw` SSM. 
  # Standard error estimation for `s` can be turned on via the `control` 
  # argument to `fit_ssm` (i.e. `control = ssm_control(se = TRUE)`, see `?ssm_control` for futher details).
  
  
  
  
  # Note: SSMs implemented in aniMotum have no information about potential 
  # barriers to animal movement,for example land for marine species
  # aniMotum makes use of the pathroutr R package route_path() function (see ?route_path() for details)
  # Josh M. London. (2020)
  # https://zenodo.org/records/5522909#.YnPxEy_b1qs
  
  # For other barriers to taken into account during the regularize position of tracking
  # e.g., roads, rails, protected areas limits, etc (spatial polygons) 
  # see detail of function in -> ?pathroutr::prt_visgraph
  
  #' see parameters.R for @params of pathroutr::routepath() function
  if (rerouted == TRUE) {
    fit <- route_path(fit, what = "predicted", map_scale = map_scale, dist = dist_buffer)
  }
  
  
  # Fit Time-varying move persistence (MPM) ------------------------------------
  # When the data have minimal measurement error (e.g. GPS locations)
  fmp <- fit_mpm(fit, what = "predicted", model = "jmpm", control = mpm_control(verbose = 1))
  
  # plot(fmp, pages = 1, ncol = 3, pal = "Cividis", rev = TRUE)
  # m <- fmap(fit, fmp, what = "predicted", pal = "Cividis")
  
  # mapping or plot ssm model
  # map(fit, what = "predicted")
 
  
  # get or extract locations for SSM and MPM
  # grab() - Extract fitted/predicted/observed locations from a aniMotum
  #          model, with or without projection information
  
  #  for state space model locations (SSM) 
  if (rerouted == TRUE) {
    data <- data.frame(grab(fit, what = "rerouted", as_sf = FALSE)) # rerouted = predict locations offshore
  } else {
    data <- data.frame(grab(fit, what = "predicted", as_sf = FALSE))
  }
  
  # for behavioral state (MPM) - Time-varying move persistence metric (g)
  datafmp <- data.frame(grab(fmp, what = "fitted", as_sf = FALSE))

  
  
  # combine and arrange results data for SSM and MPM (g)
  data <- data %>%
    # join datasets
    left_join(datafmp, by=c("id", "date")) %>%
    # rename and prepare data for further steps
    rename(tripID = id, time = date, longitude = lon, latitude = lat) %>%
    arrange(time) %>%
    mutate(organismID = organismID) %>%
    dplyr::select(organismID, everything())
  
  
  
  # calculate latitudinal and logitudinal errors position from standard erros derivated from SSM
  
  # convert the x.se and y.se from KILOMETERS into METERS 
  # (see AniMotum GitHub repo or documentation, also comment above)
  # from fit_ssm, x.se and y.se are provided in Km
  
  data$x.se <- data$x.se * 1000  # km to m
  data$y.se <- data$y.se * 1000  # km to m
  
  
  # convert the x.se and y.se from meter into radians 
  # (latitude.se and longitude.se)
  
  R <- 6378137 # Radio de la Tierra en metros
  # Convert latitude from degrees to radians
  data$lat_rad <- data$latitude * pi / 180
  # Calculate the error in longitude (longitude.se)
  data$longitude.se <- (data$x.se / R) * (180 / pi)
  # Calculate the error in latitude (latitude.se)
  data$latitude.se <- (data$y.se / (R * cos(data$lat_rad))) * (180 / pi)
  # Remove radians column
  data$lat_rad <- NULL
  

  # Add confidence intervasls (CI) from latitude and longitude position based in
  # standard error (SE) of latitude and longitude coordinates calculates previously 
  
  data <- data %>%
    mutate(
      lat.025 = latitude - 1.96 * latitude.se, # IC inferior (2.5%)
      lat.975 = latitude + 1.96 * latitude.se, # IC superior (97.5%)
      lon.025 = longitude - 1.96 * longitude.se, # IC inferior (2.5%)
      lon.975 = longitude + 1.96 * longitude.se  # IC superior (97.5%)
    )
    
  
  # export/save L2 locations results (SSM predicted models and MPM information, g)
  outfile <- sprintf(paste0(output_data,"/","%s_L2_loc.csv"), organismID)
  write.csv(data, outfile, row.names = F)
  
  # export convergence status
  convergence <- data.frame(organismID = organismID, tripID = fit$id, converged_fit = fit$converged, converged_fmp = fmp$converged)
  outfile <- sprintf(paste0(output_data,"/","%s_L2_convergence.csv"), organismID)
  write.csv(convergence, outfile, row.names = FALSE)
  
  # plot figures
  p <- mapL1(data = data)
  out_file <- paste0(output_data,"/",organismID,"_L2_loc.png")
  ggsave(out_file, p, width=30, height=15, units = "cm")  # plot figures
  
}



#---------------------------------------------------------------
# 4. Summarize processed data


# import all location files
loc_files <- list.files(output_data, full.names = TRUE, pattern = "L2_loc.csv")
df <- readTrack(loc_files)

# import convergence files
loc_files <- list.files(output_data, full.names = TRUE, pattern = "L2_convergence.csv")
data_proc <- lapply(loc_files, read.csv) %>% rbindlist

# summarize data per trip
tripstats <- summarizeTrips(df)
tripstats <- tripstats %>% rename(organismID = id,
                                  tripID = trip)

# combine track data summary and convergence status
comb <- merge(tripstats, data_proc, by=c("organismID", "tripID"))

# export table
out_file <- paste0(output_data, "/","L2_summary_ssm.csv")
write.csv(comb, out_file, row.names = FALSE)


# -------------------------------------------------------

t - Sys.time() # 11 min -- 6 Cores-16 GB RAM 
stopCluster(cl) # Stop cluster
print("Regularization ready (fitted SSM/MPM)")

