
#----------------------------------------------------------------------------------
# CAR_main.R            Main script for processing loggerhead turtle tracking
#----------------------------------------------------------------------------------

# todo: move locatations that overlap land into the ocean. Use CI intervals. Search for closest cell.


# We will select turtles that were free swimming prior to capture and release. Exclude those
# capture from longline or released from a rescue center.



# tracking functions paths
funs <- list.files("analysis/01_tracking/fun/", pattern = "\\.R$", full.names = TRUE)
# read .R scripts with source
sapply(funs, source)




#---------------------------------------------------------------
# 1. Set parameters for Caretta caretta
#---------------------------------------------------------------
sp_code <- "CAR"  # species code
tag_type <- "PTT"

# Trip definition
trip_time_gap <- 7 * 24  # Tracks with data gaps in excess of [seg_time_gap] hours were broken up for separate modeling

# Track selection
sel_min_loc <- 20  # minimum number of locations
sel_min_dur <- 10 * 24 # minimum durantion of track, in hours
sel_exclude <- NULL # custom selection of tags based on exploration of data
sel_min_dist <- 15

# Track filtering
filt_step_time <- 2/60  # time difference to consider duplicated positions, in hours
filt_step_dist <- 1/1000  # spatial distance to consider duplicated poisitions, in km
filt_land <- FALSE  # remove locations on land
filt_vmax <- 2  # value of the maximum of velocity using in sdafilter in m/s
filt_ang <- c(15, 25) # value of the angle using in sdafilter, no spikes are removed if ang=-1
filt_distlim <- c(2500, 5000) # value of the limite distance using in sdafilter, no spikes are removed if ang=-1


# Track regularization
reg_time_step <- 2  # time step to interpolate positions, in hours

# TTDR data
tfreq <- 5 * 60  # time interval from TTDR data, in seconds


#---------------------------------------------------------------
# 2. Set data paths and import libraries
#---------------------------------------------------------------

# Load dependencies
source("setup.R")
source("scr/fun_track_reading.R")  # read multiple tracking data formats
source("scr/fun_track_plot.R")  # plot tracking data
source("scr/fun_track_proc.R")  # miscellanea of processing functions
source("scr/fun_ttdr.R")

# input dir for raw tracking position (location Argos)
paste0(main_dir,"/input/tracking/loc/loc")



#---------------------------------------------------------------
# 3. Import external data
#---------------------------------------------------------------

# # Landmask
# land <- readOGR("data/raw/ext/landmask","landmask_med")
# land <- spTransform(land, crs_proj)
# 
# # Oceanmask
# ocean <- readOGR("data/raw/ext/oceanmask","WestMed_area")
# ocean <- spTransform(ocean, crs_proj)


#---------------------------------------------------------------
# 4. Processing workflow
#---------------------------------------------------------------

# Set number of cores for parallel processing
cores <- detectCores()-2

# Step 1. Pre-process data and standardize format
# Select data for turtles with TTDR that remain within the Western Mediterranean
source("analysis/01_tracking/scr/01_preproc_CAR.R")

# Step 2. Filter location data
# Filtering is based on selected parameters from above
# Segment track if there are temporal gaps
source("analysis/tracking/scr/filter_locs.R")

# Step 3. Regularize location data
# Uses correlated random walk state-space model from Jonsen et al. 2019 doi:10.1002/ecy.2566
source("analysis/tracking/scr/regularize_ssm.R")

# Step 4. Process TTDR data
source("analysis/tracking/scr/process_TTDR_CAR.R")