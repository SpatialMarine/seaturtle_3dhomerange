

#---------------------------------------------------------------
# 1. Set parameters
#---------------------------------------------------------------


# Trip definition
trip_type <- "time"  # haul: trim track by haul-out locations; time: trim track by time gaps
trip_time_gap <- 7 * 24  # (used if trip_type == time) Tracks with data gaps in excess of [seg_time_gap] hours were broken up for separate modeling

# Track selection
sel_min_loc <- 10  # minimum number of locations
sel_min_dur <- 12 # minimum durantion of track, in hours
sel_exclude <- NULL # custom selection of tags based on exploration of data
sel_min_dist <- 15 # minimum distance of tracks, in km

# Track filtering
filt_step_time <- 2/60  # time difference to consider duplicated positions, in hours
filt_step_dist <- 0/1000  # spatial distance to consider duplicated poisitions, in km
filt_land <- FALSE  # remove locations on land
filt_vmax <- 2  # value of the maximum of velocity using in sdafilter, in m/s (depend on species, seaturtle 2 m/s)
filt_ang <- c(15, 25) # value of the angle using in sdafilter, no spikes are removed if ang=-1
filt_distlim <- c(2500, 5000) # value of the limite distance using in sdafilter, no spikes are removed if ang=-1

# Track regularization
reg_time_step <- 1  # time step to interpolate positions, in hours


# re-route SSM provided by animotum package
reouted = TRUE



