

# ------------------------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles
# Vertical habitat use
  
# by Javier Menéndez-Blázquez | @jmenblaz

# ----------------------------------------------------------
# Analysed dives data derived from L3 ttdr files and dcalib (diveMove) (tracking/05_process_dives.R)

# 1) Proportion time per depth

# 2) Calculate Dives metrics following Horton et al., 2025 
  # We split dives into night / day using the begging of the descend (tracking/05_process_dives.R)

# 3) Analysis of vertical habitat metrics





# ------------------------------------------------------------------------------
# 1) Proportion time per depth

# 0) Set input and output repository -------------------------------------------
input_data <- paste0(input_dir,"/tracking/ttdr/L3")
output_data <- paste0(output_dir,"/04_habitat_use")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


# read all ttdr.files
ttdr_files <- list.files(input_data, full.names=TRUE, pattern = "L3_ttdr.csv")
# read and combine csv into one (all dives registered)
data <- do.call(rbind, lapply(ttdr_files, read.csv))
# check number of distinct organism ID
n_distinct(data$organismID)

# Asegurarse de que la columna 'time' esté en formato POSIXct si es necesario
data$time <- as.POSIXct(data$time, format = "%Y-%m-%d %H:%M:%S")

# Crear una nueva columna para los rangos de profundidad (0-10, 10-20, ...)
data$depth_range <- cut(data$depth, 
                        breaks = seq(0, max(data$depth, na.rm = TRUE), by = 10),
                        labels = paste(seq(0, max(data$depth, na.rm = TRUE) - 10, by = 10), 
                                       seq(10, max(data$depth, na.rm = TRUE), by = 10), sep = "-"),
                        include.lowest = TRUE, right = FALSE)

# Calcular la duración total y la duración por rango de profundidad para cada 'organismID'
data_time <- data %>%
  group_by(organismID, depth_range) %>%
  summarise(time_in_range = n(), .groups = "drop")

# clean some results with depth NA
data_time <- na.omit(data_time)

# Calcular el tiempo total por organismID
total_time <- data %>%
  group_by(organismID) %>%
  summarise(total_time = n(), .groups = "drop")

# Unir las tablas para calcular la proporción de tiempo por rango de profundidad
# Join tables to calculate the time proportion per depth range
data_proportion <- data_time %>%
  left_join(total_time, by = "organismID") %>%
  mutate(proportion = (time_in_range / total_time) * 100)

# export /save proportion time result per time range
f <- paste0(output_data,"/","01_proportion_time_depth.csv")
write.csv(data_proportion, f, row.names = FALSE)


# For DAY ----------------------------------------------------------------------
# filter day data 
day <- data %>% filter(daynight == "day")

# Calculate total tracking duration and duration per depth range by organismID
data_time <- day %>%
  group_by(organismID, depth_range) %>%
  summarise(time_in_range = n(), .groups = "drop")

# clean some results with depth NA
data_time <- na.omit(data_time)

# Total time per organismID
total_time <- day %>%
  group_by(organismID) %>%
  summarise(total_time = n(), .groups = "drop")  # 'n()' cuenta el tiempo total por organismo

# Join tables for calculate time proportion per depth range
data_proportion_day <- data_time %>%
  left_join(total_time, by = "organismID") %>%
  mutate(proportion = (time_in_range / total_time) * 100)

# export /save proportion time result per time range
f <- paste0(output_data,"/","01_proportion_time_depth_day.csv")
write.csv(data_proportion_day, f, row.names = FALSE)


# For NIGHT --------------------------------------------------------------------
# filter day data 
night <- data %>% filter(daynight == "night")

# Calculate total tracking duration and duration per depth range by organismID
data_time <- night %>%
  group_by(organismID, depth_range) %>%
  summarise(time_in_range = n(), .groups = "drop")

# clean some results with depth NA
data_time <- na.omit(data_time)

# Total time per organismID
total_time <- night %>%
  group_by(organismID) %>%
  summarise(total_time = n(), .groups = "drop")  # 'n()' cuenta el tiempo total por organismo

# Join tables for calculate time proportion per depth range
data_proportion_night <- data_time %>%
  left_join(total_time, by = "organismID") %>%
  mutate(proportion = (time_in_range / total_time) * 100)

# export /save proportion time result per time range
f <- paste0(output_data,"/","01_proportion_time_depth_night.csv")
write.csv(data_proportion, f, row.names = FALSE)


# 1.2) Calculate mean proportion time:

stats <- data_proportion %>%
  group_by(depth_range) %>%
  summarise(
    mean = mean(proportion, na.rm = TRUE),
    sd = sd(proportion, na.rm = TRUE)
  )

day_stats <- data_proportion_day %>%
  group_by(depth_range) %>%
  summarise(
    mean = mean(proportion, na.rm = TRUE),
    sd = sd(proportion, na.rm = TRUE)
  )

night_stats <- data_proportion_night %>%
  group_by(depth_range) %>%
  summarise(
    mean = mean(proportion, na.rm = TRUE),
    sd = sd(proportion, na.rm = TRUE)
  )

## figures of time proportion in fig/01_habitat_use_fig.R





# 2) Calculate dives metrics ---------------------------------------------------

# -----------------------------------------------------------------------------
# - Calculate Dives metric

# On a daily basin (for seasonal differences)
# and night and day basin (for daily and seassonal differences)

# Mean depth (calculate previously per dive)
# Max depth (calculate previoyslt per dive)

# VMRd (VMR the absolute depth change in meters divided by the length of the summary period in minutes)
# Modify from Horton et al., 2025 for each dive identified due the differences in dive behabiour between seaturtle and fishs
# Note: this metric was not calculated during the dive process per inmersion. Is calculate in this script

# - Horizontal habitat use  --------------------------------------------
# Calculate daily horizontal displacement (Horizontal displacement 
# (km.day− 1) the distance between successive daily locations



# 0) Set input and output repository -------------------------------------------
input_data <- paste0(input_dir,"/tracking/dives")


# Import data
# read all ttdr.files
dive_files <- list.files(input_data, full.names=TRUE, pattern = "_dive.csv")
# read and combine csv into one (all dives registered)

data <- do.call(rbind, lapply(dive_files, read.csv))
# check number of distinct organism ID
n_distinct(data$organismID)


# 2.1) VMRd -----------------------------
# Calculate Vertical Movement Rater dive [VRMd metric (m/min) or (m min-1)]
#   - absolute depth change in meters divided by the length of the summary period in minutes 

# dive absolute depth change
# due we used the min (0) and max depth of the dive, the absolute change is 
# the distance of descend and ascend
absolute_depth_change <- (2 * data$maxdep)
# length dive or dive duration (divetim) in seconds (s)
divetime_min <- data$divetim / 60
# Vertical Movement Rater of each dive (VMRd) in minutes
# High VMRd values means high activity, ascend and descend quickest 
data$VMRd <- absolute_depth_change / divetime_min

# Mean Depth and Depth Max calculate previously ----

# 2.2) add season information (winter, autum, spring, summer) -----------------

# data$day <- day(data$begdesc)
# data$month <- month(data$begdesc)
data$begdesc <- as.POSIXct(data$begdesc, format = "%Y-%m-%d %H:%M:%S")

# clean dives result. Some dives with NA in begdesc (only 3)
data <- data[!is.na(data$begdesc), ]

# extract day and month
data$date <- as.Date(data$begdesc)
data$date_month <- format(data$begdesc, "%m-%d") # for further plots

# add season information
# apply custom function [get_season()] to every record in the df
data$season <- sapply(data$date, get_season)

# export /save dive records with dive metrics
f <- paste0(input_dir,"/tracking/dives/dives_metrics.csv")
write.csv(data, f, row.names = FALSE)






# ------------------------------------------------------------------------------
# 3) Analysis dive metrics by                 ----------------------------------

# 3.1) Day/night time
# 3.2) Moon light (only night records)
# 3.3) Season


# Read previously exported data
data <- read.csv(paste0(input_dir,"/tracking/dives/dives_metrics.csv"))
                 
# As factor
data$season <- as.factor(data$season)
data$daynight <- as.factor(data$daynight)
data$moon_bright_class <- as.factor(data$moon_bright_class)





# 3.1) Day/night time ----------------------------------------

dive_metrics_stats <- data %>%
  group_by(daynight) %>%
  summarise(
    mean_depth = median(meandep, na.rm = TRUE),
    max_depth = median(maxdep, na.rm = TRUE),
    mean_VMRd = median(VMRd, na.rm = TRUE))














# Gráficos de la profundidad media por condiciones
ggplot(data_summary, aes(x = moon_bright_class, y = mean_depth, color = daynight)) +
  geom_point(position = position_jitter(width = 0.1), size = 3) +
  geom_boxplot(alpha = 0.3) +
  facet_wrap(~ season) +
  theme_minimal() +
  labs(title = "Variabilidad de la profundidad media por fase lunar y día/noche")

# Gráficos de VMR
ggplot(data_summary, aes(x = moon_bright_class, y = VMR, color = daynight)) +
  geom_point(position = position_jitter(width = 0.1), size = 3) +
  geom_boxplot(alpha = 0.3) +
  facet_wrap(~ season) +
  theme_minimal() +
  labs(title = "Variabilidad de VMR por fase lunar y día/noche")





















