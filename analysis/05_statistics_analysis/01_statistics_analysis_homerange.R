
# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Created by Jessica Ruff and David March (2021)

# Update package and standarized field names following Sequeria et al., 2021
# and day night results
# by Javier Menéndez-Blázquez | @jmenblaz


# 1) Create result summary table comparing the mean and SD of the 3D and 2D home ranges 
# 2) Create a exploratory boxplot (final figures for the study in fig/02_habita_use.R)


# new <- data.frame(ptt,
#                   volume.50 = res$volume[res$prob == 0.50],
#                   volume.75 = res$volume[res$prob == 0.75],
#                   volume.95 = res$volume[res$prob == 0.95],
#                   threshold.50 = res$threshold[res$prob == 0.50],
#                   threshold.75 = res$threshold[res$prob == 0.75],
#                   threshold.95 = res$threshold[res$prob == 0.95],
#                   mean.xy.error = mean.xy.error,
#                   mean.z.error = mean.z.error, 
#                   avgdepth = avgdepth,
#                   days.tracked = days.tracked,
#                   use.obs.mkde = use.obs)


# ------------------------------------------------------------------------------
# 0) path
source("setup")

output_data <- paste0(output_dir,"/05_statistics_results")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)


# ----------------------------------------------------------------------------
# 3D statistics analysis    -------------------------------------------

# 1)  import result from 3D home range and kde ----------------
df <- read.csv(paste0(output_dir,"/01_kde_3d/kde_3d_res.csv"))

# Note: for the different analysis we used metrics coordinates system (EPSG:3035) 
# from m3 to km3 (/10^9) 

table50 <- data.frame(UD = "50",
                    volume.mean.3d = mean(df$volume.50)/1000000000, # km3
                    volume.sd.3d = sd(df$volume.50)/1000000000,
                    area.mean.2d = mean(df$area.50)/1000000,
                    area.sd.2d = sd(df$area.50)/1000000)

table95 <- data.frame(UD = "95",
                      volume.mean.3d = mean(df$volume.95)/1000000000, # km3
                      volume.sd.3d = sd(df$volume.95)/1000000000,
                      area.mean.2d = mean(df$area.95)/1000000,
                      area.sd.2d = sd(df$area.95)/1000000)

# NA in areas for 3D (calculate volumes not areas)
table <- rbind(table50, table95)
# clean enviroment
rm(table50, table95)

# save table
file <- paste0(output_data,"/3D_UD_results.csv")
write.csv(table, file, row.names = F)



# ----------------------------------------------------------------------------
# 3) For 2D/3D nigth and day


day3d <- read.csv(paste0(output_dir,"/01_kde_3d/kde_3d_res_day.csv"))
night3d <- read.csv(paste0(output_dir,"/01_kde_3d/kde_3d_res_night.csv"))

day2d <- read.csv(paste0(output_dir,"/02_kde_2d/kde_2d_res_day.csv"))
night2d <- read.csv(paste0(output_dir,"/02_kde_2d/kde_2d_res_night.csv"))

daynight3d <- rbind(day3d, night3d)
daynight2d <- rbind(day2d, night2d)

# daynight2d <- daynight2d %>%
#   filter(!is.na(organismID))


## Create table comparing the mean and SD of the 3D and 2D home ranges in DAY vs. NIGHT
# from m3 to km3 (/10^9)

d <- data.frame(day.night = "day",
                volume.50.mean.3d = mean(daynight3d$volume.50[daynight3d$day.night == "day"])/1000000000,
                volume.50.sd.3d = sd(daynight3d$volume.50[daynight3d$day.night == "day"])/1000000000,
                volume.95.mean.3d = mean(daynight3d$volume.95[daynight3d$day.night == "day"])/1000000000,
                volume.95.sd.3d = sd(daynight3d$volume.95[daynight3d$day.night == "day"])/1000000000,
                
                area.50.mean.2d = mean(daynight2d$area.50[daynight2d$day.night == "day"])/1000000,
                area.50.sd.2d = sd(daynight2d$area.50[daynight2d$day.night == "day"])/1000000,
                area.95.mean.2d = mean(daynight2d$area.95[daynight2d$day.night == "day"])/1000000,
                area.95.sd.2d = sd(daynight2d$area.95[daynight2d$day.night == "day"])/1000000)

n <- data.frame(day.night = "night",
                volume.50.mean.3d = mean(daynight3d$volume.50[daynight3d$day.night == "night"])/1000000000,
                volume.50.sd.3d = sd(daynight3d$volume.50[daynight3d$day.night == "night"])/1000000000,
                volume.95.mean.3d = mean(daynight3d$volume.95[daynight3d$day.night == "night"])/1000000000,
                volume.95.sd.3d = sd(daynight3d$volume.95[daynight3d$day.night == "night"])/1000000000,
                
                area.50.mean.2d = mean(daynight2d$area.50[daynight2d$day.night == "night"])/1000000,
                area.50.sd.2d = sd(daynight2d$area.50[daynight2d$day.night == "night"])/1000000,
                area.95.mean.2d = mean(daynight2d$area.95[daynight2d$day.night == "night"])/1000000,
                area.95.sd.2d = sd(daynight2d$area.95[daynight2d$day.night == "night"])/1000000)


daynight <- rbind(d, n)

rm(d, n)

# save result table
#write.csv(daynight, file = "/Users/jessicaruff/Documents/2021_tortugas/TORTUGAS_2021/day_night_table_20210202.csv", row.names = F)
file <- paste0(output_data,"/3D_UD_results_daynight.csv")
write.csv(daynight, file, row.names = F)





# ------------------------------------------------------------------------------
# Box plots comparing day and night 3D and 2D Home Ranges
# ------------------------------------------------------------------------------

# load data for plot
day3d <- read.csv(paste0(output_dir,"/01_kde_3d/kde_3d_res_day.csv"))
night3d <- read.csv(paste0(output_dir,"/01_kde_3d/kde_3d_res_night.csv"))

day2d <- read.csv(paste0(output_dir,"/02_kde_2d/kde_2d_res_day.csv"))
night2d <- read.csv(paste0(output_dir,"/02_kde_2d/kde_2d_res_night.csv"))

daynight3d <- rbind(day3d, night3d)
daynight2d <- rbind(day2d, night2d)


# 1) Box plot to compare day and night

#add columns with km3
daynight3d$volume.50.km3 <- daynight3d$volume.50/1000000000
daynight3d$volume.95.km3 <- daynight3d$volume.95/1000000000

#add columns with km2
daynight2d$area.50.m2 <- daynight2d$area.50/1000000
daynight2d$area.95.m2 <- daynight2d$area.95/1000000


## box plots
daynight3d$day.night <- as.factor(daynight3d$day.nigh)
daynight2d$day.night <- as.factor(daynight2d$day.nigh)

levels(daynight3d$day.night)

# 2d ----------------------------------------------------
t.test_area50 <- t.test(area.50.m2 ~ day.night, data = daynight2d)
t.test_area95 <- t.test(area.95.m2 ~ day.night, data = daynight2d)
# No significant differences between day and night in UD (50 and 95%) in 2D

boxplot(area.50.m2~day.night, data=daynight2d, main = "Home Range Areas (50% UD) -- Diel Differences", xlab="", ylab="2D Home range (km^2)")
boxplot(area.95.m2~day.night, data=daynight2d, main = "Home Range Areas (95% UD) -- Diel Differences", xlab="", ylab="2D Home range (km^2)")

# 3d -----------------------------------------------------
# t-test
t.test_vol50 <- t.test(volume.50.km3 ~ day.night, data = daynight3d)
t.test_vol95 <- t.test(volume.95.km3 ~ day.night, data = daynight3d)

# ***Significant differences between day and night home range volume occuped in 95% UD in 3D
#    but not significant in 50% in 3D

boxplot(volume.50.km3~day.night, data=daynight3d, main = "Home Range Volumes (50% UD) -- Diel Differences", xlab="", ylab="3D Home range (km^3)")
boxplot(volume.95.km3~day.night, data=daynight3d, main = "Home Range Volumes (95% UD) -- Diel Differences", xlab="", ylab="3D Home range (km^3)")





# export result of t.test

# combine result as text; not format
results <- c(
  "Resultados de Welch Two Sample t-tests\n",
  capture.output(t.test_area50), # Convierte los resultados del t.test en texto
  "\n",
  capture.output(t.test_area95),
  "\n",
  capture.output(t.test_vol50),
  "\n",
  capture.output(t.test_vol95)
)

# export as .txt format
file <- paste0(output_data,"/2D_3D_UD_results_daynight_t.test.txt")
writeLines(results, file)


# -----------------------------------------------------------------------------
# export 2D and 3D tables for Supplementary information

file <- paste0(output_data,"/2D_UD_results_daynight_summary.csv")
write.csv(daynight2d, file, row.names = F)

file <- paste0(output_data,"/3D_UD_results_daynight_summary.csv")
write.csv(daynight3d, file, row.names = F)


