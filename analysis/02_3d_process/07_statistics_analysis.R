
# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Created by Jessica Ruff and David March (2021)

# Update package and standarized field names following Sequeria et al., 2021
# by Javier Menéndez-Blázquez | @jmenblaz


## Create table comparing the mean and SD of the 3D and 2D home ranges 

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


# 0) path
source("setup")

output_data <- 
# ------------------------------------------------------------------------
# 3D ----------------------------------------------------------------

# 1)  import result from 3D home range and kde ----------------


df <- read.csv(paste0(output_dir,"/01_kde_3d/kde_3d_res.csv"))

# remove X and X.1 columns
df <- df |> select(-X,-X.1)

str(df)


# from m3 to km3 (/10^9)

table50 <- data.frame(UD = "50",
                    volume.mean = mean(df$volume.50)/1000000000,
                    volume.sd = sd(df$volume.50)/1000000000,
                    area.mean = mean(df$area.50)/1000000,
                    area.sd = sd(df$area.50)/1000000)

table95 <- data.frame(UD = "95",
                      volume.mean = mean(df$volume.95)/1000000000,
                      volume.sd = sd(df$volume.95)/1000000000,
                      area.mean = mean(df$area.95)/1000000,
                      area.sd = sd(df$area.95)/1000000)

# NA in areas for 3D
table <- rbind(table50, table95)

rm(table50, table95)

# save table
#write.csv(table, file = "/Users/jessicaruff/Documents/2021_tortugas/TORTUGAS_2021/table_vol_area_20210202.csv", row.names = F)




# ----------------------------------------------------------------------------
# 3) For 3D nigth and day


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
                 volume.50.mean = mean(daynight3d$volume.50[daynight3d$day.night == "day"])/1000000000,
                 volume.95.mean = mean(daynight3d$volume.95[daynight3d$day.night == "day"])/1000000000,
                 area.50.mean = mean(daynight2d$area.50[daynight2d$day.night == "day"])/1000000,
                 area.95.mean = mean(daynight2d$area.95[daynight2d$day.night == "day"])/1000000)

n <- data.frame(day.night = "night",
                volume.50.mean = mean(daynight3d$volume.50[daynight3d$day.night == "night"])/1000000000,
                volume.95.mean = mean(daynight3d$volume.95[daynight3d$day.night == "night"])/1000000000,
                area.50.mean = mean(daynight2d$area.50[daynight2d$day.night == "night"])/1000000,
                area.95.mean = mean(daynight2d$area.95[daynight2d$day.night == "night"])/1000000)


daynight <- rbind(d, n)

rm(d, n)


# save table
#write.csv(daynight, file = "/Users/jessicaruff/Documents/2021_tortugas/TORTUGAS_2021/day_night_table_20210202.csv", row.names = F)





##------------------------------------------------------------------------------------------------------------------------------##
## Produce box plots comparing day and night 3D and 2D Home Ranges
##------------------------------------------------------------------------------------------------------------------------------##

##box plot to compare day and night

#add columns with km3
daynight3d$volume.50.km3 <- daynight3d$volume.50/1000000000
daynight3d$volume.95.km3 <- daynight3d$volume.95/1000000000

#add columns with km2
daynight2d$area.50.m2 <- daynight2d$area.50/1000000
daynight2d$area.95.m2 <- daynight2d$area.95/1000000

## box plots:

daynight3d$day.night <- as.factor(daynight3d$day.nigh)
daynight2d$day.night <- as.factor(daynight2d$day.nigh)

#3d -------------------
# t-test
levels(daynight3d$day.night)
t.test(volume.50.km3 ~ day.night, data = daynight3d)
t.test(volume.95.km3 ~ day.night, data = daynight3d)

boxplot(volume.50.km3~day.night, data=daynight3d, main = "Home Range Volumes (50% UD) -- Diel Differences", xlab="", ylab="3d HR volume km^3")
boxplot(volume.95.km3~day.night, data=daynight3d, main = "Home Range Volumes (95% UD) -- Diel Differences", xlab="", ylab="3d HR volume km^3")


#2d --------------------
t.test(area.50.m2 ~ day.night, data = daynight2d)
t.test(area.95.m2 ~ day.night, data = daynight2d)

boxplot(area.50.m2~day.night, data=daynight2d, main = "Home Range Areas (50% UD) -- Diel Differences", xlab="", ylab="2d HR area km^2")
boxplot(area.95.m2~day.night, data=daynight2d, main = "Home Range Areas (95% UD) -- Diel Differences", xlab="", ylab="2d HR area km^2")


##------------------------------------------------------------------------------------------------------------------------------##












