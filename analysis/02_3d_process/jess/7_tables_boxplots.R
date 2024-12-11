
##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
##------------------------------------------------------------------------------------------------------------------------------##


##------------------------------------------------------------------------------------------------------------------------------##
# 7. Create tables and box plots
## ## After running loop
##------------------------------------------------------------------------------------------------------------------------------##


## Create table with 3D mkde results

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

## Create table comparing the mean and sd of the 3D and 2D home ranges 


list.files(output_dir, recursive= TRUE, pattern = "")






names(df)


table <- data.frame(UD = "50",
                    volume.mean = mean(df$volume.50)/1000000000,
                    volume.sd = sd(df$volume.50)/1000000000,
                    area.mean = mean(twod_all$area.50)/1000000,
                    area.sd = sd(twod_all$area.50)/1000000)

table95 <- data.frame(UD = "95",
                      volume.mean = mean(df$volume.95)/1000000000,
                      volume.sd = sd(df$volume.95)/1000000000,
                      area.mean = mean(twod_all$area.95)/1000000,
                      area.sd = sd(twod_all$area.95)/1000000)

tabletable <- rbind(table, table95)

rm(table, table95)

# save table
#write.csv(tabletable, file = "/Users/jessicaruff/Documents/2021_tortugas/TORTUGAS_2021/table_vol_area_20210202.csv", row.names = F)


## Create table comparing the mean and sd of the 3D and 2D home ranges in DAY vs. NIGHT

dn <- data.frame(day.night = "day",
                 volume.50.mean = mean(daynight3d$volume.50[daynight3d$day.night == "day"])/1000000000,
                 volume.95.mean = mean(daynight3d$volume.95[daynight3d$day.night == "day"])/1000000000,
                 area.50.mean = mean(daynight2d$area.50[daynight2d$day.night == "day"])/1000000,
                 area.95.mean = mean(daynight2d$area.95[daynight2d$day.night == "day"])/1000000)

n <- data.frame(day.night = "night",
                volume.50.mean = mean(daynight3d$volume.50[daynight3d$day.night == "night"])/1000000000,
                volume.95.mean = mean(daynight3d$volume.95[daynight3d$day.night == "night"])/1000000000,
                area.50.mean = mean(daynight2d$area.50[daynight2d$day.night == "night"])/1000000,
                area.95.mean = mean(daynight2d$area.95[daynight2d$day.night == "night"])/1000000)


daynight <- rbind(dn, n)

rm(dn, n)

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
#3d
boxplot(volume.50.km3~day.night, data=daynight3d, main = "Home Range Volumes (50% UD) -- Diel Differences", xlab="", ylab="3d HR volume km^3")

boxplot(volume.95.km3~day.night, data=daynight3d, main = "Home Range Volumes (95% UD) -- Diel Differences", xlab="", ylab="3d HR volume km^3")

#2d
boxplot(area.50.m2~day.night, data=daynight2d, main = "Home Range Areas (50% UD) -- Diel Differences", xlab="", ylab="2d HR area km^2")

boxplot(area.95.m2~day.night, data=daynight2d, main = "Home Range Areas (95% UD) -- Diel Differences", xlab="", ylab="2d HR area km^2")

##------------------------------------------------------------------------------------------------------------------------------##







