
##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
# Main Script: LOOP
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
# Description of each script:
#
##------------------------------------------------------------------------------------------------------------------------------##

#run the script to load the functions: script titled: functions_3dturtlehr_2018
source("/Users/jessicaruff/Documents/2020_roarin/Tortugas_3D_2020/R_scripts_2020/functions_3dturtlehr_2018.R")

##------------------------------------------------------------------------------------------------------------------------------##
# Assign names to script files
##------------------------------------------------------------------------------------------------------------------------------##

folder <- "/Users/jessicaruff/Documents/2021_tortugas/TORTUGAS_2021/organized_scripts_2021/"

## assign names to script file locations

script.one <- paste0(folder, "1_load_data.R")

script.two <- paste0(folder, "2_process_data.R")

script.two.andahalf <- paste0(folder, "2.5_process_dayvsnight_data.R")

script.three <- paste0(folder, "3_3dmkde.R")

script.four <- paste0(folder, "4_2d_mkde.R")

script.five <- paste0(folder, "5_day.v.night_3d.R")

script.six <- paste0(folder, "6_day.v.night_2d.R")

script.seven <- paste0(folder, "7_tables_boxplots.R")

script.eight <- paste0(folder, "8_plot_3dmkde.R")

script.nine <- paste0(folder, "9_plot_3dmkde_LOOP.R")

script.ten <- paste0(folder, "10_plot_mkde_raster.R")

##------------------------------------------------------------------------------------------------------------------------------##

#prepare loop

# create empty data frames
df <- NULL
twod_all <- NULL
daynight3d <- NULL
daynight2d <- NULL

# load libraries
library(lubridate)
library(dplyr)
library(akima)
library(mkde)
library(sp)

#all turtle ptt tag ids
##Including the trimmed track of turtle 151934 who went to Turkey, new id == 1519341
turtid <- c(151935,
            151936,
            151933,
            34319,
            34322,
            34321,
            34326,
            34327,
            1519341)

i=1
turtid[i]

#loop

for(i in 1:length(turtid)){

source(script.one)
source(script.two)
source(script.two.andahalf)
source(script.three)
source(script.four)
source(script.five)
source(script.six)
}

rm(day, new, newday3d, newnight3d, newtwodday, newtwodnight, night, res, twod, ssm, ttdr)

# create tables and box plots
source(script.seven)

# produce 3d plots
source(script.eight)

#produce 3d plots with looping through saved objects
source(script.nine)

# produce 2d raster plots
source(script.ten)

##------------------------------------------------------------------------------------------------------------------------------##
# Save output dataframes
##------------------------------------------------------------------------------------------------------------------------------##
write.csv(df, file = "/Users/jessicaruff/Documents/2021_tortugas/TORTUGAS_2021/all.volumes.table.20210202.csv", row.names = F)
write.csv(twod_all, file = "/Users/jessicaruff/Documents/2021_tortugas/TORTUGAS_2021/2d.20210202.csv", row.names = F)
write.csv(daynight3d, file = "/Users/jessicaruff/Documents/2021_tortugas/TORTUGAS_2021/daynight3d.20210202.csv", row.names = F)
write.csv(daynight2d, file = "/Users/jessicaruff/Documents/2021_tortugas/TORTUGAS_2021/daynight2d.20210202.csv", row.names = F)

##------------------------------------------------------------------------------------------------------------------------------##

