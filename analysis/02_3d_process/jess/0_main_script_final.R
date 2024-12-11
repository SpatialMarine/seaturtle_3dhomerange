
##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
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


##------------------------------------------------------------------------------------------------------------------------------##
# Run scripts
##------------------------------------------------------------------------------------------------------------------------------##

source(script.one)
source(script.two)
source(script.two.andahalf)
source(script.three)
source(script.four)
source(script.five)
source(script.six)
source(script.seven)

# source(script.eight)
# source(script.nine)
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
# To save objects for further plotting / analysis
# 
# To save:
# 1. mkde object: output of 3D HR calculation
# 2. final ttdr dataframe: data used to calculate 3d HR
# 3. res: dataframe containing the volumes and thresholds for plotting
#
##------------------------------------------------------------------------------------------------------------------------------##

# current date
st <- format(Sys.time(), "%Y-%m-%d")

# file path to save objects
outdir <- "/Users/jessicaruff/Documents/2021_tortugas/3d_output_2021/"

# turtle id
ptt <- 34327

# create file names
# 3d mkde

mkdeobjfile <- paste0(outdir,ptt,"_mkde_obj_", st,".rdata")

finalttdrfile <- paste0(outdir,ptt,"_final_ttdr_", st,".rdata")

resfile <- paste0(outdir,ptt,"_res_", st, ".rdata")

# 2d mkde

mkdeobjfile_2D <- paste0(outdir,ptt,"_2dmkde_obj_", st,".rdata")

resfile_2D <- paste0(outdir,ptt,"_2dres_", st, ".rdata")

## Day vs. Night 3d

DAY_3Dmkdeobjfile <- paste0(outdir,ptt,"_3dmkdeobj_DAY_", st,".rdata")

DAY_finalttdrfile <- paste0(outdir,ptt,"_final_ttdr_DAY_", st,".rdata")

DAY_3Dresfile <- paste0(outdir,ptt,"_3dres_DAY_", st, ".rdata")

NIGHT_3Dmkdeobjfile <- paste0(outdir,ptt,"_3dmkdeobj_NIGHT_", st,".rdata")

NIGHT_finalttdrfile <- paste0(outdir,ptt,"_final_ttdr_NIGHT_", st,".rdata")

NIGHT_3Dresfile <- paste0(outdir,ptt,"_3dres_NIGHT_", st, ".rdata")

## Day vs. Night 2d

DAY_2Dmkdeobjfile <- paste0(outdir,ptt,"_2dmkdeobj_DAY_", st,".rdata")

DAY_2Dresfile <- paste0(outdir,ptt,"_2dres_DAY_", st, ".rdata")

NIGHT_2Dmkdeobjfile <- paste0(outdir,ptt,"_2dmkdeobj_NIGHT_", st,".rdata")

NIGHT_2Dresfile <- paste0(outdir,ptt,"_2dres_NIGHT_", st, ".rdata")

##------------------------------------------------------------------------------------------------------------------------------##
