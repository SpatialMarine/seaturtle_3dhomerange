
#--------------------------------------------------------------------------------
# setup.R - Setups project - seaturtle3d_homerange
#--------------------------------------------------------------------------------

project <- "seaturtle_3dhomerange"

# set computer
cpu <- "jmb"

# Load required packages
pacman::p_load("data.table", "tidyr", "dplyr", "lubridate", "openxlsx", "stringr", "reshape2", "tools", # data manipulation
               "foreach", "doParallel",  # parallel computing
               "sp", "raster", "jsonlite","geojsonsf", "geojsonio", #spatial
               "ggplot2")  # plot


# ------------------------------------------------------------------------------
# 1. Set main data paths ---------

if(cpu == "jmb") main_dir <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/",project)

if (!dir.exists(main_dir)) dir.create(main_dir, recursive = TRUE)


# ------------------------------------------------------------------------------
# 2. Create data paths for planet-api R project --------------------------------
# input_dir <- paste(main_dir, "input", sep="/")
# if (!dir.exists(input_dir)) dir.create(input_dir, recursive = TRUE)

# output_dir <- paste(main_dir, "output", sep="/")
# if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


