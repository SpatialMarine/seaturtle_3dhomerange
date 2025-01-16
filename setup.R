
#--------------------------------------------------------------------------------
# setup.R - Setups project - seaturtle3d_homerange
#--------------------------------------------------------------------------------

project <- "seaturtle_3dhomerange"

# set computer
cpu <- "jmb"


# packages

# devtools::install_github("ianjonsen/aniMotum")
# devtools::install_github("dmarch/animalsensor")
# remotes::install_github("jmlondon/pathroutr")
# install.packages("pathroutr", repos = "https://jmlondon.r-universe.dev", dependencies = TRUE)



# Load required packages
pacman::p_load("data.table", "tidyr", "dplyr", "lubridate", "openxlsx", "stringr", 
               "reshape2", "tools", "purrr", # data manipulation
               "foreach", "doParallel",  # parallel computing
               "sp", "raster", "jsonlite","geojsonsf", "geojsonio", "rworldxtra", 
               "rnaturalearthhires", "maptools", "suntools", "raster", #spatial
               "suncalc",
               "ks", "mkde", "akima","ncdf4", # 3D spatial analysis
               "rgl", "plot3D", "plotly","plot3Drgl", "ggsvg", # 3D visualization
               "diveMove", # process dive tracking data
               "ggplot2", "gridExtra", "grid", #plot
               "animalsensor", "aniMotum","move", "argosfilter", "pathroutr", "sfnetworks", "nabor")  # tracking process tools

# # instal remotes repositories
remotes::install_github('coolbutuseless/svgparser')
remotes::install_github('coolbutuseless/ggsvg')


# packages notes:
# · sfnetworks is required by pathroutr package
# · nabor

# for parse and format timestamps for ttdr series files and locs from Argos data
locale <- "en_US.UTF-8"

# ------------------------------------------------------------------------------
# 1. Set main data paths ---------

if(cpu == "jmb") main_dir <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/",project)
if (!dir.exists(main_dir)) dir.create(main_dir, recursive = TRUE)

# from SML Dropbox
if (cpu == "jmb") carto_dir <- ("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/data/carto")

# ------------------------------------------------------------------------------
# 2. Create data paths for planet-api R project --------------------------------
input_dir <- paste(main_dir, "input", sep="/")
if (!dir.exists(input_dir)) dir.create(input_dir, recursive = TRUE)

output_dir <- paste(main_dir, "output", sep="/")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


