
#--------------------------------------------------------------------------------
# setup.R - Setups project - seaturtle3d_homerange
#--------------------------------------------------------------------------------

project <- "seaturtle_3dhomerange"

# set computer
# cpu <- "jmb"
cpu <- "jmbSML"

# packages

# devtools::install_github("ianjonsen/aniMotum")
# devtools::install_github("dmarch/animalsensor")
# remotes::install_github("jmlondon/pathroutr")
# install.packages("pathroutr", repos = "https://jmlondon.r-universe.dev", dependencies = TRUE)

# Install GFW R package
if (!require("remotes"))
  install.packages("remotes")

# remotes::install_github("GlobalFishingWatch/gfwr", dependencies = TRUE)


# Load required packages
pacman::p_load("data.table", "tidyr", "dplyr", "lubridate", "openxlsx", "stringr", 
               "reshape2", "tools", "purrr", # data manipulation
               "foreach", "doParallel",  # parallel computing
               "sp", "jsonlite","geojsonsf", "geojsonio", "rworldxtra", 
               "rnaturalearthhires", "maptools", "suntools", "raster", "terra", #spatial
               "suncalc", # enviromental variables
               "gfwr", # For Global Fishing Watch API
               "ks", "mkde", "akima","ncdf4", # 3D spatial analysis
               "rgl", "plot3D", "plotly","plot3Drgl", "ggsvg", # 3D visualization
               "diveMove", # process dive tracking data
               "ggplot2", "gridExtra", "grid", "viridis", #plot
               "animalsensor", "aniMotum","move", "argosfilter", "pathroutr", "sfnetworks", "nabor")  # tracking process tools
library(raster)

# instal remotes repositories
# remotes::install_github('coolbutuseless/svgparser')
# remotes::install_github('coolbutuseless/ggsvg')
# remotes::install_github("marcosci/layer")
# remotes::install_github("hypertidy/quadmesh")

# # Need lastest version of rgl and rayshader
# remove.packages("rgl")
# installed.packages()["rgl", "Version"]
# # install 
# remotes::install_github("dmurdoch/rgl")
# 
# 
# find.package("rayshader")
# # remove.packages("rayshader")
# installed.packages()["rayshader", "Version"]
# lastest Januray 2025 version -> "0.38.11"





# Global Fishing Watch (GFW)
# devtools::install_github("GlobalFishingWatch/gfwr")


# packages notes:
# · sfnetworks is required by pathroutr package
# · nabor

# for parse and format timestamps for ttdr series files and locs from Argos data
locale <- "en_US.UTF-8"

# ------------------------------------------------------------------------------
# 1. Set main data paths ---------
if(cpu == "jmb")    main_dir <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/",project)
if(cpu == "jmbSML") main_dir <- paste0("C:/Users/jmb/SML Dropbox/gitdata/seaturtle_3dhomerange")


if (!dir.exists(main_dir)) dir.create(main_dir, recursive = TRUE)

# from SML Dropbox
if (cpu == "jmb") carto_dir <- ("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/data/carto")
if(cpu == "jmbSML") carto_dir <- ("C:/Users/jmb/SML Dropbox/data/carto")

# ------------------------------------------------------------------------------
# 2. Create data paths for planet-api R project --------------------------------
input_dir <- paste(main_dir, "input", sep="/")
if (!dir.exists(input_dir)) dir.create(input_dir, recursive = TRUE)

output_dir <- paste(main_dir, "output", sep="/")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


# ------------------------------------------------------------------------------
# 3. Global Fishing Watch API key
if(cpu == "jmb")   f <- "C:/Users/J. Menéndez Blázquez/Desktop/R/gfw_api/gfw_api.txt"
if(cpu == "jmbSML") f <- "C:/Users/jmb/R/gfw_api.txt"

key <- paste(readLines(f, warn = FALSE), collapse = "")


# ------------------------------------------------------------------------------
# 4. GEBCO Bathymetry
# if(cpu == "jmb")  bath <- paste0(input_dir, "/gis/gebco/mediterranean_sea_gebco_2024.tif")
# if(cpu == "jmbSML") bath <- paste0(input_dir, "/gis/gebco/mediterranean_sea_gebco_2024.tif")




