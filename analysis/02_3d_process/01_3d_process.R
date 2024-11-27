

source("setup.R")
source("analysis/02_3d_process/fun/fun_3d_utils.R") # functions for 3d process


## libraries
library(lubridate)
library(dplyr)
library(akima)
library(ks)
library(mkde)



# dirs
## Set paths -----------------------------------------------------------------

# for smru2L0 function (fun_track_reading.R)
# datadir = "C:/Users/david/Google Drive/TORTUGAS OCEANOGRAFAS/data"


outdir = output_dir


# plot for one turtle:


# Use L1 for ttdr files and L2 locs files (postprocessed tracking data)
ttdr_files <- list.files(paste0(main_dir,"/input/tracking/ttdr/L1"), full.names = TRUE)
locs_files <- list.files(paste0(main_dir,"/input/tracking/loc/L2"), full.names = TRUE, pattern = "L2_loc.csv")

# extract ids for ptt files (individual for sea-turtles)
ptts <- sub("_L2_loc\\.csv$", "", basename(locs_files))



# problem with 200043 ptt - has LOCS but not ttdr data
# select ptt
ptt <- ptts[12]

# import locs and ttdr data for this ptt
ttdrfile <- paste0(main_dir,"/input/tracking/ttdr/L1/",ptt,"_L1_ttdr.csv")
ssmfile <- paste0(main_dir,"/input/tracking/loc/L2/",ptt,"_L2_loc.csv")


# read tracking and ttdr data 
ttdr <- read.csv(ttdrfile, dec=",", head=TRUE)
ttdr <- ttdr |> rename(date = time,
                       depth.up.er = depthUpperError,
                       depth.lo.er = depthLowerError)

ttdr$date <- parse_date_time(ttdr$date, "Ymd HMS")
# convert to numeric:
ttdr <- ttdr |> mutate(across(c(depth.up.er, depth.lo.er, depth, depthRange), as.numeric))



ssm <- read.csv(ssmfile, dec=",", head=TRUE)
ssm <- ssm |> rename(date = time,
                     lat = latitude,
                     lon = longitude)

ssm$date <- parse_date_time(ssm$date, "Ymd HMS")

# Calculate vertical error from TTDR
ttdr$z.error <- z.error(depth.upper = ttdr$depth.up.er, depth.lower = ttdr$depth.lo.er)

# Calculate horizontal error from SSM data
ssm$xy.error <- xy.error(ssm)

# Interpolate horizontal error to TTDR data
ttdr$xy.error <- aspline(x=(as.numeric(ssm$date)), y=(ssm$xy.error), xout=(as.numeric(ttdr$date)))$y

## Resample TTDR data from 5 min to 6H
resamp <- resampTTDR(ttdr, ssm)
ssm <- merge(ssm, resamp, by="date")




## Reproject to metric system
xy <- reproject(ssm$lon, ssm$lat)
ssm <- cbind(ssm, xy)





## Filter out NA data in detph
ssm <- dplyr::filter(ssm, !is.na(depth_mean))

## Calculate 3d HR with mkde
ptt <- ssm$ptt[1]
rasterfile <- paste0(outdir, ptt,"_mkde3d_","6h_", format(Sys.time(), "%Y%m%d%H%M%S"), ".grd")
hr3ddata <- mkde3d(
  x = ssm$x,
  y = ssm$y,
  z = ssm$depth_mean,
  date = ssm$date,
  z.error = ssm$z.error_mean,
  xy.error = ssm$xy.error,
  t.max = 390,
  integration.step = 10,
  voxel.xsize = 10000,
  voxel.ysize = 10000,
  voxel.zsize = 10,
  extend.raster = 10000,
  zll = 0,
  crs = "+init=epsg:3035",
  contours = c(0.50, 0.95),
  rasterfile = rasterfile
)

# Plot mkde3d
r <- stack(rasterfile)  # import raster
#creating object with breaks at the threshold levels:
brk <- hr3ddata$volumnes$threshold
brk <- c(0, brk, max(maxValue(r)))
plot(r, breaks=brk, col=c("white", "green", "red"))




## Calculate 3d HR with ks
# ptt <- ssm$ptt[1]
# rasterfile <- paste0(outdir, ptt,"_mkde3d_","6h_", format(Sys.time(), "%Y%m%d%H%M%S"), ".grd")
ks3ddata <- ks3d(
  x = ssm$x,
  y = ssm$y,
  z = ssm$depth_mean,
  voxel.xsize = 10000,
  voxel.ysize = 10000,
  voxel.zsize = 10,
  extend.raster = 10000,
  zll = 0,
  crs = "+init=epsg:3035"
)


