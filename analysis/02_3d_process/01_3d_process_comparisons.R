




source("setup.R")  # set data paths
source("analysis/02_3d_process/fun/fun_3d_utils.R")


# dirs

## Set paths

datadir = "C:/Users/david/Google Drive/TORTUGAS OCEANOGRAFAS/data"

# plot for one turtle:
# Use L1 for ttdr files  and L2 for locs files


ttdrfile = "C:/Users/david/Google Drive/TORTUGAS OCEANOGRAFAS/data/animal/loggerhead/151935/L1/L1_TTDR_151935.csv"
ssmfile = "C:/Users/david/Google Drive/TORTUGAS OCEANOGRAFAS/data/animal/loggerhead/151935/L2/L2_ssm_DCRW_6H_151935.csv"


outdir = "D:/3dhr/"



## libraries
library(lubridate)
library(dplyr)
library(akima)

## import data
ttdr <- read.csv(ttdrfile, sep=";", dec=",", head=TRUE)
ttdr$date <- parse_date_time(ttdr$date, "Ymd HMS")

ssm <- read.csv(ssmfile, sep=";", dec=",", head=TRUE)
ssm$date <- parse_date_time(ssm$date, "Ymd HMS")

## Calculate vertical error from TTDR
ttdr$z.error <- z.error(depth.upper = ttdr$depth.up.er, depth.lower = ttdr$depth.lo.er)

## Calculate horizontal error from SSM data
ssm$xy.error <- xy.error(ssm)

## Interpolate horizontal error to TTDR data
ttdr$xy.error <- aspline(x=(as.numeric(ssm$date)), y=(ssm$xy.error), xout=(as.numeric(ttdr$date)))$y

## Resample TTDR data from 5 min to 6H
resamp <- resampTTDR(ttdr, ssm)
ssm <- merge(ssm, resamp, by="date")

## Reproject to metric system
xy <- reproject(ssm$lon, ssm$lat)
ssm <- cbind(ssm, xy)

## Filter out NA data in detph
ssm <- dplyr::filter(ssm, !is.na(depth_mean))


## Prepare loop

tmax <- c(400, 500)
istep <- c(10, 30)
df <- NULL


for (i in 1:length(tmax)){
  for (j in 1:length(istep)){
    
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
      t.max = tmax[i],
      integration.step = istep[j],
      voxel.xsize = 10000,
      voxel.ysize = 10000,
      voxel.zsize = 10,
      extend.raster = 10000,
      zll = 0,
      crs = "+init=epsg:3035",
      contours = c(0.50, 0.95),
      rasterfile = rasterfile
    )
    
    idf <- data.frame(ptt, method="mkde", hr3ddata$parameters, hr3ddata$volumnes)
    df <- rbind(df, idf)
    
  }
}



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


