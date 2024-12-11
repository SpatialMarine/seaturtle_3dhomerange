
##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
## 2. Process data
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
# Process data steps:
#
# calculate vertical error
# calculate horizontal error
# reproject to metric system
# filter out data with NA depth data
#
##------------------------------------------------------------------------------------------------------------------------------##

## Convert date and time to correct format
ttdr$date <- parse_date_time(ttdr$date, "Ymd HMS")

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

## Reproject to metric system -- 6 hour ssm
xy <- reproject(ssm$lon, ssm$lat)
ssm <- cbind(ssm, xy)

## Reproject to metric system -- 5 min ttdr
xy <- reproject(ttdr$lon, ttdr$lat)
ttdr <- cbind(ttdr, xy)

## Filter out NA data in depth
ssm <- dplyr::filter(ssm, !is.na(depth_mean))
ttdr <- dplyr::filter(ttdr, !is.na(depth))

## Remove extra objects not needed
rm(resamp, xy)

##------------------------------------------------------------------------------------------------------------------------------##




