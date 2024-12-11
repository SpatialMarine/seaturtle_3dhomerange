
##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
## 10. produce 2d plots -- mkde to raster
##------------------------------------------------------------------------------------------------------------------------------##

## Load libraries

library(mkde)
library(plot3D)
library(rgl)
library(plot3Drgl)
library(RColorBrewer)
library(rasterVis)
library(raster)
library(ncdf4)

##------------------------------------------------------------------------------------------------------------------------------##
## Load data -- if not already loaded:
## -- mkde object
## -- density threshold estimates for 50 and 95 volumes
##------------------------------------------------------------------------------------------------------------------------------##

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

# st <- format(Sys.time(), "%Y-%m-%d")
# manually set "st" to desired date

st <- "2021-02-02"

outdir <- "/Users/jessicaruff/Documents/2021_tortugas/3d_output_2021/"

ptt <- turtid[i]

## create file names:

mkdeobjfile <- paste0(outdir,ptt,"_mkde_obj_", st,".rdata")

resfile <- paste0(outdir,ptt,"_res_", st, ".rdata")

##------------------------------------------------------------------------------------------------------------------------------##
## Day vs. Night:
##------------------------------------------------------------------------------------------------------------------------------##

# DAY FILE NAMES:

# mkdeobjfile <- paste0(outdir,ptt,"_3dmkdeobj_DAY_", st,".rdata")
#   
# resfile <- paste0(outdir,ptt,"_3dres_DAY_", st, ".rdata")

# # NIGHT FILE NAMES:
# 
# mkdeobjfile <- paste0(outdir,ptt,"_3dmkdeobj_NIGHT_", st,".rdata")
# 
# resfile <- paste0(outdir,ptt,"_3dres_NIGHT_", st, ".rdata")
# 
##------------------------------------------------------------------------------------------------------------------------------##

## load files:

# mkde object
load(mkdeobjfile) 
# Load density threshold estimates for 50 and 95 volumes
load(resfile)

##------------------------------------------------------------------------------------------------------------------------------##

# create rasterfile name
rasterfile <- paste0(outdir, ptt,"_mkde3d_","5min_", format(Sys.time(), "%Y%m%d%H%M%S"), ".grd")

# Convert mkde object to raster
mkde.rst <- mkdeToRaster(mkde.obj)
writeRaster(mkde.rst, filename=rasterfile, bandorder='BIL', overwrite=TRUE)

mkde.rst
plot(mkde.rst)

#R script 8
##plotting mkde:
#function within mkde for converting the mkde object to a raster
#this creates a raster stack with a separate raster layer for each depth layer
#in this example, there are 20 depth layers of 10 m deep each, for a total of 200 m in depth

#creating object with breaks at the threshold levels:
brk <- res$threshold

#to plot first 16 layers:
plot(mkde.rst, breaks=brk, col=rainbow(3))
#to plot a subset of layers:
plot((subset(mkde.rst,1:8)), breaks=brk, col=rainbow(3))
#to plot only one layer:
plot((subset(mkde.rst,2)), breaks=brk, col=rainbow(3))
#lines(data$x, data$y) #to add the lines of the turtle track to the plot

plot((subset(mkde.rst,1)), breaks=brk, col=c("gray", "lime green", "cyan", "purple"))
##with different colors

##------------------------------------------------------------------------------------------------------------------------------##





