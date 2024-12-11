
##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
## 9. produce 3d plots -- LOOP
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

## prepare loop

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

## set plotting grid for R 
par(mfrow=c(2,4))

## If using loop to create plots with bathymetry, load bathymetry before loop

##------------------------------------------------------------------------------------------------------------------------------##
## Load bathymetry
##------------------------------------------------------------------------------------------------------------------------------##

nc <- nc_open("/Users/jessicaruff/Documents/2020_roarin/Tortugas_3D_2020/R_scripts_2020/R.Scripts.from.Erasmus.Barce.Folder/Agosto_maps_3dhr/bath.aug1.nc")

lat <- ncvar_get(nc, varid="northing")
lon <- ncvar_get(nc, varid="easting")
topo <- ncvar_get(nc, varid="layer")
nc_close(nc)  # closes netcdf

##------------------------------------------------------------------------------------------------------------------------------##

#loop

for(i in 1:length(turtid)){ }
  
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

# Assign thresholds for plotting:

#res 

vol95 <- res[3,2]
vol50 <- res[1,2]

# Get array from mkde object
x=mkde.obj$x
y=mkde.obj$y
z=mkde.obj$z*(-1)
F=mkde.obj$d

##------------------------------------------------------------------------------------------------------------------------------##
## Visualize 3dmkde
##------------------------------------------------------------------------------------------------------------------------------##
# rgl.open()     # open new device
# rgl.close()    # close current device
# rgl.cur()      # returns active device id
# rgl.set(which) # set device as active
# rgl.quit()     # shutdown rgl device system

##------------------------------------------------------------------------------------------------------------------------------##
## Create 3d plot
##------------------------------------------------------------------------------------------------------------------------------##
# adjust z limit
# main = paste0(ptt," -- Day")
# main = paste0(ptt," -- Night")

isosurf3D(x, y, z, F, level = c(vol50, vol95), 
          col = c("red", "yellow"), 
          clab = "F", alpha = 0.4,  lighting = TRUE, plot=TRUE, main = ptt, zlim = c(0,-100), ticktype = "detailed")  

plotrgl(lighting = TRUE, smooth = TRUE)


#set the window size of rgl 3d plotting device:
par3d(windowRect = c(0, 0, 1000, 1000))

#On some systems you'll need to insert Sys.sleep(1) after the resizing to let the window finish resizing before you take the snapshot. 
Sys.sleep(1)

# assign directory to save plot snapshots
outdir3d <- "/Users/jessicaruff/Documents/2021_tortugas/3d_output_2021/plots/"
# 
filename <- paste0(outdir3d, ptt, "_3d.png")
#filename <- paste0(outdir3d, ptt, "_3d_DAY.png")
#filename <- paste0(outdir3d, ptt, "_3d_NIGHT.png")

#   
rgl.snapshot(filename = filename)
# 
rgl.close()
# 
##------------------------------------------------------------------------------------------------------------------------------##
## Various ways to save 3d plots
##------------------------------------------------------------------------------------------------------------------------------##

# to create pdf
#pdffilename <- paste0(outdir3d, ptt, "_3d.pdf")

#rgl.postscript(pdffilename,fmt="pdf")

# to create 3d HTML object
#writeWebGL(dir = "webGL", filename = file.path(dir, "index.html"))

# browseURL(
#   paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), 
#                               width=500), sep="")
# )

##------------------------------------------------------------------------------------------------------------------------------##
# create 3d plot
# without z limit
##------------------------------------------------------------------------------------------------------------------------------##

# isosurf3D(x, y, z, F, level = c(vol50, vol95), 
#           col = c("red", "yellow"), 
#           clab = "F", alpha = 0.4,  lighting = TRUE, plot=FALSE, main = ptt, ticktype = "detailed")  
# 
# plotrgl(lighting = TRUE, smooth = TRUE)
# 
# #set the window size of rgl 3d plotting device:
# par3d(windowRect = c(0, 0, 1000, 1000))
# 
# #On some systems you'll need to insert Sys.sleep(1) after the resizing to let the window finish resizing before you take the snapshot. 
# Sys.sleep(1)
# 
# # assign directory to save plot snapshots
# outdir3d <- "/Users/jessicaruff/Documents/2021_tortugas/3d_output_2021/plots/"
# # 
# filename <- paste0(outdir3d, ptt, "_3d_2.png")
# #   
# rgl.snapshot(filename = filename)
# # 
# rgl.close()
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
# # 3D plot with volume and points
##------------------------------------------------------------------------------------------------------------------------------##

# points3D(ttdr$x, ttdr$y, ttdr$depth*(-1), col="black",
#          pch = ".", cex = 2, theta = 10, bty = "f", clab = "dg C",
#          colkey = list(side = 1, length = 0.5, width = 0.5,
#                        dist = 0.05, shift = -0.2, side.clab = 3, line.clab = 1,
#                        cex.clab = 0.8, cex.axis = 0.8), add = TRUE)
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
# # 3D plot with track data using lines
##------------------------------------------------------------------------------------------------------------------------------##

# plot3d(ttdr$x, ttdr$y, ttdr$depth*(-1),type = "l",
#        xlab = "", ylab = "", zlab = "",
#        col = "blue", size = 5, alpha = 0.05,
#        lit = TRUE,
#        box = FALSE, axes = FALSE)
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
## Visualize 3dmkde with bathymetry
##------------------------------------------------------------------------------------------------------------------------------##

# nc <- nc_open("/Users/jessicaruff/Documents/2020_roarin/Tortugas_3D_2020/R_scripts_2020/R.Scripts.from.Erasmus.Barce.Folder/Agosto_maps_3dhr/bath.aug1.nc")
# 
# lat <- ncvar_get(nc, varid="northing")
# lon <- ncvar_get(nc, varid="easting")
# topo <- ncvar_get(nc, varid="layer")
# nc_close(nc)  # closes netcdf

# turtle with volume
# main = paste0(ptt," -- Day")
# main = paste0(ptt," -- Night")

persp3D(lon, lat, z = topo, inttype = 2,  d = 2,
        expand = 0.1, colkey = FALSE, col = "grey98", shade = 0.2,
        lighting = TRUE, box = FALSE, axes = FALSE, plot = FALSE, main = ptt)
isosurf3D(x, y, z, F, level = c(vol50, vol95),colkey = FALSE,
          col = c("red", "yellow"), alpha = 0.4,  lighting = TRUE, add=TRUE)  
plotrgl(lighting = TRUE, smooth = TRUE)

##------------------------------------------------------------------------------------------------------------------------------##
