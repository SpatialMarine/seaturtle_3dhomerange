
##------------------------------------------------------------------------------------------------------------------------------##
## 3D Habitat Use of Loggerhead Turtles
## 2021
## By: Jessica Ruff and David March
##------------------------------------------------------------------------------------------------------------------------------##

##------------------------------------------------------------------------------------------------------------------------------##
## 8. produce 3d plots
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
## -- ttdr data frame
## -- mkde object
## -- density threshold estimates for 50 and 95 volumes
##------------------------------------------------------------------------------------------------------------------------------##

# Import data

# # load ttdr dataframe
# 
# load("/Users/jessicaruff/Documents/2020_roarin/Tortugas_3D_2020/3dHR_output_2020_Dec_1/lasi_ttdr_daily.mean.depth.rdata")
# 
# # load mkde object
# 
# load("/Users/jessicaruff/Documents/2020_roarin/Tortugas_3D_2020/3dHR_output_2020_Dec_1/lasi_mkde_daily.mean.depth.rdata")
# 
# # Load density threshold estimates for 50 and 95 volumes
# 
# load("/Users/jessicaruff/Documents/2020_roarin/Tortugas_3D_2020/3dHR_output_2020_Dec_1/lasi_res_daily.mean.depth.rdata")

##------------------------------------------------------------------------------------------------------------------------------##

# Assign thresholds for plotting:

organismID <- 34321
file <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3dmkde_obj.rdata")
load(file)





load("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/34321/34321_3dmkde_obj_night.rdata")


load("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/34321/34321_3dmkde_obj.rdata")

#res
vol95 <- res[3,2]
vol50 <- res[1,2]

# Get array from mkde object
x=mkde.obj$x
y=mkde.obj$y
z=mkde.obj$z*(-1) # change depth to negative for plotting
F=mkde.obj$d

##------------------------------------------------------------------------------------------------------------------------------##
## Visualize 3dmkde
##------------------------------------------------------------------------------------------------------------------------------##

# 3D plot with volume

ptt <- organismID

isosurf3D(x, y, z, F, level = c(vol50, vol95), 
          col = c("red", "yellow"), 
          clab = "F", alpha = 0.4,  lighting = TRUE, plot=FALSE, main = ptt, zlim = c(0,-100), ticktype = "detailed")  
plotrgl(lighting = TRUE, smooth = TRUE)

# clear3d()

# 3D plot with volume and points
plot3D::points3D(ttdr$x, ttdr$y, ttdr$depth*(-1), col="black",
                 pch = ".", cex = 2, theta = 10, bty = "f", clab = "dg C",
                 colkey = list(side = 1, length = 0.5, width = 0.5,
                               dist = 0.05, shift = -0.2, side.clab = 3, line.clab = 1,
                               cex.clab = 0.8, cex.axis = 0.8), add = TRUE)

# 3D plot with track data using lines
rgl::plot3d(ttdr$x, ttdr$y, ttdr$depth*(-1), type = "l",
            xlab = "", ylab = "", zlab = "",
            col = "blue", size = 5, alpha = 0.05,
            lit = TRUE,
            box = FALSE, axes = FALSE)



##------------------------------------------------------------------------------------------------------------------------------##
## Visualize 3dmkde with bathymetry
##------------------------------------------------------------------------------------------------------------------------------##

nc <- nc_open(paste0(carto_dir,"/bathymetry/gebco_2024/med/gebco_2024_n47.0_s30.0_w-10.0_e38.0.nc"))

# nc <- nc_open("/Users/jessicaruff/Documents/2020_roarin/Tortugas_3D_2020/R_scripts_2020/R.Scripts.from.Erasmus.Barce.Folder/Agosto_maps_3dhr/bath.aug1.nc")







lat <- ncvar_get(nc, varid="northing")
lon <- ncvar_get(nc, varid="easting")
topo <- ncvar_get(nc, varid="layer")
nc_close(nc)  # closes netcdf

# turtle with volumne
persp3D(lon, lat, z = topo, inttype = 2,  d = 2,
        expand = 0.1, colkey = FALSE, col = "grey98", shade = 0.2,
        lighting = TRUE, box = FALSE, axes = FALSE, plot = FALSE, main = ptt)
isosurf3D(x, y, z, F, level = c(vol50, vol95),colkey = FALSE,
          col = c("red", "yellow"), alpha = 0.4,  lighting = TRUE, add=TRUE)  
plotrgl(lighting = TRUE, smooth = TRUE)

##------------------------------------------------------------------------------------------------------------------------------##
