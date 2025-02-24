

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles and interact with fisheries

# Javier Menéndez-Blázquez | @jmenblaz

# Plot 3D UD difference between day and night volumes

# 0)  Load libraries

library(htmlwidgets)
library(raster)
library(rgl)
library(sf)
library(mkde)
library(dplyr)
library(plot3Drgl)
library(orientlib)

# 1) import organismID for location data, kde results, and ttdr data for plot
# 181762
# 200043
# 34327
# 20046

organismID <- "200043"

# --------------------
# DAY --------------------------------------------------------------------------
# --------------------

# 1) mport kde data for oganism ID DAY

# Prepare and add 3D 50/95UD of organism ID 
# load  kde result by organism ID
kde_res <- read.csv(paste0(main_dir,"/output/01_kde_3d/kde_3d_res_day.csv"))
kde_res<- kde_res %>% filter(kde_res$organismID == !!organismID)

# # Assign thresholds for plotting
threshold.95 <- kde_res$threshold.95  
threshold.50 <- kde_res$threshold.50

# load mkde for 3D UD
file <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3dmkde_obj_day.rdata")
load(file)
# load kde results
res <- read.csv(paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3d_res_day.csv"))

# load ttdr data for plotting suplementaty fig of 3D tracks in details
# ttdr <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3d_ttdr.rdata")
# load(ttdr)


# 2) use threshold of for filter UD KDE 
# result kde threshold
vol95 <- res$threshold.95
vol50 <- res$threshold.50

# Get array from mkde object
x=mkde.obj$x
y=mkde.obj$y
z=mkde.obj$z*(-1) # change depth to negative for plotting
F=mkde.obj$d



# 3) 3D Plot day volumes  ----------------------------------------------------------
# add UD 50
    isosurf3D(x, y, z, F, level = c(vol50, vol95), 
              col = c("#CD9B1D", "#FFFACD"), 
              clab = "F", alpha = 0.7, plot=FALSE, zlim = c(0,-230), 
              ticktype = "z",
              # labs axi
              xlab = "X", 
              ylab = "Y", 
              zlab = "Depth (m)",
              # width of mesh lines
              lwd = 1,
              # 3Dmesh lines color
              # border = "grey50",
              # lithing and shade
              lighting = FALSE,
              # shade = 1,
              # facets = TRUE, 
              # view
              theta = 120,
              phi = 30,
              # legend
              colkey = NULL)
    



# Plot rgl 3D final result
plotrgl(lighting = FALSE, smooth = TRUE)


##  edit plot (theme)
# add values to Z axi
axis3d("z", at = seq(-230, 0, by = 20), labels = seq(-230, 0, by = 20))

# view
rgl.viewpoint(theta = 0, phi = -78, fov = 20, zoom = 0.85)

# rotate plot
# play3d(spin3d(axis = c(0, 0, 1), rpm = 4))


# # extact informaton about view angles
    # myUserMatrix <- par3d()$userMatrix
    # myZoom <- par3d()$zoom
    # myObserver <- par3d()$observer
    # 
    # theta <- rglToBase(myUserMatrix)$theta
    # phi <- rglToBase(myUserMatrix)$phi
    # # info
    # cat("Theta:", theta, "\nPhi:", phi, "\n")
    # # use for current position
    # 
    # rgl.viewpoint(theta = theta_current, phi = phi_current, fov = 20, zoom = 0.85)

# # extact informaton about view angles from UDs daynight
# myUserMatrix <- par3d()$userMatrix


myUserMatrix <- matrix(c(
  0.7411513,  0.6712042, -0.0133992, 0,
  -0.2482617,  0.2925681,  0.9234552, 0,
  0.6237473, -0.6810936,  0.3834713, 0,
  0.0000000,  0.0000000,  0.0000000, 1
), nrow = 4, byrow = TRUE)


# myZoom <- par3d()$zoom
# myObserver <- par3d()$observer
# 
# theta <- rglToBase(myUserMatrix)$theta
# phi <- rglToBase(myUserMatrix)$phi
# # info
# cat("Theta:", theta, "\nPhi:", phi, "\n")
# # use for current position

# view point using a custom user matrix for export / save 3D plots
rgl.viewpoint(userMatrix = myUserMatrix, zoom = 0.83)
    
    
# Export plots  a formato web ------------------------------------------------
# save rgl window
rgl_widget <- rglwidget(width = 2560, height = 1440)

# export as .png
rgl.snapshot(paste0(output_dir,"/fig/3d_UD_day.png"), fmt="png")
    
# save as HTML interactive
saveWidget(rgl_widget, paste0(output_dir,"/fig/3d_UD_day.html"))

# export as .svg
rgl.postscript(paste0(output_dir,"/fig/3d_UD_day.svg"), fmt = "svg")







# --------------------
# NIGHT  -----------------------------------------------------------------------
# -----------------------

# 1) mport kde data for oganism ID DAY

# Prepare and add 3D 50/95UD of organism ID 
# load  kde result by organism ID
kde_res <- read.csv(paste0(main_dir,"/output/01_kde_3d/kde_3d_res_night.csv"))
kde_res<- kde_res %>% filter(kde_res$organismID == !!organismID)

# # Assign thresholds for plotting
threshold.95 <- kde_res$threshold.95  
threshold.50 <- kde_res$threshold.50

# load mkde for 3D UD
file <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3dmkde_obj_night.rdata")
load(file)
# load kde results
res <- read.csv(paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3d_res_night.csv"))

# load ttdr data for plotting suplementaty fig of 3D tracks in details
# ttdr <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3d_ttdr.rdata")
# load(ttdr)


# 2) use threshold of for filter UD KDE 
# result kde threshold
vol95 <- res$threshold.95
vol50 <- res$threshold.50

# Get array from mkde object
x=mkde.obj$x
y=mkde.obj$y
z=mkde.obj$z*(-1) # change depth to negative for plotting
F=mkde.obj$d



# 3) 3D Plot night volumes  ----------------------------------------------------------
# add UD 50
isosurf3D(x, y, z, F, level = c(vol50, vol95), 
          col = c("#2b506f", "#CAE1FF"), 
          clab = "F", alpha = 0.6, plot=FALSE, zlim = c(0,-230), 
          ticktype = "z",
          # labs axi
          xlab = "X", 
          ylab = "Y", 
          zlab = "Depth (m)",
          # width of mesh lines
          lwd = 1,
          # 3Dmesh lines color
          # border = "grey50",
          # lithing and shade
          lighting = FALSE,
          # shade = 1,
          # facets = TRUE, 
          # view
          theta = 120,
          phi = 30,
          # legend
          colkey = NULL)




# Plot rgl 3D final result
plotrgl(lighting = FALSE, smooth = TRUE)


##  edit plot (theme)
# add values to Z axi
axis3d("z", at = seq(-230, 0, by = 20), labels = seq(-230, 0, by = 20))

# view
rgl.viewpoint(theta = 0, phi = -78, fov = 20, zoom = 0.85)

# rotate plot
# play3d(spin3d(axis = c(0, 0, 1), rpm = 4))


# # extact informaton about view angles
# myUserMatrix <- par3d()$userMatrix
# myZoom <- par3d()$zoom
# myObserver <- par3d()$observer
# 
# theta <- rglToBase(myUserMatrix)$theta
# phi <- rglToBase(myUserMatrix)$phi
# # info
# cat("Theta:", theta, "\nPhi:", phi, "\n")
# # use for current position
# 
# rgl.viewpoint(theta = theta_current, phi = phi_current, fov = 20, zoom = 0.85)

# # extact informaton about view angles from UDs daynight
# myUserMatrix <- par3d()$userMatrix


myUserMatrix <- matrix(c(
  0.7411513,  0.6712042, -0.0133992, 0,
  -0.2482617,  0.2925681,  0.9234552, 0,
  0.6237473, -0.6810936,  0.3834713, 0,
  0.0000000,  0.0000000,  0.0000000, 1
), nrow = 4, byrow = TRUE)


# myZoom <- par3d()$zoom
# myObserver <- par3d()$observer
# 
# theta <- rglToBase(myUserMatrix)$theta
# phi <- rglToBase(myUserMatrix)$phi
# # info
# cat("Theta:", theta, "\nPhi:", phi, "\n")
# # use for current position

# view point using a custom user matrix for export / save 3D plots
rgl.viewpoint(userMatrix = myUserMatrix, zoom = 0.83)


# Export plots  a formato web ------------------------------------------------
# save rgl window
rgl_widget <- rglwidget(width = 2560, height = 1440)

# export as .png
rgl.snapshot(paste0(output_dir,"/fig/3d_UD_night.png"), fmt="png")

# save as HTML interactive
saveWidget(rgl_widget, paste0(output_dir,"/fig/3d_UD_night.html"))

# export as .svg
rgl.postscript(paste0(output_dir,"/fig/3d_UD_night.svg"), fmt = "svg")














