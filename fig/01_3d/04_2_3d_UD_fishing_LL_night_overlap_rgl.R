

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles and interact with fisheries

# Javier Menéndez-Blázquez | @jmenblaz

# Plot 3D UD and overlap with fisheries
# NIGHT


# 0)  Load libraries ---------------------------------------------------

library(htmlwidgets)
library(raster)
library(rgl)
library(sf)
library(mkde)
library(dplyr)
library(plot3Drgl)
library(orientlib)
library(plot3D)

gif_plot <- TRUE

# 1) import organismID for location data, kde results, and ttdr data for plot
# 181762
# 200043
# 34327
# 20046
organismID <- "200043"

# select time period
daynight <- "night"

# Prepare and add 3D 50/95UD of organism ID 
# load  kde result by organism ID
kde_res <- read.csv(paste0(main_dir,"/output/01_kde_3d/kde_3d_res_",daynight,".csv"))
kde_res <- kde_res %>% filter(kde_res$organismID == !!organismID)

# # Assign thresholds for plotting
threshold.95 <- kde_res$threshold.95  
threshold.50 <- kde_res$threshold.50


# load mkde for 3D UD 
# and time period (daynight)
file <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3dmkde_obj_",daynight,".rdata")
load(file)

# load kde results
res <- read.csv(paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3d_res_",daynight,".csv"))
# load ttdr data for plotting suplementaty fig of 3D tracks in details
ttdr <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3d_ttdr_",daynight,".rdata")
load(ttdr)





# 2) use threshold of for filter UD KDE 
# result kde threshold
vol95 <- res$threshold.95
vol50 <- res$threshold.50

# Get array from mkde object
x=mkde.obj$x
y=mkde.obj$y
z=mkde.obj$z*(-1) # change depth to negative for plotting
F=mkde.obj$d


# 3) load fishing effort from extension of organism ID processed previously
# 3.1) Longlines - --------------------------
fishing_gear <- "LL"

# load fishing data for this specific fishing gear
rstack <- raster::stack(paste0(main_dir,"/output/03_fishing_3d_daynight/",organismID,"_3d_fishing-effort_",fishing_gear,"_",daynight,".tif"))
crs(rstack) <- CRS("EPSG:3035")  # add CRS
names(rstack) <- paste("layer", 1:nlayers(rstack), sep = ".")  # rename layers
# plot(rstack)

# transform for WGS84 EPSG: 4326 CRS
# for plotting with coordinates
# rstack <- raster::projectRaster(rstack, crs = CRS("EPSG:4326"))

# Plot transformed raster
# plot(rstack)


# 4) Prepare data for visualize ------
# Trasform coordinates for mkde.objt

# # Crear un data.frame con las coordenadas en EPSG:3035
# coords <- data.frame(x = mkde.obj$x, y = mkde.obj$y)
# coords <- expand.grid(x = mkde.obj$x, y = mkde.obj$y)
# 
# # Convertir a objeto sf con el CRS original EPSG:3035
# coords <- st_as_sf(coords, coords = c("x", "y"), crs = 3035)
# 
# # Transformar a EPSG:4326
# coords <- st_transform(coords, crs = 4326)
# 
# # Extraer las coordenadas transformadas
# coords <- st_coordinates(coords)
# 
# # Asignar las nuevas coordenadas
# x <- coords[,1]  # lon
# y <- coords[,2]  # lat


# 3.2) Rasterstack as cloudpoint for 3D visualization

# Extraer coordenadas y valores de cada capa
points_df <- as.data.frame(rasterToPoints(rstack))
# Renombrar las columnas para mayor claridad
colnames(points_df) <- c("X", "Y", paste0("Layer_", 1:nlayers(rstack)))

# Convertir a formato largo para obtener una columna Z basada en los layers
library(reshape2)
long_df <- melt(points_df, id.vars = c("X", "Y"), variable.name = "Layer", value.name = "Value")
long_df <- na.omit(long_df)
# Extraer el número de capa y calcular la altura Z
long_df$Z <- as.numeric(gsub("Layer_", "", long_df$Layer)) * 10
# select variables
long_df <- long_df[, c("X", "Y", "Z")]

# # Check long dataframe
# head(long_df)
# tail(long_df)


# 3.3) load intersect volume -- -

# load fishing intersection for this specific fishing gear 
# and day period by organism ID
rstack <- raster::stack(paste0(main_dir,"/output/03_fishing_3d_overlap_daynight/",organismID,"_3d_kde_fishing_intersect_",fishing_gear,"_",daynight,".tif"))
crs(rstack) <- CRS("EPSG:3035")  # add CRS
names(rstack) <- paste("layer", 1:nlayers(rstack), sep = ".")  # rename layers

# filter raster by values
rstack <- calc(rstack, fun = function(x) { ifelse(x >= threshold.50, x, NA) })

# pointscloud of intersect volume
# extract coordinates and values
points_df <- as.data.frame(rasterToPoints(rstack))
# Renombrar las columnas para mayor claridad
colnames(points_df) <- c("X", "Y", paste0("Layer_", 1:nlayers(rstack)))

# Convertir a formato largo para obtener una columna Z basada en los layers
intersect_df <- reshape2::melt(points_df, id.vars = c("X", "Y"), variable.name = "Layer", value.name = "Value")
intersect_df <- na.omit(intersect_df)
# Extraer el número de capa y calcular la altura Z
intersect_df$Z <- as.numeric(gsub("Layer_", "", intersect_df$Layer)) * 10
# select variables
intersect_df <- intersect_df[, c("X", "Y", "Z")]



# 5) 3D Plot ----------------------------------------------------------
# Plot separate UD50 and UD 95 for improve visualization
# "#999ce8","#9396db", "#868ada", "#787bc4"

# 5.1) UD 50 -------------------------------- -------------------------------------
    
 plot3D::isosurf3D(x, y, z, F, level = c(vol50), 
              col = c("#6FA4D9"),
              clab = "F", alpha = 0.3, plot=FALSE, zlim = c(0,-200), 
              ticktype = "z",
              # labs axi
              xlab = "X", 
              ylab = "Y", 
              zlab = "Depth (m)",
              # width of mesh lines
              # = 0.9,
              # 3Dmesh lines color
              # border = "grey60",
              # lithing and shade
              lighting = FALSE,
              # shade = 1,
              # facets = TRUE, 
              # view
              theta = 120,
              phi = 30,
              # legend
              colkey = NULL)
    
    # fishing effort ----------------------------------
    # add fishing effort raster points
    plot3D::points3D(long_df$X, long_df$Y, long_df$Z*(-1), 
                     col="salmon1", alpha = 0.02,
                     pch = 19, # shape
                     cex = 1.1, # size
                     lit = TRUE, 
                     plot = FALSE,
                     add = TRUE)
    
    
    # add intermediate values
    # add filter pointcloud by maximum depth
    long_df_inter <- long_df %>% filter(Z != max(long_df$Z))
    # plot intermediate points (each 1 meter)
    for (i in 1:9) {
      plot3D::points3D(long_df_inter$X, long_df_inter$Y, (long_df_inter$Z*(-1)) - i, 
                       col="salmon1", alpha = 0.02,
                       pch = 19, # shape
                       cex = 1.1, # size
                       lit = TRUE, 
                       plot = FALSE,
                       add = TRUE)
    }
    
    # add marked minimun and maximum depths of fishing effort
    long_df_minmax <- long_df %>% filter(Z == max(long_df$Z) | Z == min(long_df$Z))
    # add
    plot3D::points3D(long_df_minmax$X, long_df_minmax$Y, long_df_minmax$Z*(-1),
                     col="grey10", alpha = 0.01,
                     pch = 19, # shape
                     cex = 1.1, # size
                     lit = TRUE,
                     plot = FALSE,
                     add = TRUE)
    
    
    # Intersect ---------------------------------------------------------
    # add intersect raster points
    plot3D::points3D(intersect_df$X, intersect_df$Y, intersect_df$Z*(-1), 
                     col="#582525", alpha = 0.25,
                     pch = 19, # shape
                     cex = 1.5, # size
                     lit = TRUE, 
                     plot = FALSE,
                     add = TRUE)
    
    intersect_df_inter <- intersect_df %>% filter(Z != max(long_df$Z))
    # add intermediate points (each 1 meter)
    for (i in 1:9) {
      plot3D::points3D(intersect_df_inter$X, intersect_df_inter$Y, (intersect_df_inter$Z*(-1)) - i, 
                       col = "#582525", alpha = 0.1,
                       pch = 20, # shape
                       cex = 2.2, # size
                       lit = TRUE, 
                       plot = FALSE,
                       add = TRUE)
    }
    
  
    
# Plot rgl 3D final result
plotrgl(lighting = FALSE, smooth = FALSE)


##  edit plot (theme)
# add values to Z axi
axis3d("z", at = seq(-200, 0, by = 20), labels = seq(-200, 0, by = 20))

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

# # extact informaton about view angles
# myUserMatrix <- par3d()$userMatrix


myUserMatrix <- matrix(c(
  0.48786455,  0.872916102, -0.002381701,  0,
  -0.01248934,  0.009708285,  0.999874711,  0,
  0.87282991, -0.487773567,  0.015638478,  0,
  0.00000000,  0.000000000,  0.000000000,  1
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
    
    
# Export plots to HTML ------------------------------------------------
# save rgl window
rgl_widget <- rglwidget(width = 2560, height = 1440)

# export as .png
rgl.snapshot(paste0(output_dir,"/fig/3d_50UD_",fishing_gear,"_fishing_overlap_",daynight,".png"), fmt="png")
    
# save as HTML interactive
saveWidget(rgl_widget, paste0(output_dir,"/fig/3d_50UD_",fishing_gear,"_fishing_overlap_",daynight,".html"))

# export as .svg
rgl.postscript(paste0(output_dir,"/fig/3d_50UD_",fishing_gear,"_fishing_overlap_",daynight,".svg"), fmt = "svg")




# v2 ----

# -----------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 5.2) UD 95 -------------------------------------------------------------------

# load fishing data for this specific fishing gear per daynight period
rstack <- raster::stack(paste0(main_dir,"/output/03_fishing_3d_overlap_daynight/",organismID,"_3d_kde_fishing_intersect_",fishing_gear,"_",daynight,".tif"))
crs(rstack) <- CRS("EPSG:3035")  # add CRS
names(rstack) <- paste("layer", 1:nlayers(rstack), sep = ".")  # rename layers

# filter raster by values of 95 threshold
rstack <- calc(rstack, fun = function(x) { ifelse(x >= threshold.95, x, NA) })

# pointscloud of intersect volume
# extract coordinates and values
points_df <- as.data.frame(rasterToPoints(rstack))
# Renombrar las columnas para mayor claridad
colnames(points_df) <- c("X", "Y", paste0("Layer_", 1:nlayers(rstack)))

# Convertir a formato largo para obtener una columna Z basada en los layers
intersect_df <- reshape2::melt(points_df, id.vars = c("X", "Y"), variable.name = "Layer", value.name = "Value")
intersect_df <- na.omit(intersect_df)
# Extraer el número de capa y calcular la altura Z
intersect_df$Z <- as.numeric(gsub("Layer_", "", intersect_df$Layer)) * 10
# select variables
intersect_df <- intersect_df[, c("X", "Y", "Z")]



# plot 3D rgl

plot3D::isosurf3D(x, y, z, F, level = c(vol95), 
                  col = c("#6FA4D9"),
                  clab = "F", alpha = 0.3, plot=FALSE, zlim = c(0,-200), 
                  ticktype = "z",
                  # labs axi
                  xlab = "X", 
                  ylab = "Y", 
                  zlab = "Depth (m)",
                  # width of mesh lines
                  # = 0.9,
                  # 3Dmesh lines color
                  # border = "grey60",
                  # lithing and shade
                  lighting = FALSE,
                  # shade = 1,
                  # facets = TRUE, 
                  # view
                  theta = 120,
                  phi = 30,
                  # legend
                  colkey = NULL)

# fishing effort ----------------------------------
# add fishing effort raster points
plot3D::points3D(long_df$X, long_df$Y, long_df$Z*(-1), 
                 col="salmon1", alpha = 0.02,
                 pch = 19, # shape
                 cex = 1.1, # size
                 lit = TRUE, 
                 plot = FALSE,
                 add = TRUE)


# add intermediate values
# add filter pointcloud by maximum depth
long_df_inter <- long_df %>% filter(Z != max(long_df$Z))
# plot intermediate points (each 1 meter)
for (i in 1:9) {
  plot3D::points3D(long_df_inter$X, long_df_inter$Y, (long_df_inter$Z*(-1)) - i, 
                   col="salmon1", alpha = 0.02,
                   pch = 19, # shape
                   cex = 1.1, # size
                   lit = TRUE, 
                   plot = FALSE,
                   add = TRUE)
}

# add marked minimun and maximum depths of fishing effort
long_df_minmax <- long_df %>% filter(Z == max(long_df$Z) | Z == min(long_df$Z))
# add
plot3D::points3D(long_df_minmax$X, long_df_minmax$Y, long_df_minmax$Z*(-1),
                 col="grey10", alpha = 0.01,
                 pch = 19, # shape
                 cex = 1.1, # size
                 lit = TRUE,
                 plot = FALSE,
                 add = TRUE)


# Intersect ---------------------------------------------------------
# add intersect raster points
plot3D::points3D(intersect_df$X, intersect_df$Y, intersect_df$Z*(-1), 
                 col="#582525", alpha = 0.25,
                 pch = 19, # shape
                 cex = 1.5, # size
                 lit = TRUE, 
                 plot = FALSE,
                 add = TRUE)

intersect_df_inter <- intersect_df %>% filter(Z != max(long_df$Z))
# add intermediate points (each 1 meter)
for (i in 1:9) {
  plot3D::points3D(intersect_df_inter$X, intersect_df_inter$Y, (intersect_df_inter$Z*(-1)) - i, 
                   col = "#582525", alpha = 0.1,
                   pch = 20, # shape
                   cex = 2.2, # size
                   lit = TRUE, 
                   plot = FALSE,
                   add = TRUE)
}




# Plot rgl 3D final result
plotrgl(lighting = FALSE, smooth = TRUE)


##  edit plot (theme)
# add values to Z axi
axis3d("z", at = seq(-200, 0, by = 20), labels = seq(-200, 0, by = 20))

# view
rgl.viewpoint(theta = 0, phi = -78, fov = 20, zoom = 0.85)

# extact informaton about view angles
# myUserMatrix <- par3d()$userMatrix

# matryx for UD95
myUserMatrix <- matrix(c(
  -0.63131070, -0.77536571, -0.01595954,  0,
  0.05698887, -0.06690454,  0.99613017,  0,
  -0.77343291,  0.62795848,  0.08642469,  0,
  0.00000000,  0.00000000,  0.00000000,  1
), nrow = 4, byrow = TRUE)

# view point using a custom user matrix for export / save 3D plots
# view matrix created previously in UD50

rgl.viewpoint(userMatrix = myUserMatrix, zoom = 0.83)

# rotate 3D plot
# play3d(spin3d(axis = c(0, 0, 0.1), rpm = 1))

# for smoothing movements
# and for create gifs

if (gif_plot) {
  
  #  1) create frames
  rgl::movie3d(spin3d(axis = c(0, 0, 1), rpm = 2),
               fps = 5,
               duration = 60,
               movie = "3d_95UD_LL_fishing_overlap_night_",
               dir = paste0(output_dir,"/temp_gif"),
               type = "gif",
               convert = NULL,
               clean = FALSE,  # keep picture per FPS --> Step 2
               verbose = TRUE,
               webshot = FALSE)
  
  # 2) create gif
  library(magick)
  
  imgs <- list.files(paste0(output_dir,"/temp_gif"), full.names = TRUE)
  frames <- magick::image_read(imgs)   # lee todas las imágenes
  gif <- magick::image_animate(frames, fps = 5)  # fps = velocidad del GIF
  # export
  magick::image_write(gif, paste0(output_dir,"/fig/3d_95UD_LL_fishing_overlap_night.gif"))
  # remove frames file
  file.remove(imgs)
  
}


# Export plots  HTML format         --------------------------------------------
# save rgl window
rgl_widget <- rglwidget(width = 2560, height = 1440)

# export as .png
rgl.snapshot(paste0(output_dir,"/fig/3d_95UD_",fishing_gear,"_fishing_overlap_",daynight,".png"), fmt="png")

# save as HTML interactive
saveWidget(rgl_widget, paste0(output_dir,"/fig/3d_95UD_",fishing_gear,"_fishing_overlap_",daynight,".html"))

# export as .svg
rgl.postscript(paste0(output_dir,"/fig/3d_95UD_",fishing_gear,"_fishing_overlap_",daynight,".svg"), fmt = "svg")



















