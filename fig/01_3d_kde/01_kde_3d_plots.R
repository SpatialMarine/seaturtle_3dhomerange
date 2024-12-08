

# Exploratory plots of 3D kde 

# load kde objects 


load("./data/kde_list.rda")




# 3d plot (more information in ks documentation)
plot(density, display="plot3D", cont=c(50,95),colors=c("purple","green"),drawpoints=TRUE,
     xlab="easting (m)", ylab="northing (m)", zlab="depth (m)",size=2, ptcol="black")


# for interactive plot
plot(density, display="rgl")
plot(density_fishing, display="rgl", add = TRUE)


library(raster)

# Crear un RasterBrick (supongamos que ya lo tienes cargado como 'raster_brick')
raster_brick <- brick("path_to_your_raster_brick.tif")  # Carga tu archivo RasterBrick

# Exportar el RasterBrick directamente a NetCDF
writeRaster(raster_brick, filename = "path_to_save_your_netCDF_file/density.nc", 
            format = "CDF", overwrite = TRUE)

# for plot raster bircks
# load export raster brick

plot(rbrick)
