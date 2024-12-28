


# Fishing effort volume
#calculate fishing effort volume for each turtle


#------------------------------------------------------------------------------
## 3D Habitat Use of Loggerhead Turtles

# Update package and standarized field names following Sequeria et al., 2021
# by Javier Menéndez-Blázquez | @jmenblaz

# 1) Extract fishing effort for extent of seaturtle kde estimate
  # 1.1) Resample fishing effort to 10x10km2 

# 2) Create a raster stack following the different depths of fishing
  # 2.1) depth with no fishing values 0
# 3) Transform fishing effort metric (hours of navigation into proporcional 0-1 values regardign the area) 
# 4) Calculate the volume of fishing effort




# 0) --------------------------------------------------------------------------

output_data <- paste0(output_dir,"/","03_fishing_3d")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

library(sp)
library(sf)
library(raster)



# Specify fishing gear and depth (specify fishing depth in meters)
fishing_gear = "sup_longline" # trawler, etc, etc)
fishing_depth = 30




# 1) Obtain fishinf effort for same area of tracking data ---------------------

# Assign thresholds for plotting:

# MODIFYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
organismID <- 34321
# file <- paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3dmkde_obj.rdata")
# load(file)




# for (organismID in organismID) {}



# read raster stack
udvolume <- stack(paste0("C:/Users/J. Menéndez Blázquez/SML_Dropbox/SML Dropbox/gitdata/seaturtle_3dhomerange/output/01_kde_3d/",organismID,"/",organismID,"_3d_UD_volume_rstack.tif"))
# modify names of the layers
names(udvolume) <- paste("layer", 1:nlayers(udvolume), sep = ".")

# Verificar los nuevos nombres de las capas
names(fishing_stack)

# add CRS
crs(udvolume) <- CRS("EPSG:3035")

# extract raster extension as bounding box to delimited the traking area of organism ID
bb <- extent(udvolume)

xmax <- xmax(bb)
xmin <- xmin(bb)
ymax <- ymax(bb)
ymin <- ymin(bb)

# bounding box of seaturtle track extension as a sfc object
bb <- st_as_sfc(st_bbox(c(
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax
    ), crs = st_crs(3035)))

# convert or transform to from EPSG:3035 to EPSG:4326 (WGS84) bounding box
bb <- st_transform(bb, crs = 4326)
bb <- as(bb, "Spatial")  # transfor, from sfc to sp class


# import fishing effort data (Global fishing Watch, Global Marine Traffic, etc)

# Global Marine Traffic Data 
file <- list.files(paste0(input_dir,"/fishing/"), full.names = T) #MODIFY.....
fishing <- raster(file)

# crop / mask fishing effort to bounding-box of tracking data 
# (kernel density estimate extent)

# for large raster (global datasets), first use crop to delimited minimum convex polygon to bb
# and then masked the less size raster with the bb
fishing <- crop(fishing, bb)  # crop raster
fishing <- raster::mask(fishing, bb)  # mask raster

# plot(fishing)
# plot(bb, add = TRUE)


# transform or reproject to CRS of study
fishing <- projectRaster(fishing, crs = CRS("EPSG:3035"))

# create references raster for resample fishing effort to dimension and exent of tracking data
# same extension, dimension and resolution
reference_raster <- raster(extent(udvolume), res = res(udvolume), crs = crs(fishing))

# resample raster
fishing <- resample(fishing, reference_raster, method = "bilinear")






# ------------------------------------------------------------------------------
# 2) Create a raster stack following the different depths of fishing -----------

# convert from 2D to 3D raster stack fishing effort depends of the fishing gear depth
# example: trawler, etc

# extract number of layers from 
depths <- nlayers(udvolume)

# create a RasterStack from RasterLayer repeating the original layer
fishing <- stack(lapply(1:depths, function(x) fishing))

# modify layers names
names(fishing) <- paste("layer", 1:nlayers(fishing), sep = ".")

# specify the fishing effort in each layer following the "metier" or 
# fishing art/gear features
# each layer 10 meters

fsh_layer = (fishing_depth/10)

# For each Rasterstack layer, except the objective fishing layer and the top layers,
# change values for 0

for (i in (fsh_layer + 1):nlayers(fishing)) {
  # change layer values by 0 (no fishing effort == no fishing volume)
  fishing[[i]][] <- 0
}

# check
# plot(fishing)

# Potential NA as 0 also:
fishing <- calc(fishing, fun = function(x) {
  x[is.na(x)] <- 0  # Reemplazar NA por 0
  return(x)
})


# save/export 3D fishing effort by spatial extent of organismID
# 10x10 km2
rst_file <- paste0(output_data,"/",organismID,"_3d_fishing-effort.tif")
writeRaster(fishing, rst_file, overwrite=TRUE)


























 

