


load(kde_file)

# use load only, don0t assign it to an object
load("./data/kde_list.rda")



#' Predict utilization distribution (UD) values from a \code{kde} object
#'
#' This function takes a \code{kde} object and predicts the utilization
#' distribution (UD) values for a 3D grid defined by a raster (horizontal
#' resolution) and a sequence of depths (vertical resolution.)
#'
#' @param kde a kde object.
#' @param raster raster to extract the horizontal coordinates of the 3D grid.
#' @param depths vector with depths values defining the vertical resolution of
#'     the 3D grid.
#'
#' @return A \code{RasterStack} object with the UD volumes of a different depth
#'     interval in each layer.
#'
#' @import raster
#' @import sp
#'
#' @export
#'
#'
#'



predictKde <- function(kde, r, depths) {
  
  # Check if arguments are correct =============================================
  if (is.null(kde) | class(kde) != "kde") {
    stop("The 'kde' object must be a 'kde' object from the 'ks' package.",
         call. = FALSE)
  }
  
  if (is.null(raster) | class(raster) != "RasterLayer") {
    stop("The 'raster' object must be a 'RasterLayer' object.", call. = FALSE)
  }
  
  pred <- lapply(depths, function(d) {
    rast.t <- raster::raster(raster)
    raster::values(rast.t) <- predict(kde,
                                      x = data.frame(sp::coordinates(rast.t),
                                                     z = -d))
    return(rast.t)
  })
  
  pred <- raster::stack(pred)
  pred <- pred / sum(raster::values(pred))
  names(pred) <- paste0("d", depths)
  
  return(pred)
  
}