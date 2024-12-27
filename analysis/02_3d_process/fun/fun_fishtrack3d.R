

# fishtrack 3D functions
# https://github.com/aspillaga/fishtrack3d






#------------------------------------------------------------------------------

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
predictKde <- function(kde, raster, depths) {
  
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
                                                     z = d)) # depth positives
    return(rast.t)
  })
  
  pred <- raster::stack(pred)
  pred <- pred / sum(raster::values(pred))
  names(pred) <- paste0("d", depths)
  
  return(pred)
  
}



# ------------------------------------------------------------------------------
# volumeUD

#' Calculate utilization distribution probability volumes
#'
#' This function calculates the utilization distribution probability volumes
#' from 2D or 3D UD values.
#'
#' @param ud a \code{RasterLayer} (2D), \code{RasterStack} or
#'     \code{RasterBrick} (3D) object with UD values.
#' @param ind.layer logical. If \code{TRUE}, the UD volume is calculated for
#'     each layer separately (each layer in the raster belongs to a different
#'     individual or a different time-period). If \code{FALSE} (the default),
#'     UD volume is calculated taking into account all the layers (for UD-3D,
#'     where all the layers correspond to different depth-intervals for the
#'     same individual and time-period).
#'
#' @return a \code{RasterLayer} or a \code{RasterStack} object with UD
#'     probability volumes.
#'
#' @export


# Modify version by J.Menéndez-Blázquez in order to not invert values

volumeUD <- function(ud, ind.layer = FALSE) {
  
  if (ind.layer & nlayers(ud) > 1) {
    return(stack(lapply(unstack(ud), volumeUD)))
  }
  
  # Check if arguments are correct =============================================
  if (is.null(ud) | !class(ud) %in% c("RasterLayer", "RasterStack",
                                      "RasterBrick")) {
    stop(paste("Utilization distributions ('ud') must be in a 'RasterLayer',",
               "'RasterStack' or 'RasterBrick' object."), call. = FALSE)
  }
  
  if (round(sum(values(ud), na.rm = TRUE), 7) != 1) {
    stop("All the UDs must sum 1.")
  }
  
  names <- names(ud)
  
  # Calcular la suma acumulada sin invertir el orden
  values_sorted <- sort(raster::values(ud))
  cumsum_values <- cumsum(values_sorted)
  
  # Asignar la suma acumulada de vuelta a los valores del raster
  raster::values(ud) <- cumsum_values[rank(raster::values(ud))]
  
  # Restablecer los nombres de las capas
  names(ud) <- names
  
  return(ud)
}



# 
# volumeUD <- function(ud, ind.layer = FALSE) {
#   
#   if (ind.layer & nlayers(ud) > 1) {
#     return(stack(lapply(unstack(ud), volumeUD)))
#   }
#   
#   # Check if arguments are correct =============================================
#   if (is.null(ud) | !class(ud) %in% c("RasterLayer", "RasterStack",
#                                       "RasterBrick")) {
#     stop(paste("Utilization distributions ('ud') must be in a 'RasterLayer',",
#                "'RasterStack' or 'RasterBrick' object."), call. = FALSE)
#   }
#   
#   if (round(sum(values(ud), na.rm = TRUE), 7) != 1) {
#     stop("All the UDs must sum 1.")
#   }
#   
#   names <- names(ud)
#   rank <- (1:length(raster::values(ud)))[rank(raster::values(ud))]
#   raster::values(ud) <- 1 - cumsum(sort(raster::values(ud)))[rank]
#   names(ud) <- names
#   
#   return(ud)
#   
# }









#-------------------------------------------------------------------------------

' Spatial overlap between two utilization distributions
#'
#' This function takes two \code{RasterLayer}, \code{RasterStack} or
#' \code{RasterBrick} objects with UD volumes and calculates the proportion
#' of the volume that is overlapped within a probability contour.
#'
#' @param ud1,ud2  \code{RasterLayer}, \code{RasterStack} or \code{RasterBrick}
#'     objects with the UD volumes to overlap. If it is a \code{RasterStack} or
#'     \code{RasterBrick} object, the number and name of the layers must
#'     coincide.
#' @param level UD volume probability contour to be used to calculate the
#'     volume overlap.
#' @param symmetric logical. If \code{TRUE}, the overlapped index is calculated
#'     referred to the total joint volume of the two UDs (volume(overlapped) /
#'     volume(\code{ud1}) + volume(\code{ud2})). If \code{FALSE}, two overlap
#'     indexes are calculated, the first one referred to the volume of
#'     \code{ud1} (volume(overlapped) / volume(\code{ud1})), and the second one
#'     referred to the volume of \code{ud2} (volume(overlapped) /
#'     volume(\code{ud2})).
#'
#' @return A vector with one (if \code{symmetric == TRUE} or two
#'     (\code{symmetric == FALSE}) overlap indexes.
#'
#' @export
#'
#'
volOverlap <- function(ud1, ud2, level, symmetric = TRUE) {
  
  # Check if arguments are correct =============================================
  if (is.null(ud1) | is.null(ud2) | class(ud1) != class(ud2) |
      any(!c(class(ud1), class(ud2)) %in% c("RasterLayer", "RasterBrick",
                                            "RasterStack"))) {
    stop(paste("Both UDs ('ud1' and 'ud2') must be provided as a ",
               "'RasterLayer', 'RasterBrick' or 'RasterStack' object."),
         call. = FALSE)
  }
  
  if (class(ud1) %in% c("RasterBrick", "RasterStack") &
      any(names(ud1) != names(ud2)) |
      any(raster::res(ud1) != raster::res(ud2))) {
    stop("The two UDs must have the same resolution and extension.",
         call. = FALSE)
  }
  
  if (is.null(level) | class(level) != "numeric" | level < 0 | level > 1) {
    stop("The probability contour ('level') must be a value between 0 and 1.",
         call. = FALSE)
  }
  
  data1 <- raster::values(ud1) <= level
  data2 <- raster::values(ud2) <= level
  
  overlap <- sum(data1 & data2)
  
  if (symmetric) {
    total <- sum(data1 | data2)
    return(round(overlap / total, 3))
  } else {
    return(round(c(overlap / sum(data1), overlap / sum(data2)), 3))
  }
  
}

