

#------------------------------------------------------------------------------
# other custom functions for processing data


# fill_inside_rings() --- remove inside rings "holes", or gaps for sf polygons


#-----------------------------------------------------------------------------
# fill_inside_rings()

# This function check the differenrt objects and remove the inside rings levels

fill_inside_rings <- function(input_sf) {
  input_sf$geometry <- st_sfc(lapply(input_sf$geometry, function(geom) {
    if (inherits(geom, "MULTIPOLYGON")) {
      # Para cada polígono en el multipolígono, conservar solo el primer anillo (el exterior)
      new_polys <- lapply(geom, function(poly) {
        poly[1]  # Conservamos solo el primer anillo
      })
      st_multipolygon(new_polys)
    } else if (inherits(geom, "POLYGON")) {
      # En caso de ser POLYGON, se asume que el primer anillo es el exterior
      st_polygon(list(geom[[1]]))
    } else {
      geom  # Si no es MULTIPOLYGON ni POLYGON, se deja tal cual
    }
  }), crs = st_crs(input_sf))
  
  return(input_sf)
}

