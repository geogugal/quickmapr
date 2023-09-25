#' Select
#' 
#' Interactively select a single \code{sp} or \code{raster} object and return the 
#' object. \code{s()} only accepts a single input point.
#' 
#' @param qmap_obj a \code{qmap} object from which to select features.
#' @param s_idx a numeric value specifying which data layer to select or a 
#'        character string indicating the name of the layer. Defaults to 1.
#' @param loc A list with an x and y numeric indicating a location.  Default is 
#'            to interactively get loc value.  
#' @return  Returns a selected \code{sp} object
#' 
#' @export
#' @import sp 
#' @examples
#' \dontrun{
#' data(lake)
#' qm<-qmap(list(lake,elev,samples))
#' s(qm,"lake")
#' s(qm,3)
#' }
s <- function(qmap_obj = NULL, s_idx = 1, loc = NULL) {
    if (!inherits(qmap_obj, "qmap")) {
        stop("Requires a valid qmap_obj.")
    } else {
        spdata <- qmap_obj$map_data[[s_idx]]
    }
    loc <- locator(1)
    switch(EXPR = get_sp_type(spdata), 
           polygon = s_poly(spdata, loc), 
           line = s_line(spdata, loc), 
           point = s_point(spdata, loc))
    }

#' select Polys
#' 
#' @import sp 
#' @importFrom graphics locator
#' @keywords internal
s_poly <- function(spdata, loc) {
    if(is.null(loc)){
      idx <- sf::st_within(sf::st_as_sf(SpatialPoints(locator(1), 
                                                      CRS(sf::st_crs(spdata)$wkt))), 
                           sparse = FALSE, sf::st_as_sf(spdata), byid = TRUE)[, 1]
    }  else {
      idx <- sf::st_within(sf::st_as_sf(SpatialPoints(loc, 
                                                      CRS(sf::st_crs(spdata)$wkt))), 
                           sparse = FALSE, sf::st_as_sf(spdata), byid = TRUE)[, 1]
    }
    if (sum(idx) == 0) {
        message("No polygon features at that location.")
        return(NULL)
    }
    if (regexpr("DataFrame", class(spdata)) > 0) {
        data <- spdata@data[idx, ]
    } else {
        data <- NULL
    }
    return(spdata[idx, ])
}

#' select Lines
#' 
#' @import sp 
#' @importFrom graphics locator
#' @keywords internal
s_line <- function(spdata, loc) {
  sfdata <- sf::st_as_sf(spdata)
    if (is.null(loc)){ 
      loc_pt <- SpatialPoints(locator(1), CRS(sf::st_crs(spdata)$wkt))
    } else {
      loc_pt <- SpatialPoints(loc, CRS(sf::st_crs(spdata)$wkt))
    }
  idx <- sf::st_is_within_distance(loc_pt, sfdata, 
                                   min(sf::st_distance(loc_pt, sfdata)), 
                                   sparse = FALSE)[1,]
    if (sum(idx) == 0) {
      message("No line features at that location.")
      return(NULL)
    }
    return(spdata[which(idx), ])
}

#' select Points
#' 
#' @import sp 
#' @importFrom graphics locator
#' @keywords internal
s_point <- function(spdata, loc) {
  sfdata <- sf::st_as_sf(spdata)
  if (is.null(loc)){
    loc_pt <- sf::st_as_sf(SpatialPoints(locator(1), CRS(sf::st_crs(spdata)$wkt)))
    idx <- sf::st_is_within_distance(loc_pt, sfdata, 
                                     min(sf::st_distance(loc_pt, sfdata)), 
                                     sparse = FALSE)[1,]
  } else {
    loc_pt <- sf::st_as_sf(SpatialPoints(loc, CRS(sf::st_crs(spdata)$wkt)))
    idx <- sf::st_is_within_distance(loc_pt, sfdata, 
                                     min(sf::st_distance(loc_pt, sfdata)), 
                                     sparse = FALSE)[1,]
  }
    if (sum(idx) == 0) {
        message("No point features at that location.")
        return(NULL)
    }
    return(spdata[which(idx), ])
}
