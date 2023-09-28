#' Identify
#' 
#' Interactively select an \code{sp} or \code{raster} object and print the 
#' data associated with it. \code{i()} only accepts a single input point.
#' 
#' @param qmap_obj a \code{qmap} object from which to identify features.
#' @param i_idx a numeric value specifying which data layer to identify or a 
#'        character string indicating the name of the layer. Defaults to 1.
#' @param loc A list with an x and y numeric indicating a location.  Default is 
#'            to interactively get loc value until escaped.  
#' @return  Returns NULL.  Identified values are printed to the screen.
#' 
#' @export
#' @import sp
#' @examples
#' \dontrun{
#' data(lake)
#' qm<-qmap(list(lake,elev,samples))
#' i(qm,"lake")
#' i(qm,"samples")
#' i(qm,2)
#' }
i <- function(qmap_obj = NULL, i_idx = 1, loc = NULL) {
    if (!inherits(qmap_obj, "qmap")) {
        stop("Requires a valid qmap_obj.")
    } else {
        spdata <- qmap_obj$map_data[[i_idx]]
    }
    if (is.null(loc)) {
    message("Click on plot to identify. Press 'Esc' to exit.")
    loc <- locator(1)
    while (!is.null(loc)) {
      switch(EXPR = get_sp_type(spdata), 
             polygon = i_poly(spdata, loc), 
             grid = i_grid(spdata, loc), 
             line = i_line(spdata, loc), 
             point = i_point(spdata, loc))
      loc <- locator(1)
    }
    } else {
      switch(EXPR = get_sp_type(spdata), 
             polygon = i_poly(spdata, loc), 
             grid = i_grid(spdata, loc), 
             line = i_line(spdata, loc), 
             point = i_point(spdata, loc))
  }
}

#' Identify Polys
#' 
#' @import sp
#' @importFrom graphics locator
#' @keywords internal
i_poly <- function(spdata, loc) {
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
    
    sfdata <- sf::st_as_sf(spdata[idx, ])
    idata <- list(data = data,
                  area = sf::st_area(sfdata), 
                  perim = sf::st_length(sf::st_cast(sfdata, "MULTILINESTRING")))
    
    print(idata)
}

#' Identify Lines
#' 
#' @import sp
#' @importFrom graphics locator
#' @keywords internal
i_line <- function(spdata, loc) {
  
  sfdata <- sf::st_as_sf(spdata)
  if (is.null(loc)){ 
    loc_pt <- sf::st_as_sf(SpatialPoints(locator(1), CRS(sf::st_crs(spdata)$wkt)))
  } else {
    loc_pt <- sf::st_as_sf(SpatialPoints(loc, CRS(sf::st_crs(spdata)$wkt)))
  }
  idx <- sf::st_is_within_distance(loc_pt, sfdata, 
                                   min(sf::st_distance(loc_pt, sfdata)), 
                                   sparse = FALSE)[1,]
    if (sum(idx) == 0) {
      message("No line features at that location.")
      return(NULL)
    }
    if (regexpr("DataFrame", class(spdata)) > 0) {
        data <- spdata@data[idx, ]
    } else {
        data <- NULL
    }
    idata <- list(data = data, 
                  length = sf::st_length(sf::st_as_sf(spdata[which(idx),])))
    print(idata)
}

#' Identify Points
#' 
#' @import sp
#' @importFrom graphics locator
#' @keywords internal
i_point <- function(spdata, loc) {
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
    if (regexpr("DataFrame", class(spdata)) > 0) {
        data <- spdata@data[idx, ]
    } else {
        data <- NULL
    }
    idata <- list(data = data, #spobj = spdata[which(idx), ])
                  location = sf::st_coordinates(loc_pt))
    print(idata)
}

#' Identify Rasters
#' 
#' @import sp
#' @importFrom methods as
#' @importFrom graphics locator
#' @keywords internal
i_grid <- function(spdata, loc) {
    
    spdata2 <- as(spdata, "SpatialGridDataFrame")
    if (is.null(loc)){
      loc_pt <- SpatialPoints(locator(1), CRS(sf::st_crs(spdata)$wkt))
      data <- over(SpatialPoints(loc_pt, CRS(sf::st_crs(spdata2)$wkt)), spdata2)
    } else {
      loc_pt <- SpatialPoints(loc, CRS(sf::st_crs(spdata)$wkt))
      data <- over(SpatialPoints(loc, CRS(sf::st_crs(spdata2)$wkt)), spdata2)
    }
    idata <- list(data = data, #spobj = spdata[which(idx), ])
                  location = coordinates(loc_pt))
    print(idata)
} 
