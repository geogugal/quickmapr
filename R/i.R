#' Identify
#' 
#' Interactively select an \code{sp} or \code{raster} object and return the 
#' data associated with it. \code{i()} only accepts a single input point.
#' 
#' @param qmap_obj a \code{qmap} object from which to identify features.
#'        An \code{sp} object may also be passed directly
#' @param i_idx a numeric value specifying which data layer to identify or a 
#'        character string indicating the name of the layer. Defaults to 1.
#' @param loc A list with an x and y numeric indicating a location.  Default is 
#'            to interactively get loc value until escaped.  
#' @return  Returns a list that contains data for the selected object (data is
#'          NULL if not a Spatial DataFrame object), the \code{sp} object, and 
#'          additional information for each object (e.g. area and perimeter for
#'          polygons).  
#' 
#' @export
#' @import sp rgeos
#' @examples
#' \dontrun{
#' data(lake)
#' qm<-qmap(list(lake,elev,samples))
#' i(qm,"lake")
#' i(qm,"samples")
#' i(qm,2)
#' }
i <- function(qmap_obj = NULL, i_idx = 1, loc = NULL) {
    if (class(qmap_obj) != "qmap") {
        stop("Requires a valid qmap_obj.")
    } else {
        spdata <- qmap_obj$map_data[[i_idx]]
    }
    switch(EXPR = get_sp_type(spdata), 
           polygon = i_poly(spdata, loc), 
           grid = i_grid(spdata, loc), 
           line = i_line(spdata, loc), 
           point = i_point(spdata, loc))
}

#' Identify Polys
#' 
#' @import sp rgeos
#' @keywords internal
i_poly <- function(spdata, loc) {
    if(is.null(loc)){
      idx <- rgeos::gWithin(SpatialPoints(locator(1), CRS(proj4string(spdata))),
                          spdata, byid = TRUE)[, 1]
    }  else {
      idx <- rgeos::gWithin(SpatialPoints(loc, CRS(proj4string(spdata))),
                            spdata, byid = TRUE)[, 1]
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
    idata <- list(data = data, spobj = spdata[idx, ], 
                  area = gArea(spdata[idx, ]), perim = gLength(spdata[idx, ]))
    
    return(idata)
}

#' Identify Lines
#' 
#' @import sp rgeos
#' @keywords internal
i_line <- function(spdata, loc) {
    if (is.null(loc)){ 
      loc_pt <- SpatialPoints(locator(1), CRS(proj4string(spdata)))
    } else {
      loc_pt <- SpatialPoints(loc, CRS(proj4string(spdata)))
    }
    idx <- gWithinDistance(loc_pt, spdata, gDistance(loc_pt, spdata), byid = T)
    if (sum(idx) == 0) {
        message("No line features at that location.")
        return(NULL)
    }
    if (regexpr("DataFrame", class(spdata)) > 0) {
        data <- spdata@data[idx, ]
    } else {
        data <- NULL
    }
    idata <- list(data = data, spobj = spdata[which(idx), ], length = gLength(spdata[which(idx), 
        ]))
    return(idata)
}

#' Identify Points
#' 
#' @import sp rgeos
#' @keywords internal
i_point <- function(spdata, loc) {
    if (is.null(loc)){
      loc_pt <- SpatialPoints(locator(1), CRS(proj4string(spdata)))
      idx <- gWithinDistance(loc_pt, spdata, gDistance(loc_pt, spdata), byid = T)
    } else {
      loc_pt <- SpatialPoints(loc, CRS(proj4string(spdata)))
      idx <- gWithinDistance(loc_pt, spdata, gDistance(loc_pt, spdata), byid = T)
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
    idata <- list(data = data, spobj = spdata[which(idx), ])
    return(idata)
}

#' Identify Rasters
#' 
#' @import sp rgeos
#' @keywords internal
i_grid <- function(spdata, loc) {
    spdata2 <- as(spdata, "SpatialGridDataFrame")
    if (is.null(loc)){
      data <- over(SpatialPoints(locator(1), CRS(proj4string(spdata2))), spdata2)
    } else {
      data <- over(SpatialPoints(loc, CRS(proj4string(spdata2))), spdata2)
    }
    return(data)
} 
