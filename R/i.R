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
#' @import sp rgeos
#' @importFrom graphics locator
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
    idata <- list(data = data, #spobj = spdata[idx, ], 
                  area = gArea(spdata[idx, ]), perim = gLength(spdata[idx, ]))
    
    print(idata)
}

#' Identify Lines
#' 
#' @import sp rgeos
#' @importFrom graphics locator
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
    idata <- list(data = data, #spobj = spdata[which(idx), ], 
                  length = gLength(spdata[which(idx), 
        ]))
    print(idata)
}

#' Identify Points
#' 
#' @import sp rgeos
#' @importFrom graphics locator
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
    idata <- list(data = data, #spobj = spdata[which(idx), ])
                  location = sp::coordinates(loc_pt))
    print(idata)
}

#' Identify Rasters
#' 
#' @import sp rgeos
#' @importFrom methods as
#' @importFrom graphics locator
#' @keywords internal
i_grid <- function(spdata, loc) {
    spdata2 <- as(spdata, "SpatialGridDataFrame")
    if (is.null(loc)){
      loc_pt <- SpatialPoints(locator(1), CRS(proj4string(spdata)))
      data <- over(SpatialPoints(loc_pt, CRS(proj4string(spdata2))), spdata2)
    } else {
      loc_pt <- SpatialPoints(loc, CRS(proj4string(spdata)))
      data <- over(SpatialPoints(loc, CRS(proj4string(spdata2))), spdata2)
    }
    idata <- list(data = data, #spobj = spdata[which(idx), ])
                  location = sp::coordinates(loc_pt))
    print(idata)
} 
