#' Pull out essential info on sp class
#' @param spdata an sp object
#' @return character vector indicating point, line, polygon, or grid
#' @keywords internal
get_sp_type <- function(spdata) {
  spclass <- tolower(class(spdata)[1])
  if (regexpr("polygon", spclass) > 0) {
    return("polygon")
  } else if (regexpr("line", spclass) > 0) {
    return("line")
  } else if (regexpr("point", spclass) > 0) {
    return("point")
  } else if (regexpr("grid", spclass) > 0) {
    return("grid")
  } else if (regexpr("pixel", spclass) > 0) {
    return("grid")
  } else if (regexpr("raster", spclass) > 0) {
    return("grid")
  }
}

#' Gets the x and y diff of a qmap_obj
#' @param qmap_obj an qmap object
#' @return numeric vector indicating the size of the x and y extent
#' @keywords internal
get_range <- function(qmap_obj) {
  x_range <- diff(as.numeric(qmap_obj$map_extent[1, ]))
  y_range <- diff(as.numeric(qmap_obj$map_extent[2, ]))
  return(c(x_range, y_range))
}

#' builds a map_data from many input types
#' @param ... list, sp, or qmap objects
#' @return list of spatial objects with names
#' @keywords internal
build_map_data <- function(...) {
  mapdata <- list(...)
  # Deal with qmaps
  qmap_idx <- na.omit(match(lapply(mapdata, class), "qmap"))[1]
  if (!is.na(qmap_idx)) {
    for (i in qmap_idx) {
      mapdata[[i]] <- mapdata[[i]]$map_data
    }
  }
  
  name <- paste(substitute(list(...)))
  name <- name[!name %in% "list"]
  names(mapdata) <- name
  mapdata <- unlist(mapdata)
  name <- names(mapdata)
  name <- gsub("^list\\(.*\\)\\.", "", name)
  name <- gsub("\\)[0-9]$", "", name)
  name <- gsub("\\)$", "", name)
  name <- gsub("^list\\(", "", name)
  name <- unlist(strsplit(name, ","))
  name <- unlist(strsplit(name, "="))
  name <- gsub(" ", "", name)
  name <- unique(name)
  names(mapdata) <- name
  return(mapdata)
}

#' Zoom it
#' @keywords internal
zoom_it <- function(qmap_obj, loc, zoom_perc, out = FALSE, pan = FALSE) {
  if (out) {
    rng <- get_range(qmap_obj) * (1 + zoom_perc)
  } else if (pan) {
    rng <- get_range(qmap_obj)
  } else {
    rng <- get_range(qmap_obj) * (1 - zoom_perc)
  }
  qmap_obj$map_extent[1, 1] <- loc$x - (rng[1]/2)
  qmap_obj$map_extent[1, 2] <- loc$x + (rng[1]/2)
  qmap_obj$map_extent[2, 1] <- loc$y - (rng[2]/2)
  qmap_obj$map_extent[2, 2] <- loc$y + (rng[2]/2)
  if(!is.null(qmap_obj$basemap.base)){
    tmp_bm <- get_basemap(qmap_obj$map_extent,
                                    proj4string(qmap_obj$map_data[[1]]),
                                    qmap_obj$basemap.base,
                                    qmap_obj$basemap.width)
    qmap_obj$basemap.img<-tmp_bm$img
    qmap_obj$basemap.base<-tmp_bm$base
  }
  plot(qmap_obj)
  return(qmap_obj)
} 

#' sp bbox to poly
#' @param sp
#' @keywords internal
bbox_to_sp<-function(sp){
  bbox <- bbox(sp)
  x <- c(bbox[1,1], bbox[1,1], bbox[1,2], bbox[1,2], bbox[1,1])
  y <- c(bbox[2,1], bbox[2,2], bbox[2,2], bbox[2,1], bbox[2,1])
  p <- Polygon(cbind(x, y))
  ps <- Polygons(list(p), "p1")
  sp <- SpatialPolygons(list(ps), 1L, proj4string = CRS(proj4string(sp)))
  return(sp)
}
