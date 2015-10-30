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
    x_range <- diff(as.numeric(c(par("usr")[1:2])))
    y_range <- diff(as.numeric(c(par("usr")[3:4])))
    return(c(x_range, y_range))
}

#' gets color from input qmap
#' @param ... list, sp, or qmap objects
#' @return vector of colors
#' @importFrom stats na.omit
#' @keywords internal
get_colors <- function(...) {
    mapdata <- list(...)
    # Deal with qmaps
    qmap_idx <- na.omit(match(lapply(mapdata, class), "qmap"))[1]
    if (!is.na(qmap_idx)) {
        for (i in qmap_idx) {
            colors <- mapdata[[i]]$colors
        }
    }
    return(colors)
}


#' 
#' builds a map_data from many input types
#' @param ... list, sp, or qmap objects
#' @return list of spatial objects with names
#' @importFrom stats na.omit
#' @importFrom raster unstack
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
    
    # Deal with RasterStacks - unstacks into individual layers
    stck_idx <- na.omit(match(lapply(mapdata, class), "RasterStack"))[1]
    if (!is.na(stck_idx)) {
      for (i in stck_idx) {
        mapdata[[i]] <- unstack(mapdata[[i]])
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
    
    me <- data.frame(min =  c(loc$x - (rng[1]/2), loc$y - (rng[2]/2)),
             max =  c(loc$x + (rng[1]/2), loc$y + (rng[2]/2)),
             row.names = c("x","y"))
  
    if(zoom_test(qmap_obj, me)&&!out&&!pan){
      message("zoom limit has been reached")
      return(qmap_obj)
    }
    
    qmap_obj$map_extent <- me
    plot(qmap_obj)
    return(qmap_obj)
}

#' Test range of zoom 
#' @keywords internal
zoom_test<-function(qmap_obj,map_extent){
  resp<-FALSE
  #need to have check happen before zoom not on old zoom
  prj<-proj4string(qmap_obj$map_data[[1]])
  if(is.na(prj)){
    orig_x<-abs(diff(as.numeric(qmap_obj$orig_extent[1,])))
    orig_y<-abs(diff(as.numeric(qmap_obj$orig_extent[2,])))
    curr_x<-abs(diff(as.numeric(map_extent[1,])))
    curr_y<-abs(diff(as.numeric(map_extent[2,])))
    if(curr_x/orig_x<0.01){resp<-TRUE}
    if(curr_y/orig_y<0.01){resp<-TRUE}
  } else {
    poly<-map_extent
    x <- c(poly[1, 1], poly[1, 1], poly[1, 2], poly[1, 2], poly[1, 1])
    y <- c(poly[2, 1], poly[2, 2], poly[2, 2], poly[2, 1], poly[2, 1])
    p <- Polygon(cbind(x, y))
    ps <- Polygons(list(p), "p1")
    poly <- SpatialPolygons(list(ps), 1L, proj4string = CRS(prj))
    poly<-sp::spTransform(poly,CRS("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 
                               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m 
                               +no_defs +ellps=GRS80 +towgs84=0,0,0"))
    if(rgeos::gArea(poly)<=10000){resp<-TRUE}
  }
  return(resp)
}

#' sp bbox to poly
#' @param sp
#' @keywords internal
bbox_to_sp <- function(sp) {
    bbox <- bbox(sp)
    x <- c(bbox[1, 1], bbox[1, 1], bbox[1, 2], bbox[1, 2], bbox[1, 1])
    y <- c(bbox[2, 1], bbox[2, 2], bbox[2, 2], bbox[2, 1], bbox[2, 1])
    p <- Polygon(cbind(x, y))
    ps <- Polygons(list(p), "p1")
    sp <- SpatialPolygons(list(ps), 1L, proj4string = CRS(proj4string(sp)))
    return(sp)
}

#' make jpeg world file
#' @param file output file name
#' @param bbx bounding box in map units
#' @param width width in pixels 
#' @keywords internal
make_jpw <- function(file, bbx, width) {
    res <- abs(bbx[1, 1] - bbx[1, 2])/width
    upper_left_x <- bbx[1, 1]
    upper_left_y <- bbx[2, 2]
    con <- file(file, "w")
    writeLines(as.character(res), con)
    writeLines("0", con)
    writeLines("0", con)
    writeLines(as.character(-res), con)
    writeLines(as.character(upper_left_x), con)
    writeLines(as.character(upper_left_y), con)
    close(con)
} 
