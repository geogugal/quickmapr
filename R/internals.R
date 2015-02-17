#' Pull out essential info on sp class
#' @param spdata an sp object
#' @return character vector indicating point, line, polygon, or grid
#' @keywords internal
get_sp_type<-function(spdata){
  spclass<-tolower(class(spdata)[1])
  if(regexpr("polygon",spclass)>0){
    return("polygon")
  } else if (regexpr("line",spclass)>0){
    return("line")
  } else if (regexpr("point",spclass)>0){
    return("point")
  } else if (regexpr("grid",spclass)>0){
    return("grid")
  } else if (regexpr("pixel",spclass)>0){
    return("grid")
  } else if (regexpr("raster",spclass)>0){
    return("grid")
  }
}

#' Gets the x and y diff of a qmap_obj
#' @param qmap_obj an qmap object
#' @return numeric vector indicating the size of the x and y extent
#' @keywords internal
get_range<-function(qmap_obj){
  x_range<-diff(as.numeric(qmap_obj$map_extent[1,]))
  y_range<-diff(as.numeric(qmap_obj$map_extent[2,]))
  return(c(x_range,y_range))
}
