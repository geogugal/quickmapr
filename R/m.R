#' Measure
#' 
#' Allows interactive selection of points and returns length of selecte line in
#' units of the current \code{qmap} object. 
#' @param qmap_obj a \code{qmap} object from which to measure features.
#' 
#' @import sp
#' @export
m <- function(qmap_obj = NULL){
  if (class(qmap_obj) != "qmap") {
    stop("Requires a valid qmap_obj.")
  } 
  plot(qmap_obj)
  message("Click on plot to draw line to measure. Press 'Esc' to exit.")
  loc <- locator(1)
  locs <- coordinates(loc)
  while (!is.null(loc)) {
    sl <- SpatialLines(list(Lines(list(Line(locs)), "id")))
    plot(sl,add=T)
    loc <- locator(1)
    if(!is.null(loc)){
      locs<-rbind(locs,coordinates(loc))
    }
  }
  rgeos::gLength(sl)
}

