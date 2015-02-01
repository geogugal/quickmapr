#' Zooms in on extent
#' 
#' This function uses the package \code{zoom} to provide zooming and panning functionality.  
#' These are simply wrappers with shortened function names. This function zooms in on a
#' selected area.
#' 
#' @param ... arguments to be passed to zoom::out.zoom()
#' 
#' @export
#' 
#' @examples
#' data(lake)
#' qmap(list(lake,buffer,elev)) %>% zi()
ze<-function(...){
  zoom::sq.zoom(...)
}