#' Zooms in on current plot
#' 
#' This function uses the package \code{zoom} to provide zooming and panning functionality.  
#' These are simply wrappers with shortened function names. This function zooms in by a set 
#' amount.
#' 
#' @param ... arguments to be passed to zoom::in.zoom()
#'
#' @export
#' 
#' @examples
#' data(lake)
#' qmap(list(lake,buffer,elev)) %>% zi()
zi<-function(...){
  zoom::in.zoom(...)
}
