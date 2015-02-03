#' Zooms in on current plot
#' 
#' Interactively zoom in on the current plot. Click on location to zoom into and 
#' use ESC to quit. This is a wrapper 
#' function/shortcut for zoom::in.zoom(...).  
#' 
#' @param ... arguments to be passed to zoom::in.zoom(...)
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qmap(list(lake,buffer,elev))
#' zi()
#' }
zi<-function(...){
  zoom::in.zoom(...)
}
