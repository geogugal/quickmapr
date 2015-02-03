#' Zooms out on current plot
#' 
#' Interactively zoom out on the current plot. Click on location to zoom out from and 
#' use ESC to quit. This is a wrapper function/shortcut for zoom::out.zoom(...).  
#' 
#' @param ... arguments to be passed to zoom::out.zoom(...)
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qmap(list(lake,buffer,elev))
#' zo()
#' }
zo<-function(...){
  zoom::out.zoom(...)
}