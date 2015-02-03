#' Pan the current plot
#' 
#' Interactively reposition the current plot. Click on location to center plot and 
#' use ESC to quit.  This is a wrapper 
#' function/shortcut for zoom::move.to.click.zoom(...).  
#' 
#' @param ... arguments to be passed to zoom::move.to.click.zoom(...)
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qmap(list(lake,buffer,elev))
#' p()
#' }
p<-function(...){
  zoom::move.to.click.zoom(...)
}