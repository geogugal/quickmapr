#' Zooms in on extent
#' 
#' Select a bounding box interactively and zoom to that extent. Click twice to 
#' select corners of extent and ESC to quit.  This is a wrapper 
#' function/shortcut for zoom::sq.zoom(...).  
#' 
#' 
#' @param ... arguments to be passed to zoom::sq.zoom()
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qmap(list(lake,buffer,elev))
#' ze()
#' }
ze<-function(qmap_obj=NULL,...){
  if(is.null(qmap_obj)){
    zoom::sq.zoom(...)
  } else {
    obj<-paste(substitute(qmap_obj))
    qmap_obj$map_extent<-bbox(SpatialPoints(locator(2)))
    assign(obj,qmap_obj,envir = .GlobalEnv)
    return(plot.qmap(qmap_obj))
  }
  
}