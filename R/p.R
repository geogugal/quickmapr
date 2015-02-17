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
p<-function(qmap_obj=NULL,...){
  if(is.null(qmap_obj)){
    zoom::move.to.click.zoom(...)
  } else {
    rng<-get_range(qmap_obj)
    loc<-locator(1)
    qmap_obj$map_extent[1,1]<-loc$x-(rng[1]/2)
    qmap_obj$map_extent[1,2]<-loc$x+(rng[1]/2)
    qmap_obj$map_extent[2,1]<-loc$y-(rng[2]/2)
    qmap_obj$map_extent[2,2]<-loc$y+(rng[2]/2)
    plot.qmap(qmap_obj)
  }
}