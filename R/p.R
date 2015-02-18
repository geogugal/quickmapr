#' Pan the current plot
#' 
#' Interactively reposition the current plot. There are two methods for 
#' panning.  First, a purely interactive pane where you simply click on 
#' location to reposition the map and use ESC to quit. This is a wrapper 
#' function/shortcut for zoom::move.to.click.zoom(...).  This works well 
#' with smaller data sets; however it lags as the data increases in size.  The 
#' second method performs better with larger datasets and requires you to 
#' create a \code{qmap} object and pass that object to \code{p()}.  A single 
#' repositioning results and the extent of the \code{qmap} object is changed.  
#' 
#' @param qmap_obj A qmap object.  Optional, but performs better with larger 
#'                  data sets. 
#' @param ... arguments to be passed to zoom::move.to.click.zoom(...)
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' x<-qmap(list(lake,buffer,elev))
#' p()
#' ## Or
#' p(x)
#' }
p<-function(qmap_obj=NULL,...){
  if(is.null(qmap_obj)){
    zoom::move.to.click.zoom(...)
  } else {
    continue<-0
    obj<-paste(substitute(qmap_obj))
    message("Click on plot to pan.")
    while(continue!="q"){ 
      rng<-get_range(qmap_obj)
      loc<-locator(1)
      qmap_obj$map_extent[1,1]<-loc$x-(rng[1]/2)
      qmap_obj$map_extent[1,2]<-loc$x+(rng[1]/2)
      qmap_obj$map_extent[2,1]<-loc$y-(rng[2]/2)
      qmap_obj$map_extent[2,2]<-loc$y+(rng[2]/2)
      plot.qmap(qmap_obj)
      continue<-readline(message("Press 'Enter' to continue, 'q' to stop: ",appendLF=FALSE))
    }
  assign(obj,qmap_obj,envir = .GlobalEnv)
  }
}