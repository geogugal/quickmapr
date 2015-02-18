#' Returns qmap object to original extent
#' 
#' Allows for restoring map to originally set extent.  The originally recorded plot
#' is replayed and the map extent of the object is reset to match the current
#' display.
#' 
#' @param qmap_obj a qmap_obj to restore
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' x<-qmap(list(lake,buffer,elev))
#' zi(x)
#' f(x)
#' }
f<-function(qmap_obj){
  obj<-paste(substitute(qmap_obj))
  replayPlot(qmap_obj$map)
  qmap_obj$map_extent[1,1]<-par("usr")[1]
  qmap_obj$map_extent[1,2]<-par("usr")[2]
  qmap_obj$map_extent[2,1]<-par("usr")[3]
  qmap_obj$map_extent[2,2]<-par("usr")[4]
  if("label" %in% names(x)){
    qmap_obj$label<-NULL
  }
  assign(obj,qmap_obj,envir = .GlobalEnv)
}
