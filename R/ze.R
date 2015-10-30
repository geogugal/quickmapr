#' Zooms in on extent
#' 
#' Select a bounding box interactively and zoom to that extent. Works on an existing
#'  \code{qmap} object.  Simply pass that object to \code{ze()}.  A single zoom in to 
#'  the selected
#' extent results and the extent of the \code{qmap} object is changed.  
#' 
#' @param qmap_obj A qmap object.  Optional, but performs better with larger 
#'                  data sets.
#' @param extent A Spatial* object to specify extent to zoom into.
#' @return NULL
#' @export
#' @importFrom graphics locator
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qmap(list(lake,buffer,elev))
#' ze()
#' }
ze <- function(qmap_obj = NULL, extent = NULL) {
    if (class(qmap_obj) != "qmap") {
        stop("Requires a valid qmap_obj.")
    } else {
        obj <- paste(substitute(qmap_obj))
        if (is.null(extent)) {
            message("Select 2 points to define the zoom extent.")
            bbx <- bbox(SpatialPoints(locator(2)))
            if(zoom_test(qmap_obj,bbx)){
              message("zoom limit has been reached")
              return(qmap_obj)
            }
            qmap_obj$map_extent <- bbx
        } else {
            qmap_obj$map_extent <- bbox(extent)
        }
        plot.qmap(qmap_obj,qmap_obj$resolution)
        assign(obj, qmap_obj, envir = parent.frame())
        
    }
  
} 


