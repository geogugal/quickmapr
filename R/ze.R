#' Zooms in on extent
#' 
#' Select a bounding box interactively and zoom to that extent. There are two 
#' methods for this.  A purely interactive zoom where you simply click on two
#' location to specify the extent and use ESC to quit. This is a wrapper 
#' function/shortcut for zoom::sq.zoom(...).  This works well with smaller data
#' sets; however it lags as the data increases in size.  The second method 
#' performs better with larger datasets and requires you to create a \code{qmap} 
#' object and pass that object to \code{zi()}.  A single zoom in to the selected
#' extent results and the extent of the \code{qmap} object is changed.  
#' 
#' @param qmap_obj A qmap object.  Optional, but performs better with larger 
#'                  data sets.
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
ze <- function(qmap_obj = NULL, ...) {
  if (is.null(qmap_obj)) {
    zoom::sq.zoom(...)
  } else {
    obj <- paste(substitute(qmap_obj))
    message("Select 2 points to define the zoom extent.")
    qmap_obj$map_extent <- bbox(SpatialPoints(locator(2)))
    assign(obj, qmap_obj, envir = parent.frame())
    return(plot.qmap(qmap_obj))
  }
  
} 
