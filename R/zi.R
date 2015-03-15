#' Zooms in on current plot
#' 
#' Interactively zoom in on the current plot. There are two methods for 
#' zooming.  A purely interactive zoom where you simply click on location to 
#' zoom into and use ESC to quit. This is a wrapper function/shortcut for 
#' zoom::in.zoom(...).  This works well with smaller data sets; however it lags
#' as the data increases in size.  The second method performs better with 
#' larger datasets and requires you to create a \code{qmap} object and pass 
#' that object to \code{zi()}.  A single zoom in results and the extent of the
#' \code{qmap} object is changed.  
#' 
#' @param qmap_obj  A qmap object.  Optional, but performs better with larger 
#'                  data sets.
#' @param zoom_perc A proportion to determine the zoom level.  The x and y 
#'                  axes are reduced by this amount.  Default is 0.5.        
#' @param ... arguments to be passed to zoom::in.zoom(...)
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' x<-qmap(list(lake,buffer,elev))
#' zi()
#' ##Or
#' zi(x)
#' }
zi <- function(qmap_obj = NULL, zoom_perc = 0.5, ...) {
  if (zoom_perc >= 1 || zoom_perc < 0) {
    stop("Argument, zoom_perc, needs to be between 0 and 1")
  }
  if (is.null(qmap_obj)) {
    zoom::in.zoom(...)
  } else {
    continue <- 0
    obj <- paste(substitute(qmap_obj))
    message("Click on plot to zoom in. Press 'Esc' to exit.")
    n <- 1
    loc <- 1
    while (!is.null(loc)) {
      if (n == 1) {
        loc <- locator(1)
        qmap_obj <- zoom_it(qmap_obj, loc, zoom_perc)
        n <- 2
      } else {
        qmap_obj <- zoom_it(qmap_obj, loc, zoom_perc)
        loc <- locator(1)
      }
    }
    assign(obj, qmap_obj, envir = parent.frame())
  }
} 
