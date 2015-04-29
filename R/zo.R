#' Zooms out on current plot
#' 
#' Interactively zoom out on the current plot. Works on an existing
#'  \code{qmap} object.  Simply pass that object to \code{zo()}.  A single zoom out 
#'  results and the extent of the
#' \code{qmap} object is changed.  
#' 
#' @param qmap_obj A qmap object.  Optional, but performs better with larger 
#'                  data sets.
#' @param zoom_perc A proportion to determine the zoom level.  The x and y 
#'                  axes are increased by this amount.  Default is 0.5.     
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qm<-qmap(list(lake,buffer,elev))
#' zo(qm)
#' }
zo <- function(qmap_obj = NULL, zoom_perc = 0.5) {
  if (zoom_perc >= 1 || zoom_perc < 0) {
    stop("Argument, zoom_perc, needs to be between 0 and 1")
  }
  if (is.null(qmap_obj)) {
    stop("Requires a valid qmap_obj.")
  } else {
    continue <- 0
    obj <- paste(substitute(qmap_obj))
    message("Click on plot to zoom in. Press 'Esc' to exit.")
    n <- 1
    loc <- 1
    while (!is.null(loc)) {
      if (n == 1) {
        loc <- locator(1)
        qmap_obj <- zoom_it(qmap_obj, loc, zoom_perc, out = TRUE)
        n <- 2
      } else {
        qmap_obj <- zoom_it(qmap_obj, loc, zoom_perc, out = TRUE)
        loc <- locator(1)
      }
    }
    assign(obj, qmap_obj, envir = parent.frame())
  }
} 
