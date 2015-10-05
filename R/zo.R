#' Zooms out on current plot
#' 
#' Interactively zoom out on the current plot. Works on an existing
#'  \code{qmap} object.  Simply pass that object to \code{zo()}.  A single zoom 
#'  out results and the extent of the \code{qmap} object is changed.  
#' 
#' @param qmap_obj A qmap object.  Optional, but performs better with larger 
#'                  data sets.
#' @param zoom_perc A proportion to determine the zoom level.  The x and y 
#'                  axes are increased by this amount.  Default is 0.5. 
#' @param loc A list with an x and y numeric indicating a location.  Default is 
#'            to interactively get loc value until escaped.  
#' @return NULL
#' @export
#' @importFrom graphics locator
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qm<-qmap(list(lake,buffer,elev))
#' zo(qm)
#' }
zo <- function(qmap_obj = NULL, zoom_perc = 0.5, loc = NULL) {
    if (zoom_perc >= 1 || zoom_perc < 0) {
        stop("Argument, zoom_perc, needs to be between 0 and 1")
    }
    if (class(qmap_obj) != "qmap") {
        stop("Requires a valid qmap_obj.")
    } else if (is.null(loc)) {
        continue <- 0
        obj <- paste(substitute(qmap_obj))
        message("Click on plot to zoom out. Press 'Esc' to exit.")
        loc <- locator(1)
        while (!is.null(loc)) {
            qmap_obj <- zoom_it(qmap_obj, loc, zoom_perc, out = TRUE)
            loc <- locator(1)
        }
    } else {
      continue <- 0
      obj <- paste(substitute(qmap_obj))
      qmap_obj <- zoom_it(qmap_obj, loc, zoom_perc, out = TRUE)
    }
    assign(obj, qmap_obj, envir = parent.frame())
} 
