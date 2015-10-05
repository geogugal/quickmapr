#' Pan the current plot
#' 
#' Interactively reposition the current plot.   Works on an existing
#'  \code{qmap} object.  Simply pass that object to \code{p()}.  A single 
#' repositioning results and the extent of the \code{qmap} object is changed.  
#' 
#' @param qmap_obj A qmap object.  Optional, but performs better with larger 
#'                  data sets. 
#' @param loc A list with an x and y numeric indicating a location.  Default is 
#'            to interactively get loc value until escaped.
#' @return NULL
#' @export
#' @importFrom graphics locator
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' x<-qmap(list(lake,buffer,elev))
#' p()
#' ## Or
#' p(x)
#' }
p <- function(qmap_obj = NULL, loc = NULL) {
    if (class(qmap_obj) != "qmap") {
        stop("Requires a valid qmap_obj.")
    } else if (is.null(loc)) {
        continue <- 0
        obj <- paste(substitute(qmap_obj))
        message("Click on plot to pan. Press 'Esc' to exit.")
        loc <- locator(1)
        while (!is.null(loc)) {
            qmap_obj <- zoom_it(qmap_obj, loc, 1, pan = TRUE)
            loc <- locator(1)
        }
    } else {
      obj <- paste(substitute(qmap_obj))
      qmap_obj <- zoom_it(qmap_obj, loc, 1, pan = TRUE)
    }
    assign(obj, qmap_obj, envir = parent.frame())
} 
