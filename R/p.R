#' Pan the current plot
#' 
#' Interactively reposition the current plot.   Works on an existing
#'  \code{qmap} object.  Simply pass that object to \code{p()}.  A single 
#' repositioning results and the extent of the \code{qmap} object is changed.  
#' 
#' @param qmap_obj A qmap object.  Optional, but performs better with larger 
#'                  data sets. 
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
p <- function(qmap_obj = NULL) {
  if (class(qmap_obj)!="qmap") {
    stop("Requires a valid qmap_obj.")
  } else {
    continue <- 0
    obj <- paste(substitute(qmap_obj))
    message("Click on plot to pan. Press 'Esc' to exit.")
    loc <- locator(1)
    while (!is.null(loc)) {       
      qmap_obj <- zoom_it(qmap_obj, loc, 1, pan = TRUE)
      loc <- locator(1)
    }
  }
  assign(obj, qmap_obj, envir = parent.frame())
}

