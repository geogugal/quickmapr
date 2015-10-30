#' Returns qmap object to original extent
#' 
#' Allows for restoring map to original extent.  The inital map is replayed and 
#' the map extent of the object is reset to match the current display.
#' 
#' @param qmap_obj a qmap_obj to restore
#' @return NULL
#' @export
#' @importFrom grDevices replayPlot
#' @importFrom graphics par
#' @examples
#' \dontrun{
#' data(lake)
#' x<-qmap(list(lake,buffer,elev))
#' zi(x)
#' f(x)
#' }
f <- function(qmap_obj = NULL) {
    if (class(qmap_obj) != "qmap") {
        stop("Requires a valid qmap_obj.")
    }
    obj <- paste(substitute(qmap_obj))
    #replayPlot(qmap_obj$map)
    #qmap_obj$map_extent[1, 1] <- par("usr")[1]
    #qmap_obj$map_extent[1, 2] <- par("usr")[2]
    #qmap_obj$map_extent[2, 1] <- par("usr")[3]
    #qmap_obj$map_extent[2, 2] <- par("usr")[4]
    qmap_obj$map_extent <- qmap_obj$orig_extent
    if ("label" %in% names(qmap_obj)) {
        qmap_obj$label <- NULL
    }
    plot.qmap(qmap_obj)
    assign(obj, qmap_obj, envir = parent.frame())
} 
