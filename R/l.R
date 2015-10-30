#' Label features
#' 
#' It is useful to be able to provide labels to features on your map when 
#' examining the results of given analysis.  This function adds labels for a 
#' given layer (currently just point or polygon layers).  
#' 
#' @param qmap_obj a qmap object from which to pull the labels.  Raster layers 
#'        are ignored.  Will also accept \code{sp} objects.
#' @param field a field in the sp object to use to label the features.  Defaults 
#'        to row.names().
#' @param layer identify which sp layer to label.  Defaults to first layer
#'        in qmap_obj$map_data.
#' 
#' @export
#' @import sp
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qm<-qmap(lake,width,buffer)
#' l(qm,'COMID')
#' l(qm, layer = 2)
#' l(qm, layer = "buffer")
#' }
l <- function(qmap_obj, field = NULL, layer = 1) {
    if (class(qmap_obj) != "qmap") {
      stop("Requires a valid qmap_obj.")
    }   
  
    if (get_sp_type(qmap_obj$map_data[[layer]]) == "grid") {
        stop("Labelling for raster data not supported.")
    }

    spdata <- qmap_obj$map_data[[layer]]
    
    if (get_sp_type(spdata) == "line") {
      stop("Line labelling not yet supported")  
    } else {
      x <- coordinates(spdata)[, 1]
      y <- coordinates(spdata)[, 2]
    }
    
    if (is.null(field)) {
        labs <- row.names(spdata)
    } else {
        labs <- spdata[[field]]
    }
    obj <- deparse(substitute(qmap_obj))
    label <- list(label = list(x = x, y = y, labs = labs))
    if ("label" %in% names(qmap_obj)) {
        # qmap_obj$label<-NULL
        qmap_obj$label <- label$label
    } else {
        qmap_obj <- c(qmap_obj, label)
    }
    class(qmap_obj) <- "qmap"
    plot(qmap_obj,qmap_obj$resolution)
    assign(obj, qmap_obj, envir = parent.frame())
    
} 
