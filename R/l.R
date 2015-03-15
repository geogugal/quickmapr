#' Label features
#' 
#' It is useful to be able to provide labels to features on your map when 
#' examining the results of given analysis.  This function adds labels for a 
#' given layer.  
#' 
#' @param qmap_obj a qmap object from which to pull the labels.  Raster layers 
#'        are ignored.  Will also accept \code{sp} objects.
#' @param lab_order identify which sp layer to label.  Defaults to first object
#'        in qmap_obj$map_data.
#' @param field a field in the sp object to use to label the features.  Defaults 
#'        to row.names().
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qmap(list(lake,width))
#' l(lake,'COMID')
#' }
l <- function(qmap_obj, lab_order = 1, field = NULL) {
  if (class(qmap_obj) == "qmap") {
    while (get_sp_type(qmap_obj$map_data[[lab_order]]) == "grid") {
      warning("Labelling for raster data not supported. Labeling first non-raster data")
      lab_order <- lab_order + 1
    }
    spdata <- qmap_obj$map_data[[lab_order]]
    x <- coordinates(spdata)[, 1]
    y <- coordinates(spdata)[, 2]
  } else {
    x <- coordinates(qmap_obj)[, 1]
    y <- coordinates(qmap_obj)[, 2]
  }
  
  if (is.null(field)) {
    labs <- row.names(spdata)
  } else {
    labs <- spdata[[field]]
  }
  obj <- paste(substitute(qmap_obj))
  label <- list(label = list(x = x, y = y, labs = labs))
  if ("label" %in% names(qmap_obj)) {
    # qmap_obj$label<-NULL
    qmap_obj$label <- label$label
  } else {
    qmap_obj <- c(qmap_obj, label)
  }
  class(qmap_obj) <- "qmap"
  assign(obj, qmap_obj, envir = parent.frame())
  plot(qmap_obj)
} 
