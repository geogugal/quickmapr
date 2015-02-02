#' Identify
#' 
#' Spatially select a SpatialPolygonsDataFrame and reutrn the data associated with it.
#' @export
#' 
i<-function(spdata){
  data<-spdata@data[over(spdata,sp::SpatialPoints(locator(1),CRS(proj4string(spdata)))),]
  return(data)
}