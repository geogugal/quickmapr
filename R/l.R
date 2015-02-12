#' Label features
#' 
#' It is useful to be able to provide labels to features on your map when 
#' examining the results of given analysis.  This function adds labels for a 
#' given layer.  
#' 
#' @param spdata a sp object from which to take the labels
#' @param spfield a field in the spdata object to use to label the features
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qmap(list(lake,width))
#' l(lake,"COMID")
#' }
l<-function(spdata,spfield){
  text(coordinates(spdata)[,1],coordinates(spdata)[,2],labels=spdata[[spfield]])
}