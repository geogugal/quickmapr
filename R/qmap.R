#' Display spatial data from a list.
#'
#' This function genarates a call to \code{ggplot2} and builds the quick map
#' visualization.
#' 
#' @param mapdata A list of spatial objects
#' @param extent A \code{sp} or \code{raster} object to use as the initial extent of the map.  Defaults
#'               to the maximum extent of all input object
#' @param order draw order of the spatial object. Defaults to order in mapdata
#' @param colors line colors. Defaults to 1:length(mapdata)  
#' @param prj Logical to check projections of input spatial objects.  Transformation, if needed, should be
#'            done prior to mapping with \code{rgdal::spTransform()}.
#' @return Function displays a map from the input \code{mapdata} paramter
#' 
#' @import sp
#' @export
#' 
#' @examples
#' data(lake)
#' qmap(list(lake,buffer,elev))
qmap<-function(mapdata,extent=NULL,order=1:length(mapdata),
               colors=1:length(mapdata),prj=TRUE){
  
  if(length(mapdata)>1){
    #Test Projections
    if(prj){
      prjs<-lapply(mapdata,sp::proj4string)
      if(length(unique(prjs))>1){
        stop("Projections do not match", call.=FALSE)
      } else if(length(unique(prjs))==0){
        stop("No projection info.  Use prj=FALSE to override projection check", call.=FALSE)
      }
    }
    #Sets Extent to all entered extents or a specific one.
    #if(is.null(extent)){
    #  bbx<-sp::bbox(mapdata[[1]])
    #  for(i in length(mapdata))
    #  {
    #   bbx[1,1]<-min(c(bbx[1,1],sp::bbox(mapdata[[i]])[1,1]))
    #   bbx[1,2]<-max(c(bbx[1,2],sp::bbox(mapdata[[i]])[1,2]))
    #   bbx[2,1]<-min(c(bbx[2,1],sp::bbox(mapdata[[i]])[2,1]))
    #   bbx[2,2]<-max(c(bbx[2,2],sp::bbox(mapdata[[i]])[2,2]))
    # }
    #} 
  } 
  
  #if(!exists("bbx")&is.null(extent)){
  #  bbx<-bbox(mapdata[[1]])
  #} else if(!is.null(extent)){
  #  bbx<-bbox(extent)
  #}
  #bbx<-data.frame(bbx)
  
  #Raster draw order
  #should maintain order of vector layers
  #should maintain order of raster layers but moves to front
  #so that the draw first with vector on top.
  mapdata<-mapdata[order]
  classes<-unlist(lapply(mapdata,class))
  rasters<-classes=="RasterLayer"
  mapdata<-c(mapdata[rasters],mapdata[!rasters])
  classes<-unlist(lapply(mapdata,class))
  for(i in 1:length(mapdata)){
    if(i == 1 & classes[i] == "RasterLayer"){
      #plot(mapdata[[i]],xlim=as.vector(bbx[1,]),ylim=as.vector(bbx[2,]),axes=TRUE)
      plot(mapdata[[i]])
    } else if(i == 1 & regexpr("Polygon",classes[i])>0){
      #plot(mapdata[[i]],xlim=as.vector(bbx[1,]),ylim=as.vector(bbx[2,]),axes=TRUE,fg=colors[i])
      plot(mapdata[[i]],border=colors[i])
    } else if(i == 1) {
      plot(mapdata[[i]],col=colors[i])
    } else if(classes[i] == "RasterLayer"){
      plot(mapdata[[i]],add=TRUE)
    } else if(regexpr("Polygon",classes[i])>0){
      plot(mapdata[[i]], border=colors[i], add=TRUE)
    } else {
      plot(mapdata[[i]], col=colors[i], add=TRUE)
    }
  }
  return(recordPlot())
}