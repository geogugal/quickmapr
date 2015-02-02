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
#' @return Function displays a map from the input \code{mapdata} parameter and returns
#'         a recorded plot.
#' 
#' @import sp
#' @export
#' 
#' @examples
#' data(lake)
#' qmap(list(lake,buffer,elev))
qmap<-function(mapdata,extent=NULL,order=1:length(mapdata),
               colors=1:length(mapdata),prj=TRUE){
  if(!is.list(mapdata)){stop("mapdata must be a list")}
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
    if(is.null(extent)){
      bbx<-sp::bbox(mapdata[[1]])
      for(i in 1:length(mapdata))
      {
       bbx[1,1]<-min(c(bbx[1,1],sp::bbox(mapdata[[i]])[1,1]))
       bbx[1,2]<-max(c(bbx[1,2],sp::bbox(mapdata[[i]])[1,2]))
       bbx[2,1]<-min(c(bbx[2,1],sp::bbox(mapdata[[i]])[2,1]))
       bbx[2,2]<-max(c(bbx[2,2],sp::bbox(mapdata[[i]])[2,2]))
     }
    } 
  } 
  
  if(!exists("bbx")&is.null(extent)){
    bbx<-bbox(mapdata[[1]])
  } else if(!is.null(extent)){
    bbx<-bbox(extent)
  }
  bbx<-data.frame(bbx)
  
  #converts rasterlayers to spatialgriddf
  for(i in 1:length(mapdata)){
    if(class(mapdata[[i]])=="RasterLayer"){
      mapdata[[i]]<-as(mapdata[[i]],"SpatialGridDataFrame")
    }
  }
    
  #if(length(order)>1){
  #  order<-na.omit(c(order[rasters],order[!rasters]))
  #} 
  first<-TRUE
  colors<-rep(colors,length(mapdata))[1:length(mapdata)]
  #browser()
  for(i in 1:length(order)){
    if(first & regexpr("grid",tolower(class(mapdata[[order[i]]])))[1]>0){
      image(mapdata[[order[i]]],xlim=as.numeric(bbx[1,]),ylim=as.numeric(bbx[2,]),axes=TRUE)
      first<-FALSE
    } else if(first & regexpr("poly",tolower(class(mapdata[[order[i]]])))[1]>0){
      plot(mapdata[[order[i]]],xlim=as.numeric(bbx[1,]),ylim=as.numeric(bbx[2,]),axes=TRUE,border=colors[i])
      first<-FALSE
    } else if(first & !regexpr("poly",tolower(class(mapdata[[order[i]]])))[1]>0){
      plot(mapdata[[order[i]]],xlim=as.numeric(bbx[1,]),ylim=as.numeric(bbx[2,]),axes=TRUE,col=colors[i])
      first<-FALSE
    } else if(!first & regexpr("poly",tolower(class(mapdata[[order[i]]])))[1]>0){
      plot(mapdata[[order[i]]],border=colors[order[i]],add=TRUE)
    } else if(!first & regexpr("grid",tolower(class(mapdata[[order[i]]])))[1]>0) {
      image(mapdata[[order[i]]],add=TRUE)
    } else {
      plot(mapdata[[order[i]]],col=colors[i],add=TRUE)
    }
  }
  return(recordPlot())
}