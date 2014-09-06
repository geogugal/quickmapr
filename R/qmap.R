#' Display spatial data from a list.
#'
#' This funtion genarates a call to \code{ggplot2} and builds the quick map
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
#' @import ggplot2
#' @export
#' 
#' @examples
#' data(lakes)
#' qmap(list(exampleLake,exampleElev))
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
    if(is.null(extent)){
      bbx<-sp::bbox(mapdata[[1]])
      for(i in length(mapdata))
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
  ggp<-ggplot(bbx,aes(x=bbx[,1],y=bbx[,2]))+coord_equal()
  for(i in order){
    spclass<-class(mapdata[[i]])[1]
    spclass<-gsub("DataFrame","",spclass)
    if(spclass=="RasterLayer"){
       ifort<-data.frame(coordinates(mapdata[[i]]))
       names(ifort)<-c("long","lat")
       ifort<-data.frame(ifort,values=raster::getValues(mapdata[[i]]))
    } else if(spclass=="SpatialGrid"){
      #do this
    } else if(spclass=="SpatialPixels") {
      #do this
    } else if(spclass=="SpatialPoints") {
      #do this
    } else if(spclass=="SpatialPolygons") {
      ifort<-fortify(mapdata[[i]])
    } else if(spclass=="Polygons") {
      #do this
    } else if(spclass=="Polygon") {
      #do this
    } else if(spclass=="SpatialLines") {
      lin<-coordinates(mapdata[[i]])
      ifort<-data.frame(lin,names(lin))
      names(ifort)<-c("long","lat","ID")
    } else if(spclass=="Lines") {
      #do this
    } else if(spclass=="Line") {
      #do this
    } 

    ggp<-switch(spclass,
           SpatialPolygons=ggp+geom_polygon(data=ifort,aes(x=long,y=lat),colour=colors[i]),
           Polygons=ggp+geom_polygon(data=ifort,aes(x=long,y=lat),colour=colors[i]),
           Polygon=ggp+geom_polygon(data=ifort,aes(x=long,y=lat),colour=colors[i]),
           SpatialLines=ggp+geom_line(data=ifort,aes(x=long,y=lat),colour=colors[i]),
           Lines=ggp+geom_line(data=ifort,aes(x=long,y=lat),colour=colors[i]),
           Line=ggp+geom_line(data=ifort,aes(x=long,y=lat),colour=colors[i]),
           SpatialPoints=ggp+geom_point(data=ifort,aes(x=long,y=lat),colour=colors[i]),
           SpatialPixels=ggp+geom_raster(data=ifort,aes(x=long,y=lat,fill=values),colour=colors[i]),
           SpatialGrid=ggp+geom_raster(data=ifort,aes(x=long,y=lat,fill=values),colour=colors[i]),
           RasterLayer=ggp+geom_raster(data=ifort,aes(x=long,y=lat,fill=values),colour=colors[i]))
  }

  return(ggp)
}