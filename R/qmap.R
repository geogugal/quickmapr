#' Build qmap object
#'
#' This function builds the qmap object that forms the basis for the rest of 
#' the \code{quickmapr} package.
#' 
#' @param ... Spatial objects to map.  Maybe passed as objects, a list of 
#'            spatial objects, or a \code{qmap} object
#' @param extent A \code{sp} or \code{raster} object to use as the initial extent 
#'        of the map.  Defaults to the maximum extent of all input object
#' @param order draw order of the spatial object. Defaults to order in mapdata
#' @param colors line colors. Defaults to 1:length(mapdata)  
#' @param fill Logical to determine if polygons should be filled (using colors) or just
#'             the border colored.
#' @param prj Logical to check projections of input spatial objects.  
#'            Transformation, if needed, should be done prior to mapping with 
#'            \code{rgdal::spTransform()}.
#' @return Function displays a map from the input \code{mapdata} parameter and returns
#'         a recorded plot.
#' 
#' @import sp
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' mymap<-list(elev,lake,buffer,length,samples)
#' qmap(mymap)
#' #change draw order and which data is displayed
#' qmap(mymap,order=c(2,3,5))
#' }
qmap<-function(...,extent=NULL,order=1:length(mapdata),
               colors=1:length(mapdata),fill=FALSE,prj=TRUE){
  mapdata<-build_map_data(...)
  if(length(mapdata)>1){
    #Test Projections
    if(prj){
      prjs<-lapply(mapdata,sp::proj4string)
      if(length(unique(prjs))>1){
        stop("Projections do not match", call.=FALSE)
      } else if(length(unique(prjs))==0){
        stop("No projection info.  Use prj=FALSE to override projection check.  
             If data are in different projections, the resultant map will not likely
             be what you expect. Best to determine projection and re-project.", 
             call.=FALSE)
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
  
  #match colors to length of mapdata
  colors<-rep(colors,length(mapdata))[1:length(mapdata)]
  
  qmap_obj<-list(map_data=mapdata,
                 map_extent=bbx,
                 draw_order=order,
                 colors=colors,
                 fill=fill,
                 map=NULL)
  class(qmap_obj)<-"qmap"
  qmap_obj$map=plot.qmap(qmap_obj)
  return(qmap_obj)
}

#' Default plotting of a qmap object
#' 
#' Plots the qmap class and uses the order, colors, extent, and fill option 
#' from \code{qmap}.
#' 
#' @param x input qmap class to plot
#' @param ... options passed to image or plot
#' @method plot qmap
#' @export
plot.qmap<-function(x,...){
  order<-x$draw_order
  mapdata<-x$map_data
  fill<-x$fill
  colors<-x$colors
  bbx<-x$map_extent
  
  #Creates the plot
  first<-TRUE
  for(i in 1:length(order)){
    if(first){
      if(get_sp_type(mapdata[[order[i]]])=="grid"){
        image(mapdata[[order[i]]],xlim=as.numeric(bbx[1,]),ylim=as.numeric(bbx[2,]),
              axes=TRUE,...)
        first<-FALSE
      } else if(get_sp_type(mapdata[[order[i]]])=="polygon"){
        if(fill){
          plot(mapdata[[order[i]]],xlim=as.numeric(bbx[1,]),ylim=as.numeric(bbx[2,]),
               axes=TRUE,col=colors[i],...)
        } else {
          plot(mapdata[[order[i]]],xlim=as.numeric(bbx[1,]),ylim=as.numeric(bbx[2,]),
               axes=TRUE,border=colors[i],...)
        }
        first<-FALSE
      } else if(!get_sp_type(mapdata[[order[i]]])=="polygon"){
        plot(mapdata[[order[i]]],xlim=as.numeric(bbx[1,]),ylim=as.numeric(bbx[2,]),
             axes=TRUE,col=colors[i])
        first<-FALSE
      }
    } else {
      if(get_sp_type(mapdata[[order[i]]])=="grid"){
        image(mapdata[[order[i]]],add=TRUE)
      } else if(get_sp_type(mapdata[[order[i]]])=="polygon"){
        if(fill){
          plot(mapdata[[order[i]]],col=colors[i],add=TRUE)
        } else {
          plot(mapdata[[order[i]]],border=colors[i],add=TRUE)
        }
      } else if(!get_sp_type(mapdata[[order[i]]])=="polygon"){
        plot(mapdata[[order[i]]],col=colors[i],add=TRUE)
      }
    }
  }
  if("label"%in%names(x)){
    text(x=x$label$x,y=x$label$y,labels=x$label$labs)
  }
  return(recordPlot())
}

#' Default printing of a qmap object
#' 
#' Prints the summary of a qmap object
#' 
#' @param x input qmap class to print
#' @param ... options passed to summary
#' @method print qmap
#' @export
print.qmap<-function(x,...){
  return(summary(x,...))
}