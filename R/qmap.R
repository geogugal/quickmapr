#' Display spatial data from a list.
#'
#' This function genarates a call to \code{ggplot2} and builds the quick map
#' visualization.
#' 
#' @param mapdata A list of spatial objects
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
qmap<-function(mapdata,extent=NULL,order=1:length(mapdata),
               colors=1:length(mapdata),fill=FALSE,prj=TRUE){
  if(!is.list(mapdata)){stop("mapdata must be a list")}
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
  
  #Creates the plot
  first<-TRUE
  for(i in 1:length(order)){
    if(first){
      if(get_sp_type(mapdata[[order[i]]])=="grid"){
        image(mapdata[[order[i]]],xlim=as.numeric(bbx[1,]),ylim=as.numeric(bbx[2,]),
              axes=TRUE)
        first<-FALSE
      } else if(get_sp_type(mapdata[[order[i]]])=="polygon"){
        if(fill){
          plot(mapdata[[order[i]]],xlim=as.numeric(bbx[1,]),ylim=as.numeric(bbx[2,]),
               axes=TRUE,col=colors[i])
        } else {
          plot(mapdata[[order[i]]],xlim=as.numeric(bbx[1,]),ylim=as.numeric(bbx[2,]),
               axes=TRUE,border=colors[i])
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
  return(recordPlot())
}

#' Pull out essential info on sp class
#' @param spdata an sp object
#' @return character vector indicating point, line, polygon, or grid
#' @keywords internal
get_sp_type<-function(spdata){
  spclass<-tolower(class(spdata)[1])
  if(regexpr("polygon",spclass)>0){
    return("polygon")
  } else if (regexpr("line",spclass)>0){
    return("line")
  } else if (regexpr("point",spclass)>0){
    return("point")
  } else if (regexpr("grid",spclass)>0){
    return("grid")
  } else if (regexpr("pixel",spclass)>0){
    return("grid")
  } else if (regexpr("raster",spclass)>0){
    return("grid")
  }
}