#' Display spatial data from a list.
#'
#' This funtion genarates a call to \code{ggplot2} and builds the quick map
#' visualization.
#' 
#' @param mapdata A list of spatial objects
#' @param extent 
#' @param order
#' @param colors
#' @param prj
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
      prjs<-lapply(mapdata,crs)
      if(length(unique(prjs)>1){
        stop("Projections do not match", call.=FALSE)
      } else if(length(unique(prjs)==0){
        stop("No projection info.  Use prj=FALSE to override projection check", call.=FALSE)
      }
    }
    #Sets Extent to all entered extents or a specific one.
    if(extent==NULL){
      bbx<-bbox(mapdata[[1]])
      for(i in length(mapdata))
      {
       bbx[1,1]<-min(c(bbx[1,1],bbox(mapdata[[i]])[1,1])
       bbx[1,2]<-min(c(bbx[1,2],bbox(mapdata[[i]])[1,2])
       bbx[2,1]<-max(c(bbx[2,1],bbox(mapdata[[i]])[2,1])
       bbx[2,2]<-max(c(bbx[2,2],bbox(mapdata[[i]])[2,2])
      }
    } 
  } 
   
  if(!exists(bbx)&is.null(extent)){
    bbx<-bbox(mapdata[[1]])
  } else {
    bbx<-bbox(extent)
  }


  ggp<<-ggplot()
  return(ggp)
}