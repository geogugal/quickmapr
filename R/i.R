#' Identify
#' 
#' Spatially select an sp object  and reutrn the data associated with it.
#' @export
#' @import sp rgeos
#' 
i<-function(spdata){
  switch(EXPR=get_sp_type(spdata),
         polygon = i_poly(spdata),
         grid = i_grid(spdata),
         line = i_line(spdata),
         point = i_point(spdata))
}

#' Identify Polys
#' 
#' @import sp rgeos
#' @keywords internal
i_poly<-function(spdata){
  idx<-over(spdata,SpatialPoints(locator(1),CRS(proj4string(spdata))))
  if(regexpr("DataFrame",class(spdata))>0){
    data<-spdata@data[idx,]
  } else {
    data<-NULL
  }
  idata<-list(data=data,
              spobj=spdata[idx],
              area=gArea(spdata[idx]),
              perim=gLength(spdata[idx]))
  
  return(idata)
}

#' Identify Lines
#' 
#' @import sp rgeos
#' @keywords internal
i_line<-function(spdata){
  loc<-SpatialPoints(locator(1),CRS(proj4string(spdata)))
  idx<-gWithinDistance(loc,spdata,gDistance(loc,spdata),byid=T)
  if(regexpr("DataFrame",class(spdata))>0){
    data<-spdata@data[idx,]
  } else {
    data<-NULL
  }
  idata<-list(data=data,
              spobj=spdata[which(idx),],
              length=gLength(spdata[which(idx),]))
  return(idata)
}

#' Identify Points
#' 
#' @import sp rgeos
#' @keywords internal
i_point<-function(spdata){
  loc<-SpatialPoints(locator(1),CRS(proj4string(spdata)))
  idx<-gWithinDistance(loc,spdata,gDistance(loc,spdata),byid=T)
  if(regexpr("DataFrame",class(spdata))>0){
    data<-spdata@data[idx,]
  } else {
    data<-NULL
  }
  idata<-list(data=data,
              spobj=spdata[which(idx),])
  return(idata)
}

#' Identify Rasters
#' 
#' @import sp rgeos
#' @keywords internal
i_grid<-function(spdata){
  spdata2<-as(spdata,"SpatialGridDataFrame")
  data<-over(SpatialPoints(locator(1),CRS(proj4string(spdata2))),spdata2)
  return(data)
}