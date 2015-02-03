#' Identify
#' 
#' Spatially select an sp object  and reutrn the data associated with it.
#' @export
#' @import sp rgeos
#' 
i<-function(spdata){
  
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
  
  i_line<-function(spdata){
    loc<-SpatialPoints(locator(1),CRS(proj4string(spdata)))
    idx<-gWithinDistance(loc,spdata,gDistance(loc,spdata),byid=T)
    if(regexpr("DataFrame",class(spdata))>0){
      data<-spdata@data[idx,]
    } else {
      data<-NULL
    }
    idata<-list(data=data,
                spobj=spdata[idx],
                length=gLength(spdata[idx]))
    return(idata)
  }
  
  i_point<-function(spdata){
    idata<-list(data=i_line(spdata)$data,
                spobj=i_line(spdata)$spobj)
    return(idata)
  }
    
  i_grid<-function(spdata){
    spdata2<-as(spdata,"SpatialGridDataFrame")
    data<-over(SpatialPoints(locator(1),CRS(proj4string(spdata2))),spdata2)
    return(data)
  }
  
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

  switch(EXPR=get_sp_type(spdata),
         polygon = i_poly(spdata),
         grid = i_grid(spdata),
         line = i_line(spdata),
         point = i_point(spdata))
  
}