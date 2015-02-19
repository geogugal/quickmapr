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

#' Gets the x and y diff of a qmap_obj
#' @param qmap_obj an qmap object
#' @return numeric vector indicating the size of the x and y extent
#' @keywords internal
get_range<-function(qmap_obj){
  x_range<-diff(as.numeric(qmap_obj$map_extent[1,]))
  y_range<-diff(as.numeric(qmap_obj$map_extent[2,]))
  return(c(x_range,y_range))
}

#' builds a map_data from many input types
#' @param ... list, sp, or qmap objects
#' @return list of spatial objects with names
#' @keywords internal
build_map_data<-function(...){
  mapdata<-list(...)
  #Deal with qmaps
  qmap_idx<-na.omit(match(lapply(mapdata,class),"qmap"))[1]
  if(!is.na(qmap_idx)){
    for(i in qmap_idx){
      mapdata[[i]]<-mapdata[[i]]$map_data
    }
  }
  
  name<-paste(substitute(list(...)))
  name<-name[!name%in%"list"]
  names(mapdata)<-name
  mapdata<-unlist(mapdata)
  name<-names(mapdata)
  name<-gsub("^list\\(.*\\)\\.","",name)
  name<-gsub("\\)[0-9]$","",name)
  name<-gsub("\\)$","",name)
  name<-gsub("^list\\(","",name)
  name<-unlist(strsplit(name,","))
  name<-unlist(strsplit(name,"="))
  name<-gsub(" ","",name)
  name<-unique(name)
  names(mapdata)<-name
  return(mapdata)
}