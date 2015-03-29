#' starting work on getting aerial basemap
#' http://raster.nationalmap.gov/arcgis/rest/services/Orthoimagery/USGS_EROS_Ortho_NAIP/ImageServer/exportImage?bbox=-8026861,5361113,-8014736,5377674
#' http://raster.nationalmap.gov/arcgis/rest/services/Orthoimagery/USGS_EROS_Ortho_NAIP/ImageServer/exportImage?bbox=-8026861,5361113,-8014736,5377674&bboxSR=&size=&imageSR=&time=&format=jpg&pixelType=U8&noData=&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation&compression=&compressionQuality=&bandIds=&mosaicRule=&renderingRule=&f=image
#' 102003- US Albers Contig
#' 102039- USGS Albers
library(sp)
library(rgdal)
library(quickmapr)
library(raster)
data(lake)
lake_bb_wm<-spTransform(bbox_to_sp(lake),CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))
download.file("http://raster.nationalmap.gov/arcgis/rest/services/Orthoimagery/USGS_EROS_Ortho_NAIP/ImageServer/exportImage?bbox=-8026861,5361113,-8014736,5377674&format=tiff&pixelType=U8&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation&f=image&imageSR=102003","test.tif")            
img<-raster::raster("test.tif")
qmap(,lake)
