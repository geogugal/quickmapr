#' Build qmap object
#'
#' This function builds the qmap object that forms the basis for the rest of 
#' the \code{\link{quickmapr}} package.
#' 
#' @param ... Spatial objects to map.  Maybe passed as objects, a list of 
#'            spatial objects, or a \code{\link{qmap}} object
#' @param extent A \code{\link{sp}} or \code{\link[raster]{raster}} object to 
#'               use as the initial extent of the map.  Defaults to the maximum
#'               extent of all input object
#' @param order draw order of the spatial object. Defaults to order in mapdata
#' @param colors line colors. Defaults to 1:length(mapdata)  
#' @param fill Logical to determine if polygons should be filled (using colors)
#'             or just the border colored.
#' @param prj Logical to check projections of input spatial objects.  
#'            Transformation, if needed, should be done prior to mapping with 
#'            \code{sp::spTransform()}.
#' @param basemap a basemap generated from \code{\link{get_basemap}}
#' @param resolution Specifies the width in pixels of the retrieved basemap.
#'                    Larger values result in higher resolution images but since
#'                    the images are downloaded for each zoom level can result
#'                    in delays.  Default is 300, while ~600 is a decent 
#'                    compromise for performance and image quality.
#' @return Function displays a map from the input \code{mapdata} parameter and 
#'         returns a recorded plot.
#' 
#' @import sp
#' @importFrom methods as
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' mymap<-list(elev,lake,buffer,length,samples)
#' qm<-qmap(mymap)
#' #change draw order and which data is displayed
#' qmap(qm,order=c(2,3,5))
#' #add a basemap
#' qm<-qmap(qm,basemap="1m_aerial", resolution = 800))
#' }
qmap <- function(..., extent = NULL, order = 1:length(mapdata), 
                 colors = 1:length(mapdata), fill = FALSE, prj = TRUE, 
                 basemap = c("none","1m_aerial","topo"),resolution = 300) {
    if (length(list(...)) == 0) {
        stop("No data passed to qmap")
    }
    
    basemap <- match.arg(basemap)
    if(basemap == "none") {basemap <- NULL}
    
    mapdata <- build_map_data(...)
    # Test Projections
    if (prj) {
        prjs <- lapply(mapdata, sp::proj4string)
        if (length(unique(prjs)) > 1) {
            stop("Projections do not match. Use prj=FALSE to override projection check.\n\n This is not recommended. Re-project to common projection instead.", 
                call. = FALSE)
        } else if (any(is.na(prjs))) {
            stop("No projection info.  Use prj=FALSE to override projection check.", 
                call. = FALSE)
        }
    }

    if (length(mapdata) > 1) {
        # Sets Extent to all entered extents or a specific one.
        if (is.null(extent)) {
            bbx <- sp::bbox(mapdata[[1]])
            for (i in 1:length(mapdata)) {
                bbx[1, 1] <- min(c(bbx[1, 1], sp::bbox(mapdata[[i]])[1, 1]))
                bbx[1, 2] <- max(c(bbx[1, 2], sp::bbox(mapdata[[i]])[1, 2]))
                bbx[2, 1] <- min(c(bbx[2, 1], sp::bbox(mapdata[[i]])[2, 1]))
                bbx[2, 2] <- max(c(bbx[2, 2], sp::bbox(mapdata[[i]])[2, 2]))
            }
        }
    }
    if (!exists("bbx") & is.null(extent)) {
        bbx <- bbox(mapdata[[1]])
    } else if (!is.null(extent)) {
        # if(is.character(extent)) { bbx <- bbox(mapdata[[extent]]) } else {
        bbx <- bbox(extent)
        # }
    }
    bbx <- data.frame(bbx)
    
    values <- NULL
    col_tbl <- NULL
    
    
    
    # match colors to length of mapdata
    colors <- rep(colors, length(mapdata))[1:length(mapdata)]
    
    qmap_obj <- list(map_data = mapdata, map_extent = bbx, orig_extent = bbx, 
                     draw_order = order, 
                     colors = colors, fill = fill, map = NULL, 
                     basemap = basemap, col_tbl = col_tbl, values = values,
                     resolution = resolution)
    class(qmap_obj) <- "qmap"
    #qmap_obj$map = plot.qmap(qmap_obj)
    plot.qmap(qmap_obj)
    return(qmap_obj)
}

#' Default plotting of a qmap object
#' 
#' Plots the qmap class and uses the order, colors, extent, and fill option 
#' from \code{qmap}.
#' 
#' @param x input qmap class to plotS
#'                   
#' @param ... options passed to image or plot
#' @method plot qmap
#' @importFrom grDevices recordPlot
#' @importFrom graphics image text
#' @importFrom raster plotRGB extent
#' 
#' @export
plot.qmap <- function(x, ...) {
    order <- x$draw_order
    mapdata <- x$map_data
    fill <- x$fill
    colors <- x$colors
    bbx <- x$map_extent
    basemap <- x$basemap
    col_tbl <- x$col_tbl
    values <- x$values
    resolution <- x$resolution
    
    # Creates the plot
    first <- TRUE
    if (!is.null(basemap)) {
        #image(basemap, red = 1, green = 2, blue = 3, 
        #      xlim = as.numeric(bbx[1, ]), ylim = as.numeric(bbx[2, ]), 
        #      axes = TRUE, ...)
        bm<-get_basemap(x,basemap,width=resolution)
        plotRGB(bm, ext = extent(c(as.numeric(bbx[1, ]),
                                        as.numeric(bbx[2, ]))),
                axes=TRUE)
        first <- FALSE
    }
    for (i in 1:length(order)) {
        if (first) {
            if (get_sp_type(mapdata[[order[i]]]) == "grid") {
                #browser()
                #if (!is.null(values)) {
                #  image(mapdata[[order[i]]], xlim = as.numeric(bbx[1, ]), 
                #        ylim = as.numeric(bbx[2,]), axes = TRUE, col = col_tbl, 
                #        breaks = c(0, values), ...)
                #} else {
                  plot(mapdata[[order[i]]], xlim = as.numeric(bbx[1, ]), 
                        ylim = as.numeric(bbx[2,]), axes = TRUE, legend=FALSE,
                       ...)
                #}
                first <- FALSE
            } else if (get_sp_type(mapdata[[order[i]]]) == "polygon") {
                if (fill) {
                  plot(mapdata[[order[i]]], xlim = as.numeric(bbx[1, ]), 
                       ylim = as.numeric(bbx[2,]), axes = TRUE, col = colors[i], 
                       ...)
                } else {
                  plot(mapdata[[order[i]]], xlim = as.numeric(bbx[1, ]), 
                       ylim = as.numeric(bbx[2,]), axes = TRUE, 
                       border = colors[i], ...)
                }
                first <- FALSE
            } else if (!get_sp_type(mapdata[[order[i]]]) == "polygon") {
                plot(mapdata[[order[i]]], xlim = as.numeric(bbx[1, ]), 
                     ylim = as.numeric(bbx[2,]), axes = TRUE, col = colors[i])
                first <- FALSE
            }
        } else {
            if (get_sp_type(mapdata[[order[i]]]) == "grid") {
                #if (!is.null(values)) {
                #  image(mapdata[[order[i]]], add = TRUE, col = col_tbl, 
                #        breaks = c(0, values), ...)
                #} else {
                  plot(mapdata[[order[i]]], add = TRUE, legend=FALSE, ...)
                #}
            } else if (get_sp_type(mapdata[[order[i]]]) == "polygon") {
                if (fill) {
                  plot(mapdata[[order[i]]], col = colors[i], add = TRUE)
                } else {
                  plot(mapdata[[order[i]]], border = colors[i], add = TRUE)
                }
            } else if (!get_sp_type(mapdata[[order[i]]]) == "polygon") {
                plot(mapdata[[order[i]]], col = colors[i], add = TRUE)
            }
        }
    }
    if ("label" %in% names(x)) {
        text(x = x$label$x, y = x$label$y, labels = x$label$labs)
    }
    
}

#' Default printing of a qmap object
#' 
#' Prints the summary of a qmap object
#' 
#' @param x input qmap class to print
#' @param ... options passed to summary
#' @method print qmap
#' @export
print.qmap <- function(x, ...) {
    print_it <- list(map_data = names(x$map_data), map_extent = x$map_extent, 
                     draw_order = x$draw_order, colors = x$colors,
                     fill = x$fill, label = x$label)
    return(print_it)
}

#' Get a basemap from USGS National Map
#' 
#' Uses the National Map Aerial Image REST API to return an aerial image to be
#' used as a basemap.  May add functionality for 1m or 1ft images.  May also add
#' topo-map.
#' 
#' @param qmap_obj A valid \code{qmap()} object
#' @param base A character indicating basemap to get (1m aerial or topo)
#' @param width Width, in pixels of image exported from The National Map web 
#'              service. Height is determined by width:height ratio of the 
#'              extent of the qmap object.
#' @param outfile an output file to save the resultant jpg.
#' @examples
#' \dontrun{
#' #Can be run alone to get jpg, but best if run through qmap()
#' data(lake)
#' x<-qmap(lake,buffer)
#' x_base<-get_basemap(x,'1m_aerial',width=1000)
#' x<-qmap(x_base)
#' }
#' 
#' @importFrom httr GET
#' @importFrom raster stack
#' @export
get_basemap <- function(qmap_obj = NULL, base = c("1m_aerial", "topo"), 
                        width = 300, outfile = tempfile()) {
    base <- match.arg(base)
    if (is.null(qmap_obj)) {
        stop("A qmap_obj is required to fetch a basemap")
    } else if (class(qmap_obj) != "qmap") {
        stop("Requires a valid qmap_obj.")
    } else {
        bbx <- qmap_obj$map_extent
        p4s <- proj4string(qmap_obj$map_data[[1]])
    }
    if (base == "1m_aerial") {
        server_url <- "http://raster.nationalmap.gov/arcgis/rest/services/Orthoimagery/USGS_EROS_Ortho_NAIP/ImageServer/exportImage?"
    } else if (base == "topo") {
        server_url <- "http://services.arcgisonline.com/arcgis/rest/services/USA_Topo_Maps/MapServer/export?"
    }
    
    xdiff <- abs(bbx[1, 1] - bbx[1, 2])
    ydiff <- abs(bbx[2, 1] - bbx[2, 2])
    big_bbx <- matrix(c(bbx[1, 1] - (xdiff * 0.25), bbx[2, 1] - (ydiff * 0.25), 
                        bbx[1,  2] + (xdiff * 0.25), 
                        bbx[2, 2] + (ydiff * 0.25)), 2, 2)
    ratio <- xdiff/ydiff
    bbx_url <- paste("bbox=", big_bbx[1, 1], ",", big_bbx[2, 1], ",", big_bbx[1, 
        2], ",", big_bbx[2, 2], sep = "")
    format_url <- "&format=jpg"
    pixel_url <- "&pixelType=U8&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation"
    file_url <- "&f=image"
    bbx_sr_url <- paste("&bboxSR={'wkt':'", rgdal::showWKT(p4s), "'}", sep = "")
    image_sr_url <- paste("&imageSR={'wkt':'", rgdal::showWKT(p4s), "'}", 
                          sep = "")
    size_url <- paste("&size=", width, ",", width/ratio, sep = "")
    request_url <- paste0(server_url, bbx_url, bbx_sr_url, image_sr_url, 
                          size_url, 
        format_url, pixel_url, file_url)
    tmp <- outfile
    tmp_jpg <- paste0(tmp, ".jpg")
    tmp_jpgw <- paste0(tmp, ".jpgw")
    r<-GET(request_url, httr::write_disk(tmp_jpg,overwrite=T))
    make_jpw(tmp_jpgw, big_bbx, width)
    #img <- rgdal::readGDAL(tmp_jpg, silent = TRUE, p4s = p4s)
    img <- stack(tmp_jpg) #some goofiness with zooming and plotRGB
    #file.remove(tmp_jpg, tmp_jpgw)
    return(img)
} 
