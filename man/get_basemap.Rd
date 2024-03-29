% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qmap.R
\name{get_basemap}
\alias{get_basemap}
\title{Get a basemap from USGS National Map}
\usage{
get_basemap(
  qmap_obj = NULL,
  base = c("1m_aerial", "topo"),
  width = 300,
  outfile = tempfile()
)
}
\arguments{
\item{qmap_obj}{A valid \code{qmap()} object}

\item{base}{A character indicating basemap to get (1m aerial or topo)}

\item{width}{Width, in pixels of image exported from The National Map web 
service. Height is determined by width:height ratio of the 
extent of the qmap object.}

\item{outfile}{an output file to save the resultant jpg.}
}
\description{
Uses the National Map Aerial Image REST API to return an aerial image to be
used as a basemap.  May add functionality for 1m or 1ft images.  May also add
topo-map.
}
\examples{
\dontrun{
#Can be run alone to get jpg, but best if run through qmap()
data(lake)
x<-qmap(lake,buffer)
x_base<-get_basemap(x,'1m_aerial',width=1000)
x<-qmap(x_base)
}

}
\keyword{internal}
