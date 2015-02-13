quickmapr
=========

![travis_status](https://travis-ci.org/jhollist/quickmapr.svg)

There are many packages that already exist or are in active development that support the visualization of spatial data in R.  However, there seems to be a gap for those that need to quickly view, compare, and explore the results of a given spatial analysis. The current thinking for `quickmapr` is to allow for quick visualization of `sp` and `raster` objects. 

Planned functionality for the first release is for easy mapping of multiple layers, simple zooming, panning, and labelling.  These tools are intended for use within an active spatial analysis workflow and not for production quality maps.

`quickmapr` is built as a series of wrapper functions for the default `sp` plotting functions and currently utilizes the `zoom` packages for zooming and panning. Currently there are 5 commands planned.  As the idea behind this is to quickly map data, an emphasis was given to brevity of function names.  The commands are:

- `qmap()`: creates the map
- `zi()`: zooms in
- `zo()`: zooms out
- `ze()`: zoom in to an extent
- `p()`: pans
- `l()`: adds labels
- `i()`: identify features

##Installation
This package is not yet on CRAN.  To install

```r
install.packages("devtools")
library("devtools")
install_github("jhollist/quickmapr")
```



