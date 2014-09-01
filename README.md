quickmapr
=========

There are many packages that already exist or are in active development that support the visualization of spatial data in R.  However, there seems to be a gap for those that need to quickly view, compare, and explore the results of a given spatial analysis. Rhe current thinking for `quickmapr` is to allow for quick visualization of `sp` and `raster` objects. 

planned functionality for the first release is for easy mapping of multiple layers, simple zooming, panning, and labelling.  These tools are intended for use within an active spatial analysis workflow and not for production quality maps.

`quickmapr` uses ggplot2 for building the plots and is built off of a new S3 object, `qmap`.  A `qmap` object contains a list of the layers to map, controls on which layers are visible, colors for each layer, and the order of drawing.

Currently there are 6 commands planned.  As the idea behind this is to quickly map data, an emphasis was given to brevity of function names.  The commands are:

- `qmap()`: builds the `qmap` object and controls display parameters
- `m()`: maps a `qmap` object
- `zi()`: zooms in
- `zo()`: zooms out
- `pan()`: pans
- `lab()`: adds labels
