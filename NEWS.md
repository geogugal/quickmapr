# quickmapr v 0.3.0 (2018-05-15)

## CRAN Issues
- Using github hosted images in README now instead non-existing (on CRAN) local images.

## Major Changes
- `sf` objects now supported.  Coerces to `Spatial`
- Non-matching projections now throw a warning instead of an error.

## Minor Changes
- several legacy objects were included in the `qmap` object.  I have removed `map`, `col_tbl`, and `values`.
- names on map_data were too agressively cleaned and caused problems when passing anything more complex than a simple object name.  Fixed.
- Aerials from the `basemap` option no longer supported by the National Map.  First step is a warning and returning a topo.  Will be deprecated in future releases.


# quickmapr v0.2.0 (2016-09-16)

## API changes
- the `i()` function now allows for selecting multiple points and only prints 
results to screen.  To access selected `sp` objects, see `s()` (listed below)

## New functions
- Two new functions were added, `s()` for selecting objects and returning the sp
object, and `m()` for measuring distances on the `qmap`.

## Minor changes
- closed several issues
- added message if number of colors doesn't match number of data layers

## Bug Fixes
- colormap on zoomed rasters not plotting correctly.

# quickmapr v0.1.2

- cleaned up code
- made get_basemap an internal function (perhaps start deprecation)
- fixed raster draw bug by specifying ext arg on plot
- added categorical example data (NLCD)