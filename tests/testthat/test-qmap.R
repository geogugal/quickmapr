#setup
context("qmap")
library(rgdal)
library(sp)
data(lake)
x<-qmap(elev,lake,samples,width)
x2<-qmap(samples,lake)
x3<-x2
l(x3)
samp_diff_proj <- spTransform(samples,
                              CRSobj = 
                                CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
lake_no_proj <- lake
proj4string(lake_no_proj)<-""

#Tests
test_that("qmap works", {
  expect_is(x, "qmap")
})

test_that("qmap fails correctly", {
  expect_error(qmap(), "No data passed to qmap")
})



test_that("zoom and pan fail correctly", {
  expect_error(zi(), "Requires a valid qmap_obj.")
  expect_error(zo(), "Requires a valid qmap_obj.")
  expect_error(ze(), "Requires a valid qmap_obj.")
  expect_error(p(), "Requires a valid qmap_obj.")
  expect_error(f(), "Requires a valid qmap_obj.")
  expect_error(zi(x,zoom_perc = 2), "Argument, zoom_perc, needs to be between 0 and 1")
  expect_error(zo(x,zoom_perc = 2), "Argument, zoom_perc, needs to be between 0 and 1")
})

test_that("zoom by extent zooms with sp", {
  expect_is(ze(x,samples),"recordedplot")
})

test_that("f() returns qmap", {
  expect_is(f(x),"qmap")
})

test_that("l() works",{
  expect_error(l(qmap(elev)),"Labelling for raster data not supported.")
  expect_error(l(lake,"COMID"), "Requires a valid qmap_obj.")
  expect_is(l(x,"samples"),"recordedplot")
  expect_is(l(qmap(samples,lake)),"recordedplot")
  expect_is(l(x2),"recordedplot")
  expect_is(l(x3),"recordedplot") #tests is qmap obj already has labels
})

test_that("projection checks work",{
  expect_error(qmap(samp_diff_proj,lake), "Projections do not match. Use prj=FALSE to override projection check.\n
           This is not recommended. Re-project to common projection instead.")
  expect_error(qmap(lake_no_proj),"No projection info.  Use prj=FALSE to override projection check.")
})

test_that("map_extent is assigned correctly with single input",{
  expect_is(qmap(lake)$map_extent,"data.frame")
  expect_is(qmap(lake,extent = samples)$map_extent,"data.frame")
})

test_that("identify fails correctly", {
  expect_error(i(),"Requires a valid qmap_obj.")
})
#need to test returns from zoom