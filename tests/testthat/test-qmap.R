context("qmap")
data(lake)
x<-qmap(elev,lake,samples,width)

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

#need to test returns fromm zoom