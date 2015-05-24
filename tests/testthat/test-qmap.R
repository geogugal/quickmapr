context("qmap")

data(lake)

test_that("qmap works", {
  expect_is(qmap(lake,buffer,elev,samples,width), "qmap")
})

test_that("lawn_buffer fails correctly", {
  expect_error(qmap(), "non-character argument")
})