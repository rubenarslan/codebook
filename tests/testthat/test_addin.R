context("Test addins")

test_that("static label browser", {
  expect_error(dt <- label_browser_static(), "No data frame")
  data('bfi')
  expect_silent(dt <- label_browser_static())
  expect_identical(class(dt), c("datatables", "htmlwidget"))
  expect_equal(nrow(dt$x$data), ncol(bfi))
  expect_silent(dt <- label_browser_static(bfi))
  expect_identical(class(dt), c("datatables", "htmlwidget"))
  expect_equal(nrow(dt$x$data), ncol(bfi))
  expect_equal(ncol(dt$x$data), 3)
})

# library(shinytest)
#
#
# # recordTest("tests/testthat/codebook_browser/")
#
# test_that("codebook browser", {
#   skip_on_cran()
#   expect_pass(testApp("codebook_browser"))
# })
#
# # recordTest("tests/testthat/label_browser/")
#
# test_that("codebook browser", {
#   skip_on_cran()
#   expect_pass(testApp("label_browser"))
# })
#
