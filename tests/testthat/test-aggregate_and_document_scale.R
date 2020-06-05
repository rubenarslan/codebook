test_that("scale aggregation", {
  testdf <- data.frame(bfi_neuro_1 = rnorm(20), bfi_neuro_2 = rnorm(20),
                      bfi_neuro_3R = rnorm(20), age = rpois(20, 30))
  item_names <- c('bfi_neuro_1', 'bfi_neuro_2', 'bfi_neuro_3R')
  expect_silent(testdf$bfi_neuro <-
                  aggregate_and_document_scale(testdf[, item_names]))
  expect_identical(attributes(testdf$bfi_neuro)$scale_item_names, item_names)
  expect_identical(attributes(testdf$bfi_neuro)$label,
                   "3 bfi_neuro items aggregated by rowMeans")

  item_names <- c('bfi_neuro_1', 'bfi_neuro_2', 'bfi_neuro_3R', 'age')
  expect_silent(testdf$bfi_neuro <-
                  aggregate_and_document_scale(testdf[, item_names]))
  expect_identical(attributes(testdf$bfi_neuro)$scale_item_names, item_names)
  expect_identical(attributes(testdf$bfi_neuro)$label,
                   "4  items aggregated by rowMeans")
})



