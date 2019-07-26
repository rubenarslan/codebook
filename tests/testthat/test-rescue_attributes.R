test_that("attributes can be rescued", {
  bfi <- data.frame(matrix(data = rnorm(300), ncol = 3))
  names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3")
  attributes(bfi$bfi_e1)$label <- "I am outgoing."
  attributes(bfi$bfi_e2R)$label <- "I prefer books to people."
  bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
  bfi <- detect_scales(bfi, quiet = TRUE) # create attributes
  bfi_no_labels <- zap_attributes(bfi, "label")
  expect_equivalent(bfi_no_labels, bfi)
  expect_failure(expect_identical(bfi_no_labels, bfi))
  bfi_with_labels <- rescue_attributes(bfi_no_labels, bfi)
  expect_identical(bfi_with_labels, bfi)
})
