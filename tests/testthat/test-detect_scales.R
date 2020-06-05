
test_that("related items of form scale_1r are detected", {
  bfi <- data.frame(matrix(data = rnorm(500), ncol = 5))
  names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3", "bfi_n1", "bfi_n2")
  bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
  expect_warning(
    expect_message(bfi <- detect_scales(bfi), "bfi_e items"),
    "bfi_n items found, but")
  expect_equal(attributes(bfi$bfi_e)$scale_item_names,
               c("bfi_e1", "bfi_e2R", "bfi_e3"))
  expect_match(attributes(bfi$bfi_e)$label, "3 bfi_e items")
  expect_equal(attributes(bfi$bfi_n1)$scale_item_names, NULL)
  expect_equal(attributes(bfi$bfi_n1)$label, NULL)

  # scales that aren't aggregates of the items we detect are warned about
  bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_n1")])
  expect_warning(detect_scales(bfi, TRUE), "not perfectly correlated")
})



test_that("related items of form scale_1r are detected", {
  bfi <- data.frame(matrix(data = rnorm(500), ncol = 5))
  names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3", "bfi_n1", "bfi_n2")
  bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
  expect_warning(
    expect_message(bfi <- detect_scales(bfi), "bfi_e items"),
    "bfi_n items found, but")
  expect_equal(attributes(bfi$bfi_e)$scale_item_names,
               c("bfi_e1", "bfi_e2R", "bfi_e3"))
  expect_match(attributes(bfi$bfi_e)$label, "3 bfi_e items")
  expect_equal(attributes(bfi$bfi_n1)$scale_item_names, NULL)
  expect_equal(attributes(bfi$bfi_n1)$label, NULL)

  # scales that aren't aggregates of the items we detect are warned about
  bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_n1")])
  expect_warning(detect_scales(bfi, TRUE), "not perfectly correlated")
})


test_that("attributes can be zapped", {
  bfi <- data.frame(matrix(data = rnorm(300), ncol = 3))
  names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3")
  attributes(bfi$bfi_e1)$label <- "I am outgoing."
  attributes(bfi$bfi_e2R)$label <- "I prefer books to people."
  attributes(bfi$bfi_e3)$label <- "I love to party."
  bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
  bfi <- detect_scales(bfi, quiet = TRUE) # create attributes
  bfi <- zap_attributes(bfi, "label")
  expect_null(attributes(bfi$bfi_e)$label)
  expect_null(attributes(bfi$bfi_e1)$label)
  expect_null(attributes(bfi$bfi_e2R)$label)
  expect_null(attributes(bfi$bfi_e3)$label)
  expect_match(attributes(bfi$bfi_e)$scale_item_names, "bfi_e")
  bfi$bfi_e <- zap_attributes(bfi$bfi_e)
  expect_null(attributes(bfi$bfi_e3)$scale_item_names)
})


