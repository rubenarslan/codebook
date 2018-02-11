context("Attribute detection functions")


test_that("user-defined missing values read as normal missing by default", {
  num <- haven::read_spss(test_path("different-missings.sav"))
  expect_identical(num[2, 1][[1]][[1]], NA_real_)
  expect_identical(num[3, 1][[1]][[1]], NA_real_)
  expect_identical(num[5, 1][[1]][[1]], NA_real_)

  expect_equal(num[2, 2][[1]][[1]], 99)
  expect_identical(num[3, 2][[1]][[1]], NA_real_)
  expect_equal(num[5, 2][[1]][[1]], 999)

  expect_identical(num[2, 3][[1]][[1]], NA_real_)
  expect_identical(num[3, 3][[1]][[1]], NA_real_)
  expect_identical(num[5, 3][[1]][[1]], NA_real_)

  expect_equal(num[2, 4][[1]][[1]], 99)
  expect_identical(num[3, 4][[1]][[1]], NA_real_)
  expect_equal(num[5, 4][[1]][[1]], 999)

  expect_silent(num <- detect_missings(num))

  expect_identical(num[2, 4][[1]][[1]], NA_real_)
  expect_identical(num[3, 4][[1]][[1]], NA_real_)
  expect_identical(num[5, 4][[1]][[1]], NA_real_)

  expect_equal(num[2, 2][[1]][[1]], 99)
  expect_identical(num[3, 2][[1]][[1]], NA_real_)
  expect_equal(num[5, 2][[1]][[1]], 999)

  expect_silent(num <- detect_missings(num, only_labelled_missings = FALSE))
  expect_equal(sum(num$val1, na.rm = TRUE), 19)
  expect_equal(sum(num$val2, na.rm = TRUE), 19)
  expect_equal(sum(num$val3, na.rm = TRUE), 19)
  expect_equal(sum(num$val4, na.rm = TRUE), 19)

  expect_equal(num[3, 2][[1]][[1]], NA_real_)
  expect_equal(num[5, 2][[1]][[1]], NA_real_)

  attributes(num[, 3][[1]])$na_values <- NULL
  attributes(num[, 3][[1]])$label <- ""
  attributes(num[, 4][[1]])$label <- ""

  expect_identical(num[, 3][[1]], num[, 4][[1]])
})

test_that("labelled_spss can be transformed into more useful tagged na", {
  num <- haven::read_spss(test_path("different-missings.sav"), user_na = TRUE)
  attributes(num[, 3][[1]])$na_values <- NULL
  attributes(num[, 3][[1]])$label <- ""
  attributes(num[, 4][[1]])$label <- ""

  expect_failure(expect_identical(num[, 3][[1]], num[, 4][[1]]))

  expect_silent(num <- detect_missings(num,
                                negative_values_are_missing = FALSE,
                                only_labelled_missings = FALSE))
  expect_equal(sum(num$val1, na.rm = TRUE), 18)
  expect_equal(sum(num$val2, na.rm = TRUE), 18)
  expect_equal(sum(num$val3, na.rm = TRUE), 18)
  expect_equal(sum(num$val4, na.rm = TRUE), 18)


  expect_identical(num[, 3][[1]], num[, 4][[1]])
  expect_equivalent(num[, 1][[1]], num[, 3][[1]])
  expect_equivalent(haven::zap_labels(num[, 1][[1]]), num[, 2][[1]])
  expect_equivalent(num[, 1][[1]], num[, 4][[1]])
  expect_equivalent(num[, 3][[1]], num[, 4][[1]])
  expect_equal(num[3, 2][[1]][[1]], NA_real_)
  expect_equal(num[5, 2][[1]][[1]], NA_real_)

  expect_identical(haven::na_tag(num[, 1][[1]]),
                   c(NA, "a", NA, NA, "b", NA, NA, NA, NA, NA, NA, NA))
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 2][[1]]) )
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 3][[1]]) )
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 4][[1]]) )

  num <- detect_missings(num, negative_values_are_missing = TRUE,
                                only_labelled_missings = FALSE)
  expect_equal(sum(num$val1, na.rm = TRUE), 19)
  expect_equal(sum(num$val2, na.rm = TRUE), 19)
  expect_equal(sum(num$val3, na.rm = TRUE), 19)
  expect_equal(sum(num$val4, na.rm = TRUE), 19)

  expect_identical(haven::na_tag(num[, 1][[1]]),
                   c(NA, "a", NA, NA, "b", "c", NA, NA, NA, NA, NA, NA))
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 2][[1]]) )
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 3][[1]]) )
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 4][[1]]) )
})

test_that("we can also keep labelled_spss", {
  num <- haven::read_spss(test_path("different-missings.sav"), user_na = TRUE)
  attributes(num[, 3][[1]])$na_values <- NULL
  attributes(num[, 3][[1]])$label <- ""
  attributes(num[, 4][[1]])$label <- ""

  expect_failure(expect_identical(num[, 3][[1]], num[, 4][[1]]))

  num <- detect_missings(num, negative_values_are_missing = FALSE,
                                only_labelled_missings = FALSE,
                                use_labelled_spss = TRUE)
  expect_failure(expect_equal(sum(num$val1, na.rm = TRUE), 18))
  # SPSS missings suck because base R does not know them
  sum <- function(x, na.rm = TRUE) { base::sum(x[!is.na(x)]) }
  expect_equal(sum(num$val1, na.rm = TRUE), 18)
  expect_equal(sum(num$val2, na.rm = TRUE), 18)
  expect_equal(sum(num$val3, na.rm = TRUE), 18)
  expect_equal(sum(num$val4, na.rm = TRUE), 18)

  expect_identical(num[, 3][[1]], num[, 4][[1]])
  expect_equivalent(num[, 1][[1]], num[, 3][[1]])
  expect_equivalent(haven::zap_labels(num[, 1][[1]]),
                    haven::zap_labels(num[, 2][[1]]))
  expect_equivalent(num[, 1][[1]], num[, 4][[1]])
  expect_equivalent(num[, 3][[1]], num[, 4][[1]])
  expect_equal(num[3, 2][[1]][[1]], NA_real_)
  expect_equal(num[5, 2][[1]][[1]], 999)

  num <- detect_missings(num, negative_values_are_missing = TRUE,
                                only_labelled_missings = FALSE)
  expect_failure(expect_equal(base::sum(num$val1, na.rm = TRUE), 18))

  expect_equal(sum(num$val1, na.rm = TRUE), 19)
  expect_equal(sum(num$val2, na.rm = TRUE), 19)
  expect_equal(sum(num$val3, na.rm = TRUE), 19)
  expect_equal(sum(num$val4, na.rm = TRUE), 19)

  expect_identical(haven::na_tag(num[, 1][[1]]),
                   c(NA, "b", NA, NA, "c", "a", NA, NA, NA, NA, NA, NA))
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 2][[1]]) )
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 3][[1]]) )
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 4][[1]]) )
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
