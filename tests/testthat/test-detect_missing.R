context("detect_missing")

haven_version = as.numeric(
  paste0(unlist(packageVersion("haven")), collapse = ""))

test_that("user-defined missing values read as normal missing by default", {
  num <- haven::read_spss(test_path("different-missings.sav"))
  expect_identical(vctrs::vec_data(num[2, 1][[1]]), NA_real_)
  expect_identical(vctrs::vec_data(num[3, 1][[1]]), NA_real_)
  expect_identical(vctrs::vec_data(num[5, 1][[1]]), NA_real_)

  expect_equal(vctrs::vec_data(num[2, 2][[1]]), 99)
  expect_identical(vctrs::vec_data(num[3, 2][[1]]), NA_real_)
  expect_equal(vctrs::vec_data(num[5, 2][[1]]), 999)

  expect_identical(vctrs::vec_data(num[2, 3][[1]]), NA_real_)
  expect_identical(vctrs::vec_data(num[3, 3][[1]]), NA_real_)
  expect_identical(vctrs::vec_data(num[5, 3][[1]]), NA_real_)

  expect_equal(vctrs::vec_data(num[2, 4][[1]]), 99)
  expect_identical(vctrs::vec_data(num[3, 4][[1]]), NA_real_)
  expect_equal(vctrs::vec_data(num[5, 4][[1]]), 999)

  expect_silent(num <- detect_missing(num))

  expect_identical(vctrs::vec_data(num[2, 4][[1]]), NA_real_)
  expect_identical(vctrs::vec_data(num[3, 4][[1]]), NA_real_)
  expect_identical(vctrs::vec_data(num[5, 4][[1]]), NA_real_)

  expect_equal(vctrs::vec_data(num[2, 2][[1]]), 99)
  expect_identical(vctrs::vec_data(num[3, 2][[1]]), NA_real_)
  expect_equal(vctrs::vec_data(num[5, 2][[1]]), 999)

  expect_silent(num <- detect_missing(num, only_labelled = FALSE))
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

  if(haven_version > 231) {
  expect_silent(num <- detect_missing(num,
                                      negative_values_are_missing = FALSE,
                                      only_labelled = FALSE))
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

  num <- detect_missing(num, negative_values_are_missing = TRUE,
                        only_labelled = FALSE)
  expect_equal(sum(num$val1, na.rm = TRUE), 19)
  expect_equal(sum(num$val2, na.rm = TRUE), 19)
  expect_equal(sum(num$val3, na.rm = TRUE), 19)
  expect_equal(sum(num$val4, na.rm = TRUE), 19)

  expect_identical(haven::na_tag(num[, 1][[1]]),
                   c(NA, "a", NA, NA, "b", "c", NA, NA, NA, NA, NA, NA))
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 2][[1]]) )
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 3][[1]]) )
  expect_identical(haven::na_tag(num[, 1][[1]]), haven::na_tag(num[, 4][[1]]) )
  }
})

test_that("we can also keep labelled_spss", {
  num <- haven::read_spss(test_path("different-missings.sav"), user_na = TRUE)
  attributes(num[, 3][[1]])$na_values <- NULL
  attributes(num[, 3][[1]])$label <- ""
  attributes(num[, 4][[1]])$label <- ""

  expect_failure(expect_identical(num[, 3][[1]], num[, 4][[1]]))

  num <- detect_missing(num, negative_values_are_missing = FALSE,
                        only_labelled = FALSE,
                        use_labelled_spss = TRUE)
  expect_failure(expect_equal(base::sum(num$val1, na.rm = TRUE), 18))
  # SPSS missing values suck because base R does not know them
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
  expect_equal(unclass(num[3, 2][[1]])[[1]], NA_real_)
  expect_equal(unclass(num[5, 2][[1]])[[1]], 999)

  if(haven_version > 231) {
    num <- detect_missing(num, negative_values_are_missing = TRUE,
                        only_labelled = FALSE)
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
  }
})


test_that("don't accidentally zap variable labels", {
  data("bfi", package = "codebook")
  bfi$shouldkeep <- haven::labelled(
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                0, 0, 0, 0, 0, 0, 0, NA, NA, NA, 0, NA),
                              label = "Test labelled",
                              labels = c(keep = 0, drop = 1))
  labels <- var_label(bfi)
  bfi2 <- detect_missing(bfi, use_labelled_spss = FALSE, vars = "shouldkeep")
  expect_identical(labels, var_label(bfi2))
  expect_identical(attributes(bfi$shouldkeep), attributes(bfi2$shouldkeep))

  bfi2 <- detect_missing(bfi, use_labelled_spss = FALSE)
  expect_identical(labels, var_label(bfi2))
  expect_identical(attributes(bfi$shouldkeep), attributes(bfi2$shouldkeep))
  expect_identical(attributes(bfi$BFIK_open_2)$labels,
                   attributes(bfi2$BFIK_open_2)$labels)
  expect_identical(attributes(bfi$BFIK_open_2)$label,
                   attributes(bfi2$BFIK_open_2)$label)
  expect_identical(attributes(bfi$BFIK_open_2)$class,
                   attributes(bfi2$BFIK_open_2)$class)
  expect_identical(attributes(bfi$BFIK_open_2)$item,
                   attributes(bfi2$BFIK_open_2)$item)
  expect_warning(
    expect_setequal(attributes(bfi$BFIK_open_2), attributes(bfi2$BFIK_open_2))
  )
  expect_identical(bfi, bfi2)
  bfi2 <- detect_missing(bfi, negative_values_are_missing = TRUE)
  expect_identical(labels, var_label(bfi2))
  bfi2 <- detect_missing(bfi, negative_values_are_missing = FALSE,
                         only_labelled = FALSE)
  expect_identical(labels, var_label(bfi2))
  bfi2 <- detect_missing(bfi, negative_values_are_missing = FALSE,
                         learn_from_labels = FALSE,
                         missing = 0, use_labelled_spss = TRUE)
  expect_identical(labels, var_label(bfi2))
  bfi2 <- detect_missing(bfi, use_labelled_spss = TRUE)
  expect_identical(labels, var_label(bfi2))
  expect_failure(expect_identical(bfi, bfi2))
})
