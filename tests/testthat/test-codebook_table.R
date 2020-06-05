context("Test plots")

test_that("codebook skim", {
  data("bfi", package = 'codebook')
  bfi$number <- 1:nrow(bfi)
  bfi$alltrue <- T
  bfi$factor <- factor(1:nrow(bfi) > 10)

  expect_silent(skim_codebook(bfi))
})


test_that("codebook skim", {
  data("bfi", package = 'codebook')
  bfi$number <- 1:nrow(bfi)
  bfi$alltrue <- T
  bfi$factor <- factor(1:nrow(bfi) > 10)

  testthat::expect_silent(skimmed <- skim_codebook(bfi))
  testthat::expect_silent(skimmed_part <- skimr::partition(skimmed))
  testthat::expect_silent(skimmed_part <-
                            coerce_skimmed_summary_to_character(skimmed_part))
  skimmed_part %>% purrr::map(~ dplyr::summarise_all(., typeof)) %>%
    dplyr::bind_rows(.id = "type")
})

test_that("wide codebook skim", {
  data("bfi", package = 'codebook')
  bfi$number <- 1:nrow(bfi)
  bfi$alltrue <- T
  bfi$factor <- factor(1:nrow(bfi) > 10)

  testthat::expect_silent(skimmed <- skim_to_wide_labelled(bfi))
})

test_that("codebook table", {
  data("bfi", package = 'codebook')
  bfi$number <- 1:nrow(bfi)
  bfi$alltrue <- T
  bfi$factor <- factor(1:nrow(bfi) > 10)

  testthat::expect_silent(cb_table <- codebook_table(bfi))
  testthat::expect_equal(nrow(cb_table), ncol(bfi))
  testthat::expect_equal(cb_table$name, colnames(bfi))
  testthat::expect_equal(typeof(cb_table$min), "character")
  testthat::expect_equal(typeof(cb_table$mean), "double")
})


test_that("codebook table generation", {
  data("bfi", package = 'codebook')
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_agree"),
                        -starts_with("BFIK_open"))
  bfi$age <- 1:nrow(bfi)
  bfi$labelled_char <- haven::labelled(letters[1:nrow(bfi)], label = "Letters",
                                      labels = c("A" = "a", "B" = "b"))
  expect_silent(cb <- codebook_table(bfi))
  expect_identical(names(cb),
                   c("name", "label", "type", "type_options", "data_type",
                     "value_labels", "optional", "scale_item_names",
                     "item_order", "n_missing", "complete_rate",
                     "n_unique", "empty", "count", "min", "median", "max",
                     "mean", "sd", "whitespace", "n_value_labels", "hist"))
  expect_equal(nrow(cb), ncol(bfi))
  expect_identical(cb$name, names(bfi))
})

test_that("codebook table generation, no attributes", {
  data("bfi", package = 'codebook')
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_agree"),
                        -starts_with("BFIK_open"))
  bfi <- bfi %>% head() %>% zap_attributes()
  # drops attributes
  bfi$age <- 1:nrow(bfi)
  expect_silent(cb <- codebook_table(bfi))
  expect_identical(names(cb),
                   c("name", "data_type", "n_missing", "complete_rate",
                     "n_unique", "empty", "count", "min", "median", "max",
                     "mean", "sd", "whitespace", "hist", "label"))
  expect_equal(nrow(cb), ncol(bfi))
  expect_identical(cb$name, names(bfi))
})

