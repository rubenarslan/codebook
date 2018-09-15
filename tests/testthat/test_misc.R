context("Miscellaneous functions")

test_that("Can print codebook table", {
  data("nhanes", package = "mice")
  expect_silent(codebook:::export_table(codebook_table(nhanes)))
  expect_output(pander::pander(codebook_table(nhanes)), "data_type")
})

test_that("Can print codebook table", {
  data("nhanes", package = "mice")
  expect_silent(codebook:::export_table(codebook_table(nhanes)))
})

test_that("Missing values are computed properly", {
  data("nhanes", package = "mice")

  expect_silent(mdp <- md_pattern(nhanes))
  expect_equal(mdp$n_miss[1], 27)
  expect_equal(nrow(mdp), 6)
  expect_silent(mdp <- md_pattern(nhanes,
                        omit_complete = FALSE, min_freq = 0.2))
  expect_equal(mdp$n_miss[1], 27)
  expect_equal(mdp$n_miss[4], 5)
  expect_equal(nrow(mdp), 4)
  expect_message(mdp <- md_pattern(data.frame()), "No missing")
  expect_message(mdp <- md_pattern(data.frame(x = 1, y = 1)), "No missing")
  expect_null(mdp)
  expect_message(mdp <- md_pattern(data.frame(x = 1, y = 1),
                                   omit_complete = FALSE,
                                 min_freq = 0), "No missing")
  expect_null(mdp)

  expect_message(mdp <- md_pattern(data.frame(x = "a", y = "b"),
                                   omit_complete = FALSE,
                                  min_freq = 0), "No missing")
  expect_null(mdp)

  expect_silent(mdp <- md_pattern(data.frame(x = NA, y = NA),
                                  omit_complete = FALSE,
                                  min_freq = 0))
  expect_equal(nrow(mdp), 2)
  expect_equal(sum(mdp$n_miss), 3)
  expect_equal(ncol(mdp), 5)
})


test_that("Progress counters", {
  survey <- data.frame(
    created = c("2016-05-27 10:11:00", NA, "2016-05-29 11:18:28"),
    ended = c("2016-05-28 10:11:00", NA, "2016-05-30 11:18:28"),
    modified = c("2016-05-28 10:11:00", NA, "2016-05-30 11:18:28"),
    expired = c(NA, "2016-05-28 10:11:00", NA)
  )
  expect_equal(ended(survey = survey), 2)
  expect_equal(modified(survey = survey), 2)
  expect_equal(expired(survey = survey), 1)

})

test_that("Require package", {
  expect_silent(require_package("codebook"))
  expect_error(require_package("3k43hb34bk3"))
})


test_that("New codebook Rmd", {
  expect_silent(new_codebook_rmd(NULL))
})


test_that("has_label(s)", {
  data('bfi')
  expect_true(has_label(bfi$created))
  expect_false(has_labels(bfi$created))
  expect_true(has_labels(bfi$BFIK_agree_1R))
  expect_false(has_label(bfi$session))
  expect_false(has_labels(bfi$session))

  x <- haven::labelled(rep(1:5, each = 1), c(Bad = 1, Good = 5))
  expect_equal(as_factor(x),
               structure(1:5, .Label = c("Bad", "2", "3", "4", "Good"),
                         class = "factor"))
  expect_equal(as_factor(x), as_factor(zap_labelled(x)))
  expect_equal(as_factor(x, "both"), as_factor(zap_labelled(x), "both"))
})
