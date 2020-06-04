context("Miscellaneous functions")


test_that("Doesnt disclose unique values", {
  set.seed(1)
  expect_true(could_disclose_unique_values(letters))
  expect_false(could_disclose_unique_values(rnorm(1000)))
  expect_true(could_disclose_unique_values(rep(letters, each = 2)))
  expect_false(could_disclose_unique_values(as.factor(rep(letters, each = 2))))
  expect_true(could_disclose_unique_values(letters[1:6]))
})

nhanes <- structure(list(age = c(1, 2, 1, 3, 1, 3, 1, 1, 2, 2, 1, 2, 3,
                                 2, 1, 1, 3, 2, 1, 3, 1, 1, 1, 3, 2),
                         bmi = c(NA, 22.7, NA, NA, 20.4, NA, 22.5, 30.1, 22, NA,
                                 NA, NA, 21.7, 28.7, 29.6, NA, 27.2, 26.3, 35.3,
                                 25.5, NA, 33.2, 27.5, 24.9, 27.4),
                         hyp = c(NA, 1, 1, NA, 1, NA, 1, 1, 1, NA, NA, NA, 1, 2,
                                 1, NA, 2, 2, 1, 2, NA, 1, 1, 1, 1),
                         chl = c(NA, 187, 187, NA, 113, 184, 118, 187, 238, NA,
                                 NA, NA, 206, 204, NA, NA, 284, 199, 218, NA,
                                 NA, 229, 131, NA, 186)),
                    class = "data.frame", row.names = as.character(1:25))

test_that("Can print codebook table", {
  expect_silent(codebook:::export_table(codebook_table(nhanes)))
})

test_that("Missing values are computed properly", {
  expect_silent(mdp <- md_pattern(nhanes))
  expect_equal(mdp$n_miss[1], 27)
  expect_equal(nrow(mdp), 6)
})

test_that("Missing values are computed properly, omit complete", {
  expect_silent(mdp <- md_pattern(nhanes,
                        omit_complete = FALSE, min_freq = 0.2))
  expect_equal(mdp$n_miss[1], 27)
  expect_equal(mdp$n_miss[4], 5)
  expect_equal(nrow(mdp), 4)
})

test_that("Missing values are computed properly, degenerate cases", {
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
})

test_that("to_factor", {
  x <- haven::labelled(rep(1:5, each = 1), c(Bad = 1, Good = 5))
  expect_equal(as_factor(x),
               structure(1:5, .Label = c("Bad", "2", "3", "4", "Good"),
                         class = "factor"))
  expect_equal(as_factor(x), to_factor(zap_labelled(x)))
  expect_equal(as_factor(x, "both"), to_factor(zap_labelled(x), "both"))

  x <- haven::labelled(letters[1:5], c(Bad = "a", Good = "e"))
  expect_equal(as_factor(x),
               structure(1:5, .Label = c("Bad", "b", "c", "d", "Good"),
                         class = "factor"))
  expect_equal(as_factor(x), to_factor(zap_labelled(x)))
  expect_equal(as_factor(x, levels = "both"),
               to_factor(zap_labelled(x), levels = "both"))
})
