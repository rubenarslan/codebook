context("Miscellaneous functions")

test_that("Missings are computed properly", {
  data("nhanes", package = "mice")
  expect_silent(mdp <- md_pattern(nhanes))
  expect_equal(mdp$n_miss[1], 27)
  expect_equal(nrow(mdp), 6)
  expect_silent(mdp <- md_pattern(nhanes,
                        only_vars_with_missings = FALSE, min_freq = 0.2))
  expect_equal(mdp$n_miss[1], 27)
  expect_equal(mdp$n_miss[4], 5)
  expect_equal(nrow(mdp), 4)
  expect_error(mdp <- md_pattern(data.frame()))
  expect_silent(mdp <- md_pattern(data.frame(x = 1, y = 1)))
  expect_equal(nrow(mdp), 2)
  expect_equal(ncol(mdp), 0)
  expect_silent(mdp <- md_pattern(data.frame(x = 1, y = 1),
                                 only_vars_with_missings = FALSE,
                                 min_freq = 0))
  expect_equal(nrow(mdp), 2)
  expect_equal(sum(mdp$n_miss), 0)
  expect_equal(ncol(mdp), 5)

  expect_silent(mdp <- md_pattern(data.frame(x = "a", y = "b"),
                                  only_vars_with_missings = FALSE,
                                  min_freq = 0))
  expect_equal(nrow(mdp), 2)
  expect_equal(sum(mdp$n_miss), 0)
  expect_equal(ncol(mdp), 5)

  expect_silent(mdp <- md_pattern(data.frame(x = NA, y = NA),
                                  only_vars_with_missings = FALSE,
                                  min_freq = 0))
  expect_equal(nrow(mdp), 2)
  expect_equal(sum(mdp$n_miss), 2)
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
