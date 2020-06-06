context("Reliability computation")


test_that("Internal consistencies can be computed", {
  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_neuro"),
                        -starts_with("BFIK_open"))
  # expect message until I know whether I can do away with ufs
  expect_message(rels <- compute_reliabilities(bfi))
  expect_equal(length(rels), 2)
  expect_equal(length(rels$BFIK_agree), 1)
  expect_identical(names(rels$BFIK_agree), "internal_consistency")
  agree_output <- rels$BFIK_agree$internal_consistency$scaleReliability$output
  open_output <- rels$BFIK_open$internal_consistency$scaleReliability$output
  expect_equal(round(
    agree_output$dat$omega,3),
               0.819)
  expect_equal(codebook:::pull_reliability(rels$BFIK_agree),
               "ω<sub>ordinal</sub> [95% CI] = 0.61 [0.37;0.84]")
  expect_equal(round(
    agree_output$dat$omega.ci.hi,3),
    0.930)
  expect_null(open_output$dat$omega.ci.hi)
})

test_that("Retest reliabilities can be computed", {
  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_neuro"),
                        -starts_with("BFIK_open"),
                        -starts_with("BFIK_consc"))
  expect_silent(bfi2 <- bind_rows(bfi, bfi %>%
                          mutate(created = created + 1e7)))
  bfi2 <- rescue_attributes(bfi2, bfi)
  expect_silent(rels <- compute_reliabilities(bfi2,
                                              survey_repetition = "repeated_once"))
  expect_equal(length(rels), 1)
  expect_equal(length(rels$BFIK_agree), 3)
  expect_identical(names(rels$BFIK_agree), c("internal_consistency_T1",
                                             "internal_consistency_T2",
                                             "retest_reliability"))
  expect_equal(codebook:::pull_reliability(rels$BFIK_agree),
               "See details tab")

  agree_output <-
    rels$BFIK_agree$internal_consistency_T1$scaleReliability$output
  expect_equal(round(
    agree_output$dat$omega,3),
    0.819)
  expect_equal(round(
    agree_output$dat$omega.ci.hi,3),
    0.930)
  agree_output <- rels$BFIK_agree$retest_reliability$output
  expect_equivalent(round(agree_output$testRetestAlpha,3), 0.801)
  expect_equivalent(round(agree_output$testRetestCES,3), 0.752)
})


test_that("Multilevel reliabilities can be computed", {
  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_neuro"),
                        -starts_with("BFIK_consc"))
  fuzz <- function(x) { x + rnorm(length(x)) }
  expect_silent(bfi3 <- bind_rows(bfi,
           bfi %>% mutate(created = created + 86400),
           bfi %>% mutate(created = created + 86400 * 2),
           bfi %>% mutate(created = created + 86400 * 3),
           bfi %>% mutate(created = created + 86400 * 4),
           bfi %>% mutate(created = created + 86400 * 5)
  ) %>% mutate_at(vars(dplyr::matches("_\\d")), fuzz))
  bfi3 <- rescue_attributes(bfi3, bfi)
  expect_warning(rels <- compute_reliabilities(bfi3,
                                  survey_repetition = "repeated_many"))

  expect_equal(length(rels), 2)
  expect_equal(length(rels$BFIK_agree), 1)
  expect_identical(names(rels$BFIK_agree), "multilevel_reliability")
  expect_equal(codebook:::pull_reliability(rels$BFIK_agree),
               "See details tab")

  expect_equal(round(rels$BFIK_agree$multilevel_reliability$Rcn,3), 0)
})

test_that("Nonconvergence warnings are caught", {
  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(starts_with("BFIK_open"))
  expect_warning(compute_reliabilities(bfi),
                 "Reliability CIs could not be computed for BFIK_open")
})
