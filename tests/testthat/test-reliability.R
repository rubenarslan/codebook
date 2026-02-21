context("Reliability computation")


test_that("Internal consistencies can be computed with psych", {
  skip_if_not_installed("psych")

  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_neuro"),
                        -starts_with("BFIK_open"))

  rels <- compute_reliabilities(bfi, use_psych = TRUE)
  expect_equal(length(rels), 2)
  expect_equal(length(rels$BFIK_agree), 1)
  expect_identical(names(rels$BFIK_agree), "internal_consistency")
  agree_output <- rels$BFIK_agree$internal_consistency
  open_output <- rels$BFIK_open$internal_consistency
  expect_equal(round(
    agree_output$total$raw_alpha,3),
    0.801)
  expect_equal(codebook:::pull_reliability(rels$BFIK_agree),
               "Cronbach's α [95% CI] = 0.8 [0.68;0.92]")
})

test_that("Retest reliabilities can be computed", {
  skip_if_not_installed("psych")
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

  agree_output <- rels$BFIK_agree$internal_consistency_T1
  expect_equal(round(agree_output$total$raw_alpha, 3), 0.801)
  agree_output <- rels$BFIK_agree$retest_reliability
  expect_equivalent(round(agree_output$estimate,3), 1)
})


test_that("Multilevel reliabilities can be computed", {
  skip_if_not_installed("psych")
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

