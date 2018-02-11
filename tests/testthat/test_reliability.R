context("Reliability computation")


test_that("Internal consistencies can be computed", {
  data("bfi", package = "codebook")
  expect_silent(rels <- compute_reliabilities(bfi))
  expect_equal(length(rels), 5)
  expect_equal(length(rels$BFIK_agree), 1)
  expect_identical(names(rels$BFIK_agree), "internal_consistency")
  expect_equal(round(rels$BFIK_consc$internal_consistency$total$std.alpha,3),
               0.787)
})

test_that("Retest reliabilities can be computed", {
  data("bfi", package = "codebook")
  library(dplyr)
  expect_warning(bfi2 <- bind_rows(bfi, bfi %>%
                          mutate(created = created + lubridate::years(1))))
  bfi2 <- rescue_attributes(bfi2, bfi)
  expect_silent(rels <- compute_reliabilities(bfi2,
                                    survey_repetition = "repeated_once"))
  expect_equal(length(rels), 5)
  expect_equal(length(rels$BFIK_agree), 3)
  expect_identical(names(rels$BFIK_agree), c("internal_consistency_T1",
                                             "internal_consistency_T2",
                                             "retest_reliability"))
  expect_equal(round(rels$BFIK_consc$internal_consistency_T1$total$std.alpha,3),
               0.787)
  expect_equal(round(rels$BFIK_consc$internal_consistency_T2$total$std.alpha,3),
               0.787)
  expect_equivalent(round(rels$BFIK_consc$retest_reliability$estimate,3), 1)
})


test_that("Multilevel reliabilities can be computed", {
  data("bfi", package = "codebook")
  library(dplyr)
  fuzz <- function(x) { x + rnorm(length(x)) }
  expect_warning(bfi3 <- bind_rows(bfi,
           bfi %>% mutate(created = created + lubridate::days(1)),
           bfi %>% mutate(created = created + lubridate::days(2)),
           bfi %>% mutate(created = created + lubridate::days(3)),
           bfi %>% mutate(created = created + lubridate::days(4)),
           bfi %>% mutate(created = created + lubridate::days(5))
  ) %>% mutate_at(vars(matches("_\\d")), fuzz))
  bfi3 <- rescue_attributes(bfi3, bfi)
  expect_warning(rels <- compute_reliabilities(bfi3,
                                  survey_repetition = "repeated_many"))
  expect_equal(length(rels), 5)
  expect_equal(length(rels$BFIK_agree), 1)
  expect_identical(names(rels$BFIK_agree), "multilevel_reliability")
  expect_equal(round(rels$BFIK_consc$multilevel_reliability$Rcn,3), 0)
})
