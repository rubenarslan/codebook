context("Test codebook")

test_that("codebook generation", {
  data("bfi", package = 'codebook')
  bfi$age <- 1:nrow(bfi)
  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_silent(md <- codebook(bfi))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 14)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Scale: BFIK_extra")
  expect_match(md, "Missings per variable")
  expect_match(md, "28 completed rows")

  unlink(paste0(dir, "/figure"), recursive = TRUE)
})


test_that("codebook generation without formr", {
  data("bfi", package = 'codebook')
  bfi$age <- 1:nrow(bfi)
  bfi <- zap_attributes(bfi)
  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_silent(md <- codebook(bfi))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 27)
  expect_failure(expect_match(md, "Scale: BFIK_neuro"))
  expect_match(md, "Missings per variable")
  expect_match(md, "28 completed rows")

  unlink(paste0(dir, "/figure"), recursive = TRUE)
})


test_that("Codebook with retest reliabilities can be computed", {
  data("bfi", package = "codebook")
  library(dplyr)
  expect_warning(bfi2 <- bind_rows(bfi, bfi %>%
                         mutate(created = created + lubridate::years(1))))



  bfi$age <- 1:nrow(bfi)
  bfi2 <- rescue_attributes(bfi2, bfi)

  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_silent(md <- codebook(bfi2))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 14)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Scale: BFIK_extra")
  expect_match(md, "Missings per variable")
  expect_match(md, "56 completed rows")

  unlink(paste0(dir, "/figure"), recursive = TRUE)

})


test_that("Codebook with multilevel reliability", {
  data("bfi", package = "codebook")
  library(dplyr)
  bfi$age <- 1:nrow(bfi)
  fuzz <- function(x) { x + rnorm(length(x)) }
  expect_warning(bfi3 <- bind_rows(bfi,
                 bfi %>% mutate(created = created + lubridate::days(1)),
                 bfi %>% mutate(created = created + lubridate::days(2)),
                 bfi %>% mutate(created = created + lubridate::days(3)),
                 bfi %>% mutate(created = created + lubridate::days(4)),
                 bfi %>% mutate(created = created + lubridate::days(5))
  ) %>% mutate_at(vars(matches("_\\d")), fuzz))
  bfi3 <- rescue_attributes(bfi3, bfi)

  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_warning(md <- codebook(bfi3))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 15)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Scale: BFIK_extra")
  expect_match(md, "Missings per variable")
  expect_match(md, "168 completed rows")

  unlink(paste0(dir, "/figure"), recursive = TRUE)
})
