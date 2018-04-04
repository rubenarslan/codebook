context("Test codebook")

test_that("codebook generation", {
  data("bfi", package = 'codebook')
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                       -starts_with("BFIK_agree"),
                       -starts_with("BFIK_open"))
  bfi$age <- 1:nrow(bfi)
  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_silent(md <- codebook(bfi))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 8)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Scale: BFIK_consc")
  expect_match(md, "Missings per variable")
  expect_match(md, "28 completed rows")
  expect_match(md, "application/ld\\+json")

  unlink(paste0(dir, "/figure"), recursive = TRUE)
})


test_that("codebook generation without formr", {
  data("bfi", package = 'codebook')
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                       -starts_with("BFIK_agree"),
                       -starts_with("BFIK_open"),
                       -created,
                       -modified,
                       -ended,
                       -expired)
  bfi$age <- 1:nrow(bfi)
  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_warning(md <- codebook(bfi),
                "automatic survey repetition detection to work")
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 6)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Scale: BFIK_consc")
  expect_match(md, "application/ld\\+json")

  unlink(paste0(dir, "/figure"), recursive = TRUE)
})


test_that("Codebook with retest reliabilities can be computed", {
  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                       -starts_with("BFIK_agree"),
                       -starts_with("BFIK_open"),
                       -starts_with("BFIK_consc"))
  expect_warning(bfi2 <- bind_rows(bfi, bfi %>%
                         mutate(created = created + lubridate::years(1),
                                ended = ended + lubridate::years(1))))



  bfi$age <- 1:nrow(bfi)
  bfi2 <- rescue_attributes(bfi2, bfi)

  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_silent(md <- codebook(bfi2, metadata_table = FALSE))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 5)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Missings per variable")
  expect_match(md, "56 completed rows")

  unlink(paste0(dir, "/figure"), recursive = TRUE)

})



test_that("Dupes are noticed", {
  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_agree"),
                        -starts_with("BFIK_open"),
                        -starts_with("BFIK_consc"))
  expect_warning(bfi2 <- bind_rows(bfi, bfi %>% filter(row_number() < 5)))



  bfi$age <- 1:nrow(bfi)
  expect_error(md <- codebook(bfi2, metadata_table = FALSE), "duplicated rows")
})

test_that("Variables with only missings work", {
  onlymiss = data.frame(x = rep(NA, 20), y = rep(1, 20))
  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_warning(md <- codebook(onlymiss),
                 "automatic survey repetition detection to work")

  unlink(paste0(dir, "/figure"), recursive = TRUE)
})

test_that("Codebook with multilevel reliability", {
  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                       -starts_with("BFIK_agree"),
                       -starts_with("BFIK_open"),
                       -starts_with("BFIK_consc"))
  bfi$age <- 1:nrow(bfi)
  fuzz <- function(x) { x + rnorm(length(x)) }
  expect_warning(bfi3 <- bind_rows(bfi,
                 bfi %>% mutate(created = created + lubridate::days(1),
                                ended = ended + lubridate::days(1)),
                 bfi %>% mutate(created = created + lubridate::days(2),
                                ended = ended + lubridate::days(2)),
                 bfi %>% mutate(created = created + lubridate::days(3),
                                ended = ended + lubridate::days(3)),
                 bfi %>% mutate(created = created + lubridate::days(4),
                                ended = ended + lubridate::days(4)),
                 bfi %>% mutate(created = created + lubridate::days(5),
                                ended = ended + lubridate::days(5))
  ) %>% mutate_at(vars(matches("_\\d")), fuzz))
  bfi3 <- rescue_attributes(bfi3, bfi)

  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_warning(md <- codebook(bfi3, metadata_table = FALSE))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 6)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Missings per variable")
  expect_match(md, "168 completed rows")

  unlink(paste0(dir, "/figure"), recursive = TRUE)
})



test_that("codebook generation via helper fun", {
  data("bfi", package = 'codebook')
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_agree"),
                        -starts_with("BFIK_open"))
  bfi$age <- 1:nrow(bfi)
  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  saveRDS(bfi, "bfi.rds")
  expect_message(md <- load_data_and_render_codebook("bfi.rds",
                                                     "
```{r}
codebook(codebook_data)
```
"))
})


test_that("codebook table generation", {
  data("bfi", package = 'codebook')
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_agree"),
                        -starts_with("BFIK_open"))
  bfi$age <- 1:nrow(bfi)
  expect_silent(cb <- codebook_table(bfi))
  expect_identical(names(cb),
    c("name", "label", "type", "type_options", "data_type", "ordered",
      "value_labels", "optional", "scale_item_names", "item_order",
      "missing", "complete", "n",  "empty", "n_unique",
      "top_counts", "count", "median", "min", "max", "mean", "sd",
      "p0", "p25", "p50", "p75", "p100", "hist"))
  expect_equal(nrow(cb), ncol(bfi))
  expect_identical(cb$name, names(bfi))
})

test_that("codebook table generation", {
  data("bfi", package = 'codebook')
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_agree"),
                        -starts_with("BFIK_open"))
  bfi <- head(bfi) # drops attributes
  bfi$age <- 1:nrow(bfi)
  expect_silent(cb <- codebook_table(bfi))
  expect_identical(names(cb),
                   c("name", "data_type", "ordered", "value_labels",
                     "missing", "complete", "n",  "empty", "n_unique",
                     "top_counts", "count", "median", "min", "max", "mean", "sd",
                     "p0", "p25", "p50", "p75", "p100", "hist"))
  expect_equal(nrow(cb), ncol(bfi))
  expect_identical(cb$name, names(bfi))
})
