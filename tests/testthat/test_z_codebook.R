context("Test codebook")

knitr::opts_chunk$set(error = FALSE)

test_that("codebook generation", {
  data("bfi", package = 'codebook')
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                       -starts_with("BFIK_agree"),
                       -starts_with("BFIK_open"))
  bfi$age <- 1:nrow(bfi)
  bfi$abode <- rep("my happy place", times = nrow(bfi))
  bfi$uniq_id <- as.character(1:nrow(bfi))
  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_silent(md <- codebook(bfi, metadata_table = FALSE))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 11)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Scale: BFIK_consc")
  expect_match(md, "Missing values per variable")
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
  expect_silent(md <- codebook(bfi, survey_repetition = "single",
                               missingness_report = FALSE,
                               metadata_table = FALSE))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 7)
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
  expect_equal(length(figs), 6)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Missing values per variable")
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

test_that("Degenerate cases: Variables with only missing data work", {
  onlymiss = data.frame(x = rep(NA_real_, 20),
                        y = rep(1, 20),
                        z = rep(NA_real_, 20))
  attributes(onlymiss$x)$label <- "X"
  onlymiss$x <- haven::labelled(onlymiss$x, labels = c("bla" = 1))
  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_silent(md <- codebook(onlymiss, survey_repetition = "single",
                               missingness_report = FALSE,
                               metadata_json = FALSE))

  unlink(paste0(dir, "/figure"), recursive = TRUE)
})

test_that("Degenerate cases: Odd variables names", {
  onlymiss = data.frame(SysJustEth02.T1 = rnorm(20),
                        `Über` = rnorm(20),
                        `var/name` = rnorm(20),
                        `reallylongvariablenamethatmayirritateknitrmaybe` =
                          rnorm(20))
  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_silent(md <- codebook(onlymiss, survey_repetition = "single",
                               missingness_report = FALSE,
                               metadata_json = FALSE))

  unlink(paste0(dir, "/figure"), recursive = TRUE)
})

test_that("HTML is escaped", {
  data('bfi')
  library(dplyr)
  funnybiz <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_agree"),
                        -starts_with("BFIK_open"),
                        -starts_with("BFIK_consc")) %>%
    rename(`Not <s>struck</s> scale name` = BFIK_neuro,
           `Not <s>struck</s> item name` = BFIK_neuro_4)

  attributes(funnybiz$`Not <s>struck</s> item name`)$label <-
    "Not <s>struck</s> item label"
  names(attributes(funnybiz$BFIK_neuro_3)$labels)[1] <-
  names(attributes(funnybiz$BFIK_neuro_2R)$labels)[1] <-
  names(attributes(funnybiz$`Not <s>struck</s> item name`)$labels)[1] <-
  attributes(funnybiz$BFIK_neuro_3)$item$choices[[1]] <-
  attributes(funnybiz$BFIK_neuro_2R)$item$choices[[1]] <-
  attributes(funnybiz$`Not <s>struck</s> item name`)$item$choices[[1]] <-
    "Not <s>struck</s> value label"
   attributes(funnybiz$`Not <s>struck</s> item name`)$item$showif <-
    "Not <s>struck</s> showif"

  funnybiz$`Not <s>struck</s> item name 2` <-
    funnybiz$`Not <s>struck</s> item name`
  attributes(funnybiz$`Not <s>struck</s> scale name`)$scale_item_names[3] <-
    "Not <s>struck</s> item name"
  attributes(funnybiz$`Not <s>struck</s> scale name`)$label <-
    "Not <s>struck</s> scale label"

  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  saveRDS(funnybiz, "funnybiz.rds")
  expect_message(html <- load_data_and_render_codebook("funnybiz.rds",
"
```{r}
codebook(codebook_data, survey_repetition = 'single',
          reliabilities = list(),
          missingness_report = FALSE,
          metadata_json = FALSE)
```
"))
  html <- paste(readLines(html), collapse = "\n")
  # browseURL("codebook.html")
  # browseURL(dir)
  expect_failure(
    expect_match(html, "Not <s>struck</s> item name", fixed = TRUE))
  expect_failure(
    expect_match(html, "Not <s>struck</s> item label", fixed = TRUE))
  expect_failure(
    expect_match(html, "Not <s>struck</s> value label", fixed = TRUE))
  expect_failure(
    expect_match(html, "Not <s>struck</s> showif", fixed = TRUE))
  expect_failure(
    expect_match(html, "Not <s>struck</s> choice label", fixed = TRUE))
  expect_failure(
    expect_match(html, "Not <s>struck</s> scale name", fixed = TRUE))
  expect_failure(
    expect_match(html, "Not <s>struck</s> scale label", fixed = TRUE))
  expect_failure(
    expect_match(html, "<s>", fixed = TRUE))

  unlink(paste0(dir, "/figure"), recursive = TRUE)
})

test_that("Codebook with multilevel reliability", {
  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                       -starts_with("BFIK_agree"),
                       -starts_with("BFIK_open"),
                       -starts_with("BFIK_consc"))
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
  ) %>% mutate_at(vars(dplyr::matches("_\\d")), fuzz))
  bfi3 <- rescue_attributes(bfi3, bfi)

  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))
  expect_warning(md <- codebook(bfi3, metadata_table = FALSE))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 5)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Missing values per variable")
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
    c("name", "label", "type", "type_options", "data_type",
      "value_labels", "optional", "scale_item_names", "item_order",
      "missing", "complete", "n",  "empty", "n_unique",
       "count", "median", "min", "max", "mean", "sd",
      "p0", "p25", "p50", "p75", "p100", "hist"))
  expect_equal(nrow(cb), ncol(bfi))
  expect_identical(cb$name, names(bfi))
})

test_that("codebook table generation, no attributes", {
  data("bfi", package = 'codebook')
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_agree"),
                        -starts_with("BFIK_open"))
  bfi <- bfi %>% head() %>% haven::zap_label() %>% haven::zap_labels()
  # drops attributes
  bfi$age <- 1:nrow(bfi)
  expect_silent(cb <- codebook_table(bfi))
  expect_identical(names(cb),
                   c("name", "data_type", "missing", "complete", "n", "empty",
                     "n_unique","count", "median", "min", "max", "mean", "sd",
                     "p0", "p25", "p50", "p75", "p100", "hist"))
  expect_equal(nrow(cb), ncol(bfi))
  expect_identical(cb$name, names(bfi))
})
