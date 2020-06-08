context("Test codebook")

knitr::opts_chunk$set(error = FALSE)

setup_tmpdir <- function() {
  test_dir <- tempfile("test_codebook")
  stopifnot(dir.create(test_dir))
  setwd(test_dir)
  test_dir <- getwd() # dumb trick to get a proper path without double slashes

  test_dir
}

codebook_wrap <- function(character) {
  knitr::knit(text = paste0("
test0
```{r}
",character,"
```
"), quiet = TRUE, envir = parent.frame())
}

test_that("codebook generation", {
  dir <- setup_tmpdir()
  on.exit({
    unlink(dir, recursive = TRUE)
  })

  data("bfi", package = 'codebook')
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                       -starts_with("BFIK_agree"),
                       -starts_with("BFIK_open"))
  bfi$age <- 1:nrow(bfi)
  bfi$abode <- rep("my happy place", times = nrow(bfi))
  bfi$uniq_id <- as.character(1:nrow(bfi))
  expect_silent(md <- codebook_wrap("codebook(bfi, metadata_table = FALSE)"))
  figs <- list.files(paste0(dir, "/figure/"))
  expect_equal(length(figs), 11)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Scale: BFIK_consc")
  expect_match(md, "Missing values per variable")
  expect_match(md, "28 completed rows")
  expect_match(md, "application/ld\\+json")
})


test_that("codebook generation without formr", {
  dir <- setup_tmpdir()
  on.exit({
    unlink(dir, recursive = TRUE)
  })
  data("bfi", package = 'codebook')
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                       -starts_with("BFIK_agree"),
                       -starts_with("BFIK_open"),
                       -starts_with("BFIK_neuro"),
                       -created,
                       -modified,
                       -ended,
                       -expired)
  bfi$age <- 1:nrow(bfi)
  expect_silent(md <- codebook_wrap('codebook(bfi, survey_repetition = "single",
                               missingness_report = FALSE,
                               metadata_table = FALSE)'))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 4)
  expect_match(md, "Scale: BFIK_consc")
  expect_match(md, "application/ld\\+json")
})


test_that("Codebook with retest reliabilities can be computed", {
  dir <- setup_tmpdir()
  on.exit({
    unlink(dir, recursive = TRUE)
  })
  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                       -starts_with("BFIK_agree"),
                       -starts_with("BFIK_open"),
                       -starts_with("BFIK_consc"))
  expect_silent(bfi2 <- bind_rows(bfi, bfi %>%
                         mutate(created = created + 1e7,
                                ended = ended + 1e7)))


  bfi2$age <- 1:nrow(bfi2)
  bfi2 <- rescue_attributes(bfi2, bfi)

  expect_silent(md <- codebook_wrap('codebook(bfi2,
                               missingness_report = FALSE,
                               metadata_table = FALSE)'))
  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 8)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "56 completed rows")
})



test_that("Dupes are noticed", {
  dir <- setup_tmpdir()
  on.exit({
    unlink(dir, recursive = TRUE)
  })
  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_agree"),
                        -starts_with("BFIK_open"),
                        -starts_with("BFIK_consc"))
  expect_silent(bfi2 <- bind_rows(bfi, bfi %>% filter(row_number() < 5)))

  bfi$age <- 1:nrow(bfi)
  expect_error(md <- codebook_wrap('codebook(bfi2,
                               metadata_table = FALSE)'),
               "duplicated rows")
})

test_that("Degenerate cases: Variables with only missing data work", {
  dir <- setup_tmpdir()
  on.exit({
    unlink(dir, recursive = TRUE)
  })
  onlymiss = data.frame(x = rep(NA_real_, 20),
                        y = rep(1, 20),
                        z = rep(NA_real_, 20))
  attributes(onlymiss$x)$label <- "X"
  onlymiss$x <- haven::labelled(onlymiss$x, labels = c("bla" = 1))

  expect_silent(md <- codebook_wrap("codebook(onlymiss,
                               survey_repetition = 'single',
                               missingness_report = FALSE,
                               metadata_json = FALSE)"))
  expect_match(md, "non-missing")
})

test_that("Degenerate cases: Odd variables names", {
  dir <- setup_tmpdir()
  on.exit({
    unlink(dir, recursive = TRUE)
  })
  onlymiss = data.frame(SysJustEth02.T1 = rnorm(20),
                        `Über` = rnorm(20),
                        `var/name` = rnorm(20),
                        `reallylongvariablenamethatmayirritateknitrmaybe` =
                          rnorm(20))

  expect_silent(md <- codebook_wrap("codebook(onlymiss,
                               survey_repetition = 'single',
                               missingness_report = FALSE,
                               metadata_json = FALSE)"))
})

test_that("HTML is escaped", {
  dir <- setup_tmpdir()
  on.exit({
    unlink(dir, recursive = TRUE)
  })
  data("bfi", package = "codebook")
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

  saveRDS(funnybiz, "funnybiz.rds")
  expect_message(html <- load_data_and_render_codebook("funnybiz.rds",
"---
title: Codebook
---

```{r}
codebook(codebook_data, survey_repetition = 'single',
          reliabilities = list(),
          missingness_report = FALSE,
          metadata_json = FALSE)
```
"))
  html <- paste(readLines(html), collapse = "\n")
  browseURL("codebook.html")
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
})

test_that("Codebook with multilevel reliability", {
  dir <- setup_tmpdir()
  on.exit({
    unlink(dir, recursive = TRUE)
  })
  data("bfi", package = "codebook")
  library(dplyr)
  bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                       -starts_with("BFIK_agree"),
                       -starts_with("BFIK_open"),
                       -starts_with("BFIK_consc"))
  fuzz <- function(x) { x + rnorm(length(x)) }
  expect_silent(bfi3 <- bind_rows(bfi,
                 bfi %>% mutate(created = created + 86400 * 1,
                                ended = ended + 86400 * 1),
                 bfi %>% mutate(created = created + 86400 * 2,
                                ended = ended + 86400 * 2),
                 bfi %>% mutate(created = created + 86400 * 3,
                                ended = ended + 86400 * 3),
                 bfi %>% mutate(created = created + 86400 * 4,
                                ended = ended + 86400 * 4),
                 bfi %>% mutate(created = created + 86400 * 5,
                                ended = ended + 86400 * 5)
  ) %>% mutate_at(vars(dplyr::matches("_\\d")), fuzz))
  bfi3 <- rescue_attributes(bfi3, bfi)

  expect_silent(md <- codebook_wrap("codebook(bfi3,
                               metadata_table = FALSE)"))

  figs <- list.files(paste0(dir, "/figure"))
  expect_equal(length(figs), 4)
  expect_match(md, "Scale: BFIK_neuro")
  expect_match(md, "Missing values per variable")
  expect_match(md, "168 completed rows")
})

