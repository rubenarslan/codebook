context("Unload cleanly")

knitr::opts_chunk$set(error = FALSE)


setup_tmpdir <- function() {
  test_dir <- tempfile("test_rmdpartials")
  stopifnot(dir.create(test_dir))
  setwd(test_dir)
  test_dir <- getwd() # dumb trick to get a proper path without double slashes

  test_dir
}

codebook_wrap <- function(character) {
  cat(paste0("
---
title: Test
---

test0
```{r}
",character,"
```
"),file = "index.Rmd")
  utils::capture.output(suppressMessages(
    rmarkdown::render("index.Rmd", quiet = TRUE)))
}

test_that("Clean unloading", {
  opts <- options()
  optc <- knitr::opts_chunk$get()
  optk <- knitr::opts_knit$get()

  wd <- getwd()
  files <- list.files(wd)

  test_dir <- tempfile("unloading_codebook")
  dir.create(test_dir)
  setwd(test_dir)
  on.exit({
    setwd(wd)
    unlink(test_dir, recursive = TRUE)
  })

  expect_silent(md <- codebook_wrap("codebook(mtcars, metadata_table = FALSE)"))

  expect_identical(opts, options()[names(opts)])
  expect_identical(optc, knitr::opts_chunk$get())
  optk_new <- knitr::opts_knit$get()
  # this is set by knitr itself even without partials
  optk_new$output.dir <- NULL
  optk$output.dir <- NULL
  expect_identical(optk, optk_new)
  expect_identical(files, list.files(wd))
})
