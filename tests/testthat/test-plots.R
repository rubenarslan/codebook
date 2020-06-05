context("Test plots")

test_that("plot labelled", {
  data("bfi", package = 'codebook')
  wd <- getwd()
  dir <- tempdir()
  setwd(dir)
  on.exit(setwd(wd))

  # Likert item (numeric, all labelled)
  expect_silent(p <- plot_labelled(bfi$BFIK_open_2))
  expect_silent(print(p))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_identical(p$scales$scales[[1]]$is_discrete(), FALSE)

  # Likert item (numeric, poles labelled)
  open2 <- bfi$BFIK_open_2
  attributes(open2)$labels <- attributes(open2)$labels[c(1,5)]
  expect_silent(p <- plot_labelled(open2))
  expect_silent(print(p))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_identical(p$scales$scales[[1]]$is_discrete(), FALSE)

  # Likert item (numeric, all labelled) vertical
  expect_silent(p <- plot_labelled(bfi$BFIK_open_2,
                                              go_vertical = TRUE))
  expect_silent(print(p))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_identical(p$scales$scales[[1]]$is_discrete(), FALSE)

  # Numeric, many, no labels
  expect_silent(p <- plot_labelled(rnorm(50)))
  expect_silent(print(p))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_identical(p$scales$scales[[1]]$is_discrete(), FALSE)

  # Numeric, many, labelled
  x <- rnorm(50)
  x <- haven::labelled(x, c("lo" = -2, "hi" = 2))
  expect_silent(p <- plot_labelled(x))
  expect_silent(print(p))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_identical(p$scales$scales[[1]]$is_discrete(), FALSE)

  # numeric, few
  expect_silent(p <- plot_labelled(
    zap_attributes(bfi$BFIK_agree_1R)))
  expect_silent(print(p))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_identical(p$scales$scales[[1]]$is_discrete(), FALSE)

  # Likert type scale, continuous values
  expect_silent(p <- plot_labelled(bfi$BFIK_agree))
  expect_silent(print(p))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_identical(p$scales$scales[[1]]$is_discrete(), FALSE)

  # Characters
  expect_silent(p <- plot_labelled(letters))
  expect_silent(print(p))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_identical(p$scales$scales, list())

  # Characters
  ll <- letters[1:3]
  names(ll) <- paste0("letter: ", ll)
  labelled_letters <- haven::labelled(ll, ll)
  expect_silent(p <- plot_labelled(labelled_letters))
  expect_silent(print(p))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_identical(p$scales$scales, list())

  im <- haven::tagged_na("i")
  mm <- haven::tagged_na("m")
  miss <- haven::labelled(c(rep(im, 10), rep(mm,2), 1:3, 2, 2),
                          c("forgot" = im, "skipped" = mm))
  expect_silent(p <- plot_labelled(miss, "miss"))
  expect_silent(print(p))
  expect_identical(p$scales$scales[[1]]$is_discrete(), FALSE)

  miss2 <- miss[is.na(miss)]
  miss3 <- miss[!is.na(miss)]
  attributes(miss3) <- attributes(miss2) <- attributes(miss)
  expect_silent(p <- plot_labelled(miss2, "miss2"))
  expect_silent(print(p))
  expect_identical(p$scales$scales, list())

  expect_silent(p <- plot_labelled(miss3, "miss3"))
  expect_silent(print(p))
  expect_identical(p$scales$scales[[1]]$is_discrete(), FALSE)

  miss4 <- haven::labelled(c(rep(im, 10), rep(mm,2), 1:3),
                          c("forgot" = im, "skipped" = mm,
                            "middle" = 2))
  expect_silent(p <- plot_labelled(miss4, "miss4"))
  expect_silent(print(p))
  expect_identical(p$scales$scales[[1]]$is_discrete(), FALSE)

  miss5 <- haven::labelled(c(rep(im, 10), rep(mm,2), letters[1:3]),
                           c("forgot" = im, "skipped" = mm,
                             "middle" = "b"))
  expect_silent(p <- plot_labelled(miss5))
  expect_silent(print(p))
  expect_identical(p$scales$scales, list())

  unlink(paste0(dir, "/Rplots.pdf"), recursive = TRUE)
})

test_that("likert from items", {
  data("bfi", package = 'codebook')
  library(dplyr)
  items <- bfi %>% select(starts_with("BFIK_consc_"))
  expect_silent(lik <- likert_from_items(items))
  expect_identical(class(lik), c("likert"))
  expect_identical(dim(lik$items), dim(items))
  expect_silent(graphics::plot(lik))

  items <- bfi %>% select(starts_with("BFIK_consc_")) %>% zap_attributes()
  expect_silent(lik <- likert_from_items(items))
  expect_identical(class(lik), c("likert"))
  expect_identical(dim(lik$items), dim(items))
})

