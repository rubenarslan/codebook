context("Test codebook")

test_that("plot labelled", {
  data("bfi", package = 'codebook')
  expect_silent(p <- plot_labelled(bfi$BFIK_open_2))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_silent(p <- plot_labelled(bfi$BFIK_open_2,
                                              go_vertical = TRUE))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_silent(p <- plot_labelled(rnorm(100)))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_silent(p <- plot_labelled(
    zap_attributes(bfi$BFIK_agree_1R)))
  expect_identical(class(p), c("gg", "ggplot"))
  expect_silent(p <- plot_labelled(bfi$BFIK_agree))
  expect_identical(class(p), c("gg", "ggplot"))

  im <- haven::tagged_na("i")
  mm <- haven::tagged_na("m")
  miss <- haven::labelled(c(rep(im, 10), rep(mm,2), 1:3),
                          c("forgot" = im, "skipped" = mm))
  expect_silent(p <- plot_labelled(miss, "miss"))
  miss2 <- miss[is.na(miss)]
  miss3 <- miss[!is.na(miss)]
  attributes(miss3) <- attributes(miss2) <- attributes(miss)
  expect_silent(p <- plot_labelled(miss2, "miss2"))
  expect_silent(p <- plot_labelled(miss3, "miss3"))
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

