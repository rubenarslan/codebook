## ----message = FALSE----------------------------------------------------------
knit_by_pkgdown <- !is.null(knitr::opts_chunk$get("fig.retina"))
knitr::opts_chunk$set(
  warning = TRUE, # show warnings during codebook generation
  message = TRUE, # show messages during codebook generation
  error = TRUE, # do not interrupt codebook generation in case of errors,
                # TRUE is usually better for debugging
  echo = TRUE  # show R code
)
ggplot2::theme_set(ggplot2::theme_bw())

## ----include=FALSE, echo=FALSE------------------------------------------------
knitr::opts_chunk$set(
  error = FALSE
)

## -----------------------------------------------------------------------------
library(codebook)
codebook_data <- codebook::bfi

## -----------------------------------------------------------------------------
codebook_data <- rio::import("https://osf.io/s87kd/download", "csv")

## -----------------------------------------------------------------------------
attributes(codebook_data$C5)$label <- "Waste my time."

## -----------------------------------------------------------------------------
library(labelled)

## -----------------------------------------------------------------------------

var_label(codebook_data$C5) <- "Waste my time."

## -----------------------------------------------------------------------------
val_labels(codebook_data$C1) <- c("Very Inaccurate" = 1, "Very Accurate" = 6)

