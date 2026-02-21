#' Detect item scales
#'
#' Did you create aggregates of items like this
#' `scale <- scale_1 + scale_2R + scale_3R`?
#' If you run this function on a dataset, it will detect these
#' relationships and set the appropriate attributes. Once they are set,
#' the codebook package can perform reliability computations for you.
#'
#'
#' @param data the data frame
#' @param quiet defaults to false. Suppresses messages about found items.
#'
#' @export
#'
#' @examples
#' bfi <- data.frame(matrix(data = rnorm(300), ncol = 3))
#' names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3")
#' bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
#' bfi <- detect_scales(bfi)
#' bfi$bfi_e
detect_scales <- function(data, quiet = FALSE) {
  item_names <- names(data)
  # fit the pattern
  scale_stubs <- stringr::str_match(item_names, "(?i)^([a-z0-9_]+?)_?[0-9]+R?$")
  scales <- stats::na.omit(unique(scale_stubs[, 2]))
  for (i in seq_along(scales)) {
    scale <- scales[i]
    if (exists(scale, data)) {
      items <- scale_stubs[which(scale_stubs[,2] == scale), 1]
      agg <- rowMeans(data[, items], na.rm = FALSE)
      corr <- stats::cor(agg, data[, scale], use = 'pairwise.complete.obs')
      if (round(corr, 2) < 1) {
        warning(scale, " was not perfectly correlated with the mean of its",
                " items. This can e.g. happen with reverse items. Please check",
                " this, as it likely will mean that the reliability",
                " computations go awry.")
      }
      attributes(data[[ scale ]])$scale_item_names <- items
      attributes(data[[ scale ]])$label <- paste("aggregate of",
                                                 length(items), scale, "items")
      if (!quiet) {
        message(paste(length(items), scale, "items connected to scale"))
      }
    } else {
      warning(scale, " items found, but no aggregate")
    }
  }
  data
}


