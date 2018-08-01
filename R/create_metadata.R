#' @importFrom haven as_factor
#' @export
haven::as_factor

#' @importFrom labelled var_label
#' @export
labelled::var_label

#' @importFrom labelled var_label<-
#' @export
labelled::`var_label<-`



#' Aggregate variables and remember which variables this were
#'
#' The resulting variables will have the attribute `scale_item_names` containing
#' the basis for aggregation. Its `label` attribute will refer to the common stem of the
#' aggregated variable names (if any), the number of variables, and the
#' aggregation function.
#'
#' @param items data.frame of the items that should be aggregated
#' @param fun aggregation function, defaults to rowMeans with na.rm = FALSE
#' @param stem common stem for the variables, specify if it should not be auto-detected
#' as the longest common stem of the variable names
#' @export
#' @examples
#' testdf <- data.frame(bfi_neuro_1 = rnorm(20), bfi_neuro_2 = rnorm(20),
#'                     bfi_neuro_3R = rnorm(20), age = rpois(20, 30))
#' item_names <- c('bfi_neuro_1', 'bfi_neuro_2', 'bfi_neuro_3R')
#' testdf$bfi_neuro <- aggregate_and_document_scale(testdf[, item_names])
#' testdf$bfi_neuro
aggregate_and_document_scale <- function(items, fun = rowMeans, stem = NULL) {
  new_scale <- fun(items)
  item_names <- names(items)
  attributes(new_scale)$scale_item_names <- item_names

  # find longest common stem
  if (is.null(stem)) {
    max_len <- min(nchar(item_names))
    for (l in max_len:0) {
      stem <- unique(stringr::str_sub(item_names, 1, l))
      if (length(stem) == 1) break
    }
  }
  # string trimming for idiots
  if (nchar(stem)) {
    stem <- stringr::str_match(stem, "^(.+?)_?$")[, 2]
  }

  attributes(new_scale)$label <- paste(ncol(items), stem, "items aggregated by",
                                       deparse(substitute(fun)))
  new_scale
}

