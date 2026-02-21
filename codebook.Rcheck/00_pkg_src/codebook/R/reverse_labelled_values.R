#' Reverse labelled values
#' reverse the underlying values for a numeric [haven::labelled()] vector while keeping the labels correct
#'
#' @param x a labelled vector
#' @return return the labelled vector with the underlying values having been reversed
#' @export
#' @examples
#' x <- haven::labelled(rep(1:3, each = 3), c(Bad = 1, Good = 5))
#' x
#' reverse_labelled_values(x)
reverse_labelled_values <- function(x) {
  labels <- attributes(x)$labels
  values <- unname(labels)
  labels <- names(labels)
  if (is.factor(x) && is.null(labels) && !is.null(attributes(x)$levels)) {
    warning("Turning a factor into a labelled numeric vector")
    values <- seq_len(length(attributes(x)$levels))
    labels <- attributes(x)$levels
    x <- as.numeric(x)
  }
  missing_labels <- labels[is.na(values)]
  missing_values <- values[is.na(values)]
  labels <- labels[!is.na(values)]
  values <- values[!is.na(values)]
  if (
    length(values) == 0 ||
    (any(x > max(values) |
          x < min(values), na.rm = TRUE))) {
    warning(deparse(substitute(x)), ": There are values outside the ",
            "labelled range. Reversion will only work if both the minimum ",
            "and maximum of the range are part of the responses.")
  }
  if (length(values) < length(unique(x)) ) {
    # if only some values have labels (e.g. extremes), make sure we include all
    possible_replies <- union(values, unique(x[!is.na(x)]))
  } else {
    possible_replies <- values
  }
  if (!is.numeric(possible_replies)) {
    warning(deparse(substitute(x)), " is not numeric and cannot be reversed.")
    x
  } else {
    range <- min(possible_replies):max(possible_replies)
    if (length(possible_replies) <
        length(range)) {
      possible_replies <- range
    }

    possible_replies <- sort(possible_replies)
    recode_replies <- stats::setNames(
      as.list(possible_replies), rev(possible_replies))
    new_x <- dplyr::recode(as.numeric(x), !!!recode_replies)

    attributes(new_x) <- attributes(x)
    attributes(new_x)$labels <- stats::setNames(
      c(rev(values), missing_values),
      c(labels, missing_labels))
    new_x
  }
}

