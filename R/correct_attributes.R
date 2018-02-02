#' treat values as missing
#'
#' SPSS users frequently label their missing values, but don't set them as missing.
#' This function will do that for negative values and for the values 99 and 999 (only if they're 5*MAD away from the median).
#'
#'
#' @param data the data frame with labelled missings
#' @param only_labelled_missings don't set values to missing if there's no label for them
#' @param missing also set these values to missing (or enforce for 99/999 within 5*MAD)
#' @param non_missing don't set these values to missing
#' @param vars only edit these variables
#' @param use_labelled_spss the labelled_spss class has a few drawbacks. Since R can't store missings like -1 and 99, we're replacing them with letters unless this option is enabled. If you prefer to keep your -1 etc, turn this on.
#'
#' @export
#'
treat_values_as_missing <- function(data, only_labelled_missings = TRUE,
                                    missing = c(), non_missing = c(),
                                    vars = names(data),
                                    use_labelled_spss = FALSE) {
  for (i in seq_along(vars)) {
    var <- vars[i]
    if (is.numeric(data[[ var ]])) {

      potential_missings <- unique(data[[var]][data[[var]] < 0]) # negative values
      if ((stats::median(data[[var]], na.rm = TRUE) +
           stats::mad(data[[var]], na.rm = TRUE) * 5) < 99) {
        potential_missings <- c(potential_missings, 99)
      }
      if ((stats::median(data[[var]], na.rm = TRUE) +
           stats::mad(data[[var]], na.rm = TRUE) * 5) < 999) {
        potential_missings <- c(potential_missings, 999)
      }
      potential_missings <- union(setdiff(potential_missings, non_missing), missing)
      if (haven::is.labelled(data[[var]]) && length(potential_missings) > 0) {
        if (only_labelled_missings) {
          potential_missings <- potential_missings[
            potential_missings %in% attributes(data[[var]])$labels]
          # add labelled missings that don't exist for completeness
          potential_missings <- union(potential_missings,
            setdiff(attributes(data[[var]])$labels, data[[var]]))
        }
        potential_missings <- sort(potential_missings)
        if (!use_labelled_spss) {
          with_tagged_na <- data[[var]]
          labels <- attributes(data[[var]])$labels
          for (i in seq_along(potential_missings)) {
            miss <- potential_missings[i]

            if (!all(potential_missings %in% letters)) new_miss <- letters[i]
            else new_miss <- potential_missings[i]
            with_tagged_na[with_tagged_na == miss] <- haven::tagged_na(new_miss)
            labels[labels == miss] <- haven::tagged_na(new_miss)
          }
          data[[var]] <- haven::labelled(with_tagged_na, labels = labels)
        } else {
          data[[var]] <- haven::labelled_spss(data[[var]],
            attributes(data[[var]])$labels, na_values = potential_missings)
        }
      }
    }
  }
  data
}

#' rescue_attributes
#'
#' You can use this function if some of your items have lost their attributes during wrangling
#' Variables have to have the same name (Duh) and no attributes should be overwritten.
#' But use with care.
#'
#'
#' @param df_no_attributes the data frame with missing attributes
#' @param df_with_attributes the data frame from which you want to restore attributes
#'
#' @export
#'
rescue_attributes <- function(df_no_attributes, df_with_attributes) {
  for (i in seq_along(names(df_no_attributes))) {
    var <- names(df_no_attributes)[i]
    if (var %in% names(df_with_attributes) &&
        is.null(attributes(df_no_attributes[[var]]))) {
      attributes(df_no_attributes[[var]]) <-
        attributes(df_with_attributes[[var]])
    } else {
      for (e in seq_along(names(attributes(df_with_attributes[[var]])))) {
        attrib_name <- names(attributes(df_with_attributes[[var]]))[e]
        if (!attrib_name %in% names(attributes(df_no_attributes[[var]]))) {
          attributes(df_no_attributes[[var]])[[attrib_name]] <-
            attributes(df_with_attributes[[var]])[[attrib_name]]
        }
      }
    }
  }
  df_no_attributes
}


#' detect_scales
#'
#' Did you create aggregates of items like this
#' scale <- scale_1 + scale_2R + scale_3R
#' ?
#' If you run this function on a dataset, it will detect these
#' relationships and set the appropriate attributes. Once they are set,
#' the codebook package can perform reliability computations for you.
#'
#'
#' @param data the data frame
#'
#' @export
#'
#' @examples
#' bfi <- data.frame(matrix(data = rnorm(500), ncol = 5))
#' names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3", "bfi_n1", "bfi_n2")
#' bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
#' bfi <- detect_scales(bfi)
#' bfi$bfi_e
detect_scales <- function(data) {
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
      message(paste(length(items), scale, "items found and connected to scale"))
    } else {
      warning(scale, " items found, but no aggregate")
    }
  }
  data
}
