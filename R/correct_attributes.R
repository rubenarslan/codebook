#' Detect missing values
#'
#' SPSS users frequently label their missing values, but don't set them as missing.
#' This function will rectify that for negative values and for the values 99 and 999 (only if they're 5*MAD away from the median).
#' Using different settings, you can also easily tag other missing values.
#'
#' @param data the data frame with labelled missing values
#' @param only_labelled don't set values to missing if there's no label for them
#' @param negative_values_are_missing by default we label negative values as missing
#' @param ninety_nine_problems SPSS users often store values as 99/999, should we do this for values with 5*MAD of the median
#' @param learn_from_labels if there are labels for missing values of the form `[-1] no answer`, set -1 in the data to the corresponding tagged missing
#' @param missing also set these values to missing (or enforce for 99/999 within 5*MAD)
#' @param non_missing don't set these values to missing
#' @param vars only edit these variables
#' @param use_labelled_spss the labelled_spss class has a few drawbacks. Since R can't store missing values like -1 and 99, we're replacing them with letters unless this option is enabled. If you prefer to keep your -1 etc, turn this on.
#'
#' @export
#'
detect_missing <- function(data, only_labelled = TRUE,
                                    negative_values_are_missing = TRUE,
                                    ninety_nine_problems = TRUE,
                                    learn_from_labels = TRUE,
                                    missing = c(), non_missing = c(),
                                    vars = names(data),
                                    use_labelled_spss = FALSE) {
  for (i in seq_along(vars)) {
    var <- vars[i]
    if (is.numeric(data[[ var ]]) && any(!is.na(data[[ var ]]))) {

      # negative values
      potential_missing_values <- c()
      if (negative_values_are_missing) {
        potential_missing_values <- unique(data[[var]][data[[var]] < 0])
      }
      labels <- attributes(data[[var]])$labels

      if (learn_from_labels && length(labels)) {
        numeric_representations <- as.numeric(
          stringr::str_match(names(labels), "\\[([0-9-]+)\\]")[, 2])
        potentially_untagged <- numeric_representations[is.na(labels)]
        potential_tags <- labels[is.na(labels)]
        if (is.double(data[[var]]) && !all(is.na(haven::na_tag(data[[var]]))) &&
            length(intersect(potentially_untagged, data[[var]]))) {
          warning("Missing values were already tagged in ", var, ". Although",
                  "there were further potential missing values as indicated by",
                  "missing labels, this was not changed.")
        } else {
          for (e in seq_along(potentially_untagged)) {
            pot <- potentially_untagged[e]
            data[[var]][data[[var]] == pot] <- potential_tags[e]
          }
        }
      }
      # classic SPSS missing values only if they are far out of real data range
      # can be turned off using non_missing or ninety_nine_problems
      if (ninety_nine_problems) {
        if (any(!is.na(data[[ var ]])) &&
            (stats::median(data[[var]], na.rm = TRUE) +
             stats::mad(data[[var]], na.rm = TRUE) * 5) < 99) {
          potential_missing_values <- c(potential_missing_values, 99)
        }
        if (any(!is.na(data[[ var ]])) &&
            (stats::median(data[[var]], na.rm = TRUE) +
             stats::mad(data[[var]], na.rm = TRUE) * 5) < 999) {
          potential_missing_values <- c(potential_missing_values, 999)
        }
      }
      potential_missing_values <- union(
        setdiff(potential_missing_values, non_missing),
        missing)
      if ((!only_labelled || haven::is.labelled(data[[var]])) &&
          length(potential_missing_values) > 0) {
        if (only_labelled) {
          potential_missing_values <- potential_missing_values[
            potential_missing_values %in% labels]
          # add labelled missing_values that don't exist for completeness
          potential_missing_values <- union(potential_missing_values,
            setdiff(labels[is.na(labels)], data[[var]]))
        }
        potential_missing_values <- sort(potential_missing_values)
        with_tagged_na <- data[[var]]
        if (is.double(data[[var]])) {
          free_na_tags <- setdiff( letters, haven::na_tag(with_tagged_na))
        } else {
          free_na_tags <- letters
        }

        for (i in seq_along(potential_missing_values)) {
          miss <- potential_missing_values[i]

          if (!use_labelled_spss &&
              !all(potential_missing_values %in% free_na_tags)) {
            new_miss <- free_na_tags[i]
          } else {
            new_miss <- potential_missing_values[i]
          }
          that_label <- which(labels == miss)
          if (length(which(with_tagged_na == miss)) &&
              is.double(data[[var]]) && !use_labelled_spss) {
              with_tagged_na[
                which(with_tagged_na == miss)] <- haven::tagged_na(new_miss)
          } else if (is.integer(data[[var]])) {
            warning("Cannot label missings for integers in variable ", var)
          }
          if (is.double(data[[var]]) && length(that_label) &&
              !use_labelled_spss) {
            labels[that_label] <- haven::tagged_na(new_miss)
            names(labels)[that_label] <- paste0("[",
                                  potential_missing_values[i],
                                      "] ", names(labels)[that_label])
          }
        }
        if (use_labelled_spss) {
          labels <- attributes(data[[var]])$labels
          if (is.null(labels)) {
            labels <- potential_missing_values
            names(labels) <- "autodetected unlabelled missing"
          }
          data[[var]] <- haven::labelled_spss(data[[var]],
                                 label = attr(data[[var]], "label", TRUE),
                                 labels = labels,
                                 na_values = potential_missing_values,
                                 na_range = attr(data[[var]], "na_range", TRUE)
                                 )
        } else if (haven::is.labelled(data[[var]])) {
            data[[var]] <- haven::labelled(with_tagged_na,
                                  label = attr(data[[var]], "label", TRUE),
                                  labels = labels
                                  )
        } else {
            data[[var]] <- with_tagged_na
        }
      }
    }
  }
  data
}
#' @describeIn detect_missing Deprecated version
#' @param only_labelled_missings passed to [detect_missing()]
#' @param ... passed to [detect_missing()]
#' @inheritParams detect_missing
#' @export
detect_missings <- function(data, only_labelled_missings = TRUE,
                           ...) {
  .Deprecated("detect_missing", package = "codebook", msg =
                "We renamed this function so as not to incorrectly pluralise
the word 'missing'.")
  detect_missing(data, only_labelled = only_labelled_missings, ...)
}

#' Rescue lost attributes
#'
#' You can use this function if some of your items have lost their attributes during wrangling
#' Variables have to have the same name (Duh) and no attributes should be overwritten.
#' But use with care. Similar to [labelled::copy_labels()].
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
#' bfi <- data.frame(matrix(data = rnorm(500), ncol = 5))
#' names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3", "bfi_n1", "bfi_n2")
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


#' Zap attributes
#'
#' Modelled on [haven::zap_labels()], but more encompassing. By default removes
#' the following attributes:
#' format.spss, format.sas, format.stata, label, labels, na_values, na_range,
#' display_width
#'
#'
#' @param x the data frame or variable
#' @param attributes character vector of attributes to zap. NULL if everything
#' (including factor levels etc) should be zapped
#'
#' @export
#'
#' @examples
#' bfi <- data.frame(matrix(data = rnorm(300), ncol = 3))
#' names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3")
#' attributes(bfi$bfi_e1)$label <- "I am outgoing."
#' attributes(bfi$bfi_e2R)$label <- "I prefer books to people."
#' attributes(bfi$bfi_e3)$label <- "I love to party."
#' bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
#' bfi <- detect_scales(bfi, quiet = TRUE) # create attributes
#' str(zap_attributes(bfi, "label"))
#' zap_attributes(bfi$bfi_e)
zap_attributes <- function(x,
       attributes = c("format.spss", "format.sas", "format.stata",
                      "label", "labels", "na_values", "na_range",
                      "display_width")) {
  stopifnot(xor(is.null(attributes), is.character(attributes)))
  UseMethod("zap_attributes")
}

#' @export
zap_attributes.default <- function(x, attributes = NULL) {
  if (is.null(attributes)) {
    attributes(x) <- NULL
  } else {
    for (i in seq_along(attributes)) {
      attributes(x)[ attributes[i] ] <- NULL
    }
  }
  x
}

#' @export
zap_attributes.data.frame <- function(x, attributes = NULL) {
  x[] <- lapply(x, zap_attributes, attributes)
  x
}



#' Zap labelled class
#'
#' Modelled on [haven::zap_labels()], zaps labelled class (not other attributes).
#'
#' @param x the data frame or variable
#' @export
zap_labelled <- function(x) {
  UseMethod("zap_labelled")
}

#' @export
zap_labelled.data.frame <- function(x) {
  x[] <- lapply(x, zap_labelled)
  x
}

#' @export
zap_labelled.haven_labelled <- function(x) {
  if (inherits(x, "haven_labelled")) {
    unclass(x)
  } else {
    x
  }
}

#' @export
zap_labelled.haven_labelled_spss <- function(x) {
  if (inherits(x, "haven_labelled_spss")) {
    unclass(x)
  } else {
    x
  }
}

#' @export
zap_labelled.default <- function(x) {
  x
}

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

