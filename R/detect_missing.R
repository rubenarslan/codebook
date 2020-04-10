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
