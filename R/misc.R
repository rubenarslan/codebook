# taken from brms https://github.com/paul-buerkner/brms/blob/master/R/misc.R

stop2 <- function(...) {
  stop(..., call. = FALSE)
}

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop2("Please install the '", package, "' package.")
  }
  invisible(TRUE)
}


#' Missing data patterns
#'
#' Generate missingness patterns using [mice::md.pattern()],
#' with options to reduce the complexity of the output.
#'
#' @param data the dataset
#' @param only_vars_with_missings defaults to TRUE, omitting variables that have no missings
#' @param min_freq minimum number of rows to have this missingness pattern
#' @export
#' @examples
#' data("nhanes", package = "mice")
#' md_pattern(nhanes)
#' md_pattern(nhanes, only_vars_with_missings = FALSE, min_freq = 0.2)
md_pattern <- function(data, only_vars_with_missings = TRUE, min_freq = 0.01) {
  if (sum(is.na(data)) == 0) {
    message("No missings.")
  } else {
    for (i in seq_along(names(data))) {
      # mice::md.pattern coerces character/factor to NA
      data[[i]] <- as.numeric(as.factor(data[[i]]))
    }
    md_pattern <- mice::md.pattern(data, plot = FALSE)
    n_miss <- rownames(md_pattern)
    if (is.null(n_miss)) {
      n_miss <- rep(0, nrow(md_pattern))
    }
    colnames(md_pattern)[ncol(md_pattern)] <- "var_miss"
    rownames(md_pattern) <- NULL
    if (only_vars_with_missings) {
      missing_by_var <- md_pattern[nrow(md_pattern), ]
      md_pattern <- md_pattern[, missing_by_var > 0]
    }
    md_pattern <- tibble::as.tibble(md_pattern)
    stopifnot(!exists("n_miss", md_pattern))
    md_pattern$n_miss <- as.numeric(n_miss)
    md_pattern$n_miss[nrow(md_pattern)] <-
      md_pattern$var_miss[nrow(md_pattern)]
    stopifnot(!exists("description", md_pattern))
    md_pattern$description <- paste0("Missings in ", md_pattern$var_miss,
                                     " variables")
    md_pattern$description[nrow(md_pattern)] <- "Missings per variable"
    md_pattern <- md_pattern[, c(ncol(md_pattern), 1:(ncol(md_pattern) - 1))]

    other <- md_pattern[ md_pattern$n_miss / nrow(data) < min_freq, -1]
    other_sums <- dplyr::summarise_all(other, dplyr::funs(sum))
    md_pattern <- md_pattern[ md_pattern$n_miss / nrow(data) >= min_freq, ]
    md_pattern <- md_pattern[order(md_pattern$n_miss, decreasing = TRUE), ]
    if (other_sums$n_miss > 0) {
      other_sums$description <- paste0(nrow(other),
                                       " other, less frequent patterns")
      md_pattern <- dplyr::bind_rows(md_pattern, other_sums)
    }
    md_pattern
  }
}



#' How many surveys were ended?
#'
#' Just a simple to check how many times a survey (e.g. diary)
#' was finished. It defaults to checking the "ended" variable for this.
#'
#' @param survey which survey are you asking about?
#' @param variable which variable should be filled out, defaults to "ended"
#' @export
#' @examples
#' survey <- data.frame(ended = c("2016-05-28 10:11:00", NA, "2016-05-30 11:18:28"))
#' ended(survey = survey)
ended <- function(survey, variable = "ended") {
  if (length(survey) > 0) {
    if (length(survey[, variable]) > 0) {
      sum(!is.na(survey[, variable]))
    } else {
      0
    }
  } else {
    0
  }
}

#' How many surveys were expired?
#'
#' Just a simple to check how many times a survey (e.g. diary)
#' has expired (i.e. user missed it). It defaults to checking the "expired" variable for this.
#'
#' @param survey which survey are you asking about?
#' @param variable which variable should be filled out, defaults to "expired"
#' @export
#' @examples
#' survey <- data.frame(expired = c(NA, "2016-05-29 10:11:00", NA))
#' expired(survey = survey)
expired <- function(survey, variable = "expired") {
  ended(survey, variable)
}

#' How many surveys were modified?
#'
#' Just a simple to check how many times a survey (e.g. diary)
#' has expired (i.e. user missed it). It defaults to checking the "expired" variable for this.
#'
#' @param survey which survey are you asking about?
#' @param variable which variable should be filled out, defaults to "modified"
#' @export
#' @examples
#' survey <- data.frame(modified = c(NA, "2016-05-29 10:11:00", NA))
#' modified(survey = survey)
modified <- function(survey, variable = "modified") {
  ended(survey, variable)
}


export_table <- function(df) {
  DT::datatable(df, filter = "top", extensions = 'Buttons',
                escape = FALSE,
                rownames = FALSE,
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength = 200
                ))
}




#' Summary function for labelled vector
#'
#'
#' @param object a labelled vector
#' @param ... passed to summary.factor
#'
#' @export
#' @examples
#' example("labelled", "haven")
#' summary(x)
#'
summary.labelled <- function(object, ...) {
  summary(haven::as_factor(object, levels = "both"), ...)
}

#' Summary function for labelled_spss vector
#'
#'
#' @param object a labelled_spss vector
#' @param ... passed to summary.factor
#'
#' @export
#' @examples
#' example("labelled", "haven")
#' summary(x)
#'
summary.labelled_spss <- function(object, ...) {
  summary(haven::as_factor(object, levels = "both"), ...)
}



#' Has label
#'
#'
#' @param x a vector
#'
#' @export
#' @examples
#' example("labelled", "haven")
#' has_label(x)
has_label <- function(x) {
  haven::is.labelled(x) ||
    !is.null(attr(x, 'label')) ||
    !is.null(attr(x, 'labels'))
}


#' Has labels
#'
#'
#' @param x a vector
#'
#' @export
#' @examples
#' example("labelled", "haven")
#' has_labels(x)
has_labels <- function(x) {
  haven::is.labelled(x) ||
    !is.null(attr(x, 'labels'))
}

#' @export
as_factor.default <- function(x,
          levels = c("default", "labels", "values", "both"),
          ordered = FALSE, ...) {
  class(x) <- c("labelled", class(x))
  haven::as_factor(x, levels, ordered, ...)
}
