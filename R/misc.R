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

# taken from mice to reduce dependencies
# https://github.com/stefvanbuuren/mice
md.pattern <- function(x, plot = FALSE)
{
  if (plot != FALSE) {
    stop("Please use the original mice::md.pattern function for plots.")
  }
  if (!(is.matrix(x) || is.data.frame(x)))
    stop("Data should be a matrix or dataframe")
  if (ncol(x) < 2)
    stop("Data should have at least two columns")
  R <- is.na(x)
  nmis <- colSums(R)
  R <- matrix(R[, order(nmis)], dim(x))
  pat <- apply(R, 1, function(x) paste(as.numeric(x), collapse = ""))
  sortR <- matrix(R[order(pat), ], dim(x))
  if (nrow(x) == 1) {
    mpat <- is.na(x)
  }
  else {
    mpat <- sortR[!duplicated(sortR), ]
  }
  if (all(!is.na(x))) {
    message(" /\\     /\\\n{  `---'  }\n{  O   O  }\n==>  V <==")
    message("  No need for mice. This data set is completely observed.\n")
    message(" \\  \\|/  /\n  `-----'\n\n")
    mpat <- t(as.matrix(mpat, byrow = TRUE))
    rownames(mpat) <- table(pat)
  }
  else {
    if (is.null(dim(mpat))) {
      mpat <- t(as.matrix(mpat))
    }
    rownames(mpat) <- table(pat)
  }
  r <- cbind(abs(mpat - 1), rowSums(mpat))
  r <- rbind(r, c(nmis[order(nmis)], sum(nmis)))
  r
}


#' Missing data patterns
#'
#' Generate missingness patterns using a function borrowed from mice,
#' with options to reduce the complexity of the output.
#'
#' @param data the dataset
#' @param omit_complete defaults to TRUE, omitting variables without missing values
#' @param min_freq minimum number of rows to have this missingness pattern
#' @export
#' @examples
#' data("bfi", package = 'psych')
#' md_pattern(bfi)
#' md_pattern(bfi, omit_complete = FALSE, min_freq = 0.2)
md_pattern <- function(data, omit_complete = TRUE, min_freq = 0.01) {
  if (sum(is.na(data)) == 0) {
    message("No missing values.")
  } else {
    for (i in seq_along(names(data))) {
      # mice::md.pattern coerces character/factor to NA
      data[[i]] <- as.numeric(as.factor(data[[i]]))
    }
    md_pattern <- suppressMessages(md.pattern(data, plot = FALSE))
    n_miss <- rownames(md_pattern)
    if (is.null(n_miss)) {
      n_miss <- rep(0, nrow(md_pattern))
    }
    colnames(md_pattern)[ncol(md_pattern)] <- "var_miss"
    rownames(md_pattern) <- NULL
    if (omit_complete) {
      missing_by_var <- md_pattern[nrow(md_pattern), ]
      md_pattern <- md_pattern[, missing_by_var > 0]
    }
    md_pattern <- tibble::as_tibble(md_pattern)
    stopifnot(!exists("n_miss", md_pattern))
    md_pattern$n_miss <- as.numeric(n_miss)
    md_pattern$n_miss[nrow(md_pattern)] <-
      md_pattern$var_miss[nrow(md_pattern)]
    stopifnot(!exists("description", md_pattern))
    md_pattern$description <- paste0("Missing values in ", md_pattern$var_miss,
                                     " variables")
    md_pattern$description[nrow(md_pattern)] <- "Missing values per variable"
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
  class(x) <- c("haven_labelled", class(x))
  haven::as_factor(x, levels, ordered, ...)
}

is_attribute_set <- function(attribute, data) {
  !is.null(attr(data, attribute, exact = TRUE))
}


is_formr_survey <- function(results) {
  exists("session", results) &&
  exists("created", results) &&
  exists("modified", results) &&
  exists("ended", results) &&
  exists("expired", results)
}


could_disclose_unique_values <- function(x) {
  disclosing <- FALSE
  if (!is.numeric(x)) {
      if (stats::median(table(x)) == 1) {
      disclosing <- TRUE # most values are unique
    } else {
      if (is.factor(x)) { # factors have defined levels, so unlikely to disclose
        if (dplyr::n_distinct(x) > 40) {
          disclosing <- TRUE
        }
      } else if (dplyr::n_distinct(x) > 20) {
        disclosing <- TRUE # too many different values, might contain free text
      }
    }
  }
  disclosing
}


is_numeric_or_time_var <- function(x) {
  is.numeric(x) ||
    lubridate::is.instant(x) ||
    lubridate::is.timespan(x)
}
