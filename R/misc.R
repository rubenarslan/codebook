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



#' How many surveys were ended?
#'
#' Just a simple to check how many times a survey (e.g. diary)
#' was finished. It defaults to checking the "ended" variable for this.
#'
#' @param survey which survey are you asking about?
#' @param variable which variable should be filled out, defaults to "ended"
#' @export
#' @examples
#' survey = data.frame(ended = c("2016-05-28 10:11:00", NA, "2016-05-30 11:18:28"))
#' ended(survey = survey)
ended = function(survey, variable = "ended") {
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
#' survey = data.frame(expired = c(NA, "2016-05-29 10:11:00", NA))
#' expired(survey = survey)
expired = function(survey, variable = "expired") {
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
#' survey = data.frame(modified = c(NA, "2016-05-29 10:11:00", NA))
#' modified(survey = survey)
modified = function(survey, variable = "modified") {
  ended(survey, variable)
}
