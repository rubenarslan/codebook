#' Pretty-print a psych::alpha object
#'
#' Make the tables HTML instead of code.
#'
#' @param x a psych alpha object
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h5
#' @param ... ignored
#'
#' @export
#' @examples
#' example("alpha", "psych")
#' knitr::knit_print(a4)
#'
knit_print.alpha <- function(x, indent = '#####', ...) {
  asis_knit_child(require_file("_knit_print_psych.Rmd"))
}

#' Print a psych::multilevel.reliability object for knitr
#'
#' Just prints the normal output.
#'
#' @param x a psych alpha object
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h5
#' @param ... ignored
#'
#' @export
#' @examples
#' example("mlr", "psych")
#' knitr::knit_print(mg)
#'
knit_print.multilevel <- function(x, indent = '#####', ...) {
  knitr::asis_output(paste0("\n\n\n```\n",
      paste0(utils::capture.output(psych::print.psych(x)), collapse = "\n"),
      "\n```\n\n\n"))
}


#' Print a stats::cor.test object for knitr
#'
#' Just prints the normal output.
#'
#' @param x a psych alpha object
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h5
#' @param ... ignored
#'
#' @export
#' @examples
#' knitr::knit_print(cor.test(rnorm(100), rnorm(100)))
#'
knit_print.htest <- function(x, indent = '#####', ...) {
  knitr::asis_output(paste0("\n\n\n```\n",
    paste0(utils::capture.output(print(x)), collapse = "\n"),
    "\n```\n\n\n"))
}

#' summary.labelled
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

#' summary.labelled_spss
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
