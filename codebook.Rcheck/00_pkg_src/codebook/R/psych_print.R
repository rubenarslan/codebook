#' Print a [psych::multilevel.reliability()] object for knitr
#'
#' Just prints the normal output of [psych::multilevel.reliability()].
#'
#' @param x a psych alpha object
#' @param ... ignored
#'
#' @exportS3Method knitr::knit_print multilevel
#' @examples
#' example("mlr", "psych")
#' knitr::knit_print(mg)
#'
knit_print.multilevel <- function(x, ...) {
  knitr::asis_output(paste0("\n\n\n```\n",
      paste0(utils::capture.output(psych::print.psych(x)), collapse = "\n"),
      "\n```\n\n\n"))
}


#' Print a [stats::cor.test()] object for knitr
#'
#' Just prints the normal output of [stats::cor.test()].
#'
#' @param x a psych alpha object
#' @param ... ignored
#'
#' @exportS3Method knitr::knit_print htest
#' @examples
#' knitr::knit_print(cor.test(rnorm(100), rnorm(100)))
#'
knit_print.htest <- function(x, ...) {
  knitr::asis_output(paste0("\n\n\n```\n",
    paste0(utils::capture.output(print(x)), collapse = "\n"),
    "\n```\n\n\n"))
}


#' Pretty-print a Cronbach's alpha object
#'
#' Turn a [psych::alpha()] object into HTML tables.
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
knit_print.alpha <- function(x, indent = '######', ...) {
  rmdpartials::partial(require_file("_knit_print_psych.Rmd"))
}
