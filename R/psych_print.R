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
