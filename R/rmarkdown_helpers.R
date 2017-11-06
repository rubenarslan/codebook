#' knit_child as is
#'
#' This slightly modifies the [knitr::knit_child()] function to have different defaults.
#' - the environment defaults to the calling environment.
#' - the output receives the class knit_asis, so that the output will be rendered "as is" by knitr when calling inside a chunk (no need to set results='asis' as a chunk option).
#' - defaults to quiet = TRUE
#'
#' Why default to the calling environment? Typically this function defaults to the global environment. This makes sense if you want to use knit_children in the same context as the rest of the document.
#' However, you may also want to use knit_children inside functions to e.g. summarise a regression using a set of commands (e.g. plot some diagnostic graphs and a summary for a regression nicely formatted).
#'
#' Some caveats:
#' - the function has to return to the top-level. There's no way to [cat()] this from loops or an if-condition without without setting results='asis'. You can however concatenate these objects with [paste.knit_asis()]
#'
#'
#' @param input if you specify a file path here, it will be read in before being passed to knitr (to avoid a working directory mess)
#' @param text passed to [knitr::knit_child()]
#' @param ... passed to [knitr::knit_child()]
#' @param quiet passed to [knitr::knit_child()]
#' @param options defaults to NULL.
#' @param envir passed to [knitr::knit_child()]
#' @param use_strings whether to read in the child file as a character string (solves working directory problems but harder to debug)
#'
#' @export
#' @examples
#' \dontrun{
#' # an example of a wrapper function that calls asis_knit_child with an argument
#' # ensures distinct paths for cache and figures, so that these calls can be looped in parallel
#' regression_summary = function(model) {
#'    child_hash = digest::digest(model)
#'    options = list(
#'        fig.path = paste0(knitr::opts_chunk$get("fig.path"), child_hash, "-"),
#'        cache.path = paste0(knitr::opts_chunk$get("cache.path"), child_hash, "-"))
#'    asis_knit_child("_regression_summary.Rmd", options = options)
#' }
#' }
asis_knit_child = function(input = NULL, text = NULL, ..., quiet = TRUE, options = NULL, envir = parent.frame(), use_strings = TRUE) {
  stopifnot( xor(is.null(text), is.null(input)))
  if (!is.null(input) && use_strings) {
    text = paste0(readLines(input), collapse = "\n")
    input = NULL
  }
  if (!knitr::opts_knit$get("child")) {
    if (!is.null(options)) {
      warning("options ignored in non-child mode")
    }
    output = knitr::knit(input = input, text = text, ..., quiet = quiet, envir = envir)
  } else {
    output = knitr::knit_child(input = input, text = text, ..., quiet = quiet, options = options, envir = envir)
  }
  knitr::asis_output(output)
}


#' paste.knit_asis
#'
#' Helper function for knit_asis objects, useful when e.g. [asis_knit_child()] was used in a loop.
#'
#' Works like [paste()] with both the sep and the collapse argument set to two empty lines
#'
#' @param ... passed to [paste()]
#' @param sep defaults to two empty lines, passed to [paste()]
#' @param collapse defaults to two empty lines, passed to [paste()]
#'
#' @export
#' @examples
#' paste.knit_asis("# Headline 1", "## Headline 2")
paste.knit_asis = function(..., sep = "\n\n\n", collapse = "\n\n\n") {
  knitr::asis_output(paste(..., sep = sep, collapse = collapse))
}

#' Print new lines in knit_asis outputs
#'
#' @param x the knit_asis object
#' @param ... ignored
#'
#' @export
print.knit_asis = function(x, ...) {
  cat(x, sep = '\n')
}
