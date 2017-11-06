#' Generate rmarkdown codebook
#'
#' If you pass the object resulting from a call to formr_results to this function, it will generate a markdown codebook for this object.
#'
#' @param x a psych alpha object
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h4
#'
#' @export
#' @examples
#' example("alpha", "psych")
#' knit_print.alpha(a4)
#'
knit_print.alpha = function(x, indent = '###') {
  asis_knit_child(system.file("_knit_print_psych.Rmd", package = 'codebook', mustWork = TRUE))
}
