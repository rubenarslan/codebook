#' Generate rmarkdown codebook
#'
#' If you pass the object resulting from a call to formr_results to this function, it will generate a markdown codebook for this object.
#'
#' @param results a formr results table with attributes set on items and scales
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
#' @examples
#' example("alpha", "psych")
#' print.psych_markdown(a4)
#'
print.psych_markdown = function(obj, indent = '###') {
  asis_knit_child(system.file("_knit_print_psych.Rmd", package = 'codebook', mustWork = TRUE))
}
