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
