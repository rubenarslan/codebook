#' Zap attributes
#'
#' Modelled on [haven::zap_labels()], but more encompassing. By default removes
#' the following attributes:
#' format.spss, format.sas, format.stata, label, labels, na_values, na_range,
#' display_width
#'
#'
#' @param x the data frame or variable
#' @param attributes character vector of attributes to zap. NULL if everything
#' (including factor levels etc) should be zapped
#'
#' @export
#'
#' @examples
#' bfi <- data.frame(matrix(data = rnorm(300), ncol = 3))
#' names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3")
#' attributes(bfi$bfi_e1)$label <- "I am outgoing."
#' attributes(bfi$bfi_e2R)$label <- "I prefer books to people."
#' attributes(bfi$bfi_e3)$label <- "I love to party."
#' bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
#' bfi <- detect_scales(bfi, quiet = TRUE) # create attributes
#' str(zap_attributes(bfi, "label"))
#' zap_attributes(bfi$bfi_e)
zap_attributes <- function(x,
       attributes = c("format.spss", "format.sas", "format.stata",
                      "label", "labels", "na_values", "na_range",
                      "display_width")) {
  stopifnot(xor(is.null(attributes), is.character(attributes)))
  UseMethod("zap_attributes", x)
}

#' @export
zap_attributes.default <- function(x, attributes = NULL) {
  if (is.null(attributes)) {
    attributes(x) <- NULL
  } else {
    for (i in seq_along(attributes)) {
      attributes(x)[ attributes[i] ] <- NULL
    }
  }
  x
}

#' @export
zap_attributes.data.frame <- function(x, attributes = NULL) {
  x[] <- lapply(x, zap_attributes, attributes)
  x
}



#' Zap labelled class
#'
#' Modelled on [haven::zap_labels()], zaps labelled class (not other attributes).
#'
#' @param x the data frame or variable
#' @export
zap_labelled <- function(x) {
  UseMethod("zap_labelled", x)
}

#' @export
zap_labelled.data.frame <- function(x) {
  x[] <- lapply(x, zap_labelled)
  x
}

#' @export
zap_labelled.haven_labelled <- function(x) {
  if (inherits(x, "haven_labelled")) {
    unclass(x)
  } else {
    x
  }
}

#' @export
zap_labelled.haven_labelled_spss <- function(x) {
  if (inherits(x, "haven_labelled_spss")) {
    unclass(x)
  } else {
    x
  }
}

#' @export
zap_labelled.default <- function(x) {
  x
}

