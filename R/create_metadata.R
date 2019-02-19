#' @importFrom haven as_factor
#' @export
haven::as_factor

#' @export
as_factor.numeric <- function(x, ...) {
  if (has_labels(x)) {
    class(x) = c("haven_labelled", class(x))
    haven::as_factor(x, ...)
  } else {
    haven::as_factor(x, ...)
  }
}

#' @export
as_factor.character <- function(x, ...) {
  if (has_labels(x)) {
    class(x) = c("haven_labelled", class(x))
    haven::as_factor(x, ...)
  } else {
    haven::as_factor(x, ...)
  }
}


#' @importFrom haven zap_label
#' @export
haven::zap_label

#' @importFrom labelled var_label
#' @export
labelled::var_label

#' @importFrom labelled var_label<-
#' @export
labelled::`var_label<-`


#' @importFrom labelled val_label
#' @export
labelled::val_label

#' @importFrom labelled val_label<-
#' @export
labelled::`val_label<-`


#' @importFrom labelled val_labels
#' @export
labelled::val_labels

#' @importFrom labelled val_labels<-
#' @export
labelled::`val_labels<-`

#' @importFrom dplyr "%>%"
#' @export
dplyr::`%>%`

#' Aggregate variables and remember which variables this were
#'
#' The resulting variables will have the attribute `scale_item_names` containing
#' the basis for aggregation. Its `label` attribute will refer to the common stem of the
#' aggregated variable names (if any), the number of variables, and the
#' aggregation function.
#'
#' @param items data.frame of the items that should be aggregated
#' @param fun aggregation function, defaults to rowMeans with na.rm = FALSE
#' @param stem common stem for the variables, specify if it should not be auto-detected
#' as the longest common stem of the variable names
#' @export
#' @examples
#' testdf <- data.frame(bfi_neuro_1 = rnorm(20), bfi_neuro_2 = rnorm(20),
#'                     bfi_neuro_3R = rnorm(20), age = rpois(20, 30))
#' item_names <- c('bfi_neuro_1', 'bfi_neuro_2', 'bfi_neuro_3R')
#' testdf$bfi_neuro <- aggregate_and_document_scale(testdf[, item_names])
#' testdf$bfi_neuro
aggregate_and_document_scale <- function(items, fun = rowMeans, stem = NULL) {
  new_scale <- fun(items)
  item_names <- names(items)
  attributes(new_scale)$scale_item_names <- item_names

  # find longest common stem
  if (is.null(stem)) {
    max_len <- min(nchar(item_names))
    for (l in max_len:0) {
      stem <- unique(stringr::str_sub(item_names, 1, l))
      if (length(stem) == 1) break
    }
  }
  # string trimming for idiots
  if (nchar(stem)) {
    stem <- stringr::str_match(stem, "^(.+?)_?$")[, 2]
  }

  attributes(new_scale)$label <- paste(ncol(items), stem, "items aggregated by",
                                       deparse(substitute(fun)))
  new_scale
}

#' Add metadata to a dataset
#'
#' Use this function to describe a data frame in preparation for JSON-LD
#' metadata generation using [codebook()] or [metadata_list()].
#'
#' @param data the data frame
#' @param value the metadata attribute
#' @export
#' @examples
#' data('bfi')
#' metadata(bfi)$name <- "MOCK Big Five Inventory dataset (German metadata demo)"
#' metadata(bfi)$description <- "a small mock Big Five Inventory dataset"
#' metadata(bfi)$identifier <- "doi:10.5281/zenodo.1326520"
#' metadata(bfi)$datePublished <- "2016-06-01"
#' metadata(bfi)$creator <- list(
#'   "@type" = "Person",
#'   givenName = "Ruben", familyName = "Arslan",
#'   email = "ruben.arslan@gmail.com",
#'   affiliation = list("@type" = "Organization",
#'                      name = "MPI Human Development, Berlin"))
#' metadata(bfi)$citation <- "Arslan (2016). Mock BFI data."
#' metadata(bfi)$url <-
#'   "https://rubenarslan.github.io/codebook/articles/codebook.html"
#' metadata(bfi)$temporalCoverage <- "2016"
#' metadata(bfi)$spatialCoverage <- "Goettingen, Germany"
#' metadata(bfi)$keywords <- c("Personality", "Psychology")
#' metadata(bfi)
#'
metadata <- function(data) {
UseMethod("metadata")
}

#' @rdname metadata
#' @export
`metadata<-` <- function(data, value) {
  UseMethod("metadata<-")
}

#' @export
`metadata<-.data.frame` <- function(data, value) {
  attributes(data)$metadata <- value
  data
}

#' @export
metadata.data.frame <- function(data) {
  attr(data, 'metadata', exact = TRUE)
}


#' Data description default
#'
#' If you do not define a dataset description yourself, this will be the
#' automatically generated default.
#'
#' @param data the data frame
#' @export
#' @examples
#' data('bfi')
#' data_description_default(bfi)
#'
#' @export
data_description_default <- function(data) {
  stopifnot(is.data.frame(data))
  glue::glue(
    "
    The dataset has N={n_observations} rows and {n_variables} columns.
    {n_completecases} rows have no missing values on any column.
    ",
    n_observations = nrow(data),
    n_completecases = sum(stats::complete.cases(data)),
    n_variables = ncol(data)
  )
}

#' Go from a named list to a key-value data frame or data dictionary and back
#'
#' Sometimes, you'll want to have variable labels in a data.frame, sometimes
#' you'll have imported an existing data dictionary and will need to turn it
#' into a list before setting [labelled::var_label()].
#'
#' @param named_list a named list with one element each (names being variable names, elements being labels)
#' @param dict a data frame with the variable names in the first and the labels in the second column. If they are named variable and label, they can also be in a different order.
#' @export
#' @examples
#' data('bfi')
#' labels <- var_label(bfi)
#' head(labels, 2)
#' dict <- list_to_dict(labels)
#' head(dict, 2)
#' head(dict_to_list(list_to_dict(labels)), 2)
#'
#'
#'
list_to_dict <- function(named_list) {
  dplyr::left_join(
  tibble::data_frame(variable = names(named_list)),
  tidyr::gather(
    tibble::as_data_frame(
      purrr::compact(named_list)),
    "variable", "label"), by = "variable")
}

#' @rdname list_to_dict
#' @export
dict_to_list <- function(dict) {
  if (all(c("variable", "label") %in% names(dict))) {
    dict <- dict[, c("variable", "label")]
  }
  labels <- unlist(dict[,2])
  names(labels) <- unlist(dict[,1])
  labels <- as.list(labels)
  labels[is.na(labels)] <- list(NULL)
  labels
}

#' Append R to string, if it doesn't end in R already.
#'
#' Use this function to conveniently rename reverse-coded variables, so that
#' they end in R.
#'
#' @param x a string
#' @export
#' @examples
#' data('bfi')
#' bfi %>% dplyr::select(BFIK_open_2,BFIK_agree_2) %>% dplyr::rename_at(1, add_R) %>% head()
add_R <- function(x) {
  ifelse(stringr::str_sub(x, -1, -1) == "R",
          x,
          paste0(x, "R"))
}
