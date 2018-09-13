#' @importFrom haven as_factor
#' @export
haven::as_factor

#' @importFrom haven zap_label
#' @export
haven::zap_label

#' @importFrom labelled var_label
#' @export
labelled::var_label

#' @importFrom labelled var_label<-
#' @export
labelled::`var_label<-`



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

#' Add description to a dataset
#'
#' Add a single character vector to describe a data frame in preparation for JSON-LD
#' metadata generation using [codebook()] or [metadata_list()].
#'
#' @param data the data frame
#' @param value the description
#' @export
#' @examples
#' data_description(bfi) <- "a small mock Big Five Inventory dataset"
#'
`data_description<-` <- function(data, value) {
  UseMethod("data_description<-")
}

#' @export
`data_description<-.data.frame` <- function(data, value) {
  if ((!is.character(value) & !is.null(value)) | length(value) >
      1)
    stop("`value` should be a single character string or NULL",
         call. = FALSE, domain = "R-codebook")
  attributes(data)$description <- value
  data
}

#' Data description default
#'
#' If you do not define a dataset description yourself, this will be the
#' automatically generated default.
#'
#' @param data the data frame
#' @export
#' @examples
#' data_description_default(bfi)
#'
#' @export
data_description_default <- function(data) {
  stopifnot(is.data.frame(data))
  glue::glue(
    "The dataset has N={n_observations} rows of
    which n={n_completecases} have no missings and a total of
    {n_variables} columns.

    <details><summary>Variable names</summary>{variable_names}</details>

    This dataset was automatically described using the codebook R package.",
    n_observations = nrow(data),
    n_completecases = sum(stats::complete.cases(data)),
    n_variables = ncol(data),
    variable_names = paste(recursive_escape(colnames(data)), collapse = ", "))
}


#' Give a name to a dataset
#'
#' Add a single character vector to name a data frame in preparation for JSON-LD
#' metadata generation using [codebook()] or [metadata_list()].
#'
#' @param data the data frame
#' @param value the name
#' @export
#' @examples
#' data_name(bfi) <- "a small mock Big Five Inventory dataset"
#'
`data_name<-` <- function(data, value) {
  UseMethod("data_name<-")
}

#' @export
`data_name<-.data.frame` <- function(data, value) {
  if ((!is.character(value) & !is.null(value)) | length(value) >
      1)
    stop("`value` should be a single character string or NULL",
         call. = FALSE, domain = "R-codebook")
  attributes(data)$name <- value
  data
}


#' Add an URL to a dataset
#'
#' Add a URL where the dataset description can be found in preparation for
#' JSON-LD metadata generation using [codebook()] or [metadata_list()].
#'
#' @param data the data frame
#' @param value the URL
#' @export
#' @examples
#' data_url(bfi) <- "https://rubenarslan.github.io/codebook/articles/codebook.html"
#'
`data_url<-` <- function(data, value) {
  UseMethod("data_url<-")
}

#' @export
`data_url<-.data.frame` <- function(data, value) {
  if ((!is.character(value) & !is.null(value)) | length(value) >
      1)
    stop("`value` should be a single character string or NULL",
         call. = FALSE, domain = "R-codebook")
  attributes(data)$url <- value
  data
}


#' Add citation information to a dataset
#'
#' Add a single character vector or a list to give citation information for
#' JSON-LD metadata generation using [codebook()] or [metadata_list()].
#'
#' @param data the data frame
#' @param value the citation information
#' @export
#' @examples
#' data_citation(bfi) <- "Arslan (2018). Mock BFI data."
#'
`data_citation<-` <- function(data, value) {
  UseMethod("data_citation<-")
}

#' @export
`data_citation<-.data.frame` <- function(data, value) {
  if (!((is.character(value) | is.null(value) | is.list(value))))
    stop("`value` should be a single character string, a list, or NULL",
         call. = FALSE, domain = "R-codebook")
  attributes(data)$citation <- value
  data
}


#' Add identifier to a dataset
#'
#' Add a single character vector or a list to give an identifier for the dataset,
#' such as a DOI.
#'
#' @param data the data frame
#' @param value the identifier
#' @export
#' @examples
#' data_identifier(bfi) <- "doi:10.5281/zenodo.1326520"
#'
`data_identifier<-` <- function(data, value) {
  UseMethod("data_identifier<-")
}

#' @export
`data_identifier<-.data.frame` <- function(data, value) {
  if (!((is.character(value) | is.null(value) | is.list(value))))
    stop("`value` should be a single character string, a list, or NULL",
         call. = FALSE, domain = "R-codebook")
  attributes(data)$identifier <- value
  data
}

#' Add temporal coverage information to a dataset
#'
#' Add a single character vector or a list to give temporal coverage the dataset
#' if it has a temporal extent for JSON-LD metadata generation, e.g., the period
#' of data collection.
#'
#' @param data the data frame
#' @param value the temporal coverage information
#' @export
#' @examples
#' data_temporalCoverage(bfi) <- "2017"
#'
`data_temporalCoverage<-` <- function(data, value) {
  UseMethod("data_temporalCoverage<-")
}

#' @export
`data_temporalCoverage<-.data.frame` <- function(data, value) {
  if (!((is.character(value) | is.null(value) | is.list(value))))
    stop("`value` should be a single character string, a list, or NULL",
         call. = FALSE, domain = "R-codebook")
  attributes(data)$temporalCoverage <- value
  data
}


#' Add spatial coverage information to a dataset
#'
#' Add a single character vector or a list to give spatial coverage the dataset
#' if it has a spatial extent for JSON-LD metadata generation, e.g., the place
#' where data was collected.
#'
#' @param data the data frame
#' @param value the spatial coverage information
#' @export
#' @examples
#' data_spatialCoverage(bfi) <- "Goettingen, Germany"
#'
`data_spatialCoverage<-` <- function(data, value) {
  UseMethod("data_spatialCoverage<-")
}

#' @export
`data_spatialCoverage<-.data.frame` <- function(data, value) {
  if (!((is.character(value) | is.null(value) | is.list(value))))
    stop("`value` should be a single character string, a list, or NULL",
         call. = FALSE, domain = "R-codebook")
  attributes(data)$spatialCoverage <- value
  data
}

#' Add keywords to a dataset
#'
#' Add a character vector to to add keywords in preparation for JSON-LD
#' metadata generation using [codebook()] or [metadata_list()].
#'
#' @param data the data frame
#' @param value the keywords
#' @export
#' @examples
#' data_keywords(bfi) <- c("Big Five", "Personality", "Psychology")
#'
`data_keywords<-` <- function(data, value) {
  UseMethod("data_keywords<-")
}

#' @export
`data_keywords<-.data.frame` <- function(data, value) {
  if (!((is.character(value) | is.null(value))))
    stop("`value` should be a character vector or NULL",
         call. = FALSE, domain = "R-codebook")
  attributes(data)$keywords <- value
  data
}
