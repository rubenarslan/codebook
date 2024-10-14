#' @importFrom forcats as_factor
#' @export
forcats::as_factor

#' Has label
#'
#'
#' @param x a vector
#'
#' @export
#' @examples
#' example("labelled", "haven")
#' has_label(x)
has_label <- function(x) {
  haven::is.labelled(x) ||
    !is.null(attr(x, 'label')) ||
    !is.null(attr(x, 'labels'))
}

#' Has labels
#'
#'
#' @param x a vector
#'
#' @export
#' @examples
#' example("labelled", "haven")
#' has_labels(x)
has_labels <- function(x) {
  haven::is.labelled(x) ||
    !is.null(attr(x, 'labels'))
}

#' To factor
#'
#' Convert a labelled vector to a factor, even if it doesn't have the proper
#' class, as long as it has the "labels" attribute.
#' You can have this attribute without, for example, the haven_labelled class,
#' when importing data with [rio::import()] for example.
#'
#' @param x a vector
#' @param ... passed to [haven::as_factor()]
#'
#' @export
#' @examples
#' example("labelled", "haven")
#' to_factor(x)
#' to_factor(zap_labelled(x))
#' to_factor(as_factor(x))
#' to_factor(1:4)

to_factor <- function(x, ...) {
  if (haven::is.labelled(x)) {
    forcats::as_factor(x, ...)
  } else if (has_labels(x)) {
    class(x) = c("haven_labelled", class(x))
    forcats::as_factor(x, ...)
  } else if (is.factor(x)) {
    x
  } else if (utils::head(class(x),1) %in% c("numeric", "character")) {
    class(x) = c("haven_labelled", class(x))
    forcats::as_factor(x, ...)
  } else {
    as.factor(x)
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
                                       deparse(substitute(fun)), collapse = "\n")
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
  tibble::tibble(variable = names(named_list)),
  tidyr::gather(
    tibble::as_tibble(
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


#' Metadata from dataframe
#'
#' Returns a list containing variable metadata (attributes) and data summaries.
#'
#' @param results a data frame, ideally with attributes set on variables
#' @param only_existing whether to drop helpful metadata to comply with the list
#' of currently defined schema.org properties
#'
#' @export
#' @examples
#' data("bfi")
#' md_list <- metadata_list(bfi)
#' md_list$variableMeasured[[20]]
metadata_list <- function(results, only_existing = TRUE) {
  metadata <- metadata(results)
  if (is.null(metadata)) {
    metadata <- list()
  }

  if (!exists("@context", metadata)) {
    metadata[["@context"]] <- "https://schema.org/"
  }

  if (!exists("@type", metadata)) {
    metadata[["@type"]] <- "Dataset"
  }

  if (!exists("variableMeasured", metadata)) {
    metadata$variableMeasured <- lapply(names(results), function(var) {
      x <- attributes(results[[var]])
      x$name <- var

      if (is.null(x)) {
        x <- list()
      } else {
        if (exists("class", x)) {
          x$class <- NULL
        }
        if (exists("tzone", x)) {
          x$tzone <- NULL
        }
        if (exists("label", x)) {
          x$description <- x$label
          x$label <- NULL
        }

        if (exists("levels", x)) {
          x$value <- paste(paste0(seq_len(length(x$levels)), ". ", x$levels),
                           collapse = ",\n")
          x$levels <- NULL
          # remove extremely deep qualtrics choices attributes
          if (exists("item", x) && exists("choices", x$item)
              && exists("variableName", x$item$choices[[1]])) {
            x$item$choices <- NULL
          }
        } else if (exists("labels", x)) {
          if (!is.null(names(x$labels))) {
            x$value <- paste(paste0(x$labels, ". ", names(x$labels)),
                             collapse = ",\n")
          } else {
            x$value <- paste(x$labels, collapse = ",\n")
          }
          x$maxValue <- max(x$labels, na.rm = TRUE)
          x$minValue <- min(x$labels, na.rm = TRUE)
          x$labels <- NULL
          if (exists("item", x) && exists("choices", x$item)) {
            x$item$choices <- NULL
          }
        }
        if (exists("item", x)) {
          if (exists("type", x$item)) {
            x$item$item_type <- x$item$type
            x$item$type <- NULL
          }
          if (exists("choices", x$item)) {
            x$item$choices[["@type"]] <-
              "http://rubenarslan.github.io/codebook/ItemChoices"
          }
          x$measurementTechnique <- "self-report"
          x$item[["@type"]] <- "http://rubenarslan.github.io/codebook/Item"
        }
      }
      if (!only_existing) {

        x$data_summary <- skim_to_wide_labelled(results[[var]])
        x$data_summary$variable <- NULL
        if (exists("type", x$data_summary)) {
          if (!exists("value", x)) {
            x$value <- switch(x$data_summary$type,
                              character = "text",
                              integer = "Number",
                              numeric = "Number",
                              factor = "StructuredValue",
                              labelled = "StructuredValue"
            )
          }
          x$data_summary$type <- NULL
        }
        x$data_summary[["@type"]] <-
          "http://rubenarslan.github.io/codebook/SummaryStatistics"
      }

      if (only_existing) {
        x <- x[intersect(names(x), legal_property_value_properties)]
      }

      x[["@type"]] <- "propertyValue"
      x
    })
  }


  if (only_existing) {
    dict <- codebook_table(results)[, c("name", "label", "n_missing")]
    dict <- knitr::kable(dict, format = "markdown")
    dict <- stringr::str_replace_all(dict, "\n", " - ")
    dict <- paste0(as.character(dict), collapse = "\n")
    if (stringr::str_length(dict) > 4000) {
      dict <- "[truncated]"
    }
    version <- as.character(utils::packageVersion("codebook"))
    template <- "
    ## Table of variables
    This table contains variable names, labels, and number of missing values.
    See the complete codebook for more.

    {dict}

    ### Note
    This dataset was automatically described using the [codebook R package](https://rubenarslan.github.io/codebook/) (version {version}).
    "

    metadata$description <- stringr::str_sub(metadata$description,
                                             1, 5000
                                             - stringr::str_length(template)
                                             - stringr::str_length(dict))
    metadata$description <- paste0(metadata$description, "\n\n\n",
                                   glue::glue(
                                     template,
                                     dict = dict,
                                     version = version))
    metadata <- metadata[intersect(names(metadata), legal_dataset_properties)]
  }

  metadata
}

legal_dataset_properties <-
  c("@type", "@context",
    "distribution", "includedInDataCatalog", "issn", "measurementTechnique",
    "variableMeasured", "about", "accessMode", "accessModeSufficient",
    "accessibilityAPI", "accessibilityControl", "accessibilityFeature",
    "accessibilityHazard", "accessibilitySummary", "accountablePerson",
    "aggregateRating", "alternativeHeadline", "associatedMedia", "audience",
    "audio", "author", "award", "character", "citation", "comment",
    "commentCount", "contentLocation", "contentRating", "contentReferenceTime",
    "contributor", "copyrightHolder", "copyrightYear", "correction", "creator",
    "dateCreated", "dateModified", "datePublished", "discussionUrl", "editor",
    "educationalAlignment", "educationalUse", "encoding", "encodingFormat",
    "exampleOfWork", "expires", "funder", "genre", "hasPart", "headline",
    "inLanguage", "interactionStatistic", "interactivityType",
    "isAccessibleForFree", "isBasedOn", "isFamilyFriendly", "isPartOf",
    "keywords", "learningResourceType", "license", "locationCreated",
    "mainEntity", "material", "mentions", "offers", "position", "producer",
    "provider", "publication", "publisher", "publisherImprint",
    "publishingPrinciples", "recordedAt", "releasedEvent", "review",
    "schemaVersion", "sdDatePublished", "sdLicense", "sdPublisher",
    "sourceOrganization", "spatialCoverage", "sponsor", "temporalCoverage",
    "text", "thumbnailUrl", "timeRequired", "translationOfWork",
    "translator", "typicalAgeRange", "version", "video", "workExample",
    "workTranslation", "additionalType", "alternateName", "description",
    "disambiguatingDescription", "identifier", "image", "mainEntityOfPage",
    "name", "potentialAction", "sameAs", "subjectOf", "url")

legal_property_value_properties <-
  c("@type", "@context",
    "maxValue", "measurementTechnique", "minValue", "propertyID", "unitCode",
    "unitText", "value", "valueReference", "additionalType", "alternateName",
    "description", "disambiguatingDescription", "identifier", "image",
    "mainEntityOfPage", "name", "potentialAction", "sameAs", "subjectOf",
    "url", "additionalProperty", "exifData", "identifier", "valueReference")
