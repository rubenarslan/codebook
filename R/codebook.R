.data <- rlang::.data

no_md <- function() {
  knitr::asis_output('')
}

#' Generate rmarkdown codebook
#'
#' Pass a data frame to this function to make a codebook for that dataset.
#' If the dataset has metadata (attributes) set on its variables, these will be
#' used to make the codebook more informative. Examples are item, value, and
#' missing labels.
#' Data frames imported via [haven::read_dta()], [haven::read_sav()], or from
#' [formr.org](https://formr.org) will have these attributes in the right format.
#' By calling this function inside a knitr code chunk, the
#' codebook will become part of the document you are generating.
#'
#' @param results a data frame, ideally with attributes set on variables
#' @param reliabilities a named list with one entry per scale and one or several printable reliability computations for this scale. if NULL, computed on-the-fly using compute_reliabilities
#' @param survey_repetition defaults to "auto" which is to try to determine the level of repetition from the "session" and "created" variables. Other values are: single, repeated_once, repeated_many
#' @param detailed_variables whether to print a graph and summary for each variable
#' @param detailed_scales whether to print a graph and summary for each scale
#' @param survey_overview whether to print an overview of survey entries, durations (depends on presence of columns session, created, modified, ended, expired)
#' @param missingness_report whether to print a missingness report. Turn off if this gets too complicated and you need a custom solution (e.g. in case of random missings).
#' @param metadata_table whether to print a metadata table/tabular codebook.
#' @param metadata_json whether to include machine-readable metadata as JSON-LD (not visible)
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
#' @examples
#' # will generate figures in a temporary directory
#' old_base_dir <- knitr::opts_knit$get("base.dir")
#' knitr::opts_knit$set(base.dir = tempdir())
#' on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
#' data("bfi")
#' bfi <- bfi[, c("BFIK_open_1", "BFIK_open_1")]
#' md <- codebook(bfi, survey_repetition = "single", metadata_table = FALSE)
codebook <- function(results, reliabilities = NULL,
    survey_repetition = c('auto', 'single', 'repeated_once', 'repeated_many'),
    detailed_variables = TRUE,
    detailed_scales = TRUE,
    survey_overview = TRUE,
    missingness_report = TRUE, metadata_table = TRUE,
    metadata_json = TRUE, indent = '#') {
  # todo: factor out the time stuff
  # todo: factor out the repetition detection stuff
  survey_repetition <- match.arg(survey_repetition)
  if (survey_repetition == "auto") {
    if (!is_formr_survey(results)) {
      survey_repetition <- 'single'
    } else {
      users <- dplyr::n_distinct(results$session)
      rows_per_user <- nrow(results)/users

      dupes <- sum(duplicated(
        dplyr::select(results, .data$session, .data$created)))
      if (dupes > 0) {
        stop("There seem to be ", dupes, " duplicated rows in this survey ",
             "(duplicate session-created combinations)")
      }
      rows_by_user <- dplyr::count(dplyr::filter(results, !is.na(.data$ended)),
                                   .data$session)$n
      survey_repetition <- ifelse( rows_per_user <= 1,
                                  "single",
                                  ifelse(rows_by_user %in% 1:2,
                                         "repeated_once",
                                         "repeated_many"))
    }
  }

  if (is.null(reliabilities)) {
    reliabilities <- compute_reliabilities(results, survey_repetition)
  }

  df_name <- deparse(substitute(results))
  df_name <- safe_name(df_name)

  old_opt <- options('knitr.duplicate.label')$knitr.duplicate.label
  options(knitr.duplicate.label = 'allow')
  on.exit(options(knitr.duplicate.label = old_opt))
  options <- list(
    fig.path =
      paste0(knitr::opts_chunk$get("fig.path"), "cb_", df_name, "_"),
    cache.path =
      paste0(knitr::opts_chunk$get("cache.path"), "cb_", df_name, "_")
  )
  knitr::opts_chunk$set(fig.path = options$fig.path,
                        cache.path = options$cache.path)

  meta <- metadata(results)
  if (is.null(meta)) {
    meta <- list()
  }

  if (!exists("name", meta)) {
    metadata(results)$name <- deparse(substitute(results))
  }

  if (!exists("datePublished", meta)) {
    metadata(results)$datePublished <- Sys.Date()
  }

  if (!exists("description", meta)) {
    metadata(results)$description <- data_description_default(results)
  }

  if (!exists("keywords", meta)) {
    metadata(results)$keywords <- names(results)
  }

  data_info <- codebook_data_info(results)

  if (survey_overview && is_formr_survey(results)) {
    survey_overview <- codebook_survey_overview(results, survey_repetition)
  } else {
    survey_overview <- no_md()
  }

  scales_items <- new.env()
  vars <- names(results)
  items_contained_in_scales <- c()
  `%<-%` <- future::`%<-%`

  if (detailed_scales) {
    for (i in seq_along(vars)) {
      var <- vars[i]
      scale <- results[[ var ]]
      scale_info <- attributes(scale)
      if ( !is.null(scale_info) && exists("scale_item_names", scale_info)) {
        items_contained_in_scales <- c(items_contained_in_scales,
                                       scale_info$scale_item_names, var)
        items <- dplyr::select(results,
                    !!!scale_info$scale_item_names)
        scales_items[[var]] %<-% {tryCatch({
          codebook_component_scale(
            scale = scale, scale_name = var,
            items = items,
            reliabilities = reliabilities[[var]], indent = indent) },
        error = function(e) stop("Could not summarise scale ", var, ". ", e)) }
      }
    }
  }

  if (detailed_variables) {
    dont_show_these <- c(items_contained_in_scales,
                         c("session", "created", "modified", "expired", "ended"))
    for (i in seq_along(vars)) {
      var <- vars[i]
      item <- results[[ var ]]
      if (var %in% dont_show_these) {
        next # don't do scales again
      } else {
        scales_items[[var]] %<-% {tryCatch({
                        codebook_component_single_item( item = item,
                                item_name = var, indent = indent ) },
        error = function(e) stop("Could not summarise item ", var, ". ", e)) }

      }
    }
  }

  scales_items <- as.list(scales_items)
  # reorder
  done_vars <- intersect(vars, names(scales_items))
  scales_items <- scales_items[done_vars]

  if (missingness_report) {
    missingness_report <- codebook_missingness(results, indent = indent)
  } else {
    missingness_report <- no_md()
  }

  if (metadata_table) {
    items <- codebook_items(results, indent = indent)
  } else {
    items <- no_md()
  }
  if (metadata_json) {
    jsonld <- metadata_jsonld(results)
  } else {
    jsonld <- no_md()
  }


  asis_knit_child(require_file("_codebook.Rmd"), options = options)
}

#' Compact Codebook
#'
#' Generate only the tabular codebook and the machine-readable JSON-LD metadata.
#'
#' @param results the data frame
#'
#' @export
#' @examples
#' # will generate figures in a figure/ subdirectory
#' old_base_dir <- knitr::opts_knit$get("base.dir")
#' knitr::opts_knit$set(base.dir = tempdir())
#' on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
#' data("bfi")
#' bfi <- bfi[, c("BFIK_open_1", "BFIK_open_2")]
#' compact_codebook(bfi)
compact_codebook <- function(results) {
  codebook(results, reliabilities = list(),
            survey_repetition = 'single',
                       detailed_variables = FALSE,
                       detailed_scales = FALSE,
                       survey_overview = FALSE,
                       missingness_report = FALSE, metadata_table = TRUE,
                       metadata_json = TRUE, indent = '#')
}


#' Codebook survey overview
#'
#' An overview of the number of rows and groups, and of the durations participants
#' needed to respond (if those data are available).
#'
#' @param results a data frame which has the following columns: session, created, modified, expired, ended
#' @param survey_repetition defaults to single (other values: repeated_once, repeated_many). controls whether internal consistency, retest reliability or multilevel reliability is computed
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
#' @examples
#' # will generate figures in a figure/ subdirectory
#' old_base_dir <- knitr::opts_knit$get("base.dir")
#' knitr::opts_knit$set(base.dir = tempdir())
#' on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
#' data("bfi")
#' codebook_survey_overview(bfi)
codebook_survey_overview <- function(results, survey_repetition = "single",
                                     indent = "##") {
  stopifnot(exists("session", results))
  stopifnot(exists("created", results))
  stopifnot(exists("modified", results))
  stopifnot(exists("expired", results))
  stopifnot(exists("ended", results))
  # stopifnot(is(results$created, "POSIXct"))
  # stopifnot(is(results$modified, "POSIXct"))
  # stopifnot(is(results$expired, "POSIXct"))
  # stopifnot(is(results$ended, "POSIXct"))

  users <- dplyr::n_distinct(results$session)
  finished_users <- dplyr::n_distinct(results[!is.na(results$ended),]$session)
  rows_per_user <- nrow(results)/users
  rows_by_user <- dplyr::count(dplyr::filter(results, !is.na(.data$ended)),
                               .data$session)$n

  duration <- dplyr::mutate(dplyr::filter(results, !is.na(ended)),
              duration = as.double(.data$ended - .data$created, unit = "mins"))
  upper_limit <- stats::median(duration$duration) +
                          4*stats::mad(duration$duration)
  high_vals <- sum(upper_limit < duration$duration)
  if (high_vals == 0) {
    upper_limit <- max(duration$duration)
  }
  lower_limit <- min(duration$duration)
  low_vals <- sum(lower_limit < 0)
  if (low_vals == 0) {
    lower_limit <- 0
  }

  started <- sum(!is.na(results$modified))
  only_viewed <- sum(is.na(results$ended) & is.na(results$modified))

  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), "overview_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), "overview_")
  )
  asis_knit_child(require_file("_codebook_survey_overview.Rmd"),
                  options = options)
}

#' Codebook data info
#'
#' A readout of the metadata for this dataset, with some defaults set
#'
#' @param results a data frame which has the following columns: session, created, modified, expired, ended
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
#' @examples
#' # will generate figures in a figure/ subdirectory
#' data("bfi")
#' metadata(bfi)$name <- "MOCK Big Five Inventory dataset (German metadata demo)"
#' metadata(bfi)$description <- "a small mock Big Five Inventory dataset"
#' metadata(bfi)$citation <- "doi:10.5281/zenodo.1326520"
#' metadata(bfi)$url <-
#'    "https://rubenarslan.github.io/codebook/articles/codebook.html"
#' codebook_data_info(bfi)
codebook_data_info <- function(results, indent = "##") {
  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), "data_info_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), "data_info_")
  )
  asis_knit_child(require_file("_codebook_data_info.Rmd"),
                  options = options)
}



#' Codebook missingness
#'
#' An overview table of missingness patterns generated using [md_pattern()].
#'
#' @param results a data frame
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
#' @examples
#' data("bfi")
#' codebook_missingness(bfi)
codebook_missingness <- function(results, indent = "##") {
  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), "overview_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), "overview_")
  )
  md_pattern <- md_pattern(results)
  asis_knit_child(require_file("_codebook_missingness.Rmd"), options = options)
}



#' Metadata as JSON-LD
#'
#' Echo a list of a metadata, generated using [metadata_list()] as JSON-LD in a
#' script tag.
#'
#' @param results a data frame, ideally with attributes set on variables
#' @export
#' @examples
#' data("bfi")
#' metadata_jsonld(bfi)
metadata_jsonld <- function(results) {
  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), "metadata_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), "metadata_")
  )
  jsonld_metadata <- metadata_list(results)
  asis_knit_child(require_file("_metadata_jsonld.Rmd"), options = options)
}


#' Tabular codebook
#'
#' Renders a tabular codebook including attributes and data summaries. The table
#' is generated using [DT::datatable()] and can be exported to CSV, Excel, etc.
#'
#'
#' @param results a data frame, ideally with attributes set on variables
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
#' @examples
#' data("bfi")
#' \dontrun{
#' # doesn't show interactively, because a html widget needs to be registered
#' codebook_items(bfi)
#' }
codebook_items <- function(results, indent = "##") {
  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), "items_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), "items_")
  )
  metadata_table <- codebook_table(results)
  metadata_table <- dplyr::mutate(metadata_table,
         name = paste0('<a href="#', safe_name(.data$name), '">',
                       recursive_escape(.data$name), '</a>'))

  # bit ugly to suppress warnings here, but necessary for escaping whatever
  # columns there may be
  suppressWarnings(
    metadata_table <- dplyr::mutate_at(metadata_table, dplyr::vars(
    dplyr::one_of("label", "scale_item_names", "value_labels", "showif")),
    dplyr::funs(recursive_escape)) )

  if (exists("value_labels", metadata_table)) {
    metadata_table$value_labels <- stringr::str_replace_all(
      metadata_table$value_labels, "\n", "<br>")
  }
  if (exists("label", metadata_table)) {
    metadata_table$label <- stringr::str_replace_all(
      metadata_table$label, "\n", "<br>")
  }

  asis_knit_child(require_file("_codebook_items.Rmd"), options = options)
}

escaped_table <- function(metadata_table) {
  if (exists("value_labels", metadata_table)) {
    metadata_table$value_labels <- stringr::str_replace_all(
      metadata_table$value_labels, "\n", "<br>")
  }
  if (exists("label", metadata_table)) {
    metadata_table$label <- stringr::str_replace_all(
      metadata_table$label, "\n", "<br>")
  }

  knitr::kable(metadata_table, escape = FALSE)
}

#' Codebook component for scales
#'
#'
#' @param scale a scale with attributes set
#' @param scale_name the variable name of this scale
#' @param items a data.frame with the items constituting the scale
#' @param reliabilities a list with one or several results from calls to psych package functions for computing reliability
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
#' @examples
#' # will generate figures in a temporary directory
#' old_base_dir <- knitr::opts_knit$get("base.dir")
#' knitr::opts_knit$set(base.dir = tempdir())
#' on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
#' data("bfi")
#' bfi <- bfi[,c("BFIK_open", paste0("BFIK_open_", 1:4))]
#' codebook_component_scale(bfi[,1], "BFIK_open", bfi[,-1],
#'    reliabilities = list(BFIK_open = psych::alpha(bfi[,-1])))
codebook_component_scale <- function(scale, scale_name, items, reliabilities,
                                     indent = '##') {
  stopifnot( exists("scale_item_names", attributes(scale)))
  stopifnot( attributes(scale)$scale_item_names %in% names(items) )
  safe_name <- safe_name(scale_name)

  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), safe_name, "_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), safe_name, "_")
  )
  old_opt <- options('knitr.duplicate.label')$knitr.duplicate.label
  options(knitr.duplicate.label = 'allow')
  on.exit(options(knitr.duplicate.label = old_opt))

  asis_knit_child(require_file("_codebook_scale.Rmd"), options = options)
}

#' Codebook component for single items
#'
#'
#' @param item an item with attributes set
#' @param item_name the item name
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
#' @examples
#' # will generate figure in a temporary directory
#' old_base_dir <- knitr::opts_knit$get("base.dir")
#' knitr::opts_knit$set(base.dir = tempdir())
#' on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
#' data("bfi")
#' codebook_component_single_item(bfi$BFIK_open_1, "BFIK_open_1")
codebook_component_single_item <- function(item, item_name, indent = '##') {
  safe_name <- safe_name(item_name)
  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), safe_name, "_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), safe_name, "_")
  )
  asis_knit_child(require_file("_codebook_item.Rmd"), options = options)
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
    metadata[["@context"]] <- "http://schema.org/"
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
