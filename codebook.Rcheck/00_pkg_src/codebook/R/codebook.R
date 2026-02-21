#' @importFrom rlang .data
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
#' @importFrom graphics plot
#' @export
#' @examples
#' # will generate figures in a temporary directory
#' \dontrun{
#' data("bfi")
#' bfi <- bfi[, c("BFIK_open_1", "BFIK_open_1")]
#' md <- codebook(bfi, survey_repetition = "single", metadata_table = FALSE)
#' }
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
        dplyr::select(results, "session", "created")))
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
  on.exit(options(knitr.duplicate.label = old_opt))
  optc <- knitr::opts_chunk$get(names(options), drop = FALSE)
  on.exit({
    for (i in names(options)) if (identical(options[[i]],
                    knitr::opts_chunk$get(i))) knitr::opts_chunk$set(optc[i])
  }, add = TRUE)
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
  `%seed%` <- future::`%seed%`

  if (detailed_scales) {
    for (i in seq_along(vars)) {
      var <- vars[i]
      scale <- results[[ var ]]
      scale_info <- attributes(scale)
      if ( !is.null(scale_info) && exists("scale_item_names", scale_info)) {
        items_contained_in_scales <- c(items_contained_in_scales,
                                       scale_info$scale_item_names, var)
        items <- dplyr::select(results,
                    dplyr::all_of(scale_info$scale_item_names))
        scales_items[[var]] %<-% {
          tryCatch({
          codebook_component_scale(
            scale = scale, scale_name = var,
            items = items,
            reliabilities = reliabilities[[var]], indent = indent) },
        error = function(e) stop("Could not summarise scale ", var, ". ", e)) }
      } %seed% TRUE
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
        scales_items[[var]] %<-% {
          tryCatch({
                        codebook_component_single_item( item = item,
                                item_name = var, indent = indent ) },
        error = function(e) stop("Could not summarise item ", var, ". ", e))
          }  %seed% TRUE

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


  rmdpartials::partial(require_file("inst/_codebook.Rmd"), options = options)
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
#' \dontrun{
#' data("bfi")
#' bfi <- bfi[, c("BFIK_open_1", "BFIK_open_2")]
#' compact_codebook(bfi)
#' }
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
#' @param results a data frame which has all the following columns: session, created, modified, expired, ended
#' @param survey_repetition defaults to single (other values: repeated_once, repeated_many). controls whether internal consistency, retest reliability or multilevel reliability is computed
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
#' @examples
#' \dontrun{
#' data("bfi")
#' codebook_survey_overview(bfi)
#' }
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
  low_vals <- sum(duration$duration < 0)
  if (low_vals != 0) {
    lower_limit <- 0
  }

  started <- sum(!is.na(results$modified))
  only_viewed <- sum(is.na(results$ended) & is.na(results$modified))

  rmdpartials::partial(require_file("inst/_codebook_survey_overview.Rmd"),
                  name = "overview_", render_preview = FALSE)
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
  rmdpartials::partial(require_file("inst/_codebook_data_info.Rmd"),
                  name = "data_info_", render_preview = FALSE)
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
  md_pattern <- md_pattern(results)
  rmdpartials::partial(require_file("inst/_codebook_missingness.Rmd"),
                       name = "missingness_", render_preview = FALSE)
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
  jsonld_metadata <- jsonlite::toJSON(metadata_list(results),
                                      pretty = TRUE, auto_unbox = TRUE)
  rmdpartials::partial(require_file("inst/_metadata_jsonld.Rmd"),
                       name = "metadata_", render_preview = FALSE)
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
  metadata_table <- codebook_table(results)
  metadata_table <- dplyr::mutate(metadata_table,
         name = paste0('<a href="#', safe_name(.data$name), '">',
                       recursive_escape(.data$name), '</a>'))

  # bit ugly to suppress warnings here, but necessary for escaping whatever
  # columns there may be
  suppressWarnings(
    metadata_table <- dplyr::mutate_at(metadata_table, dplyr::vars(
    dplyr::one_of("label", "scale_item_names", "value_labels", "showif")),
    recursive_escape) )

  if (exists("value_labels", metadata_table)) {
    metadata_table$value_labels <- stringr::str_replace_all(
      metadata_table$value_labels, "\n", "<br>")
  }
  if (exists("label", metadata_table)) {
    metadata_table$label <- stringr::str_replace_all(
      metadata_table$label, "\n", "<br>")
  }

  rmdpartials::partial(require_file("inst/_codebook_items.Rmd"),
                       name = "items_", render_preview = FALSE)
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
#' \dontrun{
#' data("bfi")
#' bfi <- bfi[,c("BFIK_open", paste0("BFIK_open_", 1:4))]
#' codebook_component_scale(bfi[,1], "BFIK_open", bfi[,-1],
#'    reliabilities = list(BFIK_open = psych::alpha(bfi[,-1])))
#' }
codebook_component_scale <- function(scale,
                                     scale_name = deparse(substitute(scale)),
                                     items, reliabilities = list(),
                                     indent = '##') {
  stopifnot( exists("scale_item_names", attributes(scale)))
  stopifnot( attributes(scale)$scale_item_names %in% names(items) )
  items <- dplyr::select(items,
                         dplyr::all_of(attributes(scale)$scale_item_names))

  safe_name <- safe_name(scale_name)

  old_opt <- options('knitr.duplicate.label')$knitr.duplicate.label
  on.exit(options(knitr.duplicate.label = old_opt))
  options(knitr.duplicate.label = 'allow')

  rmdpartials::partial(require_file("inst/_codebook_scale.Rmd"),
                       name = safe_name, render_preview = FALSE)
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
#' \dontrun{
#' data("bfi")
#' codebook_component_single_item(bfi$BFIK_open_1, "BFIK_open_1")
#' }
codebook_component_single_item <- function(item,
                                           item_name = deparse(substitute(item)), indent = '##') {
  safe_name <- paste0(
    knitr::opts_chunk$get("fig.path"),
    safe_name(item_name), "_")
  safe_name <- safe_name(item_name)

  rmdpartials::partial(require_file("inst/_codebook_item.Rmd"),
                       name = safe_name, render_preview = FALSE)
}
