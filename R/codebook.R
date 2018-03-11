.data <- rlang::.data

#' Generate rmarkdown codebook
#'
#' If you pass the object resulting from a call to formr_results to this function, it will generate a markdown codebook for this object.
#'
#' @param results a formr results table with attributes set on items and scales
#' @param reliabilities a named list with one entry per scale and one or several printable reliability computations for this scale. if NULL, computed on-the-fly using compute_reliabilities
#' @param survey_repetition defaults to "auto" which is to try to determine the level of repetition from the "session" and "created" variables. Other values are: single, repeated_once, repeated_many
#' @param missingness_report whether to print a missingness report. Turn off if this gets too complicated and you need a custom solution (e.g. in case of random missings).
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
#' @examples
#' # see vignette
codebook <- function(results, reliabilities = NULL,
    survey_repetition = c('auto', 'single', 'repeated_once', 'repeated_many'),
    missingness_report = TRUE, indent = '#') {
  # todo: factor out the time stuff
  # todo: factor out the repetition detection stuff
  survey_repetition <- match.arg(survey_repetition)
  if (survey_repetition == "auto") {
    if (!(exists("session", results) &&
      exists("created", results) &&
      exists("ended", results))) {
      survey_repetition <- 'single'
      warning("The variables session, created, ended have to be defined for ",
              "automatic survey repetition detection to work. Set to no ",
              "repetition by default.")
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
  suppressMessages(
    skimr::skim_with(labelled = skimr::get_skimmers()$factor)
  )
  on.exit(skimr::skim_with_defaults())

  if (is.null(reliabilities)) {
    reliabilities <- compute_reliabilities(results, survey_repetition)
  }

  df_name <- deparse(substitute(results))
  old_opt <- options('knitr.duplicate.label')$knitr.duplicate.label
  options(knitr.duplicate.label = 'allow')
  on.exit(options(knitr.duplicate.label = old_opt))
  options <- list(
    fig.path =
      paste0(knitr::opts_chunk$get("fig.path"), "cb_", df_name, "_"),
    cache.path =
      paste0(knitr::opts_chunk$get("cache.path"), "cb_", df_name, "_")
  )

  survey_overview <- ''
  if (!(exists("session", results) &&
        exists("created", results) &&
        exists("ended", results) &&
        exists("expired", results) &&
        exists("modified", results))) {
    warning("The variables session, created, ended, expired, modified have to ",
            "be defined for automatic survey duration calculations to work.")
  } else {
    survey_overview <- codebook_survey_overview(results, survey_repetition)
  }

  scales_items <- c()
  vars <- names(results)
  items_contained_in_scales <- c()
  for (i in seq_along(vars)) {
    var <- vars[i]
    scale <- results[[ var ]]
    scale_info <- attributes(scale)
    if ( !is.null(scale_info) && exists("scale_item_names", scale_info)) {
      items_contained_in_scales <- c(items_contained_in_scales,
                                     scale_info$scale_item_names, var)
      items <- dplyr::select(results,
                  rlang::UQS(rlang::quos(scale_info$scale_item_names)))
      scales_items[var] <- codebook_component_scale(
        scale = scale, scale_name = var,
        items = items,
        reliabilities = reliabilities[[var]], indent = indent)
    }
  }

  dont_show_these <- c(items_contained_in_scales,
                       c("session", "created", "modified", "expired", "ended"))
  for (i in seq_along(vars)) {
    var <- vars[i]
    item <- results[[ var ]]
    if (var %in% dont_show_these) {
      next # don't do scales again
    } else {
      scales_items[var] <- codebook_component_single_item( item = item,
                              item_name = var, indent = indent )
    }
  }

  if (missingness_report) {
    missingness_report <- codebook_missingness(results, indent = indent)
  } else {
    missingness_report <- ''
  }

  items <- codebook_items(results, indent = indent)

  asis_knit_child(require_file("_codebook.Rmd"), options = options)
}


#' codebook survey overview
#'
#'
#' @param results a formr results table which has the following columns: session, created, modified, expired, ended
#' @param survey_repetition defaults to single (other values: repeated_once, repeated_many). controls whether internal consistency, retest reliability or multilevel reliability is computed
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
codebook_survey_overview <- function(results, survey_repetition = "single",
                                     indent = "##") {
  stopifnot(exists("session", results))
  stopifnot(exists("created", results))
  stopifnot(exists("modified", results))
  stopifnot(exists("expired", results))
  stopifnot(exists("ended", results))

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


#' codebook missingness
#'
#'
#' @param results a formr results table which has the following columns: session, created, modified, expired, ended
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
codebook_missingness <- function(results, indent = "##") {
  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), "overview_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), "overview_")
  )
  md_pattern <- md_pattern(results)
  asis_knit_child(require_file("_codebook_missingness.Rmd"), options = options)
}


#' codebook items
#'
#' you need the formr package to use this function, and the item data has to be encoded in the item attribute
#'
#'
#' @param results a formr results table which has the following columns: session, created, modified, expired, ended
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
codebook_items <- function(results, indent = "##") {
  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), "overview_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), "overview_")
  )
  metadata_table = codebook_table(results)

  asis_knit_child(require_file("_codebook_items.Rmd"), options = options)
}

#' codebook component for scales
#'
#'
#' @param scale a scale with attributes set
#' @param scale_name the variable name of this scale
#' @param items a data.frame with the items constituting the scale
#' @param reliabilities a list with one or several results from calls to psych package functions for computing reliability
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
codebook_component_scale <- function(scale, scale_name, items, reliabilities,
                                     indent = '##') {
  stopifnot( exists("scale_item_names", attributes(scale)))
  stopifnot( attributes(scale)$scale_item_names %in% names(items) )

  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), scale_name, "_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), scale_name, "_")
  )
  asis_knit_child(require_file("_codebook_scale.Rmd"), options = options)
}

#' codebook component for single items
#'
#'
#' @param item an item with attributes set
#' @param item_name the item name
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
codebook_component_single_item <- function(item, item_name, indent = '##') {
  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), item_name, "_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), item_name, "_")
  )
  asis_knit_child(require_file("_codebook_item.Rmd"), options = options)
}

#' codebook metadata table
#'
#' will generate a table combining metadata from variable attributes
#' with data summaries generated using skimr
#'
#' @param results a data frame
#'
#' @export
codebook_table <- function(results) {
  skimmed = skimr::skim_to_wide(results)

  metadata <- dplyr::bind_rows(
    # var = results$session
  lapply(results, function(var) {
    x <- attributes(var)
    if (is.null(x)) {
      data.frame(label = NA)
    } else {
      if (exists("class", x)) {
        x$class <- NULL
      }
      if (exists("tzone", x)) {
        x$tzone <- NULL
      }
      if (exists("label", x)) {
        if (exists("item", x)) {
          if (exists("label", x$item)) {
            x$item$label <- NULL
          }
          if (exists("label_parsed", x$item)) {
            x$item$label_parsed <- NULL
          }
        }
      }
      if (exists("labels", x)) {
        if (!is.null(names(x$labels))) {
          x$value_labels <- paste(paste0(names(x$labels),
                                         "=", x$labels), collapse = ",")
        } else {
          x$value_labels <- paste(x$labels, collapse = ",")
        }
        x$labels <- NULL
        if (exists("item", x) && exists("choices", x$item)) {
          x$item$choices <- NULL
        }
      }

      if (exists("item", x) && exists("name", x$item)) {
        x$item$name <- NULL
      }
      if (exists("scale_item_names", x)) {
        x$scale_item_names <- paste(x$scale_item_names, collapse = ", ")
      }
      as.data.frame(t(purrr::flatten(x)))
    }
  }), .id = "name")

  metadata <- dplyr::left_join(metadata,
                               dplyr::rename(skimmed, data_type = .data$type),
                               by = c("name" = "variable"))
  order <- c("name", "label", "type", "type_options", "data_type", "ordered",
            "value_labels",  "optional", "showif",
            "scale_item_names",
            "value", "item_order", "block_order", "class",
            "missing", "complete", "n",  "empty", "n_unique",
            "top_counts", "count", "median", "min", "max",
            "mean", "sd", "p0", "p25", "p50", "p75", "p100", "hist")
  not_all_na <- function(x) { !all(is.na(x)) && !is.null(unlist(x)) }
  cols <- setdiff(union(
               intersect(order, names(metadata)), # order
              setdiff(names(metadata), order)), # include other cols
               c("choice_list"))

  metadata <- dplyr::select(metadata,  rlang::UQS(rlang::quos(cols)))
  dplyr::select_if(metadata, not_all_na )
}

