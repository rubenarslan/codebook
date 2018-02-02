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
codebook <- function(results, reliabilities = NULL, survey_repetition = c('auto', 'single', 'repeated_once', 'repeated_many'), missingness_report = TRUE, indent = '#') {
  # todo: factor out the time stuff
  # todo: factor out the repetition detection stuff
  survey_repetition <- match.arg(survey_repetition)
  if (survey_repetition == "auto") {
    if (!(exists("session", results) &&
      exists("created", results) &&
      exists("ended", results))) {
      survey_repetition <- 'single'
      warning("The variables session, created, ended have to be defined for automatic survey repetition detection to work. Set to no repetition by default.")
    } else {
      users <- dplyr::n_distinct(results$session)
      rows_per_user <- nrow(results)/users

      if (sum(duplicated(dplyr::select(results, .data$session, .data$created))) > 0) {
        stop("There seem to be duplicated rows in this survey (duplicate session-created variables)")
      }
      rows_by_user <- dplyr::count(dplyr::filter(results, !is.na(.data$ended)), .data$session)$n
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
  old_opt <- options('knitr.duplicate.label')$knitr.duplicate.label
  options(knitr.duplicate.label = 'allow')
  on.exit(options(knitr.duplicate.label = old_opt))
  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), "cb_", df_name, "_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), "cb_", df_name, "_")
  )

  survey_overview <- ''
  if (!(exists("session", results) &&
        exists("created", results) &&
        exists("ended", results) &&
        exists("expired", results) &&
        exists("modified", results))) {
    warning("The variables session, created, ended, expired, modified have to be defined for automatic survey duration calculations to work.")
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
      items_contained_in_scales <- c(items_contained_in_scales, scale_info$scale_item_names, var)
      scales_items[var] <- codebook_component_scale(
        scale = scale, scale_name = var,
        items = dplyr::select(results, rlang::UQS(rlang::quos(scale_info$scale_item_names))),
        reliabilities = reliabilities[[var]], indent = indent)
    }
  }

  dont_show_these <- c(items_contained_in_scales, c("session", "created", "modified", "expired", "ended"))
  for (i in seq_along(vars)) {
    var <- vars[i]
    item <- results[[ var ]]
    if (var %in% dont_show_these) {
      next # don't do scales again
    } else {
      scales_items[var] <- codebook_component_single_item( item = item, item_name = var, indent = indent )
    }
  }

  if (missingness_report) {
    missingness_report <- codebook_missingness(results, indent = indent)
  } else {
    missingness_report <- ''
  }

  items <- codebook_items(results, indent = indent)

  asis_knit_child(system.file("_codebook.Rmd", package = 'codebook', mustWork = TRUE), options = options)
}


#' codebook survey overview
#'
#'
#' @param results a formr results table which has the following columns: session, created, modified, expired, ended
#' @param survey_repetition defaults to single (other values: repeated_once, repeated_many). controls whether internal consistency, retest reliability or multilevel reliability is computed
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
codebook_survey_overview <- function(results, survey_repetition = "single", indent = "##") {
  stopifnot(exists("session", results))
  stopifnot(exists("created", results))
  stopifnot(exists("modified", results))
  stopifnot(exists("expired", results))
  stopifnot(exists("ended", results))

  users <- dplyr::n_distinct(results$session)
  finished_users <- dplyr::n_distinct(results[!is.na(results$ended),]$session)
  rows_per_user <- nrow(results)/users
  rows_by_user <- dplyr::count(dplyr::filter(results, !is.na(.data$ended)), .data$session)$n

  duration <- dplyr::mutate(dplyr::filter(results, !is.na(ended)),
                           duration = as.double(.data$ended - .data$created, unit = "mins"))
  upper_limit <- stats::median(duration$duration) + 4*stats::mad(duration$duration)
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
  asis_knit_child(system.file("_codebook_survey_overview.Rmd", package = 'codebook', mustWork = TRUE), options = options)
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
  md_pattern <- mice::md.pattern(results)
  # only show vars that have missings at all
  md_pattern <- md_pattern[, md_pattern[nrow(md_pattern), ] != 0]
  asis_knit_child(system.file("_codebook_missingness.Rmd", package = 'codebook', mustWork = TRUE), options = options)
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
  asis_knit_child(system.file("_codebook_items.Rmd", package = 'codebook', mustWork = TRUE), options = options)
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
codebook_component_scale <- function(scale, scale_name, items, reliabilities, indent = '##') {
  stopifnot( exists("scale_item_names", attributes(scale)))
  stopifnot( attributes(scale)$scale_item_names %in% names(items) )

  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), scale_name, "_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), scale_name, "_")
  )
  asis_knit_child(system.file("_codebook_scale.Rmd", package = 'codebook', mustWork = TRUE), options = options)
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
  asis_knit_child(system.file("_codebook_item.Rmd", package = 'codebook', mustWork = TRUE), options = options)
}


# todo:
# differentiate automagically between structural missings (didn't do this part because of showif), unfinished missings (didn't get this far), missings were people did not reply to optional items, and (later) missings where people said 'do not want to respond'
# visdat, vismiss
# skimr?
# scale/item sources, refs, with DOIs/links?

