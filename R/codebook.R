.data = rlang::.data

#' Compute reliabilities
#'
#' If you pass the object resulting from a call to formr_results to this function, it will compute reliabilities for each scale.
#' Internally, each reliability computation is passed to a future. If you are calculating multilevel reliabilities, it may be worthwhile to parallelise this operation using future::plan
#' If you don't plan on any complicated parallelisation, you probably do not need to call this function directly, but can rely on it being automatically called during codebook generation.
#' If you do plan to do that, you can pass the results of this operation to the codebook function.
#'
#' @param results a formr results table with attributes set on items and scales
#' @param survey_repetition defaults to "single". Can also be "repeated_once" or "repeated_many"
#'
#' @export
#' @examples
#' if (requireNamespace("formr", quietly = TRUE)) {
#'    example("formr_post_process_results", package = 'formr')
#'    reliabilities = compute_reliabilities(results)
#' }
compute_reliabilities = function(results, survey_repetition = "single") {
  reliabilities_futures = new.env()
  vars = names(results)
  for (i in seq_along(vars)) {
    var = vars[i]
    scale_info = attributes(results[[var]])
    if (!is.null(scale_info) && exists("scale_item_names", scale_info)) {
      reliabilities_futures[[ var ]] = future::future(
        tryCatch({
        compute_appropriate_reliability(var, scale_info,
                                        dplyr::select(results, .data$session, .data$created, rlang::UQ(rlang::quo(var)), rlang::UQS(rlang::quos(scale_info$scale_item_names))),
                                        survey_repetition)
        }, error = function(e) warning(e))
      )
    }
  }
  reliabilities = list()
  scale_names = names(reliabilities_futures)
  for (i in seq_along(reliabilities_futures)) {
    scale = scale_names[i]
    reliabilities[[scale]] = future::value(reliabilities_futures[[scale]])
  }
  reliabilities
}


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
codebook = function(results, reliabilities = NULL, survey_repetition = c('auto', 'single', 'repeated_once', 'repeated_many'), missingness_report = TRUE, indent = '#') {
  # todo: factor out the time stuff
  # todo: factor out the repetition detection stuff
  survey_repetition = match.arg(survey_repetition)
  if (survey_repetition == "auto") {
    if(! (exists("session", results) &&
      exists("created", results) &&
      exists("ended", results))) {
      stop("The variables session, created, ended have to be defined for automatic survey repetition to work.")
    }

    users = dplyr::n_distinct(results$session)
    rows_per_user = nrow(results)/users

    if (sum(duplicated(dplyr::select(results, .data$session, .data$created))) > 0) {
      stop("There seem to be duplicated rows in this survey (duplicate session-created variables)")
    }
    rows_by_user = dplyr::count(dplyr::filter(results, !is.na(.data$ended)), .data$session)$n
    survey_repetition = ifelse( rows_per_user <= 1,
                                "single",
                                ifelse(rows_by_user %in% 1:2,
                                       "repeated_once",
                                       "repeated_many"))
  }

  if (is.null(reliabilities)) {
    reliabilities = compute_reliabilities(results, survey_repetition)
  }

  df_name = deparse(substitute(results))
  old_opt = options('knitr.duplicate.label')$knitr.duplicate.label
  options(knitr.duplicate.label = 'allow')
  on.exit(options(knitr.duplicate.label = old_opt))
  options = list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), "cb_", df_name, "_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), "cb_", df_name, "_")
  )
  asis_knit_child(system.file("_codebook.Rmd", package = 'codebook', mustWork = TRUE), options = options)
}


#' codebook survey overview
#'
#'
#' @param results a formr results table which has the following columns: session, created, modified, expired, ended
#' @param survey_repetition defaults to single (other values: repeated_once, repeated_many). controls whether internal consistency, retest reliability or multilevel reliability is computed
#'
#' @export
codebook_survey_overview = function(results, survey_repetition = "single") {
  stopifnot(exists("session", results))
  stopifnot(exists("created", results))
  stopifnot(exists("modified", results))
  stopifnot(exists("expired", results))
  stopifnot(exists("ended", results))

  users = dplyr::n_distinct(results$session)
  finished_users = dplyr::n_distinct(results[!is.na(results$ended),]$session)
  rows_per_user = nrow(results)/users
  rows_by_user = dplyr::count(dplyr::filter(results, !is.na(.data$ended)), .data$session)$n

  duration = dplyr::mutate(dplyr::filter(results, !is.na(ended)),
                           duration = as.double(.data$ended - .data$created, unit = "mins"))
  upper_limit = stats::median(duration$duration) + 4*stats::mad(duration$duration)
  high_vals = sum(upper_limit < duration$duration)
  if (high_vals == 0) upper_limit = max(duration$duration)
  lower_limit = min(duration$duration)
  low_vals = sum(lower_limit < 0)
  if (low_vals == 0) {
    lower_limit = 0
  }

  started = sum(!is.na(results$modified))
  only_viewed = sum(is.na(results$ended) & is.na(results$modified))

  options = list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), "overview_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), "overview_")
  )
  asis_knit_child(system.file("_codebook_survey_overview.Rmd", package = 'codebook', mustWork = TRUE), options = options)
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
codebook_component_scale = function(scale, scale_name, items, reliabilities, indent = '###') {
  stopifnot( exists("scale_item_names", attributes(scale)))
  stopifnot( attributes(scale)$scale_item_names %in% names(items) )

  options = list(
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
codebook_component_single_item = function(item, item_name, indent = '###') {
  stopifnot( exists("item", attributes(item)))
  options = list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), attributes(item)$item$name, "_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), attributes(item)$item$name, "_")
  )
  asis_knit_child(system.file("_codebook_item.Rmd", package = 'codebook', mustWork = TRUE), options = options)
}

#' codebook component for scales
#'
#'
#' @param item an item without attributes set
#' @param item_name the item name
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
codebook_component_fallback = function(item, item_name, indent = '###') {
  options = list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), item_name, "_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), item_name, "_")
  )
  asis_knit_child(system.file("_codebook_fallback.Rmd", package = 'codebook', mustWork = TRUE), options = options)
}


# todo:
# differentiate automagically between structural missings (didn't do this part because of showif), unfinished missings (didn't get this far), missings were people did not reply to optional items, and (later) missings where people said 'do not want to respond'
# visdat, vismiss
# skimr?
# scale/item sources, refs, with DOIs/links?




#' How many surveys were ended?
#'
#' Just a simple to check how many times a survey (e.g. diary)
#' was finished. It defaults to checking the "ended" variable for this.
#'
#' @param survey which survey are you asking about?
#' @param variable which variable should be filled out, defaults to "ended"
#' @export
#' @examples
#' survey = data.frame(ended = c("2016-05-28 10:11:00", NA, "2016-05-30 11:18:28"))
#' ended(survey = survey)
ended = function(survey, variable = "ended") {
  if (length(survey) > 0) {
    if (length(survey[, variable]) > 0) {
      sum(!is.na(survey[, variable]))
    } else {
      0
    }
  } else {
    0
  }
}

#' How many surveys were expired?
#'
#' Just a simple to check how many times a survey (e.g. diary)
#' has expired (i.e. user missed it). It defaults to checking the "expired" variable for this.
#'
#' @param survey which survey are you asking about?
#' @param variable which variable should be filled out, defaults to "expired"
#' @export
#' @examples
#' survey = data.frame(expired = c(NA, "2016-05-29 10:11:00", NA))
#' expired(survey = survey)
expired = function(survey, variable = "expired") {
  ended(survey, variable)
}

#' How many surveys were modified?
#'
#' Just a simple to check how many times a survey (e.g. diary)
#' has expired (i.e. user missed it). It defaults to checking the "expired" variable for this.
#'
#' @param survey which survey are you asking about?
#' @param variable which variable should be filled out, defaults to "modified"
#' @export
#' @examples
#' survey = data.frame(modified = c(NA, "2016-05-29 10:11:00", NA))
#' modified(survey = survey)
modified = function(survey, variable = "modified") {
  ended(survey, variable)
}


compute_appropriate_reliability = function(scale_name, scale_info, results, survey_repetition) {
  scale_item_names = scale_info$scale_item_names
  if (survey_repetition == 'single') {
    list(
      internal_consistency =
        psych::alpha(data.frame(results[, scale_item_names]), title = scale_name, check.keys = FALSE)
    )
  } else if (survey_repetition == 'repeated_once') {
    wide = tidyr::spread(
      dplyr::select(
        dplyr::mutate(
          dplyr::group_by(
            results, .data$session),
          Time = dplyr::row_number(.data$created)), .data$session, .data$Time, rlang::UQ(rlang::quo(scale_name)) ),
      key = .data$Time, value = !!dplyr::quo(scale_name), sep = "_")
    list(
      internal_consistency_T1 =
        psych::alpha(data.frame(results[!duplicated(results$session), scale_item_names]), title = paste( scale_name, "Time 1"), check.keys = FALSE),
      internal_consistency_T2 =
        psych::alpha(data.frame(results[duplicated(results$session), scale_item_names]), title = paste( scale_name, "Time 2"), check.keys = FALSE),
      retest_reliability = stats::cor.test(wide$Time_1, wide$Time_2)
    )
  } else if (survey_repetition == 'repeated_many') {
    long_rel =  tidyr::gather(dplyr::select(dplyr::mutate(
      dplyr::group_by(
        results, .data$session),
      day_number = as.numeric(.data$created - min(.data$created), unit = 'days')), .data$session, .data$day_number, rlang::UQS(rlang::quos(scale_item_names)) ),
      "variable", "value", -.data$session, -.data$day_number)

    list(
      multilevel_reliability =
      psych::multilevel.reliability(long_rel, "session", "day_number", lme = FALSE, lmer = TRUE, items = "variable", values = "value", long = TRUE, aov = FALSE)
    )
  }
}

likert_from_items = function(items) {
  if (!methods::is(items, "data.frame") || ncol(items) < 1) {
    stop("The items argument has to be a data frame of the items of this subscale.")
  }
  for (i in seq_along(1:ncol(items))) {
    if ( !is.null(attributes(items[[i]])$labels)) {
      labels = names(attributes(items[[i]])$labels)
      names(attributes(items[[i]])$labels) = stringr::str_wrap(labels, width = 15)
      items[[i]] = haven::as_factor(items[[i]])
    } else {
      items[[i]] = as.factor(items[[i]])
    }
    if (!is.null(attributes(items[[i]])$label)) {
      item_names = names(items)
      item_names[i] = stringr::str_wrap(paste0(attributes(items[[i]])$label, " [", names(items)[i], "]"), width = 50)
      names(items) = item_names
    }
  }

  likert::likert(data.frame(items, check.names = F))
}
