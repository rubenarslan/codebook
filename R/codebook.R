.data = rlang::.data
#' Generate rmarkdown codebook
#'
#' If you pass the object resulting from a call to formr_results to this function, it will generate a markdown codebook for this object.
#'
#' @param results a formr results table with attributes set on items and scales
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
#' @examples
#' # see vignette
codebook = function(results, indent = '#') {
  # todo: factor out the time stuff
  # todo: factor out the repetition detection stuff

  stopifnot(exists("session", results))
  stopifnot(exists("created", results))
  stopifnot(exists("modified", results))
  stopifnot(exists("expired", results))
  stopifnot(exists("ended", results))

  if (sum(duplicated(dplyr::select(results, .data$session, .data$created))) > 0) {
    stop("There seem to be duplicated rows in this survey (duplicate session-created variables)")
  }


  users = dplyr::n_distinct(results$session)
  finished_users = dplyr::n_distinct(results[!is.na(results$ended),]$session)
  rows_per_user = nrow(results)/users

  repeated_survey = rows_per_user > 1
  rows_by_user = dplyr::count(dplyr::filter(results, !is.na(.data$ended)), .data$session)$n
  survey_repetition = ifelse( rows_per_user <= 1,
                              "single",
                              ifelse(rows_by_user %in% 1:2,
                                     "repeated_once",
                                     "repeated_many"))

  duration = dplyr::mutate(dplyr::filter(results, !is.na(ended)),
    duration = as.double(.data$ended - .data$created, unit = "mins"))

  started = sum(!is.na(results$modified))
  only_viewed = sum(is.na(results$ended) & is.na(results$modified))

  reliabilities_futures = new.env()
  vars = names(results)
  for (i in seq_along(vars)) {
    scale_info = attributes(results[[vars]])
    reliabilities_futures[[ vars ]] = future::future(
        compute_appropriate_reliability(results[[vars]], scale_info, dplyr::select(results, .data$session, dplyr::starts_with(scale_info$scale)), survey_repetition)
    )
  }
  reliabilities = list()
  scale_names = names(reliabilities_futures)
  for (i in seq_along(reliabilities_futures)) {
    scale = scale_names[i]
    reliabilities[[scale]] = future::value(reliabilities_futures[[scale]])
  }

  old_opt = options('knitr.duplicate.label')$knitr.duplicate.label
  options(knitr.duplicate.label = 'allow')
  options = list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), "cb_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), "cb_")
  )
  res = asis_knit_child(system.file("_codebook.Rmd", package = 'codebook', mustWork = TRUE), options = options)
  options(knitr.duplicate.label = old_opt)
  res
}


#' codebook component for scales
#'
#'
#' @param scale a scale with attributes set
#' @param results a formr results table with attributes set on items and scales
#' @param reliabilities a list with one or several results from calls to psych package functions for computing reliability
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#' @param survey_repetition defaults to single (other values: repeated_once, repeated_many). controls whether internal consistency, retest reliability or multilevel reliability is computed
#'
#' @export
codebook_component_scale = function(scale, results, reliabilities, indent = '###', survey_repetition = "single") {
  stopifnot( exists("item", attributes(scale)))
  stopifnot( exists("scale", attributes(scale)))
  stopifnot( exists("scale_item_names", attributes(scale)))
  options = list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), attributes(scale)$scale, "_"),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"), attributes(scale)$scale, "_")
  )
  asis_knit_child(system.file("_codebook_scale.Rmd", package = 'codebook', mustWork = TRUE), options = options)
}

#' codebook component for single items
#'
#'
#' @param item an item with attributes set
#' @param results a formr results table with attributes set on items and scales
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
codebook_component_single_item = function(item, results, indent = '###') {
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
#' @param results a formr results table with attributes set on items and scales
#' @param item_name the item name
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#'
#' @export
codebook_component_fallback = function(item, results, item_name, indent = '###') {
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

compute_appropriate_reliability = function(scale, scale_info, results, survey_repetition) {
  scale_item_names = scale_info$scale_item_names
  if (survey_repetition == 'single') {
    list(
      internal_consistency =
        psych::alpha(results[, scale_item_names], title = scale_info$scale, check.keys = FALSE)
    )
  } else if (survey_repetition == 'repeated_once') {
    wide = tidyr::spread(
      dplyr::select(
        dplyr::mutate(
          dplyr::group_by(
            results, .data$session),
          Time = dplyr::row_number(.data$created)), .data$session, .data$Time, !!dplyr::quo(scale_info$scale) ),
      key = .data$Time, value = !!dplyr::quo(scale_info$scale), sep="_")
    list(
      internal_consistency_T1 =
        psych::alpha(results[!duplicated(results$session), scale_item_names], title = paste( scale_info$scale, "Time 1"), check.keys = FALSE),
      internal_consistency_T2 =
        psych::alpha(results[duplicated(results$session), scale_item_names], title = paste( scale_info$scale, "Time 2"), check.keys = FALSE),
      retest_reliability = stats::cor.test(wide$Time_1, wide$Time_2)
    )
  } else if (survey_repetition == 'repeated_many') {
    long_rel =  tidyr::gather(dplyr::select(dplyr::mutate(
      dplyr::group_by(
        results, .data$session),
      day_number = as.numeric(.data$created - min(.data$created), unit = 'days')), .data$session, .data$day_number, !!!dplyr::quos(scale_item_names) ),
      "variable", "value", -.data$session, -.data$day_number)

    list(
      multilevel_reliability =
      psych::multilevel.reliability(long_rel, "session", "day_number", lme = FALSE, lmer = TRUE, items = "variable", values = "value", long = TRUE, aov = FALSE)
    )
  }
}
