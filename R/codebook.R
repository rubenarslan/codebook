utils::globalVariables(c("session", "created"))

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
  stopifnot(exists("session", results))
  stopifnot(exists("created", results))
  stopifnot(exists("modified", results))
  stopifnot(exists("expired", results))
  stopifnot(exists("ended", results))
  if (sum(duplicated(dplyr::select(results, session, created))) > 0) {
    stop("There seem to be duplicated rows in this survey (duplicate session-created variables)")
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
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#' @param survey_repetition defaults to single (other values: repeated_once, repeated_many). controls whether internal consistency, retest reliability or multilevel reliability is computed
#'
#' @export
codebook_component_scale = function(scale, results, indent = '###', survey_repetition = "single") {
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
