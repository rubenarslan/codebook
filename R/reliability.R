#' Compute reliabilities
#'
#' If you pass the object resulting from a call to formr_results to this function, it will compute reliabilities for each scale.
#' Internally, each reliability computation is passed to a [future::future()].
#' If you are calculating multilevel reliabilities, it may be worthwhile to
#' parallelise this operation using [future::plan()].
#' If you don't plan on any complicated parallelisation, you probably do not
#' need to call this function directly, but can rely on it being automatically
#' called during codebook generation.
#' If you do plan to do that, you can pass the results of this operation to the codebook function.
#'
#' @param results a formr results table with attributes set on items and scales
#' @param survey_repetition defaults to "single". Can also be "repeated_once" or "repeated_many"
#'
#' @export
#' @examples
#' data("bfi", package = "codebook")
#' reliabilities <- compute_reliabilities(bfi)

compute_reliabilities <- function(results, survey_repetition = "single") {
  vars <- names(results)
  `%<-%` <- future::`%<-%`
  reliabilities <- new.env()

  for (i in seq_along(vars)) {
    var <- vars[i]
    scale_info <- attributes(results[[var]])
    if (!is.null(scale_info) && exists("scale_item_names", scale_info)) {
      reliabilities[[ var ]] %<-% {
        tryCatch({
          id_vars <- c("session", "created")
          id_vars <- intersect(id_vars, vars)
          scale_info$scale_item_names <- unname(scale_info$scale_item_names)
          items <- dplyr::select(results,
              !!! rlang::quos(id_vars),
              !! rlang::quo(var),
              !!! rlang::quos(scale_info$scale_item_names))
          compute_appropriate_reliability(var, scale_info,
                                          items,
                                          survey_repetition)
        }, error = function(e) warning(e))
      }
    }
  }
  as.list(reliabilities)
}


compute_appropriate_reliability <- function(scale_name, scale_info,
                                            results, survey_repetition) {
  scale_item_names <- scale_info$scale_item_names
  if (survey_repetition == 'single') {
    list(
      internal_consistency =
        psych::alpha(as.data.frame(results[, scale_item_names]),
          title = scale_name, check.keys = FALSE)
    )
  } else if (survey_repetition == 'repeated_once') {
    id_vars <- c("session", "created")
    if ( !all(id_vars %in% names(results))) {
      stop("For now, the variables session and created have to be defined for ",
           "multilevel reliability computation to work.")
    }

    wide <- tidyr::spread(
      dplyr::select(
        dplyr::mutate(
          dplyr::group_by(results, .data$session),
          Time = dplyr::row_number(.data$created)), .data$session, .data$Time,
        !!rlang::quo(scale_name) ),
      key = .data$Time, value = !!dplyr::quo(scale_name), sep = "_")
    list(
      internal_consistency_T1 =
        psych::alpha(as.data.frame(results[!duplicated(results$session),
          scale_item_names]), title = paste( scale_name, "Time 1"),
          check.keys = FALSE),
      internal_consistency_T2 =
        psych::alpha(as.data.frame(results[duplicated(results$session),
          scale_item_names]), title = paste( scale_name, "Time 2"),
          check.keys = FALSE),
      retest_reliability = stats::cor.test(wide$Time_1, wide$Time_2)
    )
  } else if (survey_repetition == 'repeated_many') {
    id_vars <- c("session", "created")
    if ( !all(id_vars %in% names(results))) {
      stop("For now, the variables session and created have to be defined for ",
           "multilevel reliability computation to work.")
    }
    long_rel <- tidyr::gather(dplyr::select(dplyr::mutate(
      dplyr::group_by(results, .data$session),
      day_number = as.numeric(.data$created - min(.data$created), unit = 'days')
      ), .data$session, .data$day_number,
      !!!rlang::quos(scale_item_names) ),
      "variable", "value", -.data$session, -.data$day_number)

    list(
      multilevel_reliability =
        psych::multilevel.reliability(long_rel, "session", "day_number",
           lme = FALSE, lmer = TRUE, items = "variable", values = "value",
           long = TRUE, aov = FALSE)
    )
  }
}

pull_reliability <- function(rels) {
  if (length(rels) == 0) {
    "Not computed"
  } else if (length(rels) == 1 && "alpha" %in% class(rels[[1]])) {
    x <- rels[[1]]
    paste0("Cronbach's \u03B1 [95% CI] = ", round(x$total$raw_alpha, 2), " [",
           round(x$total$raw_alpha - 1.96 * x$total$ase, 2), ";",
           round(x$total$raw_alpha + 1.96 * x$total$ase, 2), "]")
  } else {
    "See details tab"
  }

}
