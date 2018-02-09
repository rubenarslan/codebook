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
#'    reliabilities <-compute_reliabilities(results)
#' }
compute_reliabilities <-function(results, survey_repetition = "single") {
  reliabilities_futures <- new.env()
  vars <- names(results)
  for (i in seq_along(vars)) {
    var <- vars[i]
    scale_info <- attributes(results[[var]])
    if (!is.null(scale_info) && exists("scale_item_names", scale_info)) {
      reliabilities_futures[[ var ]] <- future::future(
        tryCatch({
          items <- dplyr::select(results, .data$session, .data$created,
              rlang::UQ(rlang::quo(var)),
              rlang::UQS(rlang::quos(scale_info$scale_item_names)))
          compute_appropriate_reliability(var, scale_info,
                                          items,
                                          survey_repetition)
        }, error = function(e) warning(e))
      )
    }
  }
  reliabilities <- list()
  scale_names <- names(reliabilities_futures)
  for (i in seq_along(reliabilities_futures)) {
    scale <- scale_names[i]
    reliabilities[[scale]] <- future::value(reliabilities_futures[[scale]])
  }
  reliabilities
}


compute_appropriate_reliability <- function(scale_name, scale_info,
                                            results, survey_repetition) {
  scale_item_names <- scale_info$scale_item_names
  if (survey_repetition == 'single') {
    list(
      internal_consistency =
        psych::alpha(data.frame(results[, scale_item_names]),
          title = scale_name, check.keys = FALSE)
    )
  } else if (survey_repetition == 'repeated_once') {
    wide <- tidyr::spread(
      dplyr::select(
        dplyr::mutate(
          dplyr::group_by(results, .data$session),
          Time = dplyr::row_number(.data$created)), .data$session, .data$Time,
        rlang::UQ(rlang::quo(scale_name)) ),
      key = .data$Time, value = !!dplyr::quo(scale_name), sep = "_")
    list(
      internal_consistency_T1 =
        psych::alpha(data.frame(results[!duplicated(results$session),
          scale_item_names]), title = paste( scale_name, "Time 1"),
          check.keys = FALSE),
      internal_consistency_T2 =
        psych::alpha(data.frame(results[duplicated(results$session),
          scale_item_names]), title = paste( scale_name, "Time 2"),
          check.keys = FALSE),
      retest_reliability = stats::cor.test(wide$Time_1, wide$Time_2)
    )
  } else if (survey_repetition == 'repeated_many') {
    long_rel <- tidyr::gather(dplyr::select(dplyr::mutate(
      dplyr::group_by(results, .data$session),
      day_number = as.numeric(.data$created - min(.data$created), unit = 'days')
      ), .data$session, .data$day_number,
      rlang::UQS(rlang::quos(scale_item_names)) ),
      "variable", "value", -.data$session, -.data$day_number)

    list(
      multilevel_reliability =
        psych::multilevel.reliability(long_rel, "session", "day_number",
           lme = FALSE, lmer = TRUE, items = "variable", values = "value",
           long = TRUE, aov = FALSE)
    )
  }
}