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
#' @param use_psych compute reliabilities using the psych package, defaults to TRUE. if false, will use rosetta (computationally more expensive, more dependencies)
#'
#' @export
#' @examples
#' data("bfi", package = "codebook")
#' bfi <- bfi %>% dplyr::select(dplyr::starts_with("BFIK_agree"))
#' reliabilities <- compute_reliabilities(bfi)

compute_reliabilities <- function(results, survey_repetition = "single",
                                  use_psych = TRUE) {
  `%<-%` <- future::`%<-%`
  `%seed%` <- future::`%seed%`

  vars <- names(results)
  reliabilities <- new.env()

  for (i in seq_along(vars)) {
    var <- vars[i]
    scale_info <- attributes(results[[var]])
    if (!is.null(scale_info) && exists("scale_item_names", scale_info)) {
      reliabilities[[ var ]] %<-% {
        id_vars <- c("session", "created")
        id_vars <- intersect(id_vars, vars)
        scale_info$scale_item_names <- unname(scale_info$scale_item_names)
        items <- dplyr::select(results,
                               !!!id_vars,
                               !!var,
                               !!!scale_info$scale_item_names)

        tryCatch({
          compute_appropriate_reliability(var, scale_info,
                                          items,
                                          survey_repetition, ci = TRUE,
                        give_me_alpha_even_if_its_strictly_inferior = use_psych)
        }, error = function(e) {
          tryCatch({
            warning("Reliability CIs could not be computed for ", var)
            warning(conditionMessage(e))
            compute_appropriate_reliability(var, scale_info,
                                          items,
                                          survey_repetition, ci = FALSE,
                        give_me_alpha_even_if_its_strictly_inferior = use_psych)
          }, error = function(e) {
            warning("Reliability could not be computed for ", var)
            warning(conditionMessage(e))
            NULL
          })

        })
      } %seed% TRUE
    }
  }
  as.list(reliabilities)
}


compute_appropriate_reliability <- function(scale_name, scale_info,
                                            results, survey_repetition,
                                            ci = TRUE,
  give_me_alpha_even_if_its_strictly_inferior = TRUE) {
  scale_item_names <- scale_info$scale_item_names
  if (give_me_alpha_even_if_its_strictly_inferior) {
    internal_consistency <- function(data, scale_name) {
      if (!requireNamespace("psych", quietly = TRUE)) {
        stop("Package \"psych\" needed to compute reliabilites.",
             call. = FALSE)
      }
      psych::alpha(as.data.frame(data),
                   title = scale_name, check.keys = FALSE)
    }
  } else {
    if (!suppressMessages(
      requireNamespace("rosetta", quietly = TRUE))) {
      warning("Package \"rosetta\" needed to compute reliabilites without psych.",
           call. = FALSE)
      return(NULL)
    } else {
      internal_consistency <- function(data, scale_name) {
        suppressWarnings(
          rel <- rosetta::reliability(data, itemLevel = TRUE, scatterMatrix = FALSE, ordinal = TRUE))
      }
    }
  }

  if (survey_repetition == 'single') {
      list(
        internal_consistency =
          internal_consistency(results[, scale_item_names], scale_name)
      )
  } else if (survey_repetition == 'repeated_once') {
    id_vars <- c("session")
    if ( !all(id_vars %in% names(results))) {
      stop("For now, the variable session has to index the user ID and earlier ",
           "rows need to reflect the earlier measurement occasion, so that ",
           "retest statistics can be computed")
    }
    t1_items <- results[!duplicated(results[, id_vars]),
                        scale_item_names, drop = FALSE]
    t2_items <- results[duplicated(results[, id_vars]),
                        scale_item_names, drop = FALSE]

    list(
      internal_consistency_T1 =
        internal_consistency(t1_items, paste( scale_name, "Time 1")),
      internal_consistency_T2 =
        internal_consistency(t2_items, paste( scale_name, "Time 2")),
      retest_reliability = stats::cor.test(
        rowMeans(t1_items, na.rm = T), rowMeans(t2_items, na.rm = T))
    )
  } else if (survey_repetition == 'repeated_many') {
    if (!requireNamespace("psych", quietly = TRUE)) {
      warning("Package \"psych\" needed to compute multilevel reliabilites.",
           call. = FALSE)
      return(NULL)
    }
    id_vars <- c("session", "created")
    if ( !all(id_vars %in% names(results))) {
      stop("For now, the variables session and created have to be defined for ",
           "multilevel reliability computation to work.")
    }
    long_rel <- tidyr::gather(dplyr::select(dplyr::mutate(
      dplyr::group_by(results, .data$session),
      day_number = as.numeric(.data$created - min(.data$created), unit = 'days')
      ), .data$session, .data$day_number,
      !!!scale_item_names ),
      "variable", "value", -"session", -"day_number")

    list(
      multilevel_reliability =
        psych::multilevel.reliability(long_rel, "session", "day_number",
           lme = FALSE, lmer = TRUE, items = "variable", values = "value",
           long = TRUE, aov = FALSE)
    )
  }
}

#' Briefly summarise available reliability results
#'
#' One-line summary
#'
#' @param rels the result of a call to compute_reliabilities
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' data("bfi", package = "codebook")
#' bfi <- bfi %>% dplyr::select(dplyr::starts_with("BFIK_agree"))
#' reliabilities <- compute_reliabilities(bfi)
#' pull_reliability(reliabilities$BFIK_agree)
#' reliabilities <- compute_reliabilities(bfi, use_psych = FALSE)
#' pull_reliability(reliabilities$BFIK_agree)
#' }
pull_reliability <- function(rels) {
  if (length(rels) == 0) {
    "Not computed"
  } else if (length(rels) == 1 && inherits(rels[[1]], "alpha")) {
    x <- rels[[1]]
    paste0("Cronbach's \u03B1 [95% CI] = ", round(x$total$raw_alpha, 2), " [",
           round(x$total$raw_alpha - 1.96 * x$total$ase, 2), ";",
           round(x$total$raw_alpha + 1.96 * x$total$ase, 2), "]")
  } else if (length(rels) == 1 && inherits(rels[[1]], "rosettaReliability")) {
    coefs <- rels[[1]]$scaleStructure$output$dat
    if (exists("omega.ordinal.ci.lo", coefs)) {
      paste0("\u03C9<sub>ordinal</sub> [95% CI] = ", round(coefs$omega.ordinal, 2), " [",
             round(coefs$omega.ordinal.ci.lo, 2), ";",
             round(coefs$omega.ordinal.ci.hi, 2), "]")
    } else if (exists("omega.ci.lo", coefs)) {
      paste0("\u03C9<sub>total</sub> [95% CI] = ", round(coefs$omega, 2), " [",
             round(coefs$omega.ci.lo, 2), ";",
             round(coefs$omega.ci.hi, 2), "]")
    } else if (exists("omega", coefs)) {
      paste0("\u03C9<sub>total</sub> [95% CI] = ", round(coefs$omega, 2),
             " [not computed]")
    } else if (exists("omega.psych.tot", coefs)) {
      paste0("\u03C9<sub>psych.tot</sub> [95% CI] = ", round(coefs$omega.psych.tot, 2),
             " [not computed]")
    } else {
      "See details tab"
    }
  } else {
    "See details tab"
  }

}
