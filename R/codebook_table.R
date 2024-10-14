
#' Codebook metadata table
#'
#' will generate a table combining metadata from variable attributes
#' with data summaries generated using [skimr::skim()]
#'
#' @param results a data frame, ideally with attributes set on variables
#'
#' @export
#' @examples
#' data("bfi")
#' codebook_table(bfi)
codebook_table <- function(results) {
  skimmed <- skim_to_wide_labelled(results)
  metadata <- gather_variable_metadata(results)

  metadata <- dplyr::left_join(metadata,
                               skimmed,
                               by = c("name" = "skim_variable"))
  order <- c("name", "label", "type", "type_options", "data_type", "ordered",
             "value_labels",  "optional", "showif",
             "scale_item_names",
             "value", "item_order", "block_order", "class",
             "n_missing", "complete_rate", "n_unique", "empty",
             "top_counts", "count", "min",  "median", "max",
             "mean", "sd", "whitespace", "n_value_labels", "hist")
  not_all_na <- function(x) { !all(is.na(x)) && !is.null(unlist(x)) }
  cols <- setdiff(union(
    intersect(order, names(metadata)), # order
    setdiff(names(metadata), order)), # include other cols
    c("choice_list"))

  metadata <- dplyr::select(metadata, dplyr::all_of(cols))
  metadata <- dplyr::select_if(metadata, not_all_na )
  if (!exists("label", metadata)) {
    metadata$label <- NA_character_
  }
  metadata
}

gather_variable_metadata <- function(results) {
  metadata <- purrr::map_dfr(results, attribute_summary, .id = "name")
  stopifnot(nrow(metadata) == ncol(results))
  metadata
}


attribute_summary <- function(var) {
  x <- attributes(var)
  if (is.null(x)) {
    return(data.frame(label = NA_character_, stringsAsFactors = FALSE))
  }
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
  if (exists("levels", x)) {
    x$value_labels <- paste(paste0(seq_len(length(x$levels)), ". ", x$levels),
                            collapse = ",\n")
    x$levels <- NULL
    # remove extremely deep qualtrics choices attributes
    if (exists("item", x) && exists("choices", x$item)
        && exists("variableName", x$item$choices[[1]])) {
      x$item$choices <- NULL
    }
  } else if (exists("labels", x)) {
    if (!is.null(names(x$labels))) {
      x$value_labels <- paste(paste0(x$labels, ". ", names(x$labels)),
                              collapse = ",\n")
    } else {
      x$value_labels <- paste(x$labels, collapse = ",\n")
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
  x <- purrr::flatten_dfr(purrr::flatten(x))
  if (ncol(x) == 0) {
    x <- data.frame(label = NA_character_, stringsAsFactors = FALSE)
  }
  dplyr::mutate_all(x, as.character)
}

#' Define skimmers for haven_labelled variables
#'
#' Variables labelled using the haven_labelled class are special
#' because the underlying data can be numeric or character. This skimmers summarises
#' both and leaves non-pertinent columns missings.
#'
#' @importFrom vctrs vec_data
#' @param column the column to skim
#'
#' @exportS3Method skimr::get_skimmers haven_labelled
get_skimmers.haven_labelled <- function(column) {
  haven_labelled_sfl
}

haven_labelled_sfl <- skimr::sfl(
  skim_type = "haven_labelled",
  mean = ~ ifelse(typeof(.) != "character",
                  mean(., na.rm = TRUE),
                  NA_real_),
  sd = ~ ifelse(typeof(.) != "character",
                sd(., na.rm = TRUE),
                NA_real_),
  min = ~ ifelse(typeof(.) != "character",
                 min(vctrs::vec_data(.), na.rm = TRUE),
                 skimr::get_one_default_skimmer("character")$min(.)),
  median = ~ ifelse(typeof(.) != "character",
                    stats::median(., na.rm = TRUE),
                    NA_real_),
  max = ~ ifelse(typeof(.) != "character",
                 max(vctrs::vec_data(.), na.rm = TRUE),
                 skimr::get_one_default_skimmer("character")$max(.)),
  empty = ~ ifelse(typeof(.) != "character",
                   NA_integer_,
                   skimr::get_one_default_skimmer("character")$empty(.)),
  n_unique = ~ ifelse(typeof(.) != "character",
                      NA_integer_,
                      skimr::get_one_default_skimmer("character")$n_unique(.)),
  whitespace = ~ ifelse(typeof(.) != "character",
                        NA_integer_,
                        skimr::get_one_default_skimmer("character")$whitespace(.)),
  n_value_labels = ~ length(labelled::val_labels(.)),
  hist = ~ ifelse(typeof(.) != "character",
                  skimr::inline_hist(.),
                  NA_character_),
)
#' Define skimmers for haven_labelled_spss variables
#'
#' Variables labelled using the haven_labelled_spss class are special
#' because the underlying data can be numeric or character. This skimmers summarises
#' both and leaves non-pertinent columns missings.
#'
#' @param column the column to skim
#'
#' @exportS3Method skimr::get_skimmers haven_labelled_spss
get_skimmers.haven_labelled_spss <- function(column) {
  get_skimmers.haven_labelled(column)
}

#' Skim codebook
#'
#' Implements the regular functionality of [skimr::skim()] but renames the columns
#' p0, p50, and p100 to min, median, and max respectively for numeric types to
#' keep things consistent across type (and produce a narrower wide table).
#'
#' @param data the dataset to skim
#' @param ... passed to [skimr::skim()]
#'
#' @export
#' @examples
#' skim_codebook(bfi)
skim_codebook <- function(data, ...) {
    skimr::skim_with(
    haven_labelled = haven_labelled_sfl,
    haven_labelled_spss = haven_labelled_sfl,
    numeric = skimr::sfl(
      mean = skimr::get_one_default_skimmer("numeric")$mean,
      sd = skimr::get_one_default_skimmer("numeric")$sd,
      min = skimr::get_one_default_skimmer("numeric")$p0,
      median = skimr::get_one_default_skimmer("numeric")$p50,
      max = skimr::get_one_default_skimmer("numeric")$p100,
      hist = skimr::get_one_default_skimmer("numeric")$hist
    ),
    append = FALSE
  )(data, ...)
}

coerce_skimmed_summary_to_character <- function(df) {
  format_digits <- function(x) {
    # format(x, digits = getOption("digits"))
    format(x, digits = 2)
  }
  as_character <- function(x) {
    as.character(x)
  }
  if (exists("POSIXct", df)) {
    df$POSIXct <-
      dplyr::mutate_at(df$POSIXct,
                       dplyr::vars("min", "median", "max"),
                       as_character)
  }
  if (exists("Date", df)) {
    df$Date <-
      dplyr::mutate_at(df$Date,
                       dplyr::vars("min", "median", "max"),
                       as_character)
  }
  if (exists("difftime", df)) {
    df$difftime <-
      dplyr::mutate_at(df$difftime,
                       dplyr::vars("min", "median", "max"),
                       as_character)
  }
  if (exists("ts", df)) {
    df$ts <-
      dplyr::mutate_at(df$ts,
                       dplyr::vars("min", "median", "max"),
                       as_character)
  }
  if (exists("numeric", df)) {
    df$numeric <-
      dplyr::mutate_at(df$numeric,
                       dplyr::vars("min", "median", "max"),
                       format_digits)
  }
  if (exists("character", df)) {
    df$character <-
      dplyr::mutate_at(df$character,
                       dplyr::vars("min", "max"),
                       as_character)
  }
  if (exists("haven_labelled", df)) {
    df$haven_labelled <-
      dplyr::mutate_at(df$haven_labelled,
                       dplyr::vars("min", "median", "max"),
                     format_digits)
  }
  if (exists("haven_labelled_spss", df)) {
    df$haven_labelled_spss <-
      dplyr::mutate_at(df$haven_labelled_spss,
                       dplyr::vars("min", "median", "max"),
                       format_digits)
  }
  class(df) <- "list"
  df
}

skim_to_wide_labelled <- function(x){
  results <- dplyr::bind_rows(
    coerce_skimmed_summary_to_character(
    skimr::partition(skim_codebook(x))
    ), .id = "data_type"
    )
  results <- tibble::as_tibble(results)
  results
}
