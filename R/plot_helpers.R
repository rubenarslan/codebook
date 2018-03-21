#' Derive a likert object from items
#'
#' Pass a data.frame containing several items composing one scale, get a
#' [likert::likert()] object, which you can plot.
#' Intelligently makes use of labels and value labels if present.
#'
#' @param items a data frame of items composing one scale
#'
#' @export
#' @examples
#' data("bfi", package = "codebook")
#' open_items <- paste0("BFIK_open_",1:4)
#' graphics::plot(likert_from_items(bfi[, open_items]))
likert_from_items <- function(items) {
  if (!methods::is(items, "data.frame") || ncol(items) < 1) {
    stop("The items argument has to be a data frame.")
  }
  items <- haven::zap_missing(items)
  for (i in seq_len(ncol(items))) {
    if ( !is.null(attributes(items[[i]])$labels)) {
      labels <- names(attributes(items[[i]])$labels)
      names(attributes(items[[i]])$labels) <- stringr::str_wrap(labels,
                                                                width = 15)
      items[[i]] <- haven::as_factor(items[[i]])
    } else {
      items[[i]] <- factor(items[[i]], levels = unique(unlist(items)))
    }
    if (!is.null(attributes(items[[i]])$label)) {
      item_names <- names(items)
      item_names[i] <- stringr::str_wrap(
          paste0(attributes(items[[i]])$label, " [", names(items)[i], "]"),
        width = 50)
      names(items) <- item_names
    }
  }

  likert::likert(data.frame(items, check.names = FALSE))
}

#' Plot labelled vector
#'
#' Plot a labelled vector, making use of the variable name, label and value
#' labels to make the plot more readable. This function also works for other
#' vectors, but provides little benefit.
#'
#' @param item a vector
#' @param item_name item name, defaults to name of first argument
#' @param wrap_at the subtitle (the label) will be wrapped at this number of characters
#' @param go_vertical defaults to FALSE. Whether to show choices on the Y axis instead.
#'
#' @export
#' @examples
#' data("bfi", package = "codebook")
#' plot_labelled(bfi$BFIK_open_1)
plot_labelled <- function(item, item_name = deparse(substitute(item)),
                          wrap_at = 50, go_vertical = FALSE) {
  choices <- attributes(item)[["labels"]]
  item_label <- attributes(item)[["label"]]

  label_how <- "both"

  if (length(choices)) {
    names(attributes(item)[["labels"]]) <-
      stringr::str_wrap(names(choices), 20)
    choices <- attributes(item)[["labels"]]

    if (all(stringr::str_match(
      names(stats::na.omit(choices)), "\\[?([0-9-]+)(\\]|:)")[, 2] ==
      stats::na.omit(choices), na.rm = TRUE)) {
      label_how = "default"
    }

  }

  if (haven::is.labelled(item)) {
    item <- haven::as_factor(item, levels = label_how)
  }

  if (is.character(item)) {
    item <- stringr::str_wrap(as.character(item), 15)
  }
  dist_plot <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = item)) +
    ggplot2::stat_bin() +
    ggplot2::ggtitle(item_name,
              subtitle = stringr::str_wrap(item_label, width = wrap_at)) +
    ggplot2::scale_y_continuous() +
    ggplot2::xlab("Values")

  if ( go_vertical ) {
    dist_plot <- dist_plot + ggplot2::coord_flip()
  }

  dist_plot
}
