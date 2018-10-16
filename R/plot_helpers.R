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

  likert::likert(as.data.frame(items))
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
plot_labelled <- function(item, item_name = NULL,
                          wrap_at = 70, go_vertical = FALSE) {
  wrap_at_ticks <- ceiling(wrap_at * 0.21)
  if (is.null(item_name)) {
    item_name <- deparse(substitute(item))
  }

  choices <- attributes(item)[["labels"]]
  item_label <- attributes(item)[["label"]]
  if (is.null(item_label)) {
    item_label <- ""
  } else {
    item_label <- stringr::str_wrap(item_label, width = wrap_at)
  }

  item_nomiss <- haven::zap_missing(item)
  nonmissing_unique_values <- length(unique(item_nomiss))
  nonmissing_choices <- attributes(item_nomiss)[["labels"]]

  if (all(is.na(item_nomiss))) {
    if (has_labels(item)) {
      item <- as_factor(item, "both")
    } else {
      item <- factor(item, exclude = NULL)
    }
  }

  # possible inputs
  # * lbl+dbl -> continuous x axis with binning
  #   * without labelled missing values -> can't put on same x axis
  #   * with labelled missing values -> can't put on same x axis
  # * lbl+chr -> discrete x axis
  #    * with our without labelled missing values -> can all go on same x axis
  # * chr -> discrete
  # * factor -> discrete
  # * double/integer -> continuous, with binning

  if (has_labels(item)) {
    # for labelled values, make labels look proper
    label_how <- "both"
    if (length(choices)) {
      # wrap
      names(nonmissing_choices) <-
        stringr::str_wrap(names(nonmissing_choices), wrap_at_ticks)
      choices <- attributes(item)[["labels"]]

      # don't duplicate [1]/1: in front
      if (length(nonmissing_choices) && all(stringr::str_match(
        names(stats::na.omit(choices)), "\\[?([0-9-]+)(\\]|:)")[, 2] ==
        stats::na.omit(choices), na.rm = TRUE)) {
        label_how <- "default"
      }
    }


    type <- typeof(item)
    if (type == "double" || type == "integer") {
      item <- haven::zap_missing(haven::zap_labels(item))

      # are ALL values labelled?
      if (nonmissing_unique_values <= length(nonmissing_choices)) {
        dist_plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = item)) +
          ggplot2::geom_bar(na.rm = TRUE) +
          ggplot2::scale_x_continuous("values", breaks = nonmissing_choices,
                                    labels = names(nonmissing_choices)) +
          ggplot2::expand_limits(x = range(nonmissing_choices))
      } else {
        if (nonmissing_unique_values <= 10) {
          breaks <- unique(item_nomiss)
          names(breaks) <- breaks
          breaks <- c(nonmissing_choices, breaks)
          breaks <- breaks[!duplicated(breaks)]
        } else {
          rng <- range(item_nomiss, na.rm = TRUE)
          breaks <- labeling::extended(rng[1], rng[2],  5, only.loose = FALSE)
          names(breaks) <- breaks
          breaks <- c(nonmissing_choices, breaks)
          breaks <- breaks[!duplicated(breaks)]
        }
        bins <- nonmissing_unique_values
        bins <- ifelse(bins > 30, 30, bins)
        dist_plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = item_nomiss)) +
          ggplot2::geom_histogram(bins = bins, na.rm = TRUE) +
          ggplot2::scale_x_continuous("values", breaks = breaks,
                                      labels = names(breaks)) +
          ggplot2::expand_limits(x = range(breaks))
      }
    } else if (type == "character") {
      if (any(names(choices) != choices)) {
        label_how <- "both"
      }
      item <- as_factor(item, levels = label_how)

      dist_plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = item)) +
        ggplot2::geom_bar() +
        ggplot2::xlab("values")
    }
  } else if (is.factor(item)) {
    levels(item) <- stringr::str_wrap(levels(item), 15)

    dist_plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = item)) +
      ggplot2::geom_bar() +
      ggplot2::xlab("values") +
      ggplot2::expand_limits(x = levels(item))
  } else if (is.character(item)) {
    item <- stringr::str_wrap(as.character(item), wrap_at_ticks)

    dist_plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = item)) +
      ggplot2::geom_bar() +
      ggplot2::xlab("values")
  } else if (is.numeric(item)) {
    if (length(unique(item)) < 40) {
      bar_geom <- ggplot2::geom_bar(na.rm = TRUE)
    } else {
      bar_geom <- ggplot2::geom_histogram(bins = 30, na.rm = TRUE)
    }
    dist_plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = item_nomiss)) +
      bar_geom +
      ggplot2::scale_x_continuous("values")
  } else {
    dist_plot <- ggplot2::qplot(item) + ggplot2::xlab("values")
  }


  if ( go_vertical ) {
    dist_plot <- dist_plot + ggplot2::coord_flip()
  }

  dist_plot +
    ggplot2::ggtitle(item_name,
                     subtitle = item_label)
}
