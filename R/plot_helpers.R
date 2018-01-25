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


plot_labelled = function(item, item_name = deparse(substitute(item)), wrap_at = 50, go_vertical = FALSE) {
  choices = attributes(item)[["labels"]]
  item_label = attributes(item)[["label"]]
  if (any(is.na(item))) {
    item = haven::as_factor(item, 'both')
    choices = unique(item)
    names(choices) = choices
  }
  dist_plot = ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = item)) +
    ggplot2::stat_bin() +
    ggplot2::ggtitle(item_name, subtitle = stringr::str_wrap(item_label, width = wrap_at)) +
    ggplot2::scale_y_continuous()

  if (!is.null(choices) &&
      !any(is.na(choices)) &&
      any(names(choices) != unlist(choices))) {

    preceding_number = paste0("[", choices, "] ")
    if (all(substr(names(choices), 1, 3) == substr(preceding_number, 1, 3))) {
      labels = names(choices)
    } else {
      labels = paste0("[", choices, "] ", names(choices))
    }
    x_axis = ggplot2::scale_x_discrete("Choices", labels = stringr::str_wrap(labels, 20), limits = choices)
  } else if (!is.null(choices)) {
    x_axis = ggplot2::scale_x_discrete("Choices", labels = stringr::str_wrap(as.character(choices), 15), limits = choices)
  } else {
    x_axis = ggplot2::xlab("Values")
  }
  dist_plot = dist_plot + x_axis

  if ( go_vertical ) {
    dist_plot = dist_plot + ggplot2::coord_flip()
  }

  dist_plot
}
