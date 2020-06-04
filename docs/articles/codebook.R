## ------------------------------------------------------------------------
knitr::opts_chunk$set(warning = TRUE, message = TRUE, error = TRUE, echo = FALSE)

library(codebook)
data("bfi", package = 'codebook')
bfi$age <- rpois(nrow(bfi), 30)
ggplot2::theme_set(ggplot2::theme_bw())


## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (exists("testing")) {
	indent = '#' # ugly hack so _regression_summary can be "spun" (variables included via `r ` have to be available)
	results = data.frame()
	survey_repetition = 'single'
	reliabilities = list()
}

## ----repeated------------------------------------------------------------
if (survey_repetition != "single") {
	overview = results %>% dplyr::group_by(session) %>% 
		dplyr::summarise(
			n = sum(!is.na(session)),
			expired = sum(!is.na(expired)),
			ended = sum(!is.na(ended))
		) %>% 
		tidyr::gather(key, value, -session)
	if (length(unique(dplyr::filter(overview, key == "expired")$value)) == 1) {
		overview = dplyr::filter(overview, key != "expired")
	}
	print(
		ggplot2::ggplot(overview, ggplot2::aes(value, ..count..)) + ggplot2::geom_bar() + ggplot2::facet_wrap(~ key, nrow = 1)
	)
}

## ----starting_time-------------------------------------------------------
ggplot2::qplot(results$created) + ggplot2::scale_x_datetime("Date/time when survey was started")

## ----duration------------------------------------------------------------
if (low_vals == 0) {
  warning("Durations below 0 detected.")
}
ggplot2::qplot(duration$duration, binwidth = 0.5) + ggplot2::scale_x_continuous(paste("Duration (in minutes), excluding", high_vals, "values above median + 4*MAD"), limits = c(lower_limit, upper_limit))

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#'
}
if (exists("testing")) {
	scale <- 1:10
	reliabilities <- list()
	items <- list()
	scale_info <- list(scale_item_names = c("item1", "item2", "item3R"))
}

scale_info <- attributes(scale)

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent = ''
}
if (exists("testing")) {
	example("alpha", "psych")
  x = a4
}

## ------------------------------------------------------------------------
if (!is.null(x$total$ase)) {
  knitr::kable(data.frame(lower = x$total$raw_alpha - 1.96 * x$total$ase, 
             estimate = x$total$raw_alpha, 
             upper = x$total$raw_alpha + 1.96 * 
  x$total$ase))
}

## ------------------------------------------------------------------------
knitr::kable(x$total)

## ------------------------------------------------------------------------
knitr::kable(x$alpha.drop)

## ------------------------------------------------------------------------
knitr::kable(x$item.stats)

## ------------------------------------------------------------------------
knitr::kable(x$response.freq)

## ----reliability, results='asis'-----------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  cat(knitr::knit_print(rel, indent = paste0(indent, "####")))
}

## ----likert_setup--------------------------------------------------------
old_height <- knitr::opts_chunk$get("fig.height")
new_height <- length(scale_info$scale_item_names)
new_height <- ifelse(new_height > 20, 20, new_height)
new_height <- ifelse(new_height < 1, 1, new_height)
new_height <- ifelse(is.na(new_height) | is.nan(new_height), 
                     old_height, new_height)
knitr::opts_chunk$set(fig.height = new_height)

## ----likert--------------------------------------------------------------
likert_plot <- likert_from_items(items)
if (!is.null(likert_plot)) {
  graphics::plot(likert_plot)
}
knitr::opts_chunk$set(fig.height = old_height)

## ----setup_distribution--------------------------------------------------
binwidth <- mean(diff(sort(unique(scale))))

wrap_at <- knitr::opts_chunk$get("fig.width") * 10

## ----distribution--------------------------------------------------------
dist_plot <- plot_labelled(scale, scale_name, wrap_at)

choices <- attributes(items[[1]])$item$choices
breaks <- as.numeric(names(choices))
if (length(breaks)) {
  dist_plot <- dist_plot +
    	ggplot2::scale_x_continuous("Choices", 
	                            breaks = breaks, 
	                            labels = stringr::str_wrap(unlist(choices), 15), 
	                            limits = range(breaks))
}

dist_plot

## ----summary-------------------------------------------------------------
knitr::kable(codebook_table(items))

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#'
}
if (exists("testing")) {
	scale <- 1:10
	reliabilities <- list()
	items <- list()
	scale_info <- list(scale_item_names = c("item1", "item2", "item3R"))
}

scale_info <- attributes(scale)

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent = ''
}
if (exists("testing")) {
	example("alpha", "psych")
  x = a4
}

## ------------------------------------------------------------------------
if (!is.null(x$total$ase)) {
  knitr::kable(data.frame(lower = x$total$raw_alpha - 1.96 * x$total$ase, 
             estimate = x$total$raw_alpha, 
             upper = x$total$raw_alpha + 1.96 * 
  x$total$ase))
}

## ------------------------------------------------------------------------
knitr::kable(x$total)

## ------------------------------------------------------------------------
knitr::kable(x$alpha.drop)

## ------------------------------------------------------------------------
knitr::kable(x$item.stats)

## ------------------------------------------------------------------------
knitr::kable(x$response.freq)

## ----reliability, results='asis'-----------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  cat(knitr::knit_print(rel, indent = paste0(indent, "####")))
}

## ----likert_setup--------------------------------------------------------
old_height <- knitr::opts_chunk$get("fig.height")
new_height <- length(scale_info$scale_item_names)
new_height <- ifelse(new_height > 20, 20, new_height)
new_height <- ifelse(new_height < 1, 1, new_height)
new_height <- ifelse(is.na(new_height) | is.nan(new_height), 
                     old_height, new_height)
knitr::opts_chunk$set(fig.height = new_height)

## ----likert--------------------------------------------------------------
likert_plot <- likert_from_items(items)
if (!is.null(likert_plot)) {
  graphics::plot(likert_plot)
}
knitr::opts_chunk$set(fig.height = old_height)

## ----setup_distribution--------------------------------------------------
binwidth <- mean(diff(sort(unique(scale))))

wrap_at <- knitr::opts_chunk$get("fig.width") * 10

## ----distribution--------------------------------------------------------
dist_plot <- plot_labelled(scale, scale_name, wrap_at)

choices <- attributes(items[[1]])$item$choices
breaks <- as.numeric(names(choices))
if (length(breaks)) {
  dist_plot <- dist_plot +
    	ggplot2::scale_x_continuous("Choices", 
	                            breaks = breaks, 
	                            labels = stringr::str_wrap(unlist(choices), 15), 
	                            limits = range(breaks))
}

dist_plot

## ----summary-------------------------------------------------------------
knitr::kable(codebook_table(items))

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#'
}
if (exists("testing")) {
	scale <- 1:10
	reliabilities <- list()
	items <- list()
	scale_info <- list(scale_item_names = c("item1", "item2", "item3R"))
}

scale_info <- attributes(scale)

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent = ''
}
if (exists("testing")) {
	example("alpha", "psych")
  x = a4
}

## ------------------------------------------------------------------------
if (!is.null(x$total$ase)) {
  knitr::kable(data.frame(lower = x$total$raw_alpha - 1.96 * x$total$ase, 
             estimate = x$total$raw_alpha, 
             upper = x$total$raw_alpha + 1.96 * 
  x$total$ase))
}

## ------------------------------------------------------------------------
knitr::kable(x$total)

## ------------------------------------------------------------------------
knitr::kable(x$alpha.drop)

## ------------------------------------------------------------------------
knitr::kable(x$item.stats)

## ------------------------------------------------------------------------
knitr::kable(x$response.freq)

## ----reliability, results='asis'-----------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  cat(knitr::knit_print(rel, indent = paste0(indent, "####")))
}

## ----likert_setup--------------------------------------------------------
old_height <- knitr::opts_chunk$get("fig.height")
new_height <- length(scale_info$scale_item_names)
new_height <- ifelse(new_height > 20, 20, new_height)
new_height <- ifelse(new_height < 1, 1, new_height)
new_height <- ifelse(is.na(new_height) | is.nan(new_height), 
                     old_height, new_height)
knitr::opts_chunk$set(fig.height = new_height)

## ----likert--------------------------------------------------------------
likert_plot <- likert_from_items(items)
if (!is.null(likert_plot)) {
  graphics::plot(likert_plot)
}
knitr::opts_chunk$set(fig.height = old_height)

## ----setup_distribution--------------------------------------------------
binwidth <- mean(diff(sort(unique(scale))))

wrap_at <- knitr::opts_chunk$get("fig.width") * 10

## ----distribution--------------------------------------------------------
dist_plot <- plot_labelled(scale, scale_name, wrap_at)

choices <- attributes(items[[1]])$item$choices
breaks <- as.numeric(names(choices))
if (length(breaks)) {
  dist_plot <- dist_plot +
    	ggplot2::scale_x_continuous("Choices", 
	                            breaks = breaks, 
	                            labels = stringr::str_wrap(unlist(choices), 15), 
	                            limits = range(breaks))
}

dist_plot

## ----summary-------------------------------------------------------------
knitr::kable(codebook_table(items))

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#'
}
if (exists("testing")) {
	scale <- 1:10
	reliabilities <- list()
	items <- list()
	scale_info <- list(scale_item_names = c("item1", "item2", "item3R"))
}

scale_info <- attributes(scale)

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent = ''
}
if (exists("testing")) {
	example("alpha", "psych")
  x = a4
}

## ------------------------------------------------------------------------
if (!is.null(x$total$ase)) {
  knitr::kable(data.frame(lower = x$total$raw_alpha - 1.96 * x$total$ase, 
             estimate = x$total$raw_alpha, 
             upper = x$total$raw_alpha + 1.96 * 
  x$total$ase))
}

## ------------------------------------------------------------------------
knitr::kable(x$total)

## ------------------------------------------------------------------------
knitr::kable(x$alpha.drop)

## ------------------------------------------------------------------------
knitr::kable(x$item.stats)

## ------------------------------------------------------------------------
knitr::kable(x$response.freq)

## ----reliability, results='asis'-----------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  cat(knitr::knit_print(rel, indent = paste0(indent, "####")))
}

## ----likert_setup--------------------------------------------------------
old_height <- knitr::opts_chunk$get("fig.height")
new_height <- length(scale_info$scale_item_names)
new_height <- ifelse(new_height > 20, 20, new_height)
new_height <- ifelse(new_height < 1, 1, new_height)
new_height <- ifelse(is.na(new_height) | is.nan(new_height), 
                     old_height, new_height)
knitr::opts_chunk$set(fig.height = new_height)

## ----likert--------------------------------------------------------------
likert_plot <- likert_from_items(items)
if (!is.null(likert_plot)) {
  graphics::plot(likert_plot)
}
knitr::opts_chunk$set(fig.height = old_height)

## ----setup_distribution--------------------------------------------------
binwidth <- mean(diff(sort(unique(scale))))

wrap_at <- knitr::opts_chunk$get("fig.width") * 10

## ----distribution--------------------------------------------------------
dist_plot <- plot_labelled(scale, scale_name, wrap_at)

choices <- attributes(items[[1]])$item$choices
breaks <- as.numeric(names(choices))
if (length(breaks)) {
  dist_plot <- dist_plot +
    	ggplot2::scale_x_continuous("Choices", 
	                            breaks = breaks, 
	                            labels = stringr::str_wrap(unlist(choices), 15), 
	                            limits = range(breaks))
}

dist_plot

## ----summary-------------------------------------------------------------
knitr::kable(codebook_table(items))

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#'
}
if (exists("testing")) {
	scale <- 1:10
	reliabilities <- list()
	items <- list()
	scale_info <- list(scale_item_names = c("item1", "item2", "item3R"))
}

scale_info <- attributes(scale)

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent = ''
}
if (exists("testing")) {
	example("alpha", "psych")
  x = a4
}

## ------------------------------------------------------------------------
if (!is.null(x$total$ase)) {
  knitr::kable(data.frame(lower = x$total$raw_alpha - 1.96 * x$total$ase, 
             estimate = x$total$raw_alpha, 
             upper = x$total$raw_alpha + 1.96 * 
  x$total$ase))
}

## ------------------------------------------------------------------------
knitr::kable(x$total)

## ------------------------------------------------------------------------
knitr::kable(x$alpha.drop)

## ------------------------------------------------------------------------
knitr::kable(x$item.stats)

## ------------------------------------------------------------------------
knitr::kable(x$response.freq)

## ----reliability, results='asis'-----------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  cat(knitr::knit_print(rel, indent = paste0(indent, "####")))
}

## ----likert_setup--------------------------------------------------------
old_height <- knitr::opts_chunk$get("fig.height")
new_height <- length(scale_info$scale_item_names)
new_height <- ifelse(new_height > 20, 20, new_height)
new_height <- ifelse(new_height < 1, 1, new_height)
new_height <- ifelse(is.na(new_height) | is.nan(new_height), 
                     old_height, new_height)
knitr::opts_chunk$set(fig.height = new_height)

## ----likert--------------------------------------------------------------
likert_plot <- likert_from_items(items)
if (!is.null(likert_plot)) {
  graphics::plot(likert_plot)
}
knitr::opts_chunk$set(fig.height = old_height)

## ----setup_distribution--------------------------------------------------
binwidth <- mean(diff(sort(unique(scale))))

wrap_at <- knitr::opts_chunk$get("fig.width") * 10

## ----distribution--------------------------------------------------------
dist_plot <- plot_labelled(scale, scale_name, wrap_at)

choices <- attributes(items[[1]])$item$choices
breaks <- as.numeric(names(choices))
if (length(breaks)) {
  dist_plot <- dist_plot +
    	ggplot2::scale_x_continuous("Choices", 
	                            breaks = breaks, 
	                            labels = stringr::str_wrap(unlist(choices), 15), 
	                            limits = range(breaks))
}

dist_plot

## ----summary-------------------------------------------------------------
knitr::kable(codebook_table(items))

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so _regression_summary can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}
item_attributes <- attributes(item)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missings------------------------------------------------------
show_missings <- FALSE
if (haven::is.labelled(item)) {
  missings <- item[is.na(haven::zap_missing(item))]
  if (is.numeric(item)) {
    show_missings <- length(unique(haven::na_tag(missings))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(attributes(item)$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- attributes(item)[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(na.omit(item)) > length(non_missing_choices)) {
		choices <- unique(na.omit(item))
		names(choices) <- choices
		non_missing_choices <- choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10

## ----distribution--------------------------------------------------------
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"

if (is.numeric(item) || dplyr::n_distinct(item) < 20) {
  plot_labelled(na.omit(item), item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item), " unique, categorical values, so not shown.")
}
knitr::opts_chunk$set(fig.height = old_height)

## ----summary-------------------------------------------------------------
df = data.frame(item)
names(df) = item_name
knitr::kable(codebook_table(df))

## ------------------------------------------------------------------------
if (show_missings) {
  plot_labelled(missings, item_name, wrap_at)
}

## ----items---------------------------------------------------------------
if (!is.null(item_info)) {
  item_info$label_parsed <- item_info$choices <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(as.data.frame(t(item_info)))
}

## ----choices-------------------------------------------------------------
if (!is.null(choices) && length(choices) < 30) {
	knitr::kable(as.list(choices))
}

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack
}
if (exists("testing")) {
	results <- data.frame()
	survey_repetition <- 'single'
	reliabilities <- list()
	md_pattern <- data.frame()
}

## ----missingness_all_setup-----------------------------------------------
if (  exists("ended", results) &&
  exists("expired", results)) {
  finisher_results <- dplyr::filter(results, !is.na(.data$ended))
} else {
  finisher_results <- results
  warning("Could not figure out who finished the surveys, because the ",
          "variables expired and ended were missing.")
}
if (length(md_pattern)) {
  knitr::kable(md_pattern)
}

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack
}
if (exists("testing")) {
	results <- data.frame()
	metadata_table = data.frame()
}

## ----items, message=TRUE-------------------------------------------------
metadata_table <- dplyr::mutate(metadata_table, 
                name = paste0('<a href="#', .data$name, '">', .data$name, '</a>'))
export_table(metadata_table)

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (exists("testing")) {
	indent = '#' # ugly hack so _regression_summary can be "spun" (variables included via `r ` have to be available)
	results = data.frame()
}

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack
}
if (exists("testing")) {
	results <- data.frame()
	survey_repetition <- 'single'
	reliabilities <- list()
	missingness_report <- ''
	survey_overview <- '' 
	scales_items <- c()
}

## ------------------------------------------------------------------------
knitr::asis_output(survey_overview)

## ----scales--------------------------------------------------------------
knitr::asis_output(paste0(scales_items, sep = "\n\n\n", collapse = "\n\n\n"))

## ------------------------------------------------------------------------
missingness_report

## ------------------------------------------------------------------------
items

## ------------------------------------------------------------------------
jsonld

## ----cb------------------------------------------------------------------
codebook(bfi)

