## ----message = FALSE----------------------------------------------------------
knit_by_pkgdown <- !is.null(knitr::opts_chunk$get("fig.retina"))
knitr::opts_chunk$set(
  warning = TRUE, # show warnings during codebook generation
  message = TRUE, # show messages during codebook generation
  error = TRUE, # do not interrupt codebook generation in case of errors,
                # TRUE is usually better for debugging
  echo = TRUE  # show R code
)
ggplot2::theme_set(ggplot2::theme_bw())

## ----include=FALSE, echo=FALSE------------------------------------------------
knitr::opts_chunk$set(
  error = FALSE
)

## -----------------------------------------------------------------------------
library(codebook)
codebook_data <- codebook::bfi

## -----------------------------------------------------------------------------
codebook_data <- rio::import("https://osf.io/s87kd/download", "csv")

## -----------------------------------------------------------------------------
attributes(codebook_data$C5)$label <- "Waste my time."

## -----------------------------------------------------------------------------
library(labelled)

## -----------------------------------------------------------------------------

var_label(codebook_data$C5) <- "Waste my time."

## -----------------------------------------------------------------------------
val_labels(codebook_data$C1) <- c("Very Inaccurate" = 1, "Very Accurate" = 6)

## -----------------------------------------------------------------------------
dict <- rio::import("https://osf.io/cs678/download", "csv")

## ----warning=FALSE, message=FALSE---------------------------------------------
library(dplyr)

## -----------------------------------------------------------------------------
var_label(codebook_data) <- list(
		C5 = "Waste my time.", 
		C1 = "Am exacting in my work."
)

## -----------------------------------------------------------------------------
var_label(codebook_data) <- dict %>% select(variable, label) %>% dict_to_list()

## -----------------------------------------------------------------------------
val_labels(codebook_data$gender) <- c("male" = 1, "female" = 2)
val_labels(codebook_data$education) <- c("in high school" = 1,
   "finished high school" = 2,
              "some college" = 3, 
			   "college graduate" = 4, 
              "graduate degree" = 5)

## -----------------------------------------------------------------------------
add_likert_labels <- function(x) {
  val_labels(x) <- c("Very Inaccurate" = 1, 
                  "Moderately Inaccurate" = 2, 
                  "Slightly Inaccurate" = 3,
                  "Slightly Accurate" = 4,
                  "Moderately Accurate" = 5,
                  "Very Accurate" = 6)
  x
}

## -----------------------------------------------------------------------------
likert_items <- dict %>% filter(Big6 != "") %>% pull(variable)

## -----------------------------------------------------------------------------
codebook_data <- codebook_data %>% mutate_at(likert_items,  add_likert_labels)

## -----------------------------------------------------------------------------
codebook_data$extraversion <- codebook_data %>% select(E1:E5) %>% aggregate_and_document_scale()

## -----------------------------------------------------------------------------
reversed_items <- dict %>% filter(Keying == -1) %>% pull(variable)

## -----------------------------------------------------------------------------
codebook_data <- codebook_data %>% 
  rename_at(reversed_items,  add_R)

## -----------------------------------------------------------------------------
codebook_data <- codebook_data %>% 
	mutate_at(vars(matches("\\dR$")), reverse_labelled_values)

## -----------------------------------------------------------------------------
codebook_data$extraversion <- codebook_data %>% select(E1R:E5) %>% aggregate_and_document_scale()

## -----------------------------------------------------------------------------
codebook_data$plasticity <- codebook_data %>% select(E1R:E5, O1:O5R) %>% aggregate_and_document_scale() 

## -----------------------------------------------------------------------------
metadata(codebook_data)$name <- "25 Personality items representing 5 factors"
metadata(codebook_data)$description <- "25 personality self report items taken from the International Personality Item Pool (ipip.ori.org)[...]"

## -----------------------------------------------------------------------------
metadata(codebook_data)$identifier <- "https://dx.doi.org/10.17605/OSF.IO/K39BG"

## -----------------------------------------------------------------------------
metadata(codebook_data)$creator <- "William Revelle"
metadata(codebook_data)$citation <- "Revelle, W., Wilt, J., & Rosenthal, A. (2010). Individual differences in cognition: New methods for examining the personality-cognition link. In A. Gruszka, G. Matthews, & B. Szymura (Eds.), Handbook of individual differences in cognition: Attention, memory, and executive control (pp. 27–49). New York, NY: Springer."
metadata(codebook_data)$url <- "https://CRAN.R-project.org/package=psych"

## -----------------------------------------------------------------------------
metadata(codebook_data)$datePublished <- "2010-01-01"
metadata(codebook_data)$temporalCoverage <- "Spring 2010" 
metadata(codebook_data)$spatialCoverage <- "Online" 

## ----eval=FALSE---------------------------------------------------------------
#  rio::export(codebook_data, "bfi.rds") # to R data structure file

## ----eval=FALSE---------------------------------------------------------------
#  rio::export(codebook_data, "bfi.sav") # to SPSS file
#  rio::export(codebook_data, "bfi.dta") # to Stata file

## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE) # don't print codebook code

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (exists("testing")) {
	indent = '#' # ugly hack so _regression_summary can be "spun" (variables included via `r ` have to be available)
	results = data("bfi")
	metadata(results)$description <- data_description_default(bfi)
}

meta <- metadata(results)
description <- meta$description
meta <- recursive_escape(meta)

## ----results='asis'-----------------------------------------------------------
if (exists("name", meta)) {
  glue::glue_data(
    "__Dataset name__: {name}",
    .x = meta)
}

## ----results='asis'-----------------------------------------------------------
cat(description)

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("temporalCoverage", meta)) {
  glue::glue_data(
    "- __Temporal Coverage__: {temporalCoverage}",
    .x = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("spatialCoverage", meta)) {
  glue::glue_data(
    "- __Spatial Coverage__: {spatialCoverage}",
    .x = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("citation", meta)) {
  glue::glue_data(
    "- __Citation__: {citation}",
    .x = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("url", meta)) {
  glue::glue_data(
    "- __URL__: [{url}]({url})",
    .x = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("identifier", meta)) {
  if (stringr::str_detect(meta$identifier, "^doi:")) {
    meta$identifier <- paste0('<a href="https://dx.doi.org/', 
      stringr::str_match(meta$identifier, "^doi:(.+)")[,2], '">', 
      meta$identifier, '</a>')
  }
  glue::glue_data(
    "- __Identifier__: {identifier}",
    .x = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("datePublished", meta)) {
  glue::glue_data(
    "- __Date published__: {datePublished}",
    .x = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("creator", meta)) {
  cat("- __Creator__:")
  knitr::kable(tibble::enframe(meta$creator))
}

## -----------------------------------------------------------------------------
meta <- meta[setdiff(names(meta),
                     c("creator", "datePublished", "identifier",
                       "url", "citation", "spatialCoverage", 
                       "temporalCoverage", "description", "name"))]
if(length(meta)) {
  knitr::kable(meta)
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#'
}
if (exists("testing")) {
  scale_name <- safe_name <- "bla"
	scale <- 1:10
	reliabilities <- list()
	items <- data.frame(bla1 = 1:10, bla2 = 1:10, bla3R = 10:1)
	scale_info <- list(scale_item_names = c("bla1", "bla2", "bla3R"))
}
html_scale_name <- recursive_escape(scale_name)
names(items) <- recursive_escape(names(items))
scale_info <- recursive_escape(attributes(scale))

## ----likert_setup-------------------------------------------------------------
old_height <- knitr::opts_chunk$get("fig.height")
new_height <- length(scale_info$scale_item_names)
new_height <- ifelse(new_height > 20, 20, new_height)
new_height <- ifelse(new_height < 1, 1, new_height)
new_height <- ifelse(is.na(new_height) | is.nan(new_height), 
                     old_height, new_height)

## ----likert,fig.height=new_height,fig.cap=paste("Likert plot of scale", html_scale_name, "items")----
if (dplyr::n_distinct(na.omit(unlist(items))) < 12) {
  likert_plot <- likert_from_items(items)
  if (!is.null(likert_plot)) {
    graphics::plot(likert_plot)
  }
}

## ----distribution,fig.cap=paste("Distribution of scale", html_scale_name)-----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
dist_plot <- plot_labelled(scale, scale_name, wrap_at)

choices <- attributes(items[[1]])$item$choices
breaks <- as.numeric(names(choices))
if (length(breaks)) {
  suppressMessages( # ignore message about overwriting x axis
  dist_plot <- dist_plot +
    	ggplot2::scale_x_continuous("values", 
	                            breaks = breaks, 
	                            labels = stringr::str_wrap(unlist(choices), ceiling(wrap_at * 0.21))) +
      ggplot2::expand_limits(x = range(breaks)))
  
}

dist_plot

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent = ''
}
if (exists("testing")) {
	example("alpha", "psych")
  x = a4
}

## -----------------------------------------------------------------------------
if (!is.null(x$total$ase)) {
  knitr::kable(data.frame(lower = x$total$raw_alpha - 1.96 * x$total$ase, 
             estimate = x$total$raw_alpha, 
             upper = x$total$raw_alpha + 1.96 * 
  x$total$ase))
}

## -----------------------------------------------------------------------------
knitr::kable(x$total)

## -----------------------------------------------------------------------------
rownames(x$alpha.drop) <- recursive_escape(rownames(x$alpha.drop))
knitr::kable(x$alpha.drop)

## -----------------------------------------------------------------------------
rownames(x$item.stats) <- recursive_escape(rownames(x$item.stats))
knitr::kable(x$item.stats)

## -----------------------------------------------------------------------------
rownames(x$response.freq) <- recursive_escape(rownames(x$response.freq))
knitr::kable(x$response.freq)

## ----reliability, results='asis'----------------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  suppressMessages(rel_output <- knitr::knit_print(rel))
  cat(rel_output)
}

## ----summary------------------------------------------------------------------
for (i in seq_along(names(items))) {
  attributes(items[[i]]) = recursive_escape(attributes(items[[i]]))
}
escaped_table(codebook_table(items))

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#'
}
if (exists("testing")) {
  scale_name <- safe_name <- "bla"
	scale <- 1:10
	reliabilities <- list()
	items <- data.frame(bla1 = 1:10, bla2 = 1:10, bla3R = 10:1)
	scale_info <- list(scale_item_names = c("bla1", "bla2", "bla3R"))
}
html_scale_name <- recursive_escape(scale_name)
names(items) <- recursive_escape(names(items))
scale_info <- recursive_escape(attributes(scale))

## ----likert_setup-------------------------------------------------------------
old_height <- knitr::opts_chunk$get("fig.height")
new_height <- length(scale_info$scale_item_names)
new_height <- ifelse(new_height > 20, 20, new_height)
new_height <- ifelse(new_height < 1, 1, new_height)
new_height <- ifelse(is.na(new_height) | is.nan(new_height), 
                     old_height, new_height)

## ----likert,fig.height=new_height,fig.cap=paste("Likert plot of scale", html_scale_name, "items")----
if (dplyr::n_distinct(na.omit(unlist(items))) < 12) {
  likert_plot <- likert_from_items(items)
  if (!is.null(likert_plot)) {
    graphics::plot(likert_plot)
  }
}

## ----distribution,fig.cap=paste("Distribution of scale", html_scale_name)-----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
dist_plot <- plot_labelled(scale, scale_name, wrap_at)

choices <- attributes(items[[1]])$item$choices
breaks <- as.numeric(names(choices))
if (length(breaks)) {
  suppressMessages( # ignore message about overwriting x axis
  dist_plot <- dist_plot +
    	ggplot2::scale_x_continuous("values", 
	                            breaks = breaks, 
	                            labels = stringr::str_wrap(unlist(choices), ceiling(wrap_at * 0.21))) +
      ggplot2::expand_limits(x = range(breaks)))
  
}

dist_plot

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent = ''
}
if (exists("testing")) {
	example("alpha", "psych")
  x = a4
}

## -----------------------------------------------------------------------------
if (!is.null(x$total$ase)) {
  knitr::kable(data.frame(lower = x$total$raw_alpha - 1.96 * x$total$ase, 
             estimate = x$total$raw_alpha, 
             upper = x$total$raw_alpha + 1.96 * 
  x$total$ase))
}

## -----------------------------------------------------------------------------
knitr::kable(x$total)

## -----------------------------------------------------------------------------
rownames(x$alpha.drop) <- recursive_escape(rownames(x$alpha.drop))
knitr::kable(x$alpha.drop)

## -----------------------------------------------------------------------------
rownames(x$item.stats) <- recursive_escape(rownames(x$item.stats))
knitr::kable(x$item.stats)

## -----------------------------------------------------------------------------
rownames(x$response.freq) <- recursive_escape(rownames(x$response.freq))
knitr::kable(x$response.freq)

## ----reliability, results='asis'----------------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  suppressMessages(rel_output <- knitr::knit_print(rel))
  cat(rel_output)
}

## ----summary------------------------------------------------------------------
for (i in seq_along(names(items))) {
  attributes(items[[i]]) = recursive_escape(attributes(items[[i]]))
}
escaped_table(codebook_table(items))

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack so it can be "spun" (variables included via `r ` have to be available)
}
if (exists("testing")) {
	item <- 1:10
	item_name <- safe_name <- "yay"
	attributes(item) <- list(label = 'yayya')
}

item_attributes <- attributes(item)
item_attributes <- recursive_escape(item_attributes)
html_item_name <- recursive_escape(item_name)
item_label <- ifelse(is.null(item_attributes) || is.null(item_attributes$label), 
                     "", item_attributes$label)
item_info <- item_attributes$item
choices <- item_attributes$labels

## ----setup_missing_values-----------------------------------------------------
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  !is.null(item_info) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

fig_height_dist <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is_numeric_or_time_var(item_nomiss) || many_labels
  
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  if(!(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss))) {
    choice_multiplier <- fig_height_dist/6.5
  	fig_height_dist <- 2 + choice_multiplier * length(non_missing_choices)
  	fig_height_dist <- ifelse(fig_height_dist > 20, 20, fig_height_dist)
  	fig_height_dist <- ifelse(fig_height_dist < 1, 1, fig_height_dist)
  }
}


## ----distribution,fig.height=fig_height_dist,fig.cap=paste("Distribution of values for", html_item_name)----
wrap_at <- knitr::opts_chunk$get("fig.width") * 10
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (!could_disclose_unique_values(item_nomiss)) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
  if (is.character(item_nomiss)) {
      char_count <- stringr::str_count(item_nomiss)
      attributes(char_count)$label <- item_label
      plot_labelled(char_count, 
                    item_name, wrap_at, FALSE, trans = "log1p", "characters")
  } else {
	  cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
  }
}

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
cb_table <- codebook_table(df)
if(!is.null(choices)) {
  cb_table$value_labels <- NULL
}
escaped_table(cb_table)

## ----missing_values,fig.cap=paste("Plot of missing values for", html_item_name)----
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (is.null(choices)) {
    choices <- tibble::enframe(item_info$choices)
  }
  item_info$choices <- NULL
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(purrr::flatten_df(item_info), caption = "Item options")
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
  try({choices <- tibble::enframe(choices)}, silent = TRUE)
  knitr::kable(choices, caption = "Response choices")
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack
}
if (exists("testing")) {
	results <- data.frame()
	survey_repetition <- 'single'
	reliabilities <- list()
	md_pattern <- data.frame()
}

## ----missingness_all_setup----------------------------------------------------
if (length(md_pattern)) {
  if (knitr::is_html_output() && requireNamespace("rmarkdown", quietly = TRUE)) {
    rmarkdown::paged_table(md_pattern, options = list(rows.print = 10))
  } else {
    knitr::kable(md_pattern)
  }
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (!exists("indent")) {
	indent <- '#' # ugly hack
}
if (exists("testing")) {
	results <- data.frame()
	survey_repetition <- 'single'
	reliabilities <- list()
	missingness_report <- ''
	data_info <- '' 
	survey_overview <- '' 
	scales_items <- c()
	detailed_items <- TRUE
	detailed_scales <- TRUE
}

## -----------------------------------------------------------------------------
knitr::asis_output(data_info)

## -----------------------------------------------------------------------------
knitr::asis_output(survey_overview)

## ----scales-------------------------------------------------------------------
if (detailed_variables || detailed_scales) {
  knitr::asis_output(paste0(scales_items, sep = "\n\n\n", collapse = "\n\n\n"))
}

## -----------------------------------------------------------------------------
missingness_report

## -----------------------------------------------------------------------------
items

## -----------------------------------------------------------------------------
jsonld

## ----cb-----------------------------------------------------------------------
codebook(codebook_data, survey_repetition = "single", indent = "##",
         metadata_table = knit_by_pkgdown, metadata_json = knit_by_pkgdown)

## -----------------------------------------------------------------------------
if (!knit_by_pkgdown) {
  codebook:::escaped_table(codebook_table(codebook_data))
}

