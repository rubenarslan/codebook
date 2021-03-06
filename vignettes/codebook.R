## ----warning=FALSE,message=FALSE----------------------------------------------
knit_by_pkgdown <- !is.null(knitr::opts_chunk$get("fig.retina"))
knitr::opts_chunk$set(warning = FALSE, message = TRUE, error = FALSE)
ggplot2::theme_set(ggplot2::theme_bw())

library(codebook)
data("bfi", package = 'codebook')
if (!knit_by_pkgdown) {
  library(dplyr)
    bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_open"),
                        -starts_with("BFIK_consc"))
}
set.seed(1)
bfi$age <- rpois(nrow(bfi), 30)
library(labelled)
var_label(bfi$age) <- "Alter"

## -----------------------------------------------------------------------------
metadata(bfi)$name <- "MOCK Big Five Inventory dataset (German metadata demo)"
metadata(bfi)$description <- "a small mock Big Five Inventory dataset"
metadata(bfi)$identifier <- "doi:10.5281/zenodo.1326520"
metadata(bfi)$datePublished <- "2016-06-01"
metadata(bfi)$creator <- list(
      "@type" = "Person",
      givenName = "Ruben", familyName = "Arslan",
      email = "ruben.arslan@gmail.com", 
      affiliation = list("@type" = "Organization",
        name = "MPI Human Development, Berlin"))
metadata(bfi)$citation <- "Arslan (2016). Mock BFI data."
metadata(bfi)$url <- "https://rubenarslan.github.io/codebook/articles/codebook.html"
metadata(bfi)$temporalCoverage <- "2016" 
metadata(bfi)$spatialCoverage <- "Goettingen, Germany" 

## -----------------------------------------------------------------------------
# We don't want to look at the code in the codebook.
knitr::opts_chunk$set(warning = TRUE, message = TRUE, echo = FALSE)

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
  glue::glue(
    "__Dataset name__: {name}",
    .envir = meta)
}

## ----results='asis'-----------------------------------------------------------
cat(description)

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("temporalCoverage", meta)) {
  glue::glue(
    "- __Temporal Coverage__: {temporalCoverage}",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("spatialCoverage", meta)) {
  glue::glue(
    "- __Spatial Coverage__: {spatialCoverage}",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("citation", meta)) {
  glue::glue(
    "- __Citation__: {citation}",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("url", meta)) {
  glue::glue(
    "- __URL__: [{url}]({url})",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("identifier", meta)) {
  if (stringr::str_detect(meta$identifier, "^doi:")) {
    meta$identifier <- paste0('<a href="https://dx.doi.org/', 
      stringr::str_match(meta$identifier, "^doi:(.+)")[,2], '">', 
      meta$identifier, '</a>')
  }
  glue::glue(
    "- __Identifier__: {identifier}",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("datePublished", meta)) {
  glue::glue(
    "- __Date published__: {datePublished}",
    .envir = meta)
}

## ----results='asis', echo = FALSE---------------------------------------------
if (exists("creator", meta)) {
  cat("- __Creator__:")
  knitr::kable(meta$creator)
}

## -----------------------------------------------------------------------------
meta <- meta[setdiff(names(meta),
                     c("creator", "datePublished", "identifier",
                       "url", "citation", "spatialCoverage", 
                       "temporalCoverage", "description", "name"))]
knitr::kable(meta)

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (exists("testing")) {
	indent = '#' # ugly hack so _regression_summary can be "spun" (variables included via `r ` have to be available)
	results = data.frame()
	survey_repetition = 'single'
	reliabilities = list()
}

## ----repeated-----------------------------------------------------------------
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

## ----starting_time------------------------------------------------------------
ggplot2::qplot(results$created) + ggplot2::scale_x_datetime("Date/time when survey was started")

## ----duration-----------------------------------------------------------------
if (low_vals == 0) {
  warning("Durations below 0 detected.")
}
ggplot2::qplot(duration$duration, binwidth = 0.5) + ggplot2::scale_x_continuous(paste("Duration (in minutes), excluding", high_vals, "values above median + 4*MAD"), limits = c(lower_limit, upper_limit))

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
knitr::opts_chunk$set(fig.height = new_height)

## ----likert-------------------------------------------------------------------
if (dplyr::n_distinct(na.omit(unlist(items))) < 12) {
  likert_plot <- likert_from_items(items)
  if (!is.null(likert_plot)) {
    graphics::plot(likert_plot)
  }
}
knitr::opts_chunk$set(fig.height = old_height)

## ----setup_distribution-------------------------------------------------------
wrap_at <- knitr::opts_chunk$get("fig.width") * 10

## ----distribution-------------------------------------------------------------
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
	testDat <- bfi %>% dplyr::select(dplyr::starts_with("BFIK_open_"))
  x = userfriendlyscience::scaleDiagnosis(testDat, 
                                          scaleReliability.ci = FALSE)
	testDat <- bfi %>% dplyr::select(dplyr::starts_with("BFIK_consc_"))
  x = userfriendlyscience::scaleDiagnosis(testDat, 
                                          scaleReliability.ci = TRUE)
}

## -----------------------------------------------------------------------------
coefs <- x$scaleReliability$output$dat %>% 
  tidyr::gather(index, estimate) %>% 
  dplyr::filter(index != "n.items", index != "n.observations") %>% 
  dplyr::mutate(index = stringr::str_to_title(
    stringr::str_replace_all(index,
      stringr::fixed("."), " ")))

cis <- coefs %>% 
  dplyr::filter(stringr::str_detect(index, " Ci ")) %>% 
  tidyr::separate(index, c("index", "hilo"), sep = " Ci ") %>% 
  tidyr::spread(hilo, estimate)
if (nrow(cis)) {
  cis <- cis %>% dplyr::rename(
    `Lower 95% CI` = .data$Lo, `Upper 95% CI` = .data$Hi
  )
}

coefs_with_cis <- coefs %>% 
  dplyr::filter(!stringr::str_detect(index, " Ci ")) %>% 
    dplyr::left_join(cis, by = "index") %>% 
    dplyr::mutate(index = dplyr::if_else(index == "Glb", "Greatest Lower Bound", .data$index)) %>% 
    dplyr::arrange(!stringr::str_detect(index, "Omega")) %>% 
    dplyr::select(Index = .data$index, Estimate = .data$estimate)


knitr::kable(coefs_with_cis)

## -----------------------------------------------------------------------------
print(x$scatterMatrix$output$scatterMatrix)
x$scatterMatrix$output$scatterMatrix <- no_md()

## -----------------------------------------------------------------------------
print(x)

## ----reliability, results='asis'----------------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  cat(knitr::knit_print(rel, indent = paste0(indent, "####")))
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
knitr::opts_chunk$set(fig.height = new_height)

## ----likert-------------------------------------------------------------------
if (dplyr::n_distinct(na.omit(unlist(items))) < 12) {
  likert_plot <- likert_from_items(items)
  if (!is.null(likert_plot)) {
    graphics::plot(likert_plot)
  }
}
knitr::opts_chunk$set(fig.height = old_height)

## ----setup_distribution-------------------------------------------------------
wrap_at <- knitr::opts_chunk$get("fig.width") * 10

## ----distribution-------------------------------------------------------------
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
	testDat <- bfi %>% dplyr::select(dplyr::starts_with("BFIK_open_"))
  x = userfriendlyscience::scaleDiagnosis(testDat, 
                                          scaleReliability.ci = FALSE)
	testDat <- bfi %>% dplyr::select(dplyr::starts_with("BFIK_consc_"))
  x = userfriendlyscience::scaleDiagnosis(testDat, 
                                          scaleReliability.ci = TRUE)
}

## -----------------------------------------------------------------------------
coefs <- x$scaleReliability$output$dat %>% 
  tidyr::gather(index, estimate) %>% 
  dplyr::filter(index != "n.items", index != "n.observations") %>% 
  dplyr::mutate(index = stringr::str_to_title(
    stringr::str_replace_all(index,
      stringr::fixed("."), " ")))

cis <- coefs %>% 
  dplyr::filter(stringr::str_detect(index, " Ci ")) %>% 
  tidyr::separate(index, c("index", "hilo"), sep = " Ci ") %>% 
  tidyr::spread(hilo, estimate)
if (nrow(cis)) {
  cis <- cis %>% dplyr::rename(
    `Lower 95% CI` = .data$Lo, `Upper 95% CI` = .data$Hi
  )
}

coefs_with_cis <- coefs %>% 
  dplyr::filter(!stringr::str_detect(index, " Ci ")) %>% 
    dplyr::left_join(cis, by = "index") %>% 
    dplyr::mutate(index = dplyr::if_else(index == "Glb", "Greatest Lower Bound", .data$index)) %>% 
    dplyr::arrange(!stringr::str_detect(index, "Omega")) %>% 
    dplyr::select(Index = .data$index, Estimate = .data$estimate)


knitr::kable(coefs_with_cis)

## -----------------------------------------------------------------------------
print(x$scatterMatrix$output$scatterMatrix)
x$scatterMatrix$output$scatterMatrix <- no_md()

## -----------------------------------------------------------------------------
print(x)

## ----reliability, results='asis'----------------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  cat(knitr::knit_print(rel, indent = paste0(indent, "####")))
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

old_height <- knitr::opts_chunk$get("fig.height")
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
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	if(could_disclose_unique_values(item_nomiss) && is.character(item_nomiss)) {
	  new_height <- old_height
	}
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10

## ----distribution-------------------------------------------------------------
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
knitr::opts_chunk$set(fig.height = old_height)

## ----summary------------------------------------------------------------------
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))

## ----missing_values-----------------------------------------------------------
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}

## ----item_info----------------------------------------------------------------
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  knitr::kable(item_info)
}

## ----choices------------------------------------------------------------------
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	knitr::kable(as.list(choices))
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
  if (knitr::is_html_output()) {
    rmarkdown::paged_table(md_pattern, options = list(rows.print = 10))
  } else {
    knitr::kable(md_pattern)
  }
}

## ----setup,eval=TRUE,echo=FALSE-----------------------------------------------
if (exists("testing")) {
	indent = '#' # ugly hack so _regression_summary can be "spun" (variables included via `r ` have to be available)
	results = data.frame()
	jsonld_metadata <- list(test = 1)
}
json <- jsonlite::toJSON(jsonld_metadata, pretty = TRUE, auto_unbox = TRUE)

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
codebook(bfi, metadata_table = knit_by_pkgdown, metadata_json = TRUE)

## -----------------------------------------------------------------------------
if (!knit_by_pkgdown) {
  codebook:::escaped_table(codebook_table(bfi))
}

