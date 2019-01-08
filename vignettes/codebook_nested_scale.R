## ----message = FALSE-----------------------------------------------------
knit_by_pkgdown <- !is.null(knitr::opts_chunk$get("fig.retina"))
knitr::opts_chunk$set(warning = TRUE, message = TRUE, error = TRUE, echo = TRUE)
pander::panderOptions("table.split.table", Inf)
ggplot2::theme_set(ggplot2::theme_bw())
library(codebook)
library(dplyr)
data("bfi", package = 'codebook')

## ------------------------------------------------------------------------
bfi$alpha <- aggregate_and_document_scale(bfi %>% 
  select(starts_with("BFIK_agree_"), starts_with("BFIK_consc_"), starts_with("BFIK_neuro_")) %>% 
  mutate_at(vars(starts_with("BFIK_neuro_")), reverse_labelled_values))

## ------------------------------------------------------------------------
bfi <- bfi %>% mutate(
  alpha = aggregate_and_document_scale(bfi %>% 
  select(starts_with("BFIK_agree_"), starts_with("BFIK_consc_"), starts_with("BFIK_neuro_")) %>% 
  mutate_at(vars(starts_with("BFIK_neuro_")), reverse_labelled_values)
))

bfi <- bfi %>% mutate(beta = aggregate_and_document_scale(select(. ,
  starts_with("BFIK_extra_"), starts_with("BFIK_open_"))))

## ------------------------------------------------------------------------
library(labelled)
var_label(bfi$alpha) # old name
var_label(bfi$alpha) <- "Alpha: higher order personality factor (consc, neuro, agree)"
var_label(bfi$beta) <- "Beta: higher order personality factor (extra, open)"

## ------------------------------------------------------------------------
bfi <- bfi %>% select(beta, starts_with("BFIK_extra"), starts_with("BFIK_open"))
if (!knit_by_pkgdown) knitr::opts_chunk$set(echo = FALSE)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
# We don't want to look at the code in the codebook.
knitr::opts_chunk$set(warning = TRUE, message = TRUE, echo = FALSE)

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (exists("testing")) {
	indent = '#' # ugly hack so _regression_summary can be "spun" (variables included via `r ` have to be available)
	results = data("bfi")
	metadata(results)$description <- data_description_default(bfi)
}

meta <- metadata(results)
description <- meta$description
meta <- recursive_escape(meta)

## ----results='asis'------------------------------------------------------
if (exists("name", meta)) {
  glue::glue(
    "__Dataset name__: {name}",
    .envir = meta)
}

## ----results='asis'------------------------------------------------------
cat(description)

## ----results='asis', echo = FALSE----------------------------------------
if (exists("temporalCoverage", meta)) {
  glue::glue(
    "- __Temporal Coverage__: {temporalCoverage}",
    .envir = meta)
}

## ----results='asis', echo = FALSE----------------------------------------
if (exists("spatialCoverage", meta)) {
  glue::glue(
    "- __Spatial Coverage__: {spatialCoverage}",
    .envir = meta)
}

## ----results='asis', echo = FALSE----------------------------------------
if (exists("citation", meta)) {
  glue::glue(
    "- __Citation__: {citation}",
    .envir = meta)
}

## ----results='asis', echo = FALSE----------------------------------------
if (exists("url", meta)) {
  glue::glue(
    "- __URL__: [{url}]({url})",
    .envir = meta)
}

## ----results='asis', echo = FALSE----------------------------------------
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

## ----results='asis', echo = FALSE----------------------------------------
if (exists("datePublished", meta)) {
  glue::glue(
    "- __Date published__: {datePublished}",
    .envir = meta)
}

## ----results='asis', echo = FALSE----------------------------------------
if (exists("creator", meta)) {
  cat("- __Creator__:")
  pander::pander(meta$creator)
}

## ------------------------------------------------------------------------
meta <- meta[setdiff(names(meta),
                     c("creator", "datePublished", "identifier",
                       "url", "citation", "spatialCoverage", 
                       "temporalCoverage", "description", "name"))]
pander::pander(meta)

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#'
}
if (exists("testing")) {
  scale_name <- safe_name <- "bla"
	scale <- 1:10
	reliabilities <- list()
	items <- list()
	scale_info <- list(scale_item_names = c("item1", "item2", "item3R"))
}
html_scale_name <- recursive_escape(scale_name)
names(items) <- recursive_escape(names(items))
scale_info <- recursive_escape(attributes(scale))

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
  suppressMessages( # ignore message about overwriting x axis
  dist_plot <- dist_plot +
    	ggplot2::scale_x_continuous("values", 
	                            breaks = breaks, 
	                            labels = stringr::str_wrap(unlist(choices), ceiling(wrap_at * 0.21))) +
      ggplot2::expand_limits(x = range(breaks)))
  
}

dist_plot

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
  pander::pander(data.frame(lower = x$total$raw_alpha - 1.96 * x$total$ase, 
             estimate = x$total$raw_alpha, 
             upper = x$total$raw_alpha + 1.96 * 
  x$total$ase))
}

## ------------------------------------------------------------------------
pander::pander(x$total)

## ------------------------------------------------------------------------
rownames(x$alpha.drop) <- recursive_escape(rownames(x$alpha.drop))
pander::pander(x$alpha.drop)

## ------------------------------------------------------------------------
rownames(x$item.stats) <- recursive_escape(rownames(x$item.stats))
pander::pander(x$item.stats)

## ------------------------------------------------------------------------
rownames(x$response.freq) <- recursive_escape(rownames(x$response.freq))
pander::pander(x$response.freq)

## ----reliability, results='asis'-----------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  cat(knitr::knit_print(rel, indent = paste0(indent, "####")))
}

## ----summary-------------------------------------------------------------
for (i in seq_along(names(items))) {
  attributes(items[[i]]) = recursive_escape(attributes(items[[i]]))
}
escaped_table(codebook_table(items))

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#'
}
if (exists("testing")) {
  scale_name <- safe_name <- "bla"
	scale <- 1:10
	reliabilities <- list()
	items <- list()
	scale_info <- list(scale_item_names = c("item1", "item2", "item3R"))
}
html_scale_name <- recursive_escape(scale_name)
names(items) <- recursive_escape(names(items))
scale_info <- recursive_escape(attributes(scale))

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
  suppressMessages( # ignore message about overwriting x axis
  dist_plot <- dist_plot +
    	ggplot2::scale_x_continuous("values", 
	                            breaks = breaks, 
	                            labels = stringr::str_wrap(unlist(choices), ceiling(wrap_at * 0.21))) +
      ggplot2::expand_limits(x = range(breaks)))
  
}

dist_plot

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
  pander::pander(data.frame(lower = x$total$raw_alpha - 1.96 * x$total$ase, 
             estimate = x$total$raw_alpha, 
             upper = x$total$raw_alpha + 1.96 * 
  x$total$ase))
}

## ------------------------------------------------------------------------
pander::pander(x$total)

## ------------------------------------------------------------------------
rownames(x$alpha.drop) <- recursive_escape(rownames(x$alpha.drop))
pander::pander(x$alpha.drop)

## ------------------------------------------------------------------------
rownames(x$item.stats) <- recursive_escape(rownames(x$item.stats))
pander::pander(x$item.stats)

## ------------------------------------------------------------------------
rownames(x$response.freq) <- recursive_escape(rownames(x$response.freq))
pander::pander(x$response.freq)

## ----reliability, results='asis'-----------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  cat(knitr::knit_print(rel, indent = paste0(indent, "####")))
}

## ----summary-------------------------------------------------------------
for (i in seq_along(names(items))) {
  attributes(items[[i]]) = recursive_escape(attributes(items[[i]]))
}
escaped_table(codebook_table(items))

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#'
}
if (exists("testing")) {
  scale_name <- safe_name <- "bla"
	scale <- 1:10
	reliabilities <- list()
	items <- list()
	scale_info <- list(scale_item_names = c("item1", "item2", "item3R"))
}
html_scale_name <- recursive_escape(scale_name)
names(items) <- recursive_escape(names(items))
scale_info <- recursive_escape(attributes(scale))

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
  suppressMessages( # ignore message about overwriting x axis
  dist_plot <- dist_plot +
    	ggplot2::scale_x_continuous("values", 
	                            breaks = breaks, 
	                            labels = stringr::str_wrap(unlist(choices), ceiling(wrap_at * 0.21))) +
      ggplot2::expand_limits(x = range(breaks)))
  
}

dist_plot

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
  pander::pander(data.frame(lower = x$total$raw_alpha - 1.96 * x$total$ase, 
             estimate = x$total$raw_alpha, 
             upper = x$total$raw_alpha + 1.96 * 
  x$total$ase))
}

## ------------------------------------------------------------------------
pander::pander(x$total)

## ------------------------------------------------------------------------
rownames(x$alpha.drop) <- recursive_escape(rownames(x$alpha.drop))
pander::pander(x$alpha.drop)

## ------------------------------------------------------------------------
rownames(x$item.stats) <- recursive_escape(rownames(x$item.stats))
pander::pander(x$item.stats)

## ------------------------------------------------------------------------
rownames(x$response.freq) <- recursive_escape(rownames(x$response.freq))
pander::pander(x$response.freq)

## ----reliability, results='asis'-----------------------------------------
for (i in seq_along(reliabilities)) {
  rel <- reliabilities[[i]]
  cat(knitr::knit_print(rel, indent = paste0(indent, "####")))
}

## ----summary-------------------------------------------------------------
for (i in seq_along(names(items))) {
  attributes(items[[i]]) = recursive_escape(attributes(items[[i]]))
}
escaped_table(codebook_table(items))

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
if (!exists("ended", results) ||
  !exists("expired", results)) {
  warning("Could not figure out who finished the surveys, because the ",
          "variables expired and ended were missing.")
}
if (length(md_pattern)) {
  if (knitr::is_html_output()) {
    rmarkdown::paged_table(md_pattern, options = list(rows.print = 10))
  } else {
    knitr::kable(md_pattern)
  }
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
	data_info <- '' 
	survey_overview <- '' 
	scales_items <- c()
}

## ------------------------------------------------------------------------
knitr::asis_output(data_info)

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
codebook(bfi, survey_repetition = 'single',
         metadata_table = knit_by_pkgdown, metadata_json = knit_by_pkgdown)

## ------------------------------------------------------------------------
if (!knit_by_pkgdown) {
  codebook:::escaped_table(codebook_table(bfi))
}

