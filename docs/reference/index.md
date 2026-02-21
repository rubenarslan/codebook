# Package index

## RStudio Addins

Addins to help you browse the metadata within RStudio

- [`codebook_browser()`](https://rubenarslan.github.io/codebook/reference/codebook_browser.md)
  : Browse and search codebook
- [`label_browser()`](https://rubenarslan.github.io/codebook/reference/label_browser.md)
  : Browse and search variable and value labels
- [`label_browser_static()`](https://rubenarslan.github.io/codebook/reference/label_browser_static.md)
  : Browse and search variable and value labels

## Codebook generators

Functions to generate (parts of) the codebook

- [`codebook()`](https://rubenarslan.github.io/codebook/reference/codebook.md)
  : Generate rmarkdown codebook
- [`compact_codebook()`](https://rubenarslan.github.io/codebook/reference/compact_codebook.md)
  : Compact Codebook
- [`new_codebook_rmd()`](https://rubenarslan.github.io/codebook/reference/new_codebook_rmd.md)
  : Create a codebook rmarkdown document
- [`codebook_table()`](https://rubenarslan.github.io/codebook/reference/codebook_table.md)
  : Codebook metadata table
- [`codebook_items()`](https://rubenarslan.github.io/codebook/reference/codebook_items.md)
  : Tabular codebook
- [`codebook_missingness()`](https://rubenarslan.github.io/codebook/reference/codebook_missingness.md)
  : Codebook missingness
- [`md_pattern()`](https://rubenarslan.github.io/codebook/reference/md_pattern.md)
  : Missing data patterns
- [`codebook_survey_overview()`](https://rubenarslan.github.io/codebook/reference/codebook_survey_overview.md)
  : Codebook survey overview
- [`codebook_data_info()`](https://rubenarslan.github.io/codebook/reference/codebook_data_info.md)
  : Codebook data info
- [`codebook_component_scale()`](https://rubenarslan.github.io/codebook/reference/codebook_component_scale.md)
  : Codebook component for scales
- [`codebook_component_single_item()`](https://rubenarslan.github.io/codebook/reference/codebook_component_single_item.md)
  : Codebook component for single items
- [`metadata_jsonld()`](https://rubenarslan.github.io/codebook/reference/metadata_jsonld.md)
  : Metadata as JSON-LD
- [`metadata_list()`](https://rubenarslan.github.io/codebook/reference/metadata_list.md)
  : Metadata from dataframe
- [`load_data_and_render_codebook()`](https://rubenarslan.github.io/codebook/reference/load_data_and_render_codebook.md)
  : Submit a data file and an rmarkdown template as a file to generate a
  codebook. Used chiefly in the webapp.
- [`skim_codebook()`](https://rubenarslan.github.io/codebook/reference/skim_codebook.md)
  : Skim codebook

## Prepare data for codebook

Functions to annotate data to be more interpretable by the codebook
functions

- [`detect_missing()`](https://rubenarslan.github.io/codebook/reference/detect_missing.md)
  [`detect_missings()`](https://rubenarslan.github.io/codebook/reference/detect_missing.md)
  : Detect missing values

- [`detect_scales()`](https://rubenarslan.github.io/codebook/reference/detect_scales.md)
  : Detect item scales

- [`compute_reliabilities()`](https://rubenarslan.github.io/codebook/reference/compute_reliabilities.md)
  : Compute reliabilities

- [`rescue_attributes()`](https://rubenarslan.github.io/codebook/reference/rescue_attributes.md)
  : Rescue lost attributes

- [`aggregate_and_document_scale()`](https://rubenarslan.github.io/codebook/reference/aggregate_and_document_scale.md)
  : Aggregate variables and remember which variables this were

- [`add_R()`](https://rubenarslan.github.io/codebook/reference/add_R.md)
  : Append R to string, if it doesn't end in R already.

- [`reverse_labelled_values()`](https://rubenarslan.github.io/codebook/reference/reverse_labelled_values.md)
  :

  Reverse labelled values reverse the underlying values for a numeric
  [`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
  vector while keeping the labels correct

- [`to_factor()`](https://rubenarslan.github.io/codebook/reference/to_factor.md)
  : To factor

- [`has_label()`](https://rubenarslan.github.io/codebook/reference/has_label.md)
  : Has label

- [`has_labels()`](https://rubenarslan.github.io/codebook/reference/has_labels.md)
  : Has labels

- [`metadata()`](https://rubenarslan.github.io/codebook/reference/metadata.md)
  [`` `metadata<-`() ``](https://rubenarslan.github.io/codebook/reference/metadata.md)
  : Add metadata to a dataset

- [`data_description_default()`](https://rubenarslan.github.io/codebook/reference/data_description_default.md)
  : Data description default

- [`list_to_dict()`](https://rubenarslan.github.io/codebook/reference/list_to_dict.md)
  [`dict_to_list()`](https://rubenarslan.github.io/codebook/reference/list_to_dict.md)
  : Go from a named list to a key-value data frame or data dictionary
  and back

- [`bfi`](https://rubenarslan.github.io/codebook/reference/bfi.md) :
  Mock BFI data

## Prepare data for non-codebook stuff

Functions to remove attributes that might confuse other packages

- [`zap_attributes()`](https://rubenarslan.github.io/codebook/reference/zap_attributes.md)
  : Zap attributes
- [`reexports`](https://rubenarslan.github.io/codebook/reference/reexports.md)
  [`as_factor`](https://rubenarslan.github.io/codebook/reference/reexports.md)
  [`zap_label`](https://rubenarslan.github.io/codebook/reference/reexports.md)
  [`var_label`](https://rubenarslan.github.io/codebook/reference/reexports.md)
  [`var_label`](https://rubenarslan.github.io/codebook/reference/reexports.md)
  [`val_label`](https://rubenarslan.github.io/codebook/reference/reexports.md)
  [`val_label`](https://rubenarslan.github.io/codebook/reference/reexports.md)
  [`val_labels`](https://rubenarslan.github.io/codebook/reference/reexports.md)
  [`val_labels`](https://rubenarslan.github.io/codebook/reference/reexports.md)
  [`%>%`](https://rubenarslan.github.io/codebook/reference/reexports.md)
  : Objects exported from other packages
- [`zap_labelled()`](https://rubenarslan.github.io/codebook/reference/zap_labelled.md)
  : Zap labelled class

## Methods

Various methods to print and plot objects

- [`knit_print.htest()`](https://rubenarslan.github.io/codebook/reference/knit_print.htest.md)
  :

  Print a [`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html)
  object for knitr

- [`knit_print.multilevel()`](https://rubenarslan.github.io/codebook/reference/knit_print.multilevel.md)
  :

  Print a
  [`psych::multilevel.reliability()`](https://rdrr.io/pkg/psych/man/multilevel.reliability.html)
  object for knitr

- [`knit_print.alpha()`](https://rubenarslan.github.io/codebook/reference/knit_print.alpha.md)
  : Pretty-print a Cronbach's alpha object

- [`likert_from_items()`](https://rubenarslan.github.io/codebook/reference/likert_from_items.md)
  : Derive a likert object from items

- [`plot_labelled()`](https://rubenarslan.github.io/codebook/reference/plot_labelled.md)
  : Plot labelled vector

- [`get_skimmers.haven_labelled()`](https://rubenarslan.github.io/codebook/reference/get_skimmers.haven_labelled.md)
  : Define skimmers for haven_labelled variables

- [`get_skimmers.haven_labelled_spss()`](https://rubenarslan.github.io/codebook/reference/get_skimmers.haven_labelled_spss.md)
  : Define skimmers for haven_labelled_spss variables

## formr-data specific helpers

- [`ended()`](https://rubenarslan.github.io/codebook/reference/ended.md)
  : How many surveys were ended?
- [`expired()`](https://rubenarslan.github.io/codebook/reference/expired.md)
  : How many surveys were expired?
- [`modified()`](https://rubenarslan.github.io/codebook/reference/modified.md)
  : How many surveys were modified?
