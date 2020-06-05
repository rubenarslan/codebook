results = jsonlite::fromJSON(txt =
	system.file('extdata/BFI_post.json', package = 'formr', mustWork = TRUE))
items = formr::formr_items(path =
	system.file('extdata/BFI_post_items.json', package = 'formr', mustWork = TRUE))
item_displays = jsonlite::fromJSON(
	system.file('extdata/BFI_post_itemdisplay.json', package = 'formr', mustWork = TRUE))
results = formr::formr_post_process_results(items, results, item_displays = item_displays,
compute_alphas = FALSE, plot_likert = FALSE, quiet = TRUE)

bfi = dplyr::filter(results, !is.na(ended))
usethis::use_data(bfi, overwrite = TRUE)

