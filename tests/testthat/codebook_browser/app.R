library(codebook)

data("bfi")
codebook_browser(data = bfi, viewer = shiny::paneViewer())
