#' Submit a data file and an rmarkdown template as a file to generate a codebook.
#' Used chiefly in the webapp.
#'
#' @param file path to a file to make codebook from (sav, rds, dta, por, xpt, csv, csv2, tsv, etc.)
#' @param text codebook template
#' @param remove_file whether to remove file after rendering
#' @param ... all other arguments passed to [rmarkdown::render()]
#' @md
#'
#' @export

load_data_and_render_codebook <- function(file, text,
                                          remove_file = FALSE, ...) {
  if (!requireNamespace("rio", quietly = TRUE)) {
    stop("Package \"rio\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package \"rmarkdown\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  codebook_data <- switch(tools::file_ext(file),
                          "rdata" = rio::import_list(file, trust = TRUE)[[1]],
                          "rda" = rio::import_list(file, trust = TRUE)[[1]],
                          "rds" = rio::import(file, trust = TRUE),
                          rio::import(file)
  )
  stopifnot(!is.null(codebook_data))
  if (remove_file) {
    file.remove(file)
  }
  fileName <- rmarkdown::render(input = write_to_file(text,
                name = "codebook", ext = ".Rmd"), ...)
  fileName
}



write_to_file <- function(..., name = NULL, ext = ".Rmd") {
  if (is.null(name)) {
    filename <- paste0(tempfile(), ext)
  } else {
    filename <- paste0(name, ext)
  }
  mytext <- eval(...)
  write(mytext, filename)
  return(filename)
}


require_file <- function(file, package = 'codebook') {
  file <- gsub("^inst/", "", file)
  system.file(file, package = package, mustWork = TRUE)
}

#' Create a codebook rmarkdown document
#'
#' This function will create and open an .Rmd file in the current working
#' directory. By default, the file is named codebook.Rmd. No files will be
#' overwritten. The .Rmd file has some useful defaults set for creating codebooks.
#'
#'
#' @param filename under which file name do you want to create a template
#' @param template only "default" exists for now
#'
#' @export
#' @examples
#' \dontrun{
#' new_codebook_rmd()
#' }
#'

new_codebook_rmd <- function(filename = "codebook", template = "default") {
  if (!is.null(filename)) {
    stopifnot(!file.exists(filename))
  }
  stopifnot(template == "default")

  template <- readLines(require_file("_template_codebook.Rmd"))

  rmd_file <- write_to_file(template, name = filename, ext = ".Rmd")
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(rmd_file)
  } else if (interactive()) {
    utils::file.edit(rmd_file)
  }
}

recursive_escape <- function(x, depth = 0, max_depth = 4,
                             escape_fun = htmltools::htmlEscape) {
  if (depth < max_depth) {
    # escape names for all vectors
    if (!is.null(names(x))) {
      names(x) <- escape_fun(names(x))
    }
    if (!is.null(rownames(x))) {
      rownames(x) <- escape_fun(rownames(x))
    }

    # escape any character vectors
    if (is.character(x)) {
      x <- escape_fun(x)
    } else if (is.list(x) && inherits(x, "list")) {
      # turtle down into lists
      x <- lapply(x, function(x) { recursive_escape(x, depth + 1) })
    }
  }
  x
}

safe_name <- function(x) {
  stringr::str_replace_all(x, "[^[:alnum:]]", "_")
}
