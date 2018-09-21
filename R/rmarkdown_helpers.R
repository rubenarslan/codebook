#' Knit a child document and output as is (render markup)
#'
#' This slightly modifies the [knitr::knit_child()] function to have different defaults.
#' - the environment defaults to the calling environment.
#' - the output receives the class `knit_asis`, so that the output will be rendered "as is" by knitr when calling inside a chunk (no need to set `results='asis'` as a chunk option).
#' - defaults to `quiet = TRUE`
#'
#' Why default to the calling environment? Typically this function defaults to the global environment. This makes sense if you want to use knit children in the same context as the rest of the document.
#' However, you may also want to use knit children inside functions to e.g. summarise a regression using a set of commands (e.g. plot some diagnostic graphs and a summary for a regression nicely formatted).
#'
#' Some caveats:
#' - the function has to return to the top-level. There's no way to [cat()] this from loops or an if-condition without without setting `results='asis'`. You can however concatenate these objects with [paste.knit_asis()]
#'
#'
#' @param input if you specify a file path here, it will be read in before being passed to knitr (to avoid a working directory mess)
#' @param text passed to [knitr::knit_child()]
#' @param ... passed to [knitr::knit_child()]
#' @param quiet passed to [knitr::knit_child()]
#' @param options defaults to NULL.
#' @param envir passed to [knitr::knit_child()]
#' @param use_strings whether to read in the child file as a character string (solves working directory problems but harder to debug)
#'
#' @export
#' @examples
#' \dontrun{
#' # an example of a wrapper function that calls asis_knit_child with an argument
#' # ensures distinct paths for cache and figures, so that these calls can be looped in parallel
#' regression_summary <- function(model) {
#'    hash <- digest::digest(model)
#'    options <- list(
#'        fig.path = paste0(knitr::opts_chunk$get("fig.path"), hash, "-"),
#'        cache.path = paste0(knitr::opts_chunk$get("cache.path"), hash, "-"))
#'    asis_knit_child("_regression_summary.Rmd", options = options)
#' }
#' }
asis_knit_child <- function(input = NULL, text = NULL, ...,
                            quiet = TRUE, options = NULL,
                            envir = parent.frame(), use_strings = TRUE) {
  stopifnot( xor(is.null(text), is.null(input)))
  if (!is.null(input) && use_strings) {
    text <- paste0(readLines(input), collapse = "\n")
    input <- NULL
  }
  if (knitr::opts_knit$get("child")) {
    child <- knitr::opts_knit$get("child")
    knitr::opts_knit$set(child = TRUE)
    on.exit(knitr::opts_knit$set(child = child))
  }
  if (is.list(options)) {
    options$label <- options$child <- NULL
    if (length(options)) {
      optc <- knitr::opts_chunk$get(names(options), drop = FALSE)
      knitr::opts_chunk$set(options)
      on.exit({
        for (i in names(options)) if (identical(options[[i]],
             knitr::opts_chunk$get(i))) knitr::opts_chunk$set(optc[i])
      }, add = TRUE)
    }
  }

  encode <- knitr::opts_knit$get("encoding")
  if (is.null(encode)) {
    encode <- getOption("encoding")
  }
  res <- knitr::knit(input = input, text = text, ...,
                     quiet = quiet, tangle = knitr::opts_knit$get("tangle"),
                     envir = envir, encoding = encode)
  knitr::asis_output(paste(c("", res), collapse = "\n"))
}


#' Paste and output as is (render markup)
#'
#' Helper function for `knit_asis` objects, useful when e.g. [asis_knit_child()] was used in a loop.
#'
#' Works like [base::paste()] with both the sep and the collapse argument set to two empty lines
#'
#' @param ... passed to [base::paste()]
#' @param sep defaults to two empty lines, passed to [base::paste()]
#' @param collapse defaults to two empty lines, passed to [base::paste()]
#'
#' @export
#' @examples
#' paste.knit_asis("# Headline 1", "## Headline 2")
paste.knit_asis <- function(..., sep = "\n\n\n", collapse = "\n\n\n") {
  knitr::asis_output(paste(..., sep = sep, collapse = collapse))
}

#' Print new lines in `knit_asis` outputs
#'
#' @param x the knit_asis object
#' @param ... ignored
#'
#' @export
print.knit_asis <- function(x, ...) {
  cat(x, sep = '\n')
}



#' Render codebook based on file
#'
#'
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
  codebook_data <- switch(tools::file_ext(file),
                          "rdata" = rio::import_list(file)[[1]],
                          "rda" = rio::import_list(file)[[1]],
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


require_file <- function(file) {
  system.file(file, package = 'codebook', mustWork = TRUE)
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
new_codebook_rmd <- function(filename = "codebook", template = "default") {
  if (!is.null(filename)) {
    stopifnot(!file.exists(filename))
  }
  stopifnot(template == "default")

  template <- readLines(require_file("_template_codebook.Rmd"))

  rmd_file <- write_to_file(template, name = filename, ext = ".Rmd")
  if (rstudioapi::isAvailable()) {
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
    } else if (is.list(x) && class(x) == "list") {
      # turtle down into lists
      x <- lapply(x, function(x) { recursive_escape(x, depth + 1) })
    }
  }
  x
}

safe_name <- function(x) {
  stringr::str_replace_all(x, "[^[:alnum:]]", "_")
}
