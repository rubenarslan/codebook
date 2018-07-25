find_dfs_in_environment <- function(env = .GlobalEnv) {
  dfs <- unlist(eapply(env,is.data.frame))
  if (length(dfs)) {
    names(which(dfs))
  } else {
    stop("No data frames in the global environment.")
  }
}

#' Browse and search variable and value labels
#'
#' Same as the [codebook_browser()], but doesn't show data summaries and
#' additional attributes. This yields a static table, so you can continue
#' to edit code while viewing the labels, but you cannot switch the dataset
#' via a dropdown menu.
#'
#' @param data data frame. if left empty, will use the text you currently select in RStudio as the label or the first data frame in your environment
#' @param viewer where to show. defaults to viewer tab
#' @export
#' @examples
#' label_browser_static(bfi)
#'
#'
label_browser_static <- function(data = NULL, viewer = rstudioapi::viewer) {
  # if data argument given, use it
    if (!is.null(data)) {
      df_name <- deparse(substitute(data))
  } else {
    # if text is selected, use that
    context <- rstudioapi::getActiveDocumentContext()

    # Set the default data to use based on the selection.
    df_name <- context$selection[[1]]$text

    data <- NULL
    if (!is.null(df_name) && df_name != "" && exists(df_name)) {
      data <- get(df_name)
    }

    # if no text selected, or not name of a dataframe, use first in global env
    if (!is.data.frame(data)) {
      data_frames <- find_dfs_in_environment()
      if (length(data_frames) == 0) {
        stop("No data frame found. Make sure to select one or have one
             in your global environment.")
      }
      df_name <- data_frames[1]
      data <- get(df_name)
    }
  }

    labels <- metadata(data)
    cols <- intersect(names(labels), c("name", "label", "value_labels"))
    labels <- labels[, cols, drop = FALSE]
    labels <- dplyr::mutate_if(labels, is.character, htmltools::htmlEscape)
    if (exists("value_labels", labels)) {
      labels$value_labels <- stringr::str_replace_all(labels$value_labels,
                                                      "\n", "<br>")
    }

    DT::datatable(labels,
                  caption = paste(df_name, " columns and labels"),
                  filter = 'top',
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(
                    # searching = FALSE,
                    info = FALSE,
                    # dom = 't',
                    paging = FALSE
                    )
                  )
}

#' Browse and search variable and value labels
#'
#' Same as the [codebook_browser()], but doesn't show data summaries and
#' additional attributes.
#'
#' @inheritParams codebook_browser
#' @export
label_browser <- function(data = NULL, viewer = rstudioapi::viewer) {
  codebook_browser(data = data, labels_only = TRUE,
                   title = "Variable and value labels", viewer = viewer)
}

#' Browse and search codebook
#'
#' Usable as an Addin in RStudio. You can select it from a menu at the top,
#' when this package is installed. If you're currently selecting the name of a
#' data frame in your source code, this will be the dataset shown by default.
#' If you don't select text, you can pick a dataset from a dropdown.
#' You can add a keyboard shortcut for this command by following the
#' [instructions](https://support.rstudio.com/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts)
#' by RStudio. How about Cmd+Ctrl+C?
#'
#' @import shiny miniUI rstudioapi
#' @param data the dataset to display. If left empty will try to use selected text in RStudio or offer a dropdown
#' @param labels_only defaults to false called with TRUE from [label_browser()]
#' @param title title of the gadget
#' @param viewer defaults to displaying in the RStudio viewer
#' @export
codebook_browser <- function(
  data = NULL,
  labels_only = FALSE, title = "Codebook metadata",
                             viewer = rstudioapi::viewer) {

  # if data argument given, use it
  if (!is.null(data)) {
    df_name <- deparse(substitute(data))
  } else {
    # if text is selected, use that
    context <- rstudioapi::getActiveDocumentContext()

    # Set the default data to use based on the selection.
    df_name <- context$selection[[1]]$text

    data <- NULL
    if (!is.null(df_name) && df_name != "" && exists(df_name)) {
      data <- get(df_name)
    }

    # if no text selected, or not name of a dataframe, use first in global env
    if (!is.data.frame(data)) {
      data_frames <- find_dfs_in_environment()
      if (length(data_frames) == 0) {
        stop("No data frame found. Make sure to select one or have one
             in your global environment.")
      }
      df_name <- data_frames[1]
    }
  }
  defaultData <- df_name

  # Generate UI for the gadget.
  ui <- miniPage(
    gadgetTitleBar(title),
    miniContentPanel(
      stableColumnLayout(
        selectInput("data", "Data", selected = defaultData, choices =
                      find_dfs_in_environment())
      ),
      uiOutput("pending"),
      dataTableOutput("output")
    )
  )


  # Server code for the gadget.
  server <- function(input, output, session) {

    reactiveData <- reactive({

      # Collect inputs.
      dataString <- input$data
      searchString <- input$search

      # Check to see if there is data called 'data',
      # and access it if possible.
      if (!nzchar(dataString))
        return(errorMessage("data", "No dataset available."))

      if (!exists(dataString, envir = .GlobalEnv))
        return(errorMessage("data",
            paste("No dataset named '", dataString, "' available.")))

      data <- get(dataString, envir = .GlobalEnv)

      if (labels_only) {
        labels <- metadata(data)
        cols <- intersect(names(labels), c("name", "label", "value_labels"))
        labels <- labels[, cols, drop = FALSE]
      } else {
        labels <- codebook_table(data)
      }
      labels <- dplyr::mutate_if(labels, is.character, htmltools::htmlEscape)

      if (exists("value_labels", labels)) {
        labels$value_labels <- stringr::str_replace_all(labels$value_labels,
                                                        "\n", "<br>")
      }

      return(labels)
    })

    output$pending <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })

    output$output <- renderDataTable({
      data <- reactiveData()
      data
    },
    # filter = 'top',
    escape = FALSE,
    options = list(
      searching = TRUE,
      info = FALSE,
      paging = FALSE)
    )

    # Listen for 'done'.
    observeEvent(input$done, {

      # Pick variable
      # if (nzchar(input$data) && nzchar(input$search)) {
      #   labels <- labels[stringr::str_detect(searchString, labels$name) |
      #                      stringr::str_detect(searchString, labels$label), ]
      #   if (nrow(labels) == 1) {
      #     code <- paste(input$data, "$", labels$name, ")", sep = "")
      #     rstudioapi::insertText(text = code)
      #   }
      # }

      invisible(stopApp())
    })
  }

  if (isTRUE(getOption("shiny.testmode"))) {
    shinyApp(ui, server)
  } else {
    runGadget(ui, server, viewer = viewer)
  }
}


stableColumnLayout <- function(...) {
  dots <- list(...)
  n <- length(dots)
  width <- 12 / n
  class <- sprintf("col-xs-%s col-md-%s", width, width)
  fluidRow(
    lapply(dots, function(el) {
      div(class = class, el)
    })
  )
}

isErrorMessage <- function(object) {
  inherits(object, "error_message")
}

errorMessage <- function(type, message) {
  structure(
    list(type = type, message = message),
    class = "error_message"
  )
}
