#' Browse and search variable and value labels
#'
#' Same as the [codebook_browser()], but doesn't show data summaries and
#' additional attributes.
#'
#' @export
label_browser <- function() {
  codebook_browser(TRUE, "Variable and value labels")
}

#' Browse and search codebook
#'
#' Usable as an Addin in RStudio. You can select it from a menu at the top,
#' when this package is installed. If you're currently selecting the name of a
#' data frame in your source code, this will be the dataset shown by default.
#' If you don't select text, you can pick a dataset from a dropdown.
#' You can add a keyboard shortcut for this command by following the
#' [instructions](https://support.rstudio.com/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts)
#' by RStudio. How about Ctrl+C?
#'
#' @import shiny miniUI rstudioapi
#' @param labels_only defaults to false called with TRUE from [label_browser()]
#' @param title title of the gadget
#' @export
codebook_browser <- function(labels_only = FALSE, title = "Codebook metadata") {

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text

  # Generate UI for the gadget.
  ui <- miniPage(
    gadgetTitleBar(title),
    miniContentPanel(
      stableColumnLayout(
        selectInput("data", "Data", selected = defaultData, choices =
                      names(which(unlist(eapply(.GlobalEnv,is.data.frame)))))
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
        return(errorMessage("data", paste("No dataset named '", dataString, "' available.")))

      data <- get(dataString, envir = .GlobalEnv)

      labels <- codebook_table(data)
      if (labels_only) {
        cols <- intersect(names(labels), c("name", "label", "value_labels"))
        labels <- labels[, cols, drop = FALSE]
      }
      if (exists("value_labels", labels)) {
        labels$value_labels <- gsub(pattern = "\n", replacement = "<br>",
                                    x = labels$value_labels)
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
      if (isErrorMessage(data))
        return(NULL)
      data
    },
    escape = setdiff(names(reactiveData()), 'value_labels'),
    options = list(
      pageLength = 100, autoWidth = TRUE))

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

  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("Labels", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
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
