#' Browse and search variable and value labels
#'
#' @import shiny miniUI rstudioapi
#' @export
label_browser <- function() {

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text

  # Generate UI for the gadget.
  ui <- miniPage(
    gadgetTitleBar("Variable and value labels"),
    miniContentPanel(
      stableColumnLayout(
        textInputCode("data", "Data", value = defaultData)
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

      labels <- codebook_table(data)#[, c("name", "label")]

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


# taken from https://github.com/gadenbuie/regexplain/blob/ef1fe54958a4f42a8e46eabc15ef098bed2530ec/R/shiny_modified_inputs.R because it's not on CRAN for 3.4.4
#' Modified Text Input
#'
#' Standard [shiny::textInput()] with additional `width` parameter, added code
#' font style for the input text and with `autocomplete`, `autocorrect`,
#' `autocapitalize` and `spellcheck` set to `off` or `false`.
#'
#' @inheritParams shiny::textInput
#' @param width Width of `shiny-input-container` div.
#' @family modified shiny inputs
textInputCode <- function(inputId, label, value = "", width = NULL,
                          placeholder = NULL) {
  `%AND%` <- getFromNamespace("%AND%", "shiny")
  value <- shiny::restoreInput(id = inputId, default = value)

  shiny::div(class = "form-group shiny-input-container",
             style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
             label %AND% shiny::tags$label(label, `for` = inputId),
             shiny::tags$input(id = inputId, type="text", class="form-control", value = value,
                               style = 'font-family: "Monaco", "Inconsolata", monospace;',
                               autocomplete = "off", autocorrect = "off",
                               autocapitalize = "off", spellcheck = "false",
                               placeholder = placeholder)
  )
}
