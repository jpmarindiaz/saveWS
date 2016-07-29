#' Save workspace data in different formats.
#'
#' Save a \code{data.frame} to csv or xls.
#'
#' @export
saveWSAddin <- function() {

  wsVars <- ls(envir = .GlobalEnv)
  formats <- c("csv","xls")

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text

  # Generate UI for the gadget.
  ui <- miniPage(
    useShinyjs(),
    gadgetTitleBar("save Workspace Variable"),
    miniContentPanel(
      stableColumnLayout(
        selectInput("data", "Data", choices = wsVars),
        selectInput("format", "Format", choices = formats)
      ),
      stableColumnLayout(
        uiOutput("filenameInput"),
        uiOutput("fileControls")
      ),
      verbatimTextOutput("debug"),
      uiOutput("pending"),
      dataTableOutput("output")
    )
  )


  # Server code for the gadget.
  server <- function(input, output, session) {
    output$debug <- renderPrint({
      #class(reactiveData())
      paste0(input$data,".",input$format)
    })

    output$filenameInput <- renderUI({
      defaultFilePath <- paste0(input$data,".",input$format)
      textInput("filename", "File Name",
                value = defaultFilePath)
    })

    output$fileControls <- renderUI({
      savedMessage <-  paste("Saved File",input$filename)
      #value <- isolate(paste0(input$data,".",input$format))
      list(
        actionButton("saveButton","Save", icon = "floppy-o"),
        hidden(
          div(id="savedFile",
              h4(style = "color: #5BE372);",savedMessage))
        ),
        br()
      )
    })

    output$output <- renderDataTable({
      data <- head(reactiveData(),10)
      if (isErrorMessage(data))
        return(NULL)
      data
    })

    reactiveData <- reactive({

      # Collect inputs.
      dataString <- input$data

      # Check to see if there is data called 'data',
      # and access it if possible.
      if (!nzchar(dataString))
        return(errorMessage("data", "No dataset available."))

      if (!exists(dataString, envir = .GlobalEnv))
        return(errorMessage("data", paste("No dataset named '", dataString, "' available.")))

      data <- get(dataString, envir = .GlobalEnv)
      data
    })

    output$pending <- renderUI({
      data <- head(reactiveData(),10)
      if(!is.null(data))
        h4(style = "color: #AA7732;", "Showing only 10 rows")
    })

    output$output <- renderDataTable({
      data <- head(reactiveData(),10)
      if (isErrorMessage(data))
        return(NULL)
      data
    })

    observeEvent(input$saveButton, {
      data <- reactiveData()
      path <- input$filename
      if(input$format == "csv"){
        write_csv(data,path)
      }
      if(input$format == "xls"){
        write.xlsx(x = data, file = path, row.names = FALSE)
      }
      if(file.exists(path)){
        show("savedFile")
      }
    })

    # Listen for 'done'.
    observeEvent(input$done, {

      # # Emit a subset call if a dataset has been specified.
      # if (nzchar(input$data) && nzchar(input$subset)) {
      #   code <- paste("subset(", input$data, ", ", input$subset, ")", sep = "")
      #   rstudioapi::insertText(text = code)
      # }

      invisible(stopApp())
    })
  }

  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("Subset", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)

}
