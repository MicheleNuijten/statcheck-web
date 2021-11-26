
# Setup -------------------------------------------------------------------

# Load packages
library(shiny)
library(statcheck)
library(pdftools)
library(htm2txt)
library(readtext)
library(DT)
library(tools)

# Set options
options(shiny.sanitize.errors = FALSE)

options(shiny.maxRequestSize = 100 * 1024 ^ 2)

# UI ----------------------------------------------------------------------

ui <- navbarPage(
    title = "statcheck // web", 
    collapsible = TRUE,
    header = tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
    ),
    tabPanel("Home",
      fluidRow(
        column(12,
          includeHTML("html/home.html"),
          hr(),
          fileInput("file", 
            label = "Upload files (pdf, html, or docx):",
            multiple = FALSE,
            accept = c('pdf/html/docx')
          ),
          checkboxInput("one_tail",
            label = "Try to identify and correct for one-tailed tests?",
            value = FALSE
          ),
          br(),
          shiny::uiOutput("table"),
          br(),
          conditionalPanel(
            condition = 'output.results',
            br(),
            downloadButton('download_data'),
            br(),
            br(),
          ),
          br(),
          br()
        )
      )
    ),
    tabPanel("Documentation", 
      includeHTML("html/documentation.html")
    ),
    tabPanel("About/FAQ",
      includeHTML("html/FAQ.html")
    ),
    tabPanel("Contact"),
    tabPanel("MS Word Add-in",
      includeHTML("html/word-addin.html")
    )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Create a reactive variable for creating the results table and download 
  # feature
  results <- reactive({
    
    # Get the file extension of the file
    file_extension <- tools::file_ext(input$file$name)
    
    # Extract text from the file, depending on the file extension
    if (file_extension == "pdf") {
      text <- pdftools::pdf_text(input$file$datapath)
    } else if (file_extension == "html")  {
      html <- paste(readLines(input$file$datapath), collapse = "\n")
      text <- htm2txt::htm2txt(html)
    } else if (file_extension == "docx") {
      word <- readtext::readtext(input$file$datapath)
      text <- word$text
    } else {
      error("Nope.")
    }
    
    # Run statcheck
    res <- statcheck::statcheck(text, OneTailedTests = input$one_tail)
    
    return(res)
  })

  # Detailed:
  output$results <- DT::renderDataTable({
    req(input$file)

    res <- results()
    res$Error <- ifelse(res$Error == FALSE, "Consistent", ifelse(
      res$DecisionError == TRUE, "Decision Inconsistency", "Inconsistency"))
    
    res <- subset(res, select = c(Source, Raw, Computed, Error))
    
    # Set the source equal to the file name
    res$Source <- input$file$name

    # Reduce width of long source names
    res$Source[nchar(res$Source) > 35] <- gsub("(?<=^.{30}).*", " (...)",  
        res$Source[nchar(res$Source) > 35], perl = TRUE)
    
    # Format the computer p-value column
    res$Computed <- sprintf("%.05f", res$Computed)

    # Create human-friendly column names
    names(res) <- c("Source", "Statistical reference", "Computed p-value",
      "Consistency")

    return(res)
  })

  # Download statcheck results
  output$download_data <- downloadHandler(
    filename = function() {"statcheck.csv"},
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    }
  )
  
  output$table <- renderUI({
    div(DT::dataTableOutput("results", width = "100%"))
  })
}

# Run application ---------------------------------------------------------

shinyApp(ui = ui, server = server)
