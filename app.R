
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
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
      tags$script(src = "https://kit.fontawesome.com/0c3170759b.js", 
        crossorigin = "anonymous")
    ),
    tabPanel("Home",
      fluidRow(
        tags$div(class = "center",
          column(10, 
            includeHTML("html/home.html"),
            hr(),
            fileInput("file", 
              label = "Upload files (pdf, html, or docx):",
              multiple = FALSE,
              accept = c('pdf/html/docx')
            ),
            h5("Settings:", class = "settings"),
            checkboxInput("one_tail",
              label = "Try to identify and correct for one-tailed tests?",
              value = FALSE
            ),
            hr(),
            shiny::uiOutput("table")
          ) 
        )
      )
    ),
    tabPanel("Documentation", 
      fluidRow(
        tags$div(class = "center",
          column(10, 
            includeHTML("html/documentation.html")
          )
        )
      )
    ),
    tabPanel("About/FAQ",
      fluidRow(
        tags$div(class = "center",
          column(10, 
            includeHTML("html/FAQ.html")
          )
        )
      )
    ),
    tabPanel("Contact",
      fluidRow(
        tags$div(class = "center",
          column(10, 
            includeHTML("html/contact.html")
          )
        )
      )
    ),
    tabPanel("Contributors",
             fluidRow(
               tags$div(class = "center",
                        column(10, 
                               includeHTML("html/contributors.html")
                        )
               )
             )
    ),
    tabPanel("MS Word Add-in",
      fluidRow(
        tags$div(class = "center",
          column(10, 
            includeHTML("html/word-addin.html")
          )
        )
      )
    )
  )

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Create a reactive variable for creating the results table and download 
  # feature
  results <- reactive({
    # Get the file extension of the file
    file_extension <- tools::file_ext(input$file$name)
    
    # Check whether the user supplied a pdf, html, or Word file.
    validate(
      need(file_extension %in% c("pdf", "html", "doc", "docx"), 
        "Please select a PDF, HTML, or Word file.")
    )
    
    # Extract text from the file, depending on the file extension
    if (file_extension == "pdf") {
      text <- pdftools::pdf_text(input$file$datapath)
    } else if (file_extension == "html")  {
      html <- paste(readLines(input$file$datapath), collapse = "\n")
      text <- htm2txt::htm2txt(html)
    } else if (file_extension %in% c("doc", "docx")) {
      word <- readtext::readtext(input$file$datapath)
      text <- word$text
    }
    
    # Run statcheck
    res <- statcheck::statcheck(text, OneTailedTests = input$one_tail)
    
    # Check whether any results were found
    validate(
      need(nrow(res) > 0, "No results found.")
    )
    
    return(res)
  })

  # Detailed:
  output$results <- DT::renderDataTable(
    extensions = "Buttons", 
    options = list(
      dom = 'Bfrtip',
      buttons = 
        list('copy', 'print', list(
          extend = 'collection',
          buttons = list(
            list(extend = 'csv', filename = input$file$name),
            list(extend = 'excel', filename = input$file$name),
            list(extend = 'pdf', filename = input$file$name)
          ),
          text = 'Download'
        )
      )
    ),
    {
      req(input$file)
  
      res <- results()
      
      res$Error <- ifelse(res$Error == FALSE, "Consistent", ifelse(
      res$DecisionError == TRUE, "Decision Inconsistency", "Inconsistency"))
    
      res <- subset(res, select = c(Raw, Computed, Error))
      
      # Format the computer p-value column
      res$Computed <- sprintf("%.05f", res$Computed)
  
      # Create human-friendly column names
      names(res) <- c("Statistical reference", "Computed p-value", "Consistency")
  
      return(res)
    }
  )
  
  output$table <- renderUI({
    div(DT::dataTableOutput("results"))
  })
}

# Run application ---------------------------------------------------------

shinyApp(ui = ui, server = server)
