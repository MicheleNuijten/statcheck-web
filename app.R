
# Setup -------------------------------------------------------------------

# Load packages
library(shiny)
library(bslib)
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
    theme = bs_theme(version = 5),
    title = "statcheck // web", 
    collapsible = TRUE,
    header = tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
      tags$script(src = "https://kit.fontawesome.com/0c3170759b.js", 
        crossorigin = "anonymous")
    ),
    tabPanel("Home",
      fluidRow(
        column(width = 10, offset = 1,
          tags$div(class = "center",
            tags$img(
              src = "./img/statcheck-cropped.png", 
              title = "statcheck",
              style = "max-width: 500px"),
            tags$p(class = "fw-bold fs-4", "statcheck on the web")
          ),
          tags$p(
            "To check a PDF, DOCX or HTML file for errors in statistical 
            reporting, upload it below. See the FAQ page for more information 
            about what statcheck can and cannot do."
          ),
          hr(),
          fileInput("file", 
            label = "Upload files (pdf, html, or docx):",
            multiple = FALSE,
            accept = c("pdf", "html", "doc", "docx")
          ),
          h5("Settings:", class = "settings"),
          checkboxInput("one_tail",
            label = "Try to identify and correct for one-tailed tests?",
            value = FALSE,
            width = "100%"
          ),
          hr(),
          conditionalPanel(
            condition = "!output.error",
            DT::dataTableOutput("table"),
          ),
          conditionalPanel(
            condition = "output.error",
            tags$div(class = "text-danger fw-bold",
              textOutput("error")
            )
          )
        ) 
      ) 
    ),
    tabPanel("FAQ",
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
        column(width = 10, offset = 1, 
          includeHTML("html/contact.html")
        )
      )
    ),
    tabPanel("Contributors",
      fluidRow(
        column(width = 10, offset = 1,
          includeHTML("html/contributors.html")
        )
      )
    ),
    tabPanel("MS Word Add-in",
      fluidRow(
        column(width = 10, offset = 1,
          includeHTML("html/word-addin.html")
        )
      )
    )
  )

# Server ------------------------------------------------------------------

server <- function(input, output) {
  # Create variables to store an error status in
  values <- reactiveValues(error = NULL)
  
  # Render the error message
  output$error <- renderText(values$error)
  outputOptions(output, "error", suspendWhenHidden = FALSE)
  
  # Render the statcheck results table
  output$table <- renderDataTable(
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
      # Check whether the use uploaded a file
      file <- input$file

      if (is.null(file))
        return(NULL)
      
      # Check whether the user supplied a PDF, HTML, or MS Word file
      file_extension <- tools::file_ext(file$name)
      
      if (!file_extension %in% c("pdf", "html", "doc", "docx")) {
        values$error <- "Please select a PDF, HTML, or Word file."
      
        return(NULL)
      } 
    
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
      suppressMessages(
        res <- statcheck::statcheck(text, OneTailedTests = input$one_tail)
      )
      
      # Check whether any results were found
      if (is.null(res)) {
        values$error <- "No results found. See the FAQ page for some common 
          reasons why statcheck doesn't detect some results."
        
        return(NULL)
      }
      
      # Clean up the data frame
      res$Error <- ifelse(res$Error == FALSE, "Consistent", ifelse(
      res$DecisionError == TRUE, "Decision Inconsistency", "Inconsistency"))
    
      res <- subset(res, select = c(Raw, Computed, Error))
      
      # Format the computer p-value column
      res$Computed <- sprintf("%.05f", res$Computed)
  
      # Create human-friendly column names
      names(res) <- c("Statistical reference", "Computed p-value", "Consistency")
      
      # All went well so store there is no error in case there previously was 
      # one
      values$error <- NULL

      return(res)
    }
  )
}

# Run application ---------------------------------------------------------

shinyApp(ui = ui, server = server)
