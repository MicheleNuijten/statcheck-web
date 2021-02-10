
# Setup -------------------------------------------------------------------

# Load packages
library(shiny)
library(shinythemes)
# devtools::install_github("MicheleNuijten/statcheck")
library(statcheck)
library(DT)
library(tools)

# Set options
options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

# UI ----------------------------------------------------------------------

ui <- navbarPage(
    title = "statcheck // web", 
    theme = shinythemes::shinytheme("cosmo"),
    tabPanel("Home",
      includeMarkdown("home.md"),
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
    ),
    tabPanel("Documentation", 
      includeMarkdown("documentation.md")
    ),
    tabPanel("About/FAQ",
      includeMarkdown("FAQ.md")
    ),
    tabPanel("Contact"),
    tabPanel("MS Word Add-in",
      includeMarkdown("word-addin.md")
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
      res <- statcheck::checkPDF(input$file$datapath, 
        OneTailedTests = input$one_tail)
    } else if (file_extension == "html")  {
      res <- statcheck::checkHTML(input$file$datapath, 
        OneTailedTests = input$one_tail)
    } else if (file_extension == "docx") {
      error("To do")
    } else {
      error("Nope.")
    }
    
    # Run statcheck
    return(res)
  })

  # Detailed:
  output$results <- DT::renderDataTable({
    req(input$file)

    res <- results()
    res$consistency <- ifelse(res$error == FALSE, "Consistent", ifelse(
      res$decision_error == TRUE, "Decision Inconsistency", "Inconsistency"))
    
    res <- subset(res, select = c(source, raw, computed_p, consistency))
    
    # Set the source equal to the file name
    res$source <- input$file$name
    
    # Format the computer p-value column
    res$computed_p <- sprintf("%.05f", res$computed_p)

    # Reduce width of long source names
    res$source[nchar(res$source) > 35] <- gsub("(?<=^.{30}).*", " (...)",  
        res$source[nchar(res$source) > 35], perl = TRUE)

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
# rsconnect::deployApp()