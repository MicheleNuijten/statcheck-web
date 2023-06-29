
# Setup -------------------------------------------------------------------

# Load packages
#devtools::install_github("MicheleNuijten/statcheck@feature-statcheck2.0")
library(statcheck)
library(shiny)
library(bslib)
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
    tags$script(
      src = "https://kit.fontawesome.com/0c3170759b.js", 
      crossorigin = "anonymous")
  ),
  tabPanel("Home",
           tags$div(class = "container",
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
                    fileInput(inputId = "files", 
                              label = "Upload files (pdf, html, or docx):",
                              multiple = TRUE,
                              accept = c("pdf", "htm", "html", "doc", "docx")
                    ),
                    h5("Settings:", class = "settings"),
                    checkboxInput("one_tail",
                                  label = "Try to identify and correct for one-tailed tests",
                                  value = FALSE,
                                  width = "100%"
                    ),
                    hr(),
                    conditionalPanel(
                      condition = "!output.error",
                      DT::dataTableOutput("table"),
                      textOutput("sessionInfo")
                    ),
                    conditionalPanel(
                      condition = "output.error",
                      tags$div(
                        class = "text-danger fw-bold",
                        textOutput("error")
                      )
                    ),
                    conditionalPanel(
                      condition = "output.error",
                      tags$div(
                        class = "mt-3",
                        textOutput("sessionInfo2")
                      )
                    )
           ) 
  ),
  tabPanel("FAQ",
           tags$div(class = "container",
                    includeHTML("html/FAQ.html")
           )
  ),
  tabPanel("Contact",
           tags$div(class = "container",
                    includeHTML("html/contact.html")
           )
  ),
  tabPanel("Contributors",
           tags$div(class = "container",
                    includeHTML("html/contributors.html")
           )
  ),
  tabPanel("MS Word Add-in",
           tags$div(class = "container",
                    includeHTML("html/word-addin.html")
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
  
  observe({
    files <- input$files
    
    # Reset error messages when a new file is uploaded
    values$error <- NULL
  })
  
  # Render the statcheck results table
  output$table <- renderDataTable(
    extensions = "Buttons", 
    server = FALSE,
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
      files <- input$files
      
      # create empty list to store results of separate files
      statcheck_results <- list() 
      
      # loop over files
      for(i in seq_along(files)){
        
        file <- files[i]
        
        # Check whether the user supplied a PDF, HTML, or MS Word file
        file_extension <- tools::file_ext(file$name)
        
        if (!file_extension %in% c("pdf", "htm", "html", "doc", "docx")) {
          values$error <- "Please select only PDF, HTML, or Word files."
          
          return(NULL)
        } 
        
        
        # Extract text from the file, depending on the file extension
        if (file_extension == "pdf") {
          text <- pdftools::pdf_text(input$file$datapath)
        } else if (file_extension %in% c("htm", "html"))  {
          html <- paste(readLines(input$file$datapath), collapse = "\n")
          text <- htm2txt::htm2txt(html)
        } else if (file_extension %in% c("doc", "docx")) {
          word <- readtext::readtext(input$file$datapath)
          text <- word$text
        }
        
        # Run statcheck
        suppressMessages(
          statcheck_results[[i]] <- 
            statcheck::statcheck(text, OneTailedTxt = input$one_tail)
        )
        
        res <- merge(unlist(statcheck_results))
      }
      
      # Print which statcheck version was run
      version <- sessionInfo()$otherPkgs$statcheck$Version
      output$sessionInfo <- renderText({
        paste0("Statcheck package version: ", version)
      })
      
      # create a second session info for second conditional panel
      # bit of a hacky solution...
      output$sessionInfo2 <- renderText({
        paste0("Statcheck package version: ", version)
      })
      
      # Check whether any results were found
      if (is.null(res)) {
        values$error <- "No results found. See the FAQ page for some common 
          reasons why statcheck doesn't detect some results."
        
        return(NULL)
      }
      
      # Check whether old or new variable names are used in results data frame
      # If old: change to the new names to ensure compatibility with the app.
      # This is a bit of a hacky solution to make sure the transition to the new
      # statcheck version on CRAN goes smoothly. In time we can remove this code
      if ("Source" %in% names(res)) {
        names(res) <- c("source", "test_type", "df1", "df2",  "test_comp", 
                        "test_value", "p_comp", "reported_p", "computed_p", "raw", "error", 
                        "decision_error", "one_tailed_in_txt", "apa_factor")
      }
      
      # Clean up the data frame
      res$error <- ifelse(res$error == FALSE, "Consistent", ifelse(
        res$decision_error == TRUE, "Decision Inconsistency", "Inconsistency")
      )
      
      res <- subset(res, select = c(raw, computed_p, error))
      
      # Format the computer p-value column
      res$computed_p <- sprintf("%.05f", res$computed_p)
      
      # Create human-friendly column names
      names(res) <- c("Statistical reference", "Computed p-value", 
                      "Consistency")
      
      # All went well so store there is no error in case there previously was 
      # one
      values$error <- NULL
      
      return(res)
    }
  )
}

# Run application ---------------------------------------------------------

shinyApp(ui = ui, server = server)
