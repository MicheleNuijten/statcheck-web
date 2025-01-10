
# Setup -------------------------------------------------------------------

# Load packages
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
                    
                    # Only show the download button if there are results
                    uiOutput("downloadButtonUI"), 
                    
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
  tabPanel("Software/Packages",
           tags$div(class = "container",
                    includeHTML("html/software.html")
           )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  # Create variables to store statcheck results and error status
  values <- reactiveValues(res = NULL, error = NULL)
  
  # Render the error message
  output$error <- renderText(values$error)
  outputOptions(output, "error", suspendWhenHidden = FALSE)
  
  observe({
    files <- input$files
    
    # Reset error messages when a new file is uploaded
    values$error <- NULL
    
    # Validate file types
    valid_extensions <- c("pdf", "htm", "html", "doc", "docx")
    invalid_files <- !tools::file_ext(files$name) %in% valid_extensions
    if (any(invalid_files)) {
      values$error <- "Please select only PDF, HTML, or Word files."
      return(NULL)
    }
    
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
      req(input$files)
      files <- input$files # this creates a dataframe with file info
      
      # create empty list to store results of separate files
      statcheck_results <- list() 
      
      # Use progress indicator
      withProgress(message = 'Processing files...', value = 0, {
        
        # loop over files
        for(i in 1:nrow(files)){
          
          file <- files[i, ]
          
          # Check whether the user supplied a PDF, HTML, or MS Word file
          file_extension <- tools::file_ext(file$name)
          
          # Extract text from the file, depending on the file extension
          if (file_extension == "pdf") {
            text <- pdftools::pdf_text(file$datapath)
          } else if (file_extension %in% c("htm", "html"))  {
            html <- paste(readLines(file$datapath), collapse = "\n")
            text <- htm2txt::htm2txt(html)
          } else if (file_extension %in% c("doc", "docx")) {
            word <- readtext::readtext(file$datapath)
            text <- word$text
          }
          
          # store filename to return in final dataframe
          names(text) <- file$name
          
          # run statcheck in a try() environment to avoid the app from breaking
          # if a paper throws an error
          try_statcheck <- try(suppressMessages(
              statcheck::statcheck(text, OneTailedTxt = input$one_tail)
          ))
          
          if("try-error" %in% class(try_statcheck)){
            statcheck_results[[i]] <- NULL
          } else {
            statcheck_results[[i]] <- try_statcheck
          }
        }
        
        res <- do.call(rbind, statcheck_results)
  
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
        
        res <- subset(res, select = c(source, raw, computed_p, error))
        
        # Format the computer p-value column
        res$computed_p <- sprintf("%.05f", res$computed_p)
        
        # Create human-friendly column names
        names(res) <- c("File", "Statistical reference", "Computed p-value", 
                        "Consistency")
        
        # All went well so store that there is no error in case there previously was 
        # one
        values$error <- NULL
        
        # store result in a reactive value so that it can be accessed outside
        # this function as wel
        values$res <- res
        
        return(res)
      })
    }
  )
  
  
  # Conditionally render the download button if there are results
  output$downloadButtonUI <- renderUI({
    req(values$res)  # Ensure that results are available
    
    # Only render the download button if there are results
    if (!is.null(values$res) && nrow(values$res) > 0) {
      downloadButton("report", "Download report")
    }
  })
  
  # Render the report
  output$report <- downloadHandler(
    
    filename = function() {
      paste("statcheck_report_", Sys.Date(), ".pdf", sep = "")
    },
    
    content = function(file) {
      req(values$res)  # Ensure `res` is available
      
      # Copy the report template to a temporary location
      tempReport <- file.path(tempdir(), "report_template.Rmd")
      file.copy("templates/report_template.Rmd", tempReport, overwrite = TRUE)
      tempImg <- file.path(tempdir(), "statcheck-cropped.png")
      file.copy("www/img/statcheck-cropped.png", tempImg, overwrite = TRUE)
      
      # Collect additional information
      file_name <- input$files$name
      date <- Sys.Date()
      statcheck_version <- sessionInfo()$otherPkgs$statcheck$Version
      one_tailed <- ifelse(input$one_tail, "On", "Off")
      
      # Knit the document, passing the `params` list, and evaluate in an 
      # isolated environment
      rmarkdown::render(tempReport, output_file = file,
                        params = list(results = values$res, 
                                      file_name = file_name,
                                      date = date,
                                      statcheck_version = statcheck_version,
                                      one_tailed = one_tailed),  
                        output_format = "pdf_document",
                        envir = new.env(parent = globalenv()))
    }
  )
}

# Run application ---------------------------------------------------------

shinyApp(ui = ui, server = server)