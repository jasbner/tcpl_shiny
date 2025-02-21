library(shiny)
library(tcpl)  # Load the tcpl package
library(DT)  # Load the DT package for interactive tables
library(ggplot2)  # Load ggplot2 for visualization

# Define UI for the application
ui <- fluidPage(
    titlePanel("My Shiny App"),  # Add a title panel with the app name

    tabsetPanel(
        id = "steps",  # ID for the tabset
        tabPanel("Introduction", 
                 h3("Welcome to the ToxCast Data Analysis Pipeline"),
                 p("This application will guide you through the steps of analyzing ToxCast data.")
        ),
        tabPanel("Data Upload", 
                 sidebarLayout(
                     sidebarPanel(
                         fileInput("file", "Upload CSV File"),  # A file input for uploading CSV files
                         actionButton("view", "View Data")  # An action button to view the data
                     ),
                     mainPanel(
                         DT::dataTableOutput("dataTable")  # A main panel to display the uploaded data in a DT table
                     )
                 )
        ),
        tabPanel("Level 1 Processing",  # New tab for Level 1 processing
                 h3("Level 1 Processing Overview"),
                 actionButton("toggle_desc", "Show Description"),  # Button to toggle description
                 uiOutput("description_ui"),  # Use uiOutput to dynamically render the description
                 actionButton("process_data", "Process Level 1 Data"),  # Button to initiate Level 1 processing
                 DT::dataTableOutput("processedData"),  # Output for processed data
                 plotOutput("processedPlot")  # Output for visualization
        ),
        tabPanel("Level 2 Processing",  # New tab for Level 2 processing
                 h3("Level 2 Processing Overview"),
                 selectInput("method_select", "Select Correction Method:", 
                             choices = names(tcpl:::mc2_mthds()), selected = names(tcpl:::mc2_mthds())[1]),  # Dropdown for method selection
                 actionButton("toggle_desc_lvl2", "Show Description"),  # Button to toggle description
                 uiOutput("description_ui_lvl2"),  # Use uiOutput to dynamically render the description
                 actionButton("process_data_lvl2", "Process Level 2 Data"),  # Button to initiate Level 2 processing
                 DT::dataTableOutput("processedData_lvl2"),  # Output for processed Level 2 data
                 plotOutput("processedPlot_lvl2")  # Output for visualization of Level 2 data
        ),
        tabPanel("Level 3 Processing",  # New tab for Level 3 processing
                 h3("Level 3 Processing Overview"),
                 selectInput("method_select_lvl3", "Select Correction Method:", 
                             choices = names(tcpl:::mc3_mthds()), selected = names(tcpl:::mc3_mthds())[1]),  # Dropdown for method selection
                 actionButton("toggle_desc_lvl3", "Show Description"),  # Button to toggle description
                 uiOutput("description_ui_lvl3"),  # Use uiOutput to dynamically render the description
                 actionButton("process_data_lvl3", "Process Level 3 Data"),  # Button to initiate Level 3 processing
                 DT::dataTableOutput("processedData_lvl3"),  # Output for processed Level 3 data
                 plotOutput("processedPlot_lvl3")  # Output for visualization of Level 3 data
        ),
        tabPanel("Level 4 Processing",  # New tab for Level 4 processing
                 h3("Level 4 Processing Overview"),
                 selectInput("method_select_lvl4", "Select Correction Method:", 
                             choices = names(tcpl:::mc4_mthds()), selected = names(tcpl:::mc4_mthds())[1]),  # Dropdown for method selection
                 actionButton("toggle_desc_lvl4", "Show Description"),  # Button to toggle description
                 uiOutput("description_ui_lvl4"),  # Use uiOutput to dynamically render the description
                 actionButton("process_data_lvl4", "Process Level 4 Data"),  # Button to initiate Level 4 processing
                 DT::dataTableOutput("processedData_lvl4"),  # Output for processed Level 4 data
                 plotOutput("processedPlot_lvl4")  # Output for visualization of Level 4 data
        ),
        tabPanel("Level 5 Processing",  # New tab for Level 5 processing
                 h3("Level 5 Processing Overview"),
                 actionButton("toggle_desc_lvl5", "Show Description"),  # Button to toggle description
                 uiOutput("description_ui_lvl5"),  # Use uiOutput to dynamically render the description
                 selectInput("method_select_lvl5", "Select Correction Method:", 
                             choices = names(tcpl:::mc5_mthds()), selected = names(tcpl:::mc5_mthds())[1]),  # Dropdown for method selection
                 actionButton("process_data_lvl5", "Process Level 5 Data"),  # Button to initiate Level 5 processing
                 DT::dataTableOutput("processedData_lvl5")  # Output for processed Level 5 data
        )
    )
)

# Define server logic
server <- function(input, output) {
    # Create a reactive dataset to hold the uploaded data
    uploaded_data <- reactiveVal(NULL)
    processed_data <- reactiveVal(NULL)  # Reactive value for Level 1 processed data
    processed_data_lvl2 <- reactiveVal(NULL)  # Reactive value for Level 2 processed data
    processed_data_lvl3 <- reactiveVal(NULL)  # Reactive value for Level 3 processed data
    processed_data_lvl4 <- reactiveVal(NULL)  # Reactive value for Level 4 processed data
    processed_data_lvl5 <- reactiveVal(NULL)  # Reactive value for Level 5 processed data

    # Render the data table using DT when the "View Data" button is clicked
    observeEvent(input$view, {
        if (is.null(input$file)) {
            uploaded_data(mcdat)  # Store the default dataset in the reactive value
        } else {
            data <- read.csv(input$file$datapath)
            uploaded_data(data)  # Store the uploaded data in the reactive value
        }

        # Render the uploaded data table
        output$dataTable <- DT::renderDataTable({
            req(uploaded_data())  # Ensure data is available
            uploaded_data()  # Display the uploaded dataset
        })
    })

    # Render the description based on button click for Level 1
    output$description_ui <- renderUI({
        if (input$toggle_desc %% 2 == 1) {  # Check if the button has been clicked an odd number of times
            withMathJax(HTML("<p>Level 1 processing converts the assay component to assay endpoint(s), defines the normalized-response value (resp), and optionally, derives the baseline value (bval) and positive control value (pval).</p>"))
        }
    })

    # Render the description based on button click for Level 2
    output$description_ui_lvl2 <- renderUI({
        if (input$toggle_desc_lvl2 %% 2 == 1) {  # Check if the button has been clicked an odd number of times
            withMathJax(HTML("<p>Level 2 processing involves defining the corrected value (cval) based on the correction methods applied to the assay data. This step aggregates and processes the data to prepare it for further analysis, ensuring that the results are normalized and ready for interpretation.</p>"))
        }
    })

    # Render the description based on button click for Level 3
    output$description_ui_lvl3 <- renderUI({
        if (input$toggle_desc_lvl3 %% 2 == 1) {  # Check if the button has been clicked an odd number of times
            withMathJax(HTML("<p>Level 3 processing converts the assay component to assay endpoint(s), defines the normalized-response value (resp), and optionally, derives the baseline value (bval) and positive control value (pval).</p>"))
        }
    })

    # Render the description based on button click for Level 4
    output$description_ui_lvl4 <- renderUI({
        if (input$toggle_desc_lvl4 %% 2 == 1) {  # Check if the button has been clicked an odd number of times
            withMathJax(HTML("<p>Level 4 processing applies advanced fitting methods to the processed Level 3 data, generating final results for analysis.</p>"))
        }
    })

    # Render the description based on button click for Level 5
    output$description_ui_lvl5 <- renderUI({
        if (input$toggle_desc_lvl5 %% 2 == 1) {  # Check if the button has been clicked an odd number of times
            withMathJax(HTML("<p>Level 5 processing determines the winning model and activity for the concentration series, bins all of the concentration series into fitc categories, and calculates various potency estimates.</p>"))
        }
    })

    # Process Level 1 data when the "Process Level 1 Data" button is clicked
    observeEvent(input$process_data, {
        req(uploaded_data())  # Ensure data has been uploaded and stored
        data <- uploaded_data()  # Get the uploaded data

        # Perform Level 1 processing using the mc1_df function
        processed_data(mc1_df(data))  # Update the reactive value for Level 1 processed data

        # Render the processed data table for Level 1
        output$processedData <- DT::renderDataTable({
            req(processed_data())  # Ensure Level 1 processed data is available
            processed_data()  # Display the processed dataset
        })

        # Create a simple plot of the processed data (example visualization)
        output$processedPlot <- renderPlot({
            ggplot(processed_data(), aes(x = acid, y = cndx)) +
                geom_bar(stat = "identity") +
                labs(title = "Processed Level 1 Data", x = "Assay Component ID (acid)", y = "Concentration Index (cndx)")
        })
    })

    # Process Level 2 data when the "Process Level 2 Data" button is clicked
    observeEvent(input$process_data_lvl2, {
        req(processed_data())  # Ensure Level 1 processed data is available

        # Check if processed data is empty
        if (nrow(processed_data()) == 0) {
            showNotification("No data available for Level 2 processing. Please ensure Level 1 processing has valid data.", type = "error")
            return()  # Exit the function if no data is available
        }

        # Call the mc2 function with the processed Level 1 data
        tryCatch({
            method_selected <- input$method_select  # Get the selected method
            method_func <- tcpl:::mc2_mthds()[[method_selected]]  # Get the selected method function

            lvl2_data <- mc2_df(processed_data(), method_func)  # Call mc2 with the processed Level 1 data and selected method

            # Store the processed Level 2 data
            processed_data_lvl2(lvl2_data)

            # Render the processed Level 2 data table
            output$processedData_lvl2 <- DT::renderDataTable({
                req(processed_data_lvl2())  # Ensure Level 2 data is available
                processed_data_lvl2()  # Display the processed Level 2 dataset
            })

            # Create a plot of the processed Level 2 data (example visualization)
            output$processedPlot_lvl2 <- renderPlot({
                ggplot(processed_data_lvl2(), aes(x = acid, y = cval)) +  # Adjust the aesthetics as needed
                    geom_bar(stat = "identity") +
                    labs(title = "Processed Level 2 Data", x = "Assay Component ID (acid)", y = "Corrected Value (cval)")
            })
        }, error = function(e) {
            showNotification(paste("Error during Level 2 processing:", e$message), type = "error")
        })
    })

    # Process Level 3 data when the "Process Level 3 Data" button is clicked
    observeEvent(input$process_data_lvl3, {
        req(processed_data_lvl2())  # Ensure Level 2 processed data is available

        # Check if processed Level 2 data is empty
        if (nrow(processed_data_lvl2()) == 0) {
            showNotification("No data available for Level 3 processing. Please ensure Level 2 processing has valid data.", type = "error")
            return()  # Exit the function if no data is available
        }

        # Call the mc3 function with the processed Level 2 data
        tryCatch({
            method_selected <- input$method_select_lvl3  # Get the selected method
            method_func <- tcpl:::mc3_mthds()[[method_selected]]  # Get the selected method function

            lvl3_data <- mc3_df(processed_data_lvl2(), method_func)  # Call mc3 with the processed Level 2 data and selected method

            # Store the processed Level 3 data
            processed_data_lvl3(lvl3_data)

            # Render the processed Level 3 data table
            output$processedData_lvl3 <- DT::renderDataTable({
                req(processed_data_lvl3())  # Ensure Level 3 data is available
                processed_data_lvl3()  # Display the processed Level 3 dataset
            })

            # Create a plot of the processed Level 3 data (example visualization)
            output$processedPlot_lvl3 <- renderPlot({
                ggplot(processed_data_lvl3(), aes(x = aeid, y = resp)) +  # Adjust the aesthetics as needed
                    geom_bar(stat = "identity") +
                    labs(title = "Processed Level 3 Data", x = "Assay Endpoint ID (aeid)", y = "Normalized Response (resp)")
            })
        }, error = function(e) {
            showNotification(paste("Error during Level 3 processing:", e$message), type = "error")
        })
    })

    # Process Level 4 data when the "Process Level 4 Data" button is clicked
    observeEvent(input$process_data_lvl4, {
        req(processed_data_lvl3())  # Ensure Level 3 processed data is available

        # Check if processed Level 3 data is empty
        if (nrow(processed_data_lvl3()) == 0) {
            showNotification("No data available for Level 4 processing. Please ensure Level 3 processing has valid data.", type = "error")
            return()  # Exit the function if no data is available
        }

        # Call the mc4 function with the processed Level 3 data
        tryCatch({
            method_selected <- input$method_select_lvl4  # Get the selected method
            method_func <- tcpl:::mc4_mthds()[[method_selected]]  # Get the selected method function

            lvl4_data <- mc4_df(processed_data_lvl3(), method_func)  # Call mc4 with the processed Level 3 data and selected method

            # Store the processed Level 4 data
            processed_data_lvl4(lvl4_data)

            # Render the processed Level 4 data table
            output$processedData_lvl4 <- DT::renderDataTable({
                req(processed_data_lvl4())  # Ensure Level 4 data is available
                processed_data_lvl4()  # Display the processed Level 4 dataset
            })

            
        }, error = function(e) {
            showNotification(paste("Error during Level 4 processing:", e$message), type = "error")
        })
    })

    # Process Level 5 data when the "Process Level 5 Data" button is clicked
    observeEvent(input$process_data_lvl5, {
        req(processed_data_lvl4())  # Ensure Level 4 processed data is available

        # Check if processed Level 4 data is empty
        if (nrow(processed_data_lvl4()) == 0) {
            showNotification("No data available for Level 5 processing. Please ensure Level 4 processing has valid data.", type = "error")
            return()  # Exit the function if no data is available
        }

        # Call the mc5 function with the processed Level 4 data
        tryCatch({
            method_selected <- input$method_select_lvl5  # Get the selected method
            method_func <- tcpl:::mc5_mthds()[[method_selected]]  # Get the selected method function

            lvl5_data <- mc5_df(processed_data_lvl4(), method_func)  # Call mc5 with the processed Level 4 data and selected method

            # Store the processed Level 5 data
            processed_data_lvl5(lvl5_data)

            # Render the processed Level 5 data table
            output$processedData_lvl5 <- DT::renderDataTable({
                req(processed_data_lvl5())  # Ensure Level 5 data is available
                processed_data_lvl5()  # Display the processed Level 5 dataset
            })

        }, error = function(e) {
            showNotification(paste("Error during Level 5 processing:", e$message), type = "error")
        })
    })
}

# Function to perform Level 1 processing on a data frame
mc1_df <- function(data) {
    data <- data.table(data)
    data[, acid := 1]  # Add a dummy acid column for processing
    # Check if the required columns are present
    if (!all(c("acid", "conc", "spid","rval", "wllt", "srcf", "apid") %in% colnames(data))) {
        stop("Data frame must contain columns: acid, conc, spid, rval, wllt, srcf, apid")
    }

    # Set concentration to three significant figures
    data[, conc := signif(conc, 3)]

    # Set concentration index
    data[, cndx := frank(conc, ties.method = "dense"), by = .(acid, spid, wllt, srcf, apid)]

    # Set replicate index
    data[, repi := rowid(conc), by = .(acid, spid, wllt, srcf, apid)]

    processed_data <- data

    return(processed_data)
}

# Function to perform Level 2 processing on a data frame
mc2_df <- function(data, method_func) {
     ## Load level 1 data
  dat <- data

  ## Add corrected value column
  dat[ , cval := rval]
  setkey(dat, acid)

  ## Set all wllq to 0 for all NA cvals
  dat[is.na(cval), wllq := 0]

  ## Remove data with well quality of 0
  dat <- dat[wllq == 1]

 
  ## Load the functions to generate correction expressions
  invisible(eval(do.call(method_func,list())[[1]]))
  ## Remove data with well quality of 0 after correction methods
  dat <- dat[wllq == 1]

  
  dat

}

# Function to perform Level 3 processing on a data frame
mc3_df <- function(data, method_func) {
  
  ## Load level 2 data
  dat <- as.data.table(data)

  dat[, aeid := 1]
  setkey(dat, aeid)

  
  ## Initialize the bval, pval, and resp columns
  dat[ , c('bval', 'pval', 'resp') := NA_real_]

  aeids <- c(1)

  invisible(eval(do.call(method_func,list(1))[[1]]))

  dat
  
}

mc4_df <- function(data, method_func) {
  
  dat <- data[!is.na(conc)]

  invisible(eval(do.call(method_func,list())[[1]]))

  fitmodels <- c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3", "exp4", "exp5")
  dat <- tcpl:::tcplFit2(dat, fitmodels = fitmodels)

  dat
}

mc5_df <- function(data, method_func) {
  dat <- data
  
  ## Initialize coff vector
  coff <- 0
  model_type <- 0
  loec.mthd <- FALSE
  
  
  
  ## Determine final cutoff
  dat[ , coff := max(coff)]
  cutoff <- max(dat$coff)
  invisible(eval(do.call(method_func,list())[[1]]))
  myfun2 <- function(y) {
    res <- tcplfit2::tcplhit2_core(params = y$params[[1]],
                                   conc = y$conc,
                                   resp = y$resp,
                                   cutoff = 3*bmad,
                                   onesd = onesd
    )
    list(list(res))
  }
  
  # continue with hitcalling
  res <- dat[wllt == 't', myfun2(.SD), by = .(spid)]
  
  # pivot wider
  res_wide <- rbindlist(Map(cbind, spid = res$spid, res$V1))
  dat <- tcpl:::tcplHit2(dat, coff = cutoff)
  dat

}

# Run the application 
shinyApp(ui = ui, server = server)
