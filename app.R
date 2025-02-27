library(shiny)
library(tcpl)  # Load the tcpl package
library(DT)  # Load the DT package for interactive tables
library(ggplot2)  # Load ggplot2 for visualization
library(data.table)
library(shinyjs)  # For advanced JavaScript functionality

# Define UI for the application
ui <- fluidPage(
    useShinyjs(),  # Initialize shinyjs for showing/hiding content
    tags$head(
        tags$style(HTML("
            /* Stepper container */
            .stepper-container {
                width: 100%;
                margin: 20px 0;
            }
            
            /* Progress bar */
            .progress-bar-container {
                display: flex;
                justify-content: space-between;
                position: relative;
                margin-bottom: 30px;
            }
            
            /* Progress line */
            .progress-line {
                position: absolute;
                top: 15px;
                left: 0;
                width: 100%;
                height: 4px;
                background-color: #e0e0e0;
                z-index: 1;
            }
            
            /* Active progress line */
            .progress-line-active {
                position: absolute;
                top: 15px;
                left: 0;
                height: 4px;
                background-color: #4CAF50;
                z-index: 2;
                transition: width 0.3s ease;
            }
            
            /* Step circle */
            .step {
                width: 30px;
                height: 30px;
                border-radius: 50%;
                background-color: #e0e0e0;
                display: flex;
                align-items: center;
                justify-content: center;
                font-weight: bold;
                color: #fff;
                position: relative;
                z-index: 3;
                cursor: pointer;
            }
            
            /* Active step */
            .step.active {
                background-color: #4CAF50;
            }
            
            /* Completed step */
            .step.completed {
                background-color: #4CAF50;
            }
            
            /* Step label */
            .step-label {
                position: absolute;
                top: 35px;
                left: 50%;
                transform: translateX(-50%);
                white-space: nowrap;
                font-size: 12px;
                font-weight: bold;
            }
            
            /* Step content */
            .step-content {
                padding: 20px;
                border: 1px solid #e0e0e0;
                border-radius: 5px;
                margin-top: 20px;
            }
            
            /* Navigation buttons */
            .nav-buttons {
                display: flex;
                justify-content: space-between;
                margin-top: 20px;
            }
            
            /* Button styling */
            .btn-navigate {
                background-color: #4CAF50;
                color: white;
                padding: 10px 20px;
                border: none;
                border-radius: 4px;
                cursor: pointer;
            }
            
            .btn-navigate:disabled {
                background-color: #cccccc;
                cursor: not-allowed;
            }
        "))
    ),
    
    titlePanel("ToxCast Data Analysis Pipeline"),  # Add a title panel with the app name
    
    # Stepper container
    div(class = "stepper-container",
        # Progress bar
        div(class = "progress-bar-container",
            # Progress line background
            div(class = "progress-line"),
            # Active progress line (will be updated by JavaScript)
            div(id = "progress-line-active", class = "progress-line-active"),
            
            # Step 1: Introduction
            div(id = "step-1", class = "step active", "I",
                div(class = "step-label", "Introduction")),
            
            # Step 2: Data Upload
            div(id = "step-2", class = "step", "U",
                div(class = "step-label", "Data Upload")),
            
            # Step 3: Level 1 Processing
            div(id = "step-3", class = "step", "1",
                div(class = "step-label", "Level 1")),
            
            # Step 4: Level 2 Processing
            div(id = "step-4", class = "step", "2",
                div(class = "step-label", "Level 2")),
            
            # Step 5: Level 3 Processing
            div(id = "step-5", class = "step", "3",
                div(class = "step-label", "Level 3")),
            
            # Step 6: Level 4 Processing
            div(id = "step-6", class = "step", "4",
                div(class = "step-label", "Level 4")),
            
            # Step 7: Level 5 Processing
            div(id = "step-7", class = "step", "5",
                div(class = "step-label", "Level 5")),
            
            # Step 8: Level 6 Processing
            div(id = "step-8", class = "step", "6",
                div(class = "step-label", "Level 6"))
        ),
        
        # Step content containers
        div(id = "step-1-content", class = "step-content",
            h3("Welcome to the ToxCast Data Analysis Pipeline"),
            p("This application will guide you through the steps of analyzing ToxCast data."),
            p("Use the navigation buttons below to move through each processing level."),
            
            h4("Pipeline Steps:"),
            tags$ul(
                tags$li(strong("Level 1:"), " Converts the assay component to assay endpoint(s), defines the normalized-response value (resp), and optionally, derives the baseline value (bval) and positive control value (pval)."),
                tags$li(strong("Level 2:"), " Involves defining the corrected value (cval) based on the correction methods applied to the assay data. This step aggregates and processes the data to prepare it for further analysis, ensuring that the results are normalized and ready for interpretation."),
                tags$li(strong("Level 3:"), " Converts the assay component to assay endpoint(s), defines the normalized-response value (resp), and optionally, derives the baseline value (bval) and positive control value (pval)."),
                tags$li(strong("Level 4:"), " Applies advanced fitting methods to the processed Level 3 data, generating final results for analysis."),
                tags$li(strong("Level 5:"), " Determines the winning model and activity for the concentration series, bins all of the concentration series into fitc categories, and calculates various potency estimates.")
            ),
            
            div(class = "nav-buttons",
                actionButton("next-1", "Begin Analysis", class = "btn-navigate")
            )
        ),
        
        div(id = "step-2-content", class = "step-content", style = "display: none;",
            h3("Data Upload"),
            fileInput("file", "Upload CSV File"),  # A file input for uploading CSV files
            actionButton("view", "Preview Data"),  # An action button to view the data
            HTML("<div style='margin-top: 5px; margin-bottom: 15px; font-size: 12px; color: #666;'><i>Note: If no CSV file is uploaded, example data will be used.</i></div>"),
            div(class = "nav-buttons",
                actionButton("prev-2", "Previous", class = "btn-navigate"),
                actionButton("next-2", "Next", class = "btn-navigate")
            ),
            DT::dataTableOutput("dataTable")  # Display the uploaded data in a DT table AFTER the buttons
        ),
        
        div(id = "step-3-content", class = "step-content", style = "display: none;",
            h3("Level 1 Processing"),
            actionButton("process_data", "Process Level 1 Data"),  # Button to initiate Level 1 processing
            div(class = "nav-buttons",
                actionButton("prev-3", "Previous", class = "btn-navigate"),
                actionButton("next-3", "Next", class = "btn-navigate")
            ),
            DT::dataTableOutput("processedData"),  # Output for processed data
            plotOutput("processedPlot")  # Output for visualization
        ),
        
        div(id = "step-4-content", class = "step-content", style = "display: none;",
            h3("Level 2 Processing"),
            selectInput("method_select", "Select Correction Method:", 
                        choices = names(tcpl:::mc2_mthds()), selected = names(tcpl:::mc2_mthds())[1]),  # Dropdown for method selection
            actionButton("process_data_lvl2", "Process Level 2 Data"),  # Button to initiate Level 2 processing
            div(class = "nav-buttons",
                actionButton("prev-4", "Previous", class = "btn-navigate"),
                actionButton("next-4", "Next", class = "btn-navigate")
            ),
            DT::dataTableOutput("processedData_lvl2"),  # Output for processed Level 2 data
            plotOutput("processedPlot_lvl2")  # Output for visualization of Level 2 data
        ),
        
        div(id = "step-5-content", class = "step-content", style = "display: none;",
            h3("Level 3 Processing"),
            selectInput("method_select_lvl3", "Select Correction Method:", 
                        choices = names(tcpl:::mc3_mthds()), selected = names(tcpl:::mc3_mthds())[36]),  # Dropdown for method selection
            actionButton("process_data_lvl3", "Process Level 3 Data"),  # Button to initiate Level 3 processing
            div(class = "nav-buttons",
                actionButton("prev-5", "Previous", class = "btn-navigate"),
                actionButton("next-5", "Next", class = "btn-navigate")
            ),
            DT::dataTableOutput("processedData_lvl3"),  # Output for processed Level 3 data
            plotOutput("processedPlot_lvl3")  # Output for visualization of Level 3 data
        ),
        
        div(id = "step-6-content", class = "step-content", style = "display: none;",
            h3("Level 4 Processing"),
            selectInput("method_select_lvl4", "Select Correction Method:", 
                        choices = names(tcpl:::mc4_mthds()), selected = names(tcpl:::mc4_mthds())[1]),  # Dropdown for method selection
            actionButton("process_data_lvl4", "Process Level 4 Data"),  # Button to initiate Level 4 processing
            div(class = "nav-buttons",
                actionButton("prev-6", "Previous", class = "btn-navigate"),
                actionButton("next-6", "Next", class = "btn-navigate")
            ),
            DT::dataTableOutput("processedData_lvl4"),  # Output for processed Level 4 data
            plotOutput("processedPlot_lvl4")  # Output for visualization of Level 4 data
        ),
        
        div(id = "step-7-content", class = "step-content", style = "display: none;",
            h3("Level 5 Processing"),
            selectInput("method_select_lvl5", "Select Correction Method:", 
                        choices = names(tcpl:::mc5_mthds()), selected = names(tcpl:::mc5_mthds())[1]),  # Dropdown for method selection
            actionButton("process_data_lvl5", "Process Level 5 Data"),  # Button to initiate Level 5 processing
            div(class = "nav-buttons",
                actionButton("prev-7", "Previous", class = "btn-navigate"),
                actionButton("next-7", "Next", class = "btn-navigate")
            ),
            DT::dataTableOutput("processedData_lvl5")  # Output for processed Level 5 data
        ),
        
        div(id = "step-8-content", class = "step-content", style = "display: none;",
            h3("Level 6 Processing"),
            selectInput("method_select_lvl6", "Select Correction Method:", 
                        choices = c("method1", "method2"), selected = "method1"),  # Placeholder dropdown for method selection
            actionButton("process_data_lvl6", "Process Level 6 Data"),  # Button to initiate Level 6 processing
            div(class = "nav-buttons",
                actionButton("prev-8", "Previous", class = "btn-navigate")
            ),
            DT::dataTableOutput("processedData_lvl6")  # Output for processed Level 6 data
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    # Create a reactive dataset to hold the uploaded data
    uploaded_data <- reactiveVal(NULL)
    processed_data <- reactiveVal(NULL)  # Reactive value for Level 1 processed data
    processed_data_lvl2 <- reactiveVal(NULL)  # Reactive value for Level 2 processed data
    processed_data_lvl3 <- reactiveVal(NULL)  # Reactive value for Level 3 processed data
    processed_data_lvl4 <- reactiveVal(NULL)  # Reactive value for Level 4 processed data
    processed_data_lvl5 <- reactiveVal(NULL)  # Reactive value for Level 5 processed data
    processed_data_lvl6 <- reactiveVal(NULL)  # Reactive value for Level 6 processed data
    
    # Keep track of the current step
    current_step <- reactiveVal(1)
    
    # Function to update the progress bar and step indicators
    updateProgressBar <- function(step) {
        # Calculate the percentage for progress line
        percentage <- (step - 1) / 7 * 100
        
        # Update progress line width
        runjs(sprintf("document.getElementById('progress-line-active').style.width = '%s%%';", percentage))
        
        # Update step classes
        for (i in 1:8) {
            if (i < step) {
                # Mark previous steps as completed
                runjs(sprintf("document.getElementById('step-%s').className = 'step completed';", i))
            } else if (i == step) {
                # Mark current step as active
                runjs(sprintf("document.getElementById('step-%s').className = 'step active';", i))
            } else {
                # Mark future steps as inactive
                runjs(sprintf("document.getElementById('step-%s').className = 'step';", i))
            }
        }
        
        # Hide all step content
        for (i in 1:8) {
            shinyjs::hide(paste0("step-", i, "-content"))
        }
        
        # Show the current step content
        shinyjs::show(paste0("step-", step, "-content"))
        
        # Update current step value
        current_step(step)
    }
    
    # Step navigation event handlers
    observeEvent(input$`next-1`, {
        updateProgressBar(2)
    })
    
    observeEvent(input$`prev-2`, {
        updateProgressBar(1)
    })
    
    observeEvent(input$`next-2`, {
        updateProgressBar(3)
    })
    
    observeEvent(input$`prev-3`, {
        updateProgressBar(2)
    })
    
    observeEvent(input$`next-3`, {
        updateProgressBar(4)
    })
    
    observeEvent(input$`prev-4`, {
        updateProgressBar(3)
    })
    
    observeEvent(input$`next-4`, {
        updateProgressBar(5)
    })
    
    observeEvent(input$`prev-5`, {
        updateProgressBar(4)
    })
    
    observeEvent(input$`next-5`, {
        updateProgressBar(6)
    })
    
    observeEvent(input$`prev-6`, {
        updateProgressBar(5)
    })
    
    observeEvent(input$`next-6`, {
        updateProgressBar(7)
    })
    
    observeEvent(input$`prev-7`, {
        updateProgressBar(6)
    })
    
    observeEvent(input$`next-7`, {
        updateProgressBar(8)
    })
    
    observeEvent(input$`prev-8`, {
        updateProgressBar(7)
    })
    
    # Allow direct clicking on step indicators
    for (i in 1:7) {
        local({
            step_num <- i
            observeEvent(input[[paste0("step-", step_num)]], {
                updateProgressBar(step_num)
            })
        })
    }

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

    # Description UI rendered in introduction step only

    # Process Level 1 data when the "Process Level 1 Data" button is clicked
    observeEvent(input$process_data, {
        req(uploaded_data())  # Ensure data has been uploaded and stored
        data <- uploaded_data()  # Get the uploaded data

        # Show loading message
        showNotification("Processing Level 1 data...", type = "message", id = "process-notification")
        
        # Perform Level 1 processing using the mc1_df function
        processed_data(mc1_df(data))  # Update the reactive value for Level 1 processed data

        # Remove the loading message
        removeNotification("process-notification")
        
        # Show success message
        showNotification("Level 1 processing complete!", type = "message")
        
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

        # Show loading message
        showNotification("Processing Level 2 data...", type = "message", id = "process-notification-2")
        
        # Call the mc2 function with the processed Level 1 data
        tryCatch({
            method_selected <- input$method_select  # Get the selected method
            method_func <- tcpl:::mc2_mthds()[[method_selected]]  # Get the selected method function

            lvl2_data <- mc2_df(processed_data(), method_func)  # Call mc2 with the processed Level 1 data and selected method

            # Store the processed Level 2 data
            processed_data_lvl2(lvl2_data)
            
            # Remove the loading message
            removeNotification("process-notification-2")
            
            # Show success message
            showNotification("Level 2 processing complete!", type = "message")

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
            removeNotification("process-notification-2")
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

        # Show loading message
        showNotification("Processing Level 3 data...", type = "message", id = "process-notification-3")
        
        # Call the mc3 function with the processed Level 2 data
        tryCatch({
            method_selected <- input$method_select_lvl3  # Get the selected method
            method_func <- tcpl:::mc3_mthds()[[method_selected]]  # Get the selected method function

            lvl3_data <- mc3_df(processed_data_lvl2(), method_func)  # Call mc3 with the processed Level 2 data and selected method

            # Store the processed Level 3 data
            processed_data_lvl3(lvl3_data)
            
            # Remove the loading message
            removeNotification("process-notification-3")
            
            # Show success message
            showNotification("Level 3 processing complete!", type = "message")

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
            removeNotification("process-notification-3")
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

        # Show loading message
        showNotification("Processing Level 4 data...", type = "message", id = "process-notification-4")
        
        # Call the mc4 function with the processed Level 3 data
        tryCatch({
            method_selected <- input$method_select_lvl4  # Get the selected method
            method_func <- tcpl:::mc4_mthds()[[method_selected]]  # Get the selected method function

            lvl4_data <- mc4_df(processed_data_lvl3(), method_func)  # Call mc4 with the processed Level 3 data and selected method

            # Store the processed Level 4 data
            processed_data_lvl4(lvl4_data)
            
            # Remove the loading message
            removeNotification("process-notification-4")
            
            # Show success message
            showNotification("Level 4 processing complete!", type = "message")

            # Render the processed Level 4 data table
            output$processedData_lvl4 <- DT::renderDataTable({
                req(processed_data_lvl4())  # Ensure Level 4 data is available
                processed_data_lvl4()  # Display the processed Level 4 dataset
            })

            # Create a simple plot for Level 4 data
            output$processedPlot_lvl4 <- renderPlot({
                plot(processed_data_lvl4()$conc, processed_data_lvl4()$resp, 
                     xlab = "Concentration", ylab = "Response",
                     main = "Level 4 Data: Concentration vs Response")
            })
            
        }, error = function(e) {
            removeNotification("process-notification-4")
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

        # Show loading message
        showNotification("Processing Level 5 data...", type = "message", id = "process-notification-5")
        
        # Call the mc5 function with the processed Level 4 data
        tryCatch({
            method_selected <- input$method_select_lvl5  # Get the selected method
            method_func <- tcpl:::mc5_mthds()[[method_selected]]  # Get the selected method function

            lvl5_data <- mc5_df(processed_data_lvl4(), method_func)  # Call mc5 with the processed Level 4 data and selected method

            # Store the processed Level 5 data
            processed_data_lvl5(lvl5_data)
            
            # Remove the loading message
            removeNotification("process-notification-5")
            
            # Show success message
            showNotification("Level 5 processing complete!", type = "message")

            # Render the processed Level 5 data table
            output$processedData_lvl5 <- DT::renderDataTable({
                req(processed_data_lvl5())  # Ensure Level 5 data is available
                processed_data_lvl5()  # Display the processed Level 5 dataset
            })

        }, error = function(e) {
            removeNotification("process-notification-5")
            showNotification(paste("Error during Level 5 processing:", e$message), type = "error")
        })
    })
    
    # Process Level 6 data when the "Process Level 6 Data" button is clicked
    observeEvent(input$process_data_lvl6, {
        req(processed_data_lvl5())  # Ensure Level 5 processed data is available

        # Check if processed Level 5 data is empty
        if (nrow(processed_data_lvl5()) == 0) {
            showNotification("No data available for Level 6 processing. Please ensure Level 5 processing has valid data.", type = "error")
            return()  # Exit the function if no data is available
        }

        # Show loading message
        showNotification("Processing Level 6 data...", type = "message", id = "process-notification-6")
        
        # Call the mc6 function with the processed Level 5 data
        tryCatch({
            method_selected <- input$method_select_lvl6  # Get the selected method
            # Uncomment when mc6_mthds is implemented
            # method_func <- tcpl:::mc6_mthds()[[method_selected]]  # Get the selected method function
            
            # For now, use a placeholder method function
            method_func <- function() { list(quote({})) }
            
            lvl6_data <- mc6_df(processed_data_lvl5(), method_func)  # Call mc6 with the processed Level 5 data and selected method

            # Store the processed Level 6 data
            processed_data_lvl6(lvl6_data)
            
            # Remove the loading message
            removeNotification("process-notification-6")
            
            # Show success message
            showNotification("Level 6 processing complete!", type = "message")

            # Render the processed Level 6 data table
            output$processedData_lvl6 <- DT::renderDataTable({
                req(processed_data_lvl6())  # Ensure Level 6 data is available
                processed_data_lvl6()  # Display the processed Level 6 dataset
            })

        }, error = function(e) {
            removeNotification("process-notification-6")
            showNotification(paste("Error during Level 6 processing:", e$message), type = "error")
        })
    })
    
    # JavaScript to handle the dynamic progress bar
    js <- "
    $(document).ready(function() {
        // Make the step indicators clickable
        $('.step').on('click', function() {
            var stepId = $(this).attr('id');
            Shiny.setInputValue(stepId, Math.random());
        });
    });
    "
    
    session$onSessionEnded(function() {
        # Clean up code when the session ends
    })
    
    # Run the JavaScript code once the session is initialized
    runjs(js)
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
  
  myfun <- function(y) {
    res <- tcplfit2::tcplfit2_core(y$conc,
                                   y$resp,
                                   cutoff = bmad,
                                   bidirectional = TRUE,
                                   verbose = FALSE,
                                   force.fit = TRUE,
                                   fitmodels = fitmodels
    )
    list(list(res)) #use list twice because data.table uses list(.) to look for values to assign to columns
  }
  
  bmad <- max(unique(dat$bmad))
  dat[wllt == 't',params:= myfun(.SD), by = .(spid)]

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
  bmad <- max(dat$bmad)
  onesd <- bmad
  
  myfun2 <- function(y) {
    res <- tcplfit2::tcplhit2_core(params = y$params[[1]],
                                   conc = y$conc,
                                   resp = y$resp,
                                   cutoff = bmad,
                                   onesd = onesd
    )
    list(list(res))
  }
  
  # continue with hitcalling
  res <- dat[wllt == 't', myfun2(.SD), by = .(spid)]
  
  # pivot wider
  res_wide <- rbindlist(Map(cbind, spid = res$spid, res$V1))
  res_wide

}

mc6_df <- function(data, method_func) {
  # Start with the data from Level 5
  dat <- data
  
  # Return the processed data
  return(dat)
}


# Run the application 
shinyApp(ui = ui, server = server)