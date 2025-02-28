library(shiny)
library(tcpl)  # Load the tcpl package
library(DT)  # Load the DT package for interactive tables
library(ggplot2)  # Load ggplot2 for visualization
library(data.table)
library(shinyjs)  # For advanced JavaScript functionality
library(bslib)  # For Bootstrap 5 components
library(plotly)  # For interactive plots

# Define scientific color scheme
toxcast_colors <- list(
  primary = "#2C3E50",    # Dark blue-slate
  secondary = "#18BC9C",  # Teal
  info = "#3498DB",       # Blue
  warning = "#F39C12",    # Orange
  danger = "#E74C3C"      # Red
)

# Define UI for the application
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    primary = toxcast_colors$primary,
    secondary = toxcast_colors$secondary,
    success = toxcast_colors$secondary,
    info = toxcast_colors$info,
    warning = toxcast_colors$warning,
    danger = toxcast_colors$danger,
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    font_scale = 1
  ),
  
  useShinyjs(),  # Initialize shinyjs for showing/hiding content
  
  tags$head(
    tags$style(HTML("
      :root {
        --spacing-sm: 8px;
        --spacing-md: 16px;
        --spacing-lg: 24px;
        --border-radius: 8px;
        --box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        --transition-speed: 0.3s;
      }
      
      body {
        background-color: #f8f9fa;
      }
      
      .card {
        border-radius: var(--border-radius);
        box-shadow: var(--box-shadow);
        margin-bottom: var(--spacing-lg);
        border: none;
        overflow: hidden;
      }
      
      .card-header {
        background-color: #f8f9fa;
        border-bottom: 1px solid rgba(0,0,0,0.1);
        padding: var(--spacing-md);
      }
      
      .card-body {
        padding: var(--spacing-lg);
      }
      
      /* Stepper container */
      .stepper-container {
        width: 100%;
        margin: var(--spacing-lg) 0;
      }
      
      /* Progress bar */
      .progress-bar-container {
        display: flex;
        justify-content: space-between;
        position: relative;
        margin-bottom: var(--spacing-lg);
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
        background-color: var(--bs-secondary);
        z-index: 2;
        transition: width var(--transition-speed) ease;
      }
      
      /* Step circle */
      .step {
        width: 36px;
        height: 36px;
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
        transition: all var(--transition-speed) ease;
      }
      
      /* Active step */
      .step.active {
        background-color: var(--bs-secondary);
        transform: scale(1.1);
      }
      
      /* Completed step */
      .step.completed {
        background-color: var(--bs-secondary);
      }
      
      /* Step label */
      .step-label {
        position: absolute;
        top: 40px;
        left: 50%;
        transform: translateX(-50%);
        white-space: nowrap;
        font-size: 12px;
        font-weight: bold;
        color: var(--bs-primary);
      }
      
      /* Step content */
      .step-content {
        padding: var(--spacing-lg);
        border-radius: var(--border-radius);
        margin-top: var(--spacing-lg);
        transition: all var(--transition-speed) ease;
      }
      
      /* Navigation buttons */
      .nav-buttons {
        display: flex;
        justify-content: space-between;
        margin-top: var(--spacing-lg);
      }
      
      /* Button styling */
      .btn-navigate {
        padding: 10px 16px;
        border-radius: 4px;
        transition: all var(--transition-speed) ease;
      }
      
      /* Loading animation */
      .loading-container {
        display: flex;
        align-items: center;
        justify-content: center;
        height: 200px;
      }
      
      .loading-spinner {
        width: 40px;
        height: 40px;
        border: 4px solid rgba(0, 0, 0, 0.1);
        border-radius: 50%;
        border-top-color: var(--bs-secondary);
        animation: spin 1s linear infinite;
      }
      
      @keyframes spin {
        to { transform: rotate(360deg); }
      }
      
      /* Skeleton loading */
      .skeleton {
        background: linear-gradient(90deg, #f0f0f0 25%, #e0e0e0 50%, #f0f0f0 75%);
        background-size: 200% 100%;
        animation: skeleton-loading 1.5s infinite;
        border-radius: 4px;
        height: 20px;
        margin-bottom: 8px;
      }
      
      @keyframes skeleton-loading {
        0% { background-position: 200% 0; }
        100% { background-position: -200% 0; }
      }
      
      /* Interactive elements */
      .interactive-element {
        transition: transform 0.2s ease, box-shadow 0.2s ease;
      }
      
      .interactive-element:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.1);
      }
      
      /* Tables */
      .dataTables_wrapper {
        padding: var(--spacing-md);
        border-radius: var(--border-radius);
        background-color: white;
        box-shadow: var(--box-shadow);
      }
      
      /* Plot container */
      .plot-container {
        background-color: white;
        border-radius: var(--border-radius);
        padding: var(--spacing-md);
        box-shadow: var(--box-shadow);
        margin-top: var(--spacing-lg);
      }
      
      /* Collapsible sections */
      .collapsible-header {
        cursor: pointer;
        padding: var(--spacing-md);
        background-color: #f8f9fa;
        border-radius: var(--border-radius);
        margin-bottom: var(--spacing-sm);
      }
      
      .collapsible-content {
        padding: var(--spacing-md);
        border-radius: var(--border-radius);
        background-color: white;
        box-shadow: var(--box-shadow);
        margin-bottom: var(--spacing-md);
      }
    "))
  ),
  
  # App header
  page_navbar(
    title = span(
      img(src = "https://www.epa.gov/sites/default/files/styles/medium/public/2016-02/epa-logo.png", height = "30px", style = "margin-right: 10px;"),
      "ToxCast Data Analysis Pipeline"
    ),
    bg = toxcast_colors$primary,
    
    # Stepper container
    card(
      div(class = "stepper-container",
        # Progress bar
        div(class = "progress-bar-container",
          # Progress line background
          div(class = "progress-line"),
          # Active progress line (will be updated by JavaScript)
          div(id = "progress-line-active", class = "progress-line-active"),
          
          # Step 1: Introduction
          div(id = "step-1", class = "step active", icon("info"),
              div(class = "step-label", "Introduction")),
          
          # Step 2: Data Upload
          div(id = "step-2", class = "step", icon("upload"),
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
              div(class = "step-label", "Level 6")),
          
          # Step 9: Summary
          div(id = "step-9", class = "step", icon("file-alt"),
              div(class = "step-label", "Summary"))
        ),
        
        # Step content containers
        card(
          id = "step-1-content", class = "step-content",
          card_header(
            h3("Welcome to the ToxCast Data Analysis Pipeline", class = "mb-0")
          ),
          card_body(
            p("This application guides you through the steps of analyzing ToxCast data, providing a streamlined workflow for processing and interpreting chemical toxicity data."),
            
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Overview"),
                card_body(
                  p("ToxCast is a high-throughput screening program that uses in vitro assays to evaluate thousands of chemicals for potential toxicity. This pipeline helps process and analyze the resulting data through multiple levels of increasing complexity."),
                  p("Use the navigation to move through each processing level, or click directly on a step in the progress bar above.")
                )
              ),
              card(
                card_header("Pipeline Steps"),
                card_body(
                  tags$ul(
                    tags$li(strong("Level 1:"), " Converts assay component to endpoints, defines normalized response values (resp), and optionally derives baseline (bval) and positive control values (pval)."),
                    tags$li(strong("Level 2:"), " Defines corrected values (cval) based on the applied correction methods, aggregating and processing data for further analysis."),
                    tags$li(strong("Level 3:"), " Processes data to prepare for model fitting, ensuring proper normalization and preparation."),
                    tags$li(strong("Level 4:"), " Applies advanced fitting methods to the Level 3 data, generating concentration-response models."),
                    tags$li(strong("Level 5:"), " Determines winning models and activity for concentration series, bins series into categories, and calculates potency estimates."),
                    tags$li(strong("Level 6:"), " Finalizes analysis with advanced statistical evaluations and summarization.")
                  )
                )
              )
            ),
            
            div(class = "nav-buttons mt-4",
                actionButton("next-1", "Begin Analysis", class = "btn-navigate btn-lg btn-secondary")
            )
          )
        ),
        
        card(
          id = "step-2-content", class = "step-content", style = "display: none;",
          card_header(
            h3("Data Upload", class = "mb-0")
          ),
          card_body(
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Upload Data File"),
                card_body(
                  fileInput("file", "Upload CSV File", 
                            accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv"),
                            buttonLabel = "Browse...",
                            placeholder = "No file selected"),
                  HTML("<div class='text-muted'><i>Note: If no CSV file is uploaded, example data will be used.</i></div>")
                )
              ),
              card(
                card_header("Data Preview"),
                card_body(
                  actionButton("view", "Preview Data", class = "btn btn-secondary mb-3"),
                  div(id = "data-loading", class = "loading-container", style = "display: none;",
                      div(class = "loading-spinner")
                  )
                )
              )
            ),
            
            div(class = "nav-buttons",
                actionButton("prev-2", "Previous", class = "btn-navigate btn-outline-secondary"),
                actionButton("next-2", "Next", class = "btn-navigate btn-secondary")
            ),
            
            div(class = "mt-4",
                DT::dataTableOutput("dataTable")  # Display the uploaded data in a DT table
            )
          )
        ),
        
        card(
          id = "step-3-content", class = "step-content", style = "display: none;",
          card_header(
            h3("Level 1 Processing", class = "mb-0")
          ),
          card_body(
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Level 1 Configuration"),
                card_body(
                  p("Level 1 processing converts assay component to endpoints and defines normalized response values."),
                  actionButton("process_data", "Process Level 1 Data", 
                               class = "btn btn-secondary mt-2 interactive-element")
                )
              ),
              card(
                card_header("Results"),
                card_body(
                  div(id = "lvl1-loading", class = "loading-container", style = "display: none;",
                      div(class = "loading-spinner")
                  ),
                  conditionalPanel(
                    condition = "output.level1_processed",
                    p("Level 1 processing complete!", class = "text-success"),
                    tags$span(icon("check-circle"), class = "text-success")
                  )
                )
              )
            ),
            
            div(class = "nav-buttons",
                actionButton("prev-3", "Previous", class = "btn-navigate btn-outline-secondary"),
                actionButton("next-3", "Next", class = "btn-navigate btn-secondary")
            ),
            
            # Collapsible sections
            div(class = "mt-4", id = "lvl1-results-section", style = "display: none;",
                div(class = "collapsible-header", id = "lvl1-data-header",
                    layout_column_wrap(
                      width = NULL,
                      fill = FALSE,
                      class = "d-flex justify-content-between",
                      span("Level 1 Data Table", class = "h5 mb-0"),
                      icon("chevron-down")
                    )
                ),
                div(class = "collapsible-content", id = "lvl1-data-content",
                    DT::dataTableOutput("processedData")
                ),
                
                div(class = "collapsible-header mt-3", id = "lvl1-plot-header",
                    layout_column_wrap(
                      width = NULL,
                      fill = FALSE,
                      class = "d-flex justify-content-between",
                      span("Level 1 Visualization", class = "h5 mb-0"),
                      icon("chevron-down")
                    )
                ),
                div(class = "collapsible-content", id = "lvl1-plot-content",
                    div(class = "plot-container",
                        plotlyOutput("processedPlot")
                    )
                )
            )
          )
        ),
        
        card(
          id = "step-4-content", class = "step-content", style = "display: none;",
          card_header(
            h3("Level 2 Processing", class = "mb-0")
          ),
          card_body(
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Level 2 Configuration"),
                card_body(
                  p("Level 2 processing defines corrected values based on selected correction methods."),
                  selectInput("method_select", "Select Correction Method:", 
                              choices = names(tcpl:::mc2_mthds()), 
                              selected = names(tcpl:::mc2_mthds())[1],
                              width = "100%"),
                  actionButton("process_data_lvl2", "Process Level 2 Data", 
                               class = "btn btn-secondary mt-2 interactive-element")
                )
              ),
              card(
                card_header("Results"),
                card_body(
                  div(id = "lvl2-loading", class = "loading-container", style = "display: none;",
                      div(class = "loading-spinner")
                  ),
                  conditionalPanel(
                    condition = "output.level2_processed",
                    p("Level 2 processing complete!", class = "text-success"),
                    tags$span(icon("check-circle"), class = "text-success")
                  )
                )
              )
            ),
            
            div(class = "nav-buttons",
                actionButton("prev-4", "Previous", class = "btn-navigate btn-outline-secondary"),
                actionButton("next-4", "Next", class = "btn-navigate btn-secondary")
            ),
            
            # Collapsible sections
            div(class = "mt-4", id = "lvl2-results-section", style = "display: none;",
                div(class = "collapsible-header", id = "lvl2-data-header",
                    layout_column_wrap(
                      width = NULL,
                      fill = FALSE,
                      class = "d-flex justify-content-between",
                      span("Level 2 Data Table", class = "h5 mb-0"),
                      icon("chevron-down")
                    )
                ),
                div(class = "collapsible-content", id = "lvl2-data-content",
                    DT::dataTableOutput("processedData_lvl2")
                ),
                
                div(class = "collapsible-header mt-3", id = "lvl2-plot-header",
                    layout_column_wrap(
                      width = NULL,
                      fill = FALSE,
                      class = "d-flex justify-content-between",
                      span("Level 2 Visualization", class = "h5 mb-0"),
                      icon("chevron-down")
                    )
                ),
                div(class = "collapsible-content", id = "lvl2-plot-content",
                    div(class = "plot-container",
                        plotlyOutput("processedPlot_lvl2")
                    )
                )
            )
          )
        ),
        
        card(
          id = "step-5-content", class = "step-content", style = "display: none;",
          card_header(
            h3("Level 3 Processing", class = "mb-0")
          ),
          card_body(
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Level 3 Configuration"),
                card_body(
                  p("Level 3 processing prepares data for model fitting and ensures proper normalization."),
                  selectInput("method_select_lvl3", "Select Correction Method:", 
                              choices = names(tcpl:::mc3_mthds()), 
                              selected = names(tcpl:::mc3_mthds())[36],
                              width = "100%"),
                  actionButton("process_data_lvl3", "Process Level 3 Data", 
                               class = "btn btn-secondary mt-2 interactive-element")
                )
              ),
              card(
                card_header("Results"),
                card_body(
                  div(id = "lvl3-loading", class = "loading-container", style = "display: none;",
                      div(class = "loading-spinner")
                  ),
                  conditionalPanel(
                    condition = "output.level3_processed",
                    p("Level 3 processing complete!", class = "text-success"),
                    tags$span(icon("check-circle"), class = "text-success")
                  )
                )
              )
            ),
            
            div(class = "nav-buttons",
                actionButton("prev-5", "Previous", class = "btn-navigate btn-outline-secondary"),
                actionButton("next-5", "Next", class = "btn-navigate btn-secondary")
            ),
            
            # Collapsible sections
            div(class = "mt-4", id = "lvl3-results-section", style = "display: none;",
                div(class = "collapsible-header", id = "lvl3-data-header",
                    layout_column_wrap(
                      width = NULL,
                      fill = FALSE,
                      class = "d-flex justify-content-between",
                      span("Level 3 Data Table", class = "h5 mb-0"),
                      icon("chevron-down")
                    )
                ),
                div(class = "collapsible-content", id = "lvl3-data-content",
                    DT::dataTableOutput("processedData_lvl3")
                ),
                
                div(class = "collapsible-header mt-3", id = "lvl3-plot-header",
                    layout_column_wrap(
                      width = NULL,
                      fill = FALSE,
                      class = "d-flex justify-content-between",
                      span("Level 3 Visualization", class = "h5 mb-0"),
                      icon("chevron-down")
                    )
                ),
                div(class = "collapsible-content", id = "lvl3-plot-content",
                    div(class = "plot-container",
                        plotlyOutput("processedPlot_lvl3")
                    )
                )
            )
          )
        ),
        
        card(
          id = "step-6-content", class = "step-content", style = "display: none;",
          card_header(
            h3("Level 4 Processing", class = "mb-0")
          ),
          card_body(
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Level 4 Configuration"),
                card_body(
                  p("Level 4 processing applies advanced fitting methods to generate concentration-response models."),
                  selectInput("method_select_lvl4", "Select Correction Method:", 
                              choices = names(tcpl:::mc4_mthds()), 
                              selected = names(tcpl:::mc4_mthds())[1],
                              width = "100%"),
                  actionButton("process_data_lvl4", "Process Level 4 Data", 
                               class = "btn btn-secondary mt-2 interactive-element")
                )
              ),
              card(
                card_header("Results"),
                card_body(
                  div(id = "lvl4-loading", class = "loading-container", style = "display: none;",
                      div(class = "loading-spinner")
                  ),
                  conditionalPanel(
                    condition = "output.level4_processed",
                    p("Level 4 processing complete!", class = "text-success"),
                    tags$span(icon("check-circle"), class = "text-success")
                  )
                )
              )
            ),
            
            div(class = "nav-buttons",
                actionButton("prev-6", "Previous", class = "btn-navigate btn-outline-secondary"),
                actionButton("next-6", "Next", class = "btn-navigate btn-secondary")
            ),
            
            # Collapsible sections
            div(class = "mt-4", id = "lvl4-results-section", style = "display: none;",
                div(class = "collapsible-header", id = "lvl4-data-header",
                    layout_column_wrap(
                      width = NULL,
                      fill = FALSE,
                      class = "d-flex justify-content-between",
                      span("Level 4 Data Table", class = "h5 mb-0"),
                      icon("chevron-down")
                    )
                ),
                div(class = "collapsible-content", id = "lvl4-data-content",
                    DT::dataTableOutput("processedData_lvl4")
                ),
                
                div(class = "collapsible-header mt-3", id = "lvl4-plot-header",
                    layout_column_wrap(
                      width = NULL,
                      fill = FALSE,
                      class = "d-flex justify-content-between",
                      span("Level 4 Visualization", class = "h5 mb-0"),
                      icon("chevron-down")
                    )
                ),
                div(class = "collapsible-content", id = "lvl4-plot-content",
                    div(class = "plot-container",
                        plotlyOutput("processedPlot_lvl4")
                    )
                )
            )
          )
        ),
        
        card(
          id = "step-7-content", class = "step-content", style = "display: none;",
          card_header(
            h3("Level 5 Processing", class = "mb-0")
          ),
          card_body(
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Level 5 Configuration"),
                card_body(
                  p("Level 5 determines winning models and activity for concentration series, binning series into categories and calculating potency estimates."),
                  selectInput("method_select_lvl5", "Select Correction Method:", 
                              choices = names(tcpl:::mc5_mthds()), 
                              selected = names(tcpl:::mc5_mthds())[1],
                              width = "100%"),
                  actionButton("process_data_lvl5", "Process Level 5 Data", 
                               class = "btn btn-secondary mt-2 interactive-element")
                )
              ),
              card(
                card_header("Results"),
                card_body(
                  div(id = "lvl5-loading", class = "loading-container", style = "display: none;",
                      div(class = "loading-spinner")
                  ),
                  conditionalPanel(
                    condition = "output.level5_processed",
                    p("Level 5 processing complete!", class = "text-success"),
                    tags$span(icon("check-circle"), class = "text-success")
                  )
                )
              )
            ),
            
            div(class = "nav-buttons",
                actionButton("prev-7", "Previous", class = "btn-navigate btn-outline-secondary"),
                actionButton("next-7", "Next", class = "btn-navigate btn-secondary")
            ),
            
            # Collapsible sections
            div(class = "mt-4", id = "lvl5-results-section", style = "display: none;",
                div(class = "collapsible-header", id = "lvl5-data-header",
                    layout_column_wrap(
                      width = NULL,
                      fill = FALSE,
                      class = "d-flex justify-content-between",
                      span("Level 5 Data Table", class = "h5 mb-0"),
                      icon("chevron-down")
                    )
                ),
                div(class = "collapsible-content", id = "lvl5-data-content",
                    DT::dataTableOutput("processedData_lvl5")
                )
            )
          )
        ),
        
        card(
          id = "step-8-content", class = "step-content", style = "display: none;",
          card_header(
            h3("Level 6 Processing", class = "mb-0")
          ),
          card_body(
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Level 6 Configuration"),
                card_body(
                  p("Level 6 finalizes analysis with advanced statistical evaluations and summarization."),
                  selectInput("method_select_lvl6", "Select Method:", 
                              choices = c("method1", "method2"), 
                              selected = "method1",
                              width = "100%"),
                  actionButton("process_data_lvl6", "Process Level 6 Data", 
                               class = "btn btn-secondary mt-2 interactive-element")
                )
              ),
              card(
                card_header("Results"),
                card_body(
                  div(id = "lvl6-loading", class = "loading-container", style = "display: none;",
                      div(class = "loading-spinner")
                  ),
                  conditionalPanel(
                    condition = "output.level6_processed",
                    p("Level 6 processing complete!", class = "text-success"),
                    tags$span(icon("check-circle"), class = "text-success")
                  )
                )
              )
            ),
            
            div(class = "nav-buttons",
                actionButton("prev-8", "Previous", class = "btn-navigate btn-outline-secondary"),
                actionButton("next-8", "Next", class = "btn-navigate btn-secondary")
            ),
            
            # Collapsible sections
            div(class = "mt-4", id = "lvl6-results-section", style = "display: none;",
                div(class = "collapsible-header", id = "lvl6-data-header",
                    layout_column_wrap(
                      width = NULL,
                      fill = FALSE,
                      class = "d-flex justify-content-between",
                      span("Level 6 Data Table", class = "h5 mb-0"),
                      icon("chevron-down")
                    )
                ),
                div(class = "collapsible-content", id = "lvl6-data-content",
                    DT::dataTableOutput("processedData_lvl6")
                )
            )
          )
        ),
        
        card(
          id = "step-9-content", class = "step-content", style = "display: none;",
          card_header(
            h3("Summary of ToxCast Analysis", class = "mb-0")
          ),
          card_body(
            layout_column_wrap(
              width = 1/1,
              card(
                card_header("Analysis Overview"),
                card_body(
                  p("This summary provides an overview of all processing steps and results from your ToxCast data analysis."),
                  actionButton("generate_summary", "Generate Summary Report", 
                               class = "btn btn-secondary mt-2 interactive-element")
                )
              )
            ),
            
            div(class = "nav-buttons",
                actionButton("prev-9", "Previous", class = "btn-navigate btn-outline-secondary")
            ),
            
            # Summary results
            div(class = "mt-4", id = "summary-results-section", style = "display: none;",
                card(
                  card_header("Processing Summary"),
                  card_body(
                    DT::dataTableOutput("summaryTable")
                  )
                )
            )
          )
        )
      )
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
  
  # Processing state trackers
  output$level1_processed <- reactive(FALSE)
  output$level2_processed <- reactive(FALSE)
  output$level3_processed <- reactive(FALSE)
  output$level4_processed <- reactive(FALSE)
  output$level5_processed <- reactive(FALSE)
  output$level6_processed <- reactive(FALSE)
  
  outputOptions(output, "level1_processed", suspendWhenHidden = FALSE)
  outputOptions(output, "level2_processed", suspendWhenHidden = FALSE)
  outputOptions(output, "level3_processed", suspendWhenHidden = FALSE)
  outputOptions(output, "level4_processed", suspendWhenHidden = FALSE)
  outputOptions(output, "level5_processed", suspendWhenHidden = FALSE)
  outputOptions(output, "level6_processed", suspendWhenHidden = FALSE)
  
  # Keep track of the current step
  current_step <- reactiveVal(1)
  
  # Function to update the progress bar and step indicators
  updateProgressBar <- function(step) {
    # Calculate the percentage for progress line
    percentage <- (step - 1) / 8 * 100
    
    # Update progress line width
    runjs(sprintf("document.getElementById('progress-line-active').style.width = '%s%%';", percentage))
    
    # Update step classes
    for (i in 1:9) {
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
    
    # Hide all step content with animation
    for (i in 1:9) {
      if (i != step) {
        shinyjs::hide(paste0("step-", i, "-content"), anim = TRUE)
      }
    }
    
    # Show the current step content with animation
    shinyjs::show(paste0("step-", step, "-content"), anim = TRUE)
    
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
  
  observeEvent(input$`next-8`, {
    updateProgressBar(9)
  })
  
  observeEvent(input$`prev-9`, {
    updateProgressBar(8)
  })
  
  # Allow direct clicking on step indicators
  for (i in 1:9) {
    local({
      step_num <- i
      observeEvent(input[[paste0("step-", step_num)]], {
        updateProgressBar(step_num)
      })
    })
  }
  
  # Handle collapsible sections
  observeEvent(input$`lvl1-data-header`, {
    shinyjs::toggle("lvl1-data-content", anim = TRUE)
  })
  
  observeEvent(input$`lvl1-plot-header`, {
    shinyjs::toggle("lvl1-plot-content", anim = TRUE)
  })
  
  observeEvent(input$`lvl2-data-header`, {
    shinyjs::toggle("lvl2-data-content", anim = TRUE)
  })
  
  observeEvent(input$`lvl2-plot-header`, {
    shinyjs::toggle("lvl2-plot-content", anim = TRUE)
  })
  
  observeEvent(input$`lvl3-data-header`, {
    shinyjs::toggle("lvl3-data-content", anim = TRUE)
  })
  
  observeEvent(input$`lvl3-plot-header`, {
    shinyjs::toggle("lvl3-plot-content", anim = TRUE)
  })
  
  observeEvent(input$`lvl4-data-header`, {
    shinyjs::toggle("lvl4-data-content", anim = TRUE)
  })
  
  observeEvent(input$`lvl4-plot-header`, {
    shinyjs::toggle("lvl4-plot-content", anim = TRUE)
  })
  
  observeEvent(input$`lvl5-data-header`, {
    shinyjs::toggle("lvl5-data-content", anim = TRUE)
  })
  
  observeEvent(input$`lvl6-data-header`, {
    shinyjs::toggle("lvl6-data-content", anim = TRUE)
  })
  
  # Render the data table using DT when the "View Data" button is clicked
  observeEvent(input$view, {
    # Show loading animation
    shinyjs::show("data-loading")
    
    if (is.null(input$file)) {
      uploaded_data(mcdat)  # Store the default dataset in the reactive value
    } else {
      data <- read.csv(input$file$datapath)
      uploaded_data(data)  # Store the uploaded data in the reactive value
    }

    # Render the uploaded data table
    output$dataTable <- DT::renderDataTable({
      req(uploaded_data())  # Ensure data is available
      DT::datatable(
        uploaded_data(),  # Display the uploaded dataset
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = "cell-border stripe"
      )
    })
    
    # Hide loading animation after rendering
    shinyjs::hide("data-loading")
  })

  # Process Level 1 data when the "Process Level 1 Data" button is clicked
  observeEvent(input$process_data, {
    req(uploaded_data())  # Ensure data has been uploaded and stored
    data <- uploaded_data()  # Get the uploaded data

    # Show loading message
    shinyjs::show("lvl1-loading")
    
    # Perform Level 1 processing using the mc1_df function
    processed_data(mc1_df(data))  # Update the reactive value for Level 1 processed data

    # Mark level 1 as processed
    output$level1_processed <- reactive(TRUE)
    
    # Hide loading message
    shinyjs::hide("lvl1-loading")
    
    # Show success message
    showNotification("Level 1 processing complete!", type = "message", duration = 3)
    
    # Render the processed data table for Level 1
    output$processedData <- DT::renderDataTable({
      req(processed_data())  # Ensure Level 1 processed data is available
      DT::datatable(
        processed_data(),  # Display the processed dataset
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = "cell-border stripe"
      )
    })

    # Create an interactive plot of the processed data
    output$processedPlot <- renderPlotly({
      p <- ggplot(processed_data(), aes(x = acid, y = cndx, fill = factor(repi))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Processed Level 1 Data", 
             x = "Assay Component ID (acid)", 
             y = "Concentration Index (cndx)",
             fill = "Replicate Index") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set2")
      
      ggplotly(p) %>%
        layout(
          hoverlabel = list(bgcolor = "white", font = list(family = "Inter"))
        )
    })
    
    # Show results section
    shinyjs::show("lvl1-results-section")
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
    shinyjs::show("lvl2-loading")
    
    # Call the mc2 function with the processed Level 1 data
    tryCatch({
      method_selected <- input$method_select  # Get the selected method
      method_func <- tcpl:::mc2_mthds()[[method_selected]]  # Get the selected method function

      lvl2_data <- mc2_df(processed_data(), method_func)  # Call mc2 with the processed Level 1 data and selected method

      # Store the processed Level 2 data
      processed_data_lvl2(lvl2_data)
      
      # Mark level 2 as processed
      output$level2_processed <- reactive(TRUE)
      
      # Hide loading message
      shinyjs::hide("lvl2-loading")
      
      # Show success message
      showNotification("Level 2 processing complete!", type = "message", duration = 3)

      # Render the processed Level 2 data table
      output$processedData_lvl2 <- DT::renderDataTable({
        req(processed_data_lvl2())  # Ensure Level 2 data is available
        DT::datatable(
          processed_data_lvl2(),  # Display the processed Level 2 dataset
          options = list(
            pageLength = 10,
            autoWidth = TRUE,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          rownames = FALSE,
          class = "cell-border stripe"
        )
      })

      # Create an interactive plot of the processed Level 2 data
      output$processedPlot_lvl2 <- renderPlotly({
        p <- ggplot(processed_data_lvl2(), aes(x = acid, y = cval, fill = factor(cndx))) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Processed Level 2 Data", 
               x = "Assay Component ID (acid)", 
               y = "Corrected Value (cval)",
               fill = "Concentration Index") +
          theme_minimal() +
          scale_fill_viridis_d()
        
        ggplotly(p) %>%
          layout(
            hoverlabel = list(bgcolor = "white", font = list(family = "Inter"))
          )
      })
      
      # Show results section
      shinyjs::show("lvl2-results-section")
      
    }, error = function(e) {
      shinyjs::hide("lvl2-loading")
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
    shinyjs::show("lvl3-loading")
    
    # Call the mc3 function with the processed Level 2 data
    tryCatch({
      method_selected <- input$method_select_lvl3  # Get the selected method
      method_func <- tcpl:::mc3_mthds()[[method_selected]]  # Get the selected method function

      lvl3_data <- mc3_df(processed_data_lvl2(), method_func)  # Call mc3 with the processed Level 2 data and selected method

      # Store the processed Level 3 data
      processed_data_lvl3(lvl3_data)
      
      # Mark level 3 as processed
      output$level3_processed <- reactive(TRUE)
      
      # Hide loading message
      shinyjs::hide("lvl3-loading")
      
      # Show success message
      showNotification("Level 3 processing complete!", type = "message", duration = 3)

      # Render the processed Level 3 data table
      output$processedData_lvl3 <- DT::renderDataTable({
        req(processed_data_lvl3())  # Ensure Level 3 data is available
        DT::datatable(
          processed_data_lvl3(),  # Display the processed Level 3 dataset
          options = list(
            pageLength = 10,
            autoWidth = TRUE,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          rownames = FALSE,
          class = "cell-border stripe"
        )
      })

      # Create an interactive plot of the processed Level 3 data
      output$processedPlot_lvl3 <- renderPlotly({
        p <- ggplot(processed_data_lvl3(), aes(x = aeid, y = resp, fill = factor(cndx))) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Processed Level 3 Data", 
               x = "Assay Endpoint ID (aeid)", 
               y = "Normalized Response (resp)",
               fill = "Concentration Index") +
          theme_minimal() +
          scale_fill_viridis_d()
        
        ggplotly(p) %>%
          layout(
            hoverlabel = list(bgcolor = "white", font = list(family = "Inter"))
          )
      })
      
      # Show results section
      shinyjs::show("lvl3-results-section")
      
    }, error = function(e) {
      shinyjs::hide("lvl3-loading")
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
    shinyjs::show("lvl4-loading")
    
    # Call the mc4 function with the processed Level 3 data
    tryCatch({
      method_selected <- input$method_select_lvl4  # Get the selected method
      method_func <- tcpl:::mc4_mthds()[[method_selected]]  # Get the selected method function

      lvl4_data <- mc4_df(processed_data_lvl3(), method_func)  # Call mc4 with the processed Level 3 data and selected method

      # Store the processed Level 4 data
      processed_data_lvl4(lvl4_data)
      
      # Mark level 4 as processed
      output$level4_processed <- reactive(TRUE)
      
      # Hide loading message
      shinyjs::hide("lvl4-loading")
      
      # Show success message
      showNotification("Level 4 processing complete!", type = "message", duration = 3)

      # Render the processed Level 4 data table
      output$processedData_lvl4 <- DT::renderDataTable({
        req(processed_data_lvl4())  # Ensure Level 4 data is available
        DT::datatable(
          processed_data_lvl4(),  # Display the processed Level 4 dataset
          options = list(
            pageLength = 10,
            autoWidth = TRUE,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          rownames = FALSE,
          class = "cell-border stripe"
        )
      })

      # Create an interactive plot for Level 4 data
      output$processedPlot_lvl4 <- renderPlotly({
        p <- ggplot(processed_data_lvl4(), aes(x = conc, y = resp, color = factor(spid))) +
          geom_point(size = 2) +
          geom_smooth(method = "loess", se = FALSE) +
          labs(title = "Level 4 Data: Concentration-Response Curve", 
               x = "Concentration", 
               y = "Response",
               color = "Sample ID") +
          theme_minimal() +
          scale_x_log10() +
          scale_color_viridis_d()
        
        ggplotly(p) %>%
          layout(
            hoverlabel = list(bgcolor = "white", font = list(family = "Inter"))
          )
      })
      
      # Show results section
      shinyjs::show("lvl4-results-section")
      
    }, error = function(e) {
      shinyjs::hide("lvl4-loading")
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
    shinyjs::show("lvl5-loading")
    
    # Call the mc5 function with the processed Level 4 data
    tryCatch({
      method_selected <- input$method_select_lvl5  # Get the selected method
      method_func <- tcpl:::mc5_mthds()[[method_selected]]  # Get the selected method function

      lvl5_data <- mc5_df(processed_data_lvl4(), method_func)  # Call mc5 with the processed Level 4 data and selected method

      # Store the processed Level 5 data
      processed_data_lvl5(lvl5_data)
      
      # Mark level 5 as processed
      output$level5_processed <- reactive(TRUE)
      
      # Hide loading message
      shinyjs::hide("lvl5-loading")
      
      # Show success message
      showNotification("Level 5 processing complete!", type = "message", duration = 3)

      # Render the processed Level 5 data table
      output$processedData_lvl5 <- DT::renderDataTable({
        req(processed_data_lvl5())  # Ensure Level 5 data is available
        DT::datatable(
          processed_data_lvl5(),  # Display the processed Level 5 dataset
          options = list(
            pageLength = 10,
            autoWidth = TRUE,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          rownames = FALSE,
          class = "cell-border stripe"
        )
      })
      
      # Show results section
      shinyjs::show("lvl5-results-section")

    }, error = function(e) {
      shinyjs::hide("lvl5-loading")
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
    shinyjs::show("lvl6-loading")
    
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
      
      # Mark level 6 as processed
      output$level6_processed <- reactive(TRUE)
      
      # Hide loading message
      shinyjs::hide("lvl6-loading")
      
      # Show success message
      showNotification("Level 6 processing complete!", type = "message", duration = 3)

      # Render the processed Level 6 data table
      output$processedData_lvl6 <- DT::renderDataTable({
        req(processed_data_lvl6())  # Ensure Level 6 data is available
        DT::datatable(
          processed_data_lvl6(),  # Display the processed Level 6 dataset
          options = list(
            pageLength = 10,
            autoWidth = TRUE,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          rownames = FALSE,
          class = "cell-border stripe"
        )
      })
      
      # Show results section
      shinyjs::show("lvl6-results-section")

    }, error = function(e) {
      shinyjs::hide("lvl6-loading")
      showNotification(paste("Error during Level 6 processing:", e$message), type = "error")
    })
  })
  
  # Generate Summary Report
  observeEvent(input$generate_summary, {
    # Create a summary table with information from all processing levels
    summary_data <- data.frame(
      Level = c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5", "Level 6"),
      Status = c(
        ifelse(output$level1_processed(), "Completed", "Not Processed"),
        ifelse(output$level2_processed(), "Completed", "Not Processed"),
        ifelse(output$level3_processed(), "Completed", "Not Processed"),
        ifelse(output$level4_processed(), "Completed", "Not Processed"),
        ifelse(output$level5_processed(), "Completed", "Not Processed"),
        ifelse(output$level6_processed(), "Completed", "Not Processed")
      ),
      Method = c(
        "Default",
        ifelse(output$level2_processed(), input$method_select, "N/A"),
        ifelse(output$level3_processed(), input$method_select_lvl3, "N/A"),
        ifelse(output$level4_processed(), input$method_select_lvl4, "N/A"),
        ifelse(output$level5_processed(), input$method_select_lvl5, "N/A"),
        ifelse(output$level6_processed(), input$method_select_lvl6, "N/A")
      ),
      "Rows Processed" = c(
        ifelse(is.null(processed_data()), 0, nrow(processed_data())),
        ifelse(is.null(processed_data_lvl2()), 0, nrow(processed_data_lvl2())),
        ifelse(is.null(processed_data_lvl3()), 0, nrow(processed_data_lvl3())),
        ifelse(is.null(processed_data_lvl4()), 0, nrow(processed_data_lvl4())),
        ifelse(is.null(processed_data_lvl5()), 0, nrow(processed_data_lvl5())),
        ifelse(is.null(processed_data_lvl6()), 0, nrow(processed_data_lvl6()))
      )
    )
    
    # Render the summary table
    output$summaryTable <- DT::renderDataTable({
      DT::datatable(
        summary_data,
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = "cell-border stripe"
      ) %>%
        DT::formatStyle(
          'Status',
          backgroundColor = DT::styleEqual(
            c("Completed", "Not Processed"),
            c('#d4edda', '#f8d7da')
          )
        )
    })
    
    # Show summary section
    shinyjs::show("summary-results-section")
  })
  
  # JavaScript to handle the dynamic progress bar and collapsible sections
  js <- "
  $(document).ready(function() {
    // Make the step indicators clickable
    $('.step').on('click', function() {
      var stepId = $(this).attr('id');
      Shiny.setInputValue(stepId, Math.random());
    });
    
    // Make collapsible headers clickable
    $('.collapsible-header').on('click', function() {
      var headerId = $(this).attr('id');
      Shiny.setInputValue(headerId, Math.random());
      
      // Toggle chevron icon
      var icon = $(this).find('i.fa-chevron-down, i.fa-chevron-up');
      if (icon.hasClass('fa-chevron-down')) {
        icon.removeClass('fa-chevron-down').addClass('fa-chevron-up');
      } else {
        icon.removeClass('fa-chevron-up').addClass('fa-chevron-down');
      }
    });
    
    // Add hover effects to interactive elements
    $('.interactive-element').hover(
      function() {
        $(this).css('transform', 'translateY(-2px)');
        $(this).css('box-shadow', '0 6px 12px rgba(0, 0, 0, 0.1)');
      },
      function() {
        $(this).css('transform', 'translateY(0)');
        $(this).css('box-shadow', '0 0 0 rgba(0, 0, 0, 0)');
      }
    );
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