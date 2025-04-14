# app.R
# Shiny application for renal risk prediction with popup functionality

# Libraries
library(shiny)
library(reticulate)
library(data.table)
library(tidyverse)
library(tensorflow)
library(keras)
library(caret)
library(lubridate)
library(metrica)
library(zoo)
library(DT)
library(shinydashboard)
library(plotly)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Renal Risk Prediction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raw Data", tabName = "raw", icon = icon("table")),
      menuItem("New Data", tabName = "new_data", icon = icon("database")),
      menuItem("Predictions", tabName = "predictions", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab 1: Raw Data
      tabItem(tabName = "raw",
              fluidRow(
                box(
                  title = "Raw Data Table",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("raw_table")
                )
              )
      ),
      
      # Tab 2: New Data
      tabItem(tabName = "new_data",
              fluidRow(
                box(
                  title = "New Data Table",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("new_data_table")
                )
              )
      ),
      
      # Tab 3: Predictions
      tabItem(tabName = "predictions",
              fluidRow(
                box(
                  title = "Model Predictions",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  actionButton("predict_button", "Run Predictions", 
                               icon = icon("play"),
                               class = "btn-success"),
                  br(), br(),
                  DTOutput("prediction_table")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load data
  raw_data <- reactive({
    req(file.exists('Simulated_Raw_table.csv'))
    fread('Simulated_Raw_table.csv')
  })
  
  test_data <- reactive({
    req(file.exists('Simulated_NewData_table.csv'))
    fread('Simulated_NewData_table.csv')
  })
  
  # Output for Tab 1: Raw Data
  output$raw_table <- renderDT({
    datatable(raw_data(), 
              options = list(scrollX = TRUE, 
                             pageLength = 10,
                             autoWidth = TRUE),
              filter = 'top',
              rownames = FALSE)
  })
  
  # Output for Tab 2: New Data
  output$new_data_table <- renderDT({
    nw_tble = test_data()
    datatable(nw_tble, 
              options = list(scrollX = TRUE, 
                             pageLength = 10,
                             autoWidth = TRUE),
              filter = 'top',
              rownames = FALSE)
  })
  
  # Predictions - will be generated when button is clicked
  predictions <- eventReactive(input$predict_button, {
    # handle potential errors gracefully
    tryCatch({
      # Get the test data
      test_data_val <- test_data()
      
      # Prepare test input and target
      test_X <- test_data_val[, 4:NCOL(test_data_val)]
      test_y <- test_data_val[, 3]
      
      # Convert to matrix
      test_X <- as.matrix(test_X)
      test_y <- as.matrix(test_y)
      
      # Data structure
      variables <- NCOL(test_X)
      timestamp <- 1
      test_samples <- nrow(test_data_val)
      
      # Reshape for LSTM
      test_X <- reticulate::array_reshape(x = test_X, dim = c(test_samples, timestamp, variables))
      
      # Load trained model
      withProgress(message = 'Loading model...', value = 0.3, {
        # Look for model in multiple locations
        possible_paths <- c(
          "MODEL.hdf5",  # Current directory
          "MODEL DIRECTORY"  # Original path
        )
        
        model_path <- NULL
        for (path in possible_paths) {
          if (file.exists(path)) {
            model_path <- path
            break
          }
        }
        
        if (is.null(model_path)) {
          return(data.frame(Error = "Model file not found. Please place 'MODEL.hdf5' in the app directory."))
        }
        
        model <- load_model_hdf5(model_path)
        
        # Make predictions
        incProgress(0.3, detail = "Generating predictions...")
        predictions <- model %>% predict(test_X)
        
        # Create results dataframe
        df <- data.frame(predictions)
        df <- cbind(test_data_val$myid, test_data_val$serum_creatinine, df)
        colnames(df)[1] <- 'myid'
        colnames(df)[2] <- 'last_SCr'
        
        # Calculate predicted SCr in 30 days using formula: nextSCr = last_SCr + (predictions * 30)
        df$predicted_SCr_30d <- df$last_SCr + (df$predictions * 30)
        
        # Calculate percentage change from last SCr to predicted 30-day value
        df$percent_change <- ((df$predicted_SCr_30d - df$last_SCr) / df$last_SCr) * 100
        
        # Function to find 30% increase days
        calculate_days_to_increase <- function(last_serum_SCr, roc, target_percent = 30) {
          # Calculate the target serum creatinine (30% increase)
          target_serum_SCr <- last_serum_SCr * (1 + target_percent/100)
          
          # Formula: Future_SCr = last_serum_SCr + (Days * ROC)
          # Rearranging: Days = (Future_SCr - last_serum_SCr) / ROC
          
          # Vectorized implementation to handle multiple values
          days <- rep(Inf, length(roc))  # Default to Infinity for all
          
          # Only calculate for non-zero ROC values to avoid division by zero
          non_zero_idx <- which(roc != 0)
          if (length(non_zero_idx) > 0) {
            days[non_zero_idx] <- (target_serum_SCr[non_zero_idx] - last_serum_SCr[non_zero_idx]) / roc[non_zero_idx]
          }
          
          return(days)
        }
        
        # Calculate for each prediction
        incProgress(0.3, detail = "Calculating risk status...")
        df$days_30pcnt <- calculate_days_to_increase(df$last_SCr, df$predictions)
        
        # Use vectorized ifelse for the Status column
        df$Status <- ifelse(df$days_30pcnt < 30, 'Attention', 'No attention')
        
        # Make sure Status is a character vector, not a factor
        df$Status <- as.character(df$Status)
        
        # Remove days column for final display
        df$days_30pcnt <- NULL
        
        return(df)
      })
    }, error = function(e) {
      # Return the error message in a data frame
      return(data.frame(Error = paste("Error during prediction:", e$message)))
    })
  })
  
  # Create a reactive value to store selected patient data
  selected_patient_data <- reactiveVal(NULL)
  
  # Output for Tab 3: Predictions
  output$prediction_table <- renderDT({
    # Only proceed if predictions are available
    req(predictions())
    
    # Get the predictions data
    df <- predictions()
    
    # Check if there was an error message returned instead of predictions
    if("Error" %in% colnames(df)) {
      return(datatable(df, options = list(scrollX = TRUE)))
    }
    
    # Only apply formatting if the Status column exists
    dt <- datatable(df,
                    options = list(
                      scrollX = TRUE,
                      pageLength = 10,
                      autoWidth = FALSE,  # Don't auto-adjust widths
                      columnDefs = list(
                        list(className = 'dt-center', targets = "_all")  # Center align all columns
                      )
                    ),
                    rownames = FALSE,
                    selection = "single"  # Enable single row selection
    )
    
    # Only apply formatting if the necessary columns exist
    if("Status" %in% colnames(df)) {
      dt <- dt %>% formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c('Attention', 'No attention'),
          c('#FF7F7F', '#90EE90')  # Red for Attention, Green for No attention
        ),
        textAlign = 'center'
      )
    }
    
    
    if("Status" %in% colnames(df)) {
      dt <- dt %>% formatStyle('Status', width = '120px')
    }
    
    return(dt)
  })
  
  # Observer for row selection in prediction table
  observeEvent(input$prediction_table_rows_selected, {
    req(predictions(), raw_data())
    
    # Get selected row
    selected_row <- input$prediction_table_rows_selected
    
    # Get myid from selected row
    v1_value <- predictions()[selected_row, "myid"]
    
    # Get prediction data for this patient
    pred_data <- predictions()[selected_row, ]
    
    # Filter raw data based on selected myid
    df2 <- raw_data() %>% 
      select(myid, visit, serum_creatinine) %>% 
      filter(myid == v1_value)
    
    # Save to reactive value for plot
    selected_patient_data(df2)
    
    # Determine status for the big text display
    status_text <- ifelse(pred_data$percent_change >= 30, "Immediate",
                          ifelse(pred_data$percent_change >= 10, "Watch", "Routine"))
    
    status_color <- ifelse(pred_data$percent_change >= 30, "#FF0000",
                           ifelse(pred_data$percent_change >= 10, "#FFA500", "#008000"))
    
    # Show the modal dialog with both plot and summary stats
    showModal(modalDialog(
      title = NULL,
      div(style = "display: flex; flex-direction: column;",
          # Top banner with patient ID
          div(style = paste0("background-color: #FF0000; color: white; font-size: 24px; font-weight: bold; padding: 10px; text-align: center;"),
              paste0("Patient ", sprintf("%03d", v1_value))
          ),
          
          # Main content area with two columns
          div(style = "display: flex; flex-direction: row;",
              # Left column: Stats
              div(style = "flex: 1; padding: 15px; border-right: 1px solid #ccc;",
                  div(style = "color: #2a7db5; font-size: 18px; margin-bottom: 5px;", "Future SCr value"),
                  div(style = "font-size: 32px; font-weight: bold; color: #1a4971; margin-bottom: 20px;", 
                      round(pred_data$predicted_SCr_30d, 1)),
                  
                  div(style = "color: #2a7db5; font-size: 18px; margin-bottom: 5px;", "SCr change (%)"),
                  div(style = "font-size: 32px; font-weight: bold; color: #1a4971; margin-bottom: 20px;", 
                      paste0(round(pred_data$percent_change, 0), "%")),
                  
                  div(style = "color: #2a7db5; font-size: 18px; margin-bottom: 5px;", "Renal test"),
                  div(style = paste0("font-size: 32px; font-weight: bold; color: ", status_color, ";"), 
                      status_text)
              ),
              
              # Right column: Plot
              div(style = "flex: 2;",
                  plotlyOutput("patient_trend_plot", height = "300px")
              )
          )
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Plot in popup window
  output$patient_trend_plot <- renderPlotly({
    req(selected_patient_data(), predictions())
    
    # Create data frame with original points
    df <- selected_patient_data()
    
    # Get selected patient ID
    patient_id <- df$myid[1]
    
    # Get the predicted 30-day SCr value for this patient
    pred_row <- predictions() %>% filter(myid == patient_id)
    
    # Get the last visit number from the data
    last_visit <- max(df$visit)
    
    # Create x-axis labels
    x_labels <- c()
    for(i in 1:last_visit) {
      x_labels <- c(x_labels, paste("visit", i))
    }
    x_labels <- c(x_labels, "Today", "next visit")
    
    # Rename visits for display
    visit_mapping <- data.frame(
      original = c(1:last_visit),
      new = c(1:(last_visit-1), last_visit)
    )
    
    df$display_visit <- visit_mapping$new[match(df$visit, visit_mapping$original)]
    
    # Add two points: one for "Today" (duplicate of last visit) and one for predicted
    today_point <- df[df$visit == last_visit,]
    today_point$display_visit <- last_visit + 1  # "Today"
    
    # Add a future predicted point if we have prediction data
    if(nrow(pred_row) > 0 && "predicted_SCr_30d" %in% names(pred_row)) {
      # Create a row for the predicted value
      pred_point <- data.frame(
        myid = patient_id,
        visit = last_visit + 1,
        display_visit = last_visit + 2, # "next visit" 
        serum_creatinine = pred_row$predicted_SCr_30d[1],
        is_prediction = TRUE
      )
      
      # Add is_prediction column to original data
      df$is_prediction <- FALSE
      today_point$is_prediction <- FALSE
      
      # Combine original data with today and prediction point
      df <- rbind(df, today_point, pred_point)
    } else {
      df$is_prediction <- FALSE
      df <- rbind(df, today_point)
    }
    
    # Create a custom x-axis with the labels we want
    custom_x <- list(
      title = "Visit",
      tickmode = "array",
      tickvals = c(1:(last_visit+2)),
      ticktext = x_labels
    )
    
    # Create the plot
    p <- plot_ly(df, x = ~display_visit, y = ~serum_creatinine, type = 'scatter', mode = 'lines+markers',
                 line = list(shape = 'spline', smoothing = 1, width = 3, color = 'red'),
                 marker = list(size = 10, color = 'red')) %>%
      layout(xaxis = custom_x,
             yaxis = list(title = "Serum Creatinine"),
             showlegend = FALSE,
             margin = list(l = 50, r = 50, b = 50, t = 10, pad = 4))
    
    return(p)
  })
}

# Run the app
shinyApp(ui, server)