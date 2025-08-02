#| echo: false
#| message: false

# Universal Interactive Prediction Widget for Quarto document
library(shiny)
library(bslib)
library(rms)
library(Hmisc)
library(ggplot2)
library(scales)
library(DT)

# Load optional performance libraries
if(requireNamespace("pROC", quietly = TRUE)) {
  library(pROC)
}

# Configuration for different datasets
DATASET_CONFIGS <- list(
  titanic = list(
    name = "Titanic Dataset",
    load_function = function() {
      getHdata(titanic3)
      v <- c('pclass','survived','age','sex','sibsp','parch')
      data <- titanic3[, v]
      units(data$age) <- 'years'
      return(data)
    },
    outcome_var = "survived",
    outcome_label = "Survival",
    predictors = c("age", "sex", "pclass", "sibsp", "parch"),
    formula = "survived ~ sex * pclass * rcs(age, 4) + rcs(age, 4) * (sibsp + parch)",
    impute_formula = "~ age + sex + pclass + I(sibsp) + I(parch) + survived",
    presets = list(
      list(name = "1st Class Woman", sex = "female", pclass = "1st", age = 25, sibsp = 0, parch = 0),
      list(name = "3rd Class Man", sex = "male", pclass = "3rd", age = 30, sibsp = 0, parch = 0),
      list(name = "Child (5 years)", sex = "male", pclass = "3rd", age = 5, sibsp = 0, parch = 2)
    ),
    input_configs = list(
      sex = list(type = "select", label = "Gender", choices = list("Female" = "female", "Male" = "male"), default = "female"),
      pclass = list(type = "select", label = "Passenger Class", choices = list("1st Class" = "1st", "2nd Class" = "2nd", "3rd Class" = "3rd"), default = "1st"),
      age = list(type = "numeric", label = "Age (years)", default = 30, min = 0, max = 100, step = 1),
      sibsp = list(type = "numeric", label = "Number of Siblings/Spouses", default = 0, min = 0, max = 8, step = 1),
      parch = list(type = "numeric", label = "Number of Parents/Children", default = 0, min = 0, max = 6, step = 1)
    )
  ),
  custom = list(
    name = "Custom Dataset",
    load_function = function() {
      # This will be set dynamically
      return(NULL)
    },
    outcome_var = NULL,
    outcome_label = "Outcome",
    predictors = NULL,
    formula = NULL,
    impute_formula = NULL,
    presets = list(),
    input_configs = list()
  )
)

# Global variables for the model and data
model_data <- NULL
fitted_model <- NULL
current_config <- NULL

# Function to detect data types and suggest configurations
detect_data_structure <- function(data) {
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  factor_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  binary_vars <- names(data)[sapply(data, function(x) {
    if(is.factor(x) || is.character(x)) return(length(unique(x)) == 2)
    if(is.numeric(x)) return(all(x %in% c(0, 1), na.rm = TRUE))
    FALSE
  })]
  
  return(list(
    numeric = numeric_vars,
    factor = factor_vars,
    binary = binary_vars,
    all_vars = names(data)
  ))
}

# Function to create input configurations automatically
create_input_configs <- function(data, predictors) {
  configs <- list()
  
  for(var in predictors) {
    if(var %in% names(data)) {
      col_data <- data[[var]]
      
      if(is.numeric(col_data)) {
        configs[[var]] <- list(
          type = "numeric",
          label = tools::toTitleCase(gsub("_", " ", var)),
          default = round(median(col_data, na.rm = TRUE), 1),
          min = min(col_data, na.rm = TRUE),
          max = max(col_data, na.rm = TRUE),
          step = if(all(col_data == round(col_data), na.rm = TRUE)) 1 else 0.1
        )
      } else {
        unique_vals <- unique(col_data[!is.na(col_data)])
        choices <- setNames(as.list(unique_vals), unique_vals)
        
        configs[[var]] <- list(
          type = "select",
          label = tools::toTitleCase(gsub("_", " ", var)),
          choices = choices,
          default = unique_vals[1]
        )
      }
    }
  }
  
  return(configs)
}

# Initialize with default dataset (can be changed by user)
SELECTED_DATASET <- "titanic"

# Function to load and prepare model
prepare_model <- function(dataset_key = SELECTED_DATASET, custom_data = NULL, custom_config = NULL) {
  if(dataset_key == "custom" && !is.null(custom_data) && !is.null(custom_config)) {
    config <- custom_config
    data <- custom_data
  } else {
    config <- DATASET_CONFIGS[[dataset_key]]
    data <- config$load_function()
  }
  
  # Store globally
  model_data <<- data
  current_config <<- config
  
  # Setup datadist for rms
  dd <- datadist(data)
  options(datadist = dd)
  
  # Multiple imputation
  set.seed(17)
  impute_vars <- config$predictors
  if(!is.null(config$outcome_var)) {
    impute_vars <- c(impute_vars, config$outcome_var)
  }
  
  # Build imputation formula dynamically
  impute_formula_str <- paste("~", paste(impute_vars, collapse = " + "))
  impute_formula <- as.formula(impute_formula_str)
  
  mi <- aregImpute(impute_formula, data = data, n.impute = 10, nk = 4, pr = FALSE)
  
  # Fit the model
  if(!is.null(config$formula)) {
    model_formula <- as.formula(config$formula)
  } else {
    # Create a simple formula if not provided
    predictors_str <- paste(config$predictors, collapse = " + ")
    model_formula <- as.formula(paste(config$outcome_var, "~", predictors_str))
  }
  
  fitted_model <<- fit.mult.impute(model_formula, lrm, mi, data = data, pr = FALSE, lrt = TRUE)
  
  return(list(model = fitted_model, data = data, config = config))
}

# Initialize the model
if(is.null(fitted_model)) {
  prepare_model()
}

# Dynamic UI generation function
generate_inputs <- function(config) {
  inputs <- list()
  
  for(var_name in names(config$input_configs)) {
    var_config <- config$input_configs[[var_name]]
    
    if(var_config$type == "select") {
      inputs[[length(inputs) + 1]] <- selectInput(
        var_name, 
        var_config$label,
        choices = var_config$choices,
        selected = var_config$default
      )
    } else if(var_config$type == "numeric") {
      inputs[[length(inputs) + 1]] <- numericInput(
        var_name,
        var_config$label,
        value = var_config$default,
        min = var_config$min,
        max = var_config$max,
        step = var_config$step
      )
    }
  }
  
  return(inputs)
}

# Universal Embedded App UI
ui <- page_sidebar(
  title = "Universal Prediction Tool",
  sidebar = sidebar(
    width = 300,
    
    # Dataset selection
    selectInput("dataset_choice", "Choose Dataset:",
               choices = list(
                 "Titanic Survival" = "titanic",
                 "Upload Custom Dataset" = "custom"
               ),
               selected = SELECTED_DATASET),
    
    # File upload for custom dataset
    conditionalPanel(
      condition = "input.dataset_choice == 'custom'",
      fileInput("custom_file", "Upload CSV File:",
               accept = c(".csv", ".txt")),
      
      conditionalPanel(
        condition = "output.show_custom_config",
        selectInput("outcome_variable", "Outcome Variable:", choices = NULL),
        checkboxGroupInput("predictor_variables", "Predictor Variables:", choices = NULL),
        textInput("outcome_label", "Outcome Label:", value = "Outcome"),
        
        # Spline configuration option
        div(style = "margin-top: 15px; padding: 10px; background: #f8f9fa; border-radius: 4px;",
          h6("Model Configuration:", style = "margin-bottom: 10px; color: #2c3e50;"),
          checkboxInput("use_splines", 
                       "Use 4-knot splines for continuous variables", 
                       value = TRUE),
          div(style = "font-size: 0.85em; color: #666; margin-top: 5px;",
              "Recommended: Splines allow for non-linear relationships and improve model flexibility.")
        )
      )
    ),
    
    # Dynamic inputs based on selected dataset
    div(id = "dynamic_inputs"),
    
    hr(),
    
    # Preset buttons (dynamic)
    div(id = "preset_buttons"),
    
    # Model information
    hr(),
    div(id = "model_info", 
        style = "font-size: 0.8em; color: #666;",
        h5("📋 Model Information", style = "margin-bottom: 10px; color: #2c3e50; font-size: 1em;"),
        div(
          style = "background: #f8f9fa; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
          tags$strong("Formula:"), br(),
          verbatimTextOutput("model_formula", placeholder = TRUE)
        ),
        textOutput("model_summary"))
  ),
  
  
  
  layout_columns(
    card(
      card_header(textOutput("prediction_title")),
      card_body(
        div(id = "prediction-display",
            style = "text-align: center; padding: 20px;",
            h3(textOutput("prediction_prob"), style = "margin-bottom: 10px;"),
            h5(textOutput("risk_level")),
            hr(),
            plotOutput("prob_gauge", height = "200px")
        )
      )
    ),
    
    card(
      card_header("Variable Effects"),
      card_body(
        conditionalPanel(
          condition = "input.dataset_choice != 'custom' || output.model_ready",
          plotOutput("variable_effects", height = "300px")
        ),
        conditionalPanel(
          condition = "input.dataset_choice == 'custom' && !output.model_ready",
          div("Upload and configure dataset to see variable effects", 
              style = "text-align: center; color: #666; padding: 50px;")
        )
      )
    ),
    
    col_widths = c(6, 6)
  ),
  
  # Model Performance Metrics and Variable Importance
  layout_columns(
    card(
      card_header("📊 Model Performance Metrics"),
      card_body(
        conditionalPanel(
          condition = "output.model_ready",
          div(
            style = "padding: 10px;",
            h5("Discrimination Metrics:", style = "margin-bottom: 15px; color: #2c3e50;"),
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 15px;",
              div(
                tags$strong("C-index:"), " ", textOutput("c_index", inline = TRUE),
                style = "padding: 8px; background: #ecf0f1; border-radius: 4px;"
              ),
              div(
                tags$strong("AUC:"), " ", textOutput("auc_value", inline = TRUE),
                style = "padding: 8px; background: #ecf0f1; border-radius: 4px;"
              ),
              div(
                tags$strong("Brier Score:"), " ", textOutput("brier_score", inline = TRUE),
                style = "padding: 8px; background: #ecf0f1; border-radius: 4px;"
              ),
              div(
                tags$strong("Discrimination Slope:"), " ", textOutput("discrim_slope", inline = TRUE),
                style = "padding: 8px; background: #ecf0f1; border-radius: 4px;"
              )
            ),
            h5("Model Quality:", style = "margin-bottom: 10px; color: #2c3e50;"),
            div(
              textOutput("model_quality"),
              style = "padding: 10px; background: #d5f4e6; border-radius: 4px; font-weight: bold;"
            )
          )
        ),
        conditionalPanel(
          condition = "!output.model_ready",
          div("Configure dataset to see performance metrics", 
              style = "text-align: center; color: #666; padding: 50px;")
        )
      )
    ),
    
    card(
      card_header("📈 Variable Importance"),
      card_body(
        conditionalPanel(
          condition = "output.model_ready",
          plotOutput("variable_importance", height = "300px")
        ),
        conditionalPanel(
          condition = "!output.model_ready",
          div("Configure dataset to see variable importance", 
              style = "text-align: center; color: #666; padding: 50px;")
        )
      )
    ),
    
    col_widths = c(6, 6)
  ),
  
  # Model Formula and Technical Details
  conditionalPanel(
    condition = "output.model_ready",
    card(
      card_header("🔧 Model Technical Details"),
      card_body(
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 15px;",
          div(
            h6("Model Type:", style = "margin-bottom: 5px; color: #34495e;"),
            textOutput("model_type", inline = TRUE),
            style = "padding: 10px; background: #ecf0f1; border-radius: 4px;"
          ),
          div(
            h6("Coefficients:", style = "margin-bottom: 5px; color: #34495e;"),
            textOutput("model_coefficients", inline = TRUE),
            style = "padding: 10px; background: #ecf0f1; border-radius: 4px;"
          ),
          div(
            h6("Sample Size:", style = "margin-bottom: 5px; color: #34495e;"),
            textOutput("model_sample_size", inline = TRUE),
            style = "padding: 10px; background: #ecf0f1; border-radius: 4px;"
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    current_data = NULL,
    current_model = NULL,
    current_config = NULL,
    model_ready = FALSE
  )
  
  # Initialize with default dataset
  observe({
    if(is.null(values$current_model)) {
      result <- prepare_model(SELECTED_DATASET)
      values$current_data <- result$data
      values$current_model <- result$model
      values$current_config <- result$config
      values$model_ready <- TRUE
    }
  })
  
  # Handle custom dataset upload
  observeEvent(input$custom_file, {
    req(input$custom_file)
    
    ext <- tools::file_ext(input$custom_file$datapath)
    if(ext == "csv") {
      custom_data <- read.csv(input$custom_file$datapath, stringsAsFactors = FALSE)
      values$custom_raw_data <- custom_data
      
      # Detect structure
      structure <- detect_data_structure(custom_data)
      
      # Update variable choices
      updateSelectInput(session, "outcome_variable", 
                       choices = setNames(structure$all_vars, structure$all_vars))
      updateCheckboxGroupInput(session, "predictor_variables",
                              choices = setNames(structure$all_vars, structure$all_vars))
    }
  })
  
  # Custom dataset configuration
  observeEvent(c(input$outcome_variable, input$predictor_variables, input$outcome_label), {
    req(input$outcome_variable, input$predictor_variables, values$custom_raw_data)
    
    if(length(input$predictor_variables) > 0 && input$outcome_variable != "") {
      # Create custom configuration
      custom_config <- DATASET_CONFIGS$custom
      custom_config$name <- paste("Custom:", basename(input$custom_file$name))
      custom_config$outcome_var <- input$outcome_variable
      custom_config$outcome_label <- input$outcome_label
      custom_config$predictors <- input$predictor_variables
      
      # Create input configurations
      custom_config$input_configs <- create_input_configs(values$custom_raw_data, input$predictor_variables)
      
      # Create formula with optional splines
      if(input$use_splines) {
        # Identify continuous variables for spline transformation
        continuous_vars <- c()
        categorical_vars <- c()
        
        for(var in input$predictor_variables) {
          if(is.numeric(values$custom_raw_data[[var]])) {
            # Check if it's truly continuous (more than 10 unique values)
            if(length(unique(values$custom_raw_data[[var]])) > 10) {
              continuous_vars <- c(continuous_vars, var)
            } else {
              categorical_vars <- c(categorical_vars, var)
            }
          } else {
            categorical_vars <- c(categorical_vars, var)
          }
        }
        
        # Build formula components
        formula_parts <- c()
        
        # Add continuous variables with splines
        if(length(continuous_vars) > 0) {
          spline_terms <- paste0("rcs(", continuous_vars, ", 4)")
          formula_parts <- c(formula_parts, spline_terms)
        }
        
        # Add categorical variables as-is
        if(length(categorical_vars) > 0) {
          formula_parts <- c(formula_parts, categorical_vars)
        }
        
        # Combine all terms
        predictors_str <- paste(formula_parts, collapse = " + ")
        custom_config$formula <- paste(input$outcome_variable, "~", predictors_str)
        
      } else {
        # Create a simple formula without splines
        predictors_str <- paste(input$predictor_variables, collapse = " + ")
        custom_config$formula <- paste(input$outcome_variable, "~", predictors_str)
      }
      
      # Prepare model
      tryCatch({
        result <- prepare_model("custom", values$custom_raw_data, custom_config)
        values$current_data <- result$data
        values$current_model <- result$model
        values$current_config <- result$config
        values$model_ready <- TRUE
      }, error = function(e) {
        showNotification(paste("Error fitting model:", e$message), type = "error")
        values$model_ready <- FALSE
      })
    }
  })
  
  # Handle dataset change
  observeEvent(input$dataset_choice, {
    if(input$dataset_choice != "custom") {
      result <- prepare_model(input$dataset_choice)
      values$current_data <- result$data
      values$current_model <- result$model
      values$current_config <- result$config
      values$model_ready <- TRUE
    } else {
      values$model_ready <- FALSE
    }
  })
  
  # Dynamic UI rendering
  output$show_custom_config <- reactive({
    !is.null(input$custom_file) && input$dataset_choice == "custom"
  })
  outputOptions(output, "show_custom_config", suspendWhenHidden = FALSE)
  
  output$model_ready <- reactive({
    values$model_ready
  })
  outputOptions(output, "model_ready", suspendWhenHidden = FALSE)
  
  # Generate dynamic inputs
  observe({
    req(values$current_config)
    
    inputs <- generate_inputs(values$current_config)
    
    # Clear existing inputs
    removeUI(selector = "#dynamic_inputs > *", multiple = TRUE)
    
    # Add new inputs
    for(i in seq_along(inputs)) {
      insertUI(
        selector = "#dynamic_inputs",
        where = "beforeEnd",
        ui = inputs[[i]]
      )
    }
    
    # Generate preset buttons
    presets_ui <- list()
    if(length(values$current_config$presets) > 0) {
      presets_ui[[1]] <- h5("Quick Presets:")
      for(i in seq_along(values$current_config$presets)) {
        preset <- values$current_config$presets[[i]]
        presets_ui[[length(presets_ui) + 1]] <- actionButton(
          paste0("preset_", i), 
          preset$name,
          class = "btn-sm btn-outline-primary w-100 mb-2"
        )
      }
    }
    
    # Clear and add preset buttons
    removeUI(selector = "#preset_buttons > *", multiple = TRUE)
    for(ui_element in presets_ui) {
      insertUI(
        selector = "#preset_buttons",
        where = "beforeEnd",
        ui = ui_element
      )
    }
  })
  
  # Handle preset buttons dynamically
  observe({
    req(values$current_config)
    
    for(i in seq_along(values$current_config$presets)) {
      local({
        preset_index <- i
        preset <- values$current_config$presets[[preset_index]]
        
        observeEvent(input[[paste0("preset_", preset_index)]], {
          for(var_name in names(preset)) {
            if(var_name != "name" && !is.null(input[[var_name]])) {
              if(inherits(input[[var_name]], "character")) {
                updateSelectInput(session, var_name, selected = preset[[var_name]])
              } else {
                updateNumericInput(session, var_name, value = preset[[var_name]])
              }
            }
          }
        })
      })
    }
  })
  
  
  # Reactive prediction
  current_prediction <- reactive({
    req(values$current_model, values$current_config)
    
    # Build prediction data frame
    new_data <- data.frame(row.names = 1)
    
    for(var_name in values$current_config$predictors) {
      if(!is.null(input[[var_name]])) {
        if(var_name %in% names(values$current_data)) {
          # Match the data type from original data
          original_var <- values$current_data[[var_name]]
          if(is.factor(original_var)) {
            new_data[[var_name]] <- factor(input[[var_name]], levels = levels(original_var))
          } else {
            new_data[[var_name]] <- input[[var_name]]
          }
        }
      }
    }
    
    if(nrow(new_data) > 0 && ncol(new_data) > 0) {
      tryCatch({
        prob <- predict(values$current_model, new_data, type = "fitted")
        return(as.numeric(prob))
      }, error = function(e) {
        return(0.5)  # Default probability if prediction fails
      })
    } else {
      return(0.5)
    }
  })
  
  # Output: Prediction title
  output$prediction_title <- renderText({
    req(values$current_config)
    paste(values$current_config$outcome_label, "Prediction")
  })
  
  # Output: Prediction probability
  output$prediction_prob <- renderText({
    prob <- current_prediction()
    paste0(round(prob * 100, 1), "% Probability")
  })
  
  # Output: Risk level
  output$risk_level <- renderText({
    prob <- current_prediction()
    if (prob >= 0.7) {
      "🟢 High Probability"
    } else if (prob >= 0.3) {
      "🟡 Moderate Probability" 
    } else {
      "🔴 Low Probability"
    }
  })
  
  # Output: Probability gauge
  output$prob_gauge <- renderPlot({
    prob <- current_prediction()
    req(values$current_config)
    
    # Create a simple gauge chart
    gauge_data <- data.frame(
      group = c(values$current_config$outcome_label, paste("No", values$current_config$outcome_label)),
      value = c(prob, 1 - prob),
      stringsAsFactors = FALSE
    )
    
    ggplot(gauge_data, aes(x = "", y = .data$value, fill = .data$group)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("darkgreen", "darkred")) +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(fill = "Outcome") +
      geom_text(aes(label = paste0(round(.data$value * 100), "%")), 
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold", size = 4)
  })
  
  # Output: Variable effects plot
  output$variable_effects <- renderPlot({
    req(values$current_model, values$current_config, values$current_data)
    
    # Find a continuous variable for effects plot
    continuous_vars <- values$current_config$predictors[
      sapply(values$current_config$predictors, function(x) is.numeric(values$current_data[[x]]))
    ]
    
    if(length(continuous_vars) > 0) {
      # Use the first continuous variable
      var_name <- continuous_vars[1]
      var_data <- values$current_data[[var_name]]
      var_range <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), length.out = 50)
      
      # Build data for prediction
      effect_data <- data.frame(row.names = 1:length(var_range))
      effect_data[[var_name]] <- var_range
      
      # Set other variables to their defaults or modes
      for(other_var in values$current_config$predictors[values$current_config$predictors != var_name]) {
        if(!is.null(input[[other_var]])) {
          if(other_var %in% names(values$current_data)) {
            original_var <- values$current_data[[other_var]]
            if(is.factor(original_var)) {
              effect_data[[other_var]] <- factor(input[[other_var]], levels = levels(original_var))
            } else {
              effect_data[[other_var]] <- input[[other_var]]
            }
          }
        }
      }
      
      # Get predictions
      tryCatch({
        probs <- predict(values$current_model, effect_data, type = "fitted")
        current_prob <- current_prediction()
        current_value <- if(!is.null(input[[var_name]])) input[[var_name]] else median(var_data, na.rm = TRUE)
        
        plot_data <- data.frame(Variable = var_range, Probability = as.numeric(probs))
        
        ggplot(plot_data, aes(x = .data$Variable, y = .data$Probability)) +
          geom_line(color = "steelblue", size = 1.2) +
          geom_point(x = current_value, y = current_prob, 
                     color = "red", size = 4) +
          labs(title = paste("Effect of", tools::toTitleCase(var_name)),
               subtitle = "Current value (red dot)",
               x = tools::toTitleCase(var_name), 
               y = paste(values$current_config$outcome_label, "Probability")) +
          theme_minimal() +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
          theme(plot.title = element_text(size = 12))
      }, error = function(e) {
        # Fallback plot
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Effect plot not available\nfor current configuration") +
          theme_void()
      })
    } else {
      # No continuous variables - show a message
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No continuous variables\navailable for effect plot") +
        theme_void()
    }
  })
  
  # Output: Model summary
  output$model_summary <- renderText({
    req(values$current_config, values$current_model)
    paste("Dataset:", values$current_config$name, 
          "\nPredictors:", paste(values$current_config$predictors, collapse = ", "),
          "\nOutcome:", values$current_config$outcome_var,
          "\nObservations:", nrow(values$current_data))
  })
  
  # Output: Model formula
  output$model_formula <- renderText({
    req(values$current_config, values$current_model)
    
    # Get the formula from the model or config
    if(!is.null(values$current_model$sformula)) {
      # Use the stored formula from the fitted model
      formula_text <- deparse(values$current_model$sformula)
    } else if(!is.null(values$current_config$formula)) {
      # Use the formula from config
      formula_text <- values$current_config$formula
    } else {
      # Create a basic formula
      predictors_str <- paste(values$current_config$predictors, collapse = " + ")
      formula_text <- paste(values$current_config$outcome_var, "~", predictors_str)
    }
    
    # Clean up the formula display
    formula_clean <- gsub("\\s+", " ", formula_text)
    formula_clean <- gsub("^c\\(|\\)$", "", formula_clean)
    
    return(formula_clean)
  })
  
  # Model technical details
  output$model_type <- renderText({
    req(values$current_model, values$current_config)
    
    # Check if splines are used in the model
    formula_text <- if(!is.null(values$current_model$sformula)) {
      paste(deparse(values$current_model$sformula), collapse = " ")
    } else if(!is.null(values$current_config$formula)) {
      values$current_config$formula
    } else {
      ""
    }
    
    # Ensure we get a single logical value
    splines_used <- any(grepl("rcs\\(", formula_text))
    
    if(splines_used) {
      "Logistic Regression with Restricted Cubic Splines"
    } else {
      "Logistic Regression (Linear)"
    }
  })
  
  output$model_coefficients <- renderText({
    req(values$current_model)
    tryCatch({
      n_coef <- length(coef(values$current_model))
      paste(n_coef, "parameters")
    }, error = function(e) "Parameters not available")
  })
  
  output$model_sample_size <- renderText({
    req(values$current_model)
    tryCatch({
      n_obs <- values$current_model$stats["Obs"]
      if (!is.na(n_obs)) {
        paste(n_obs, "observations")
      } else if (!is.null(values$current_data)) {
        paste(nrow(values$current_data), "observations")
      } else {
        "Sample size not available"
      }
    }, error = function(e) {
      if (!is.null(values$current_data)) {
        paste(nrow(values$current_data), "observations")
      } else {
        "Sample size not available"
      }
    })
  })
  
  # Performance metrics calculations
  performance_metrics <- reactive({
    req(values$current_model, values$current_data, values$current_config)
    
    tryCatch({
      # Get complete cases for validation
      complete_cases <- complete.cases(values$current_data)
      if(sum(complete_cases) < 10) return(NULL)
      
      complete_data <- values$current_data[complete_cases, ]
      pred_complete <- predict(values$current_model, complete_data, type = "fitted")
      obs_complete <- complete_data[[values$current_config$outcome_var]]
      
      # Calculate metrics
      c_index <- values$current_model$stats['C']
      
      # AUC calculation
      if(requireNamespace("pROC", quietly = TRUE)) {
        roc_obj <- pROC::roc(obs_complete, pred_complete, quiet = TRUE)
        auc_val <- pROC::auc(roc_obj)
      } else {
        auc_val <- c_index  # Fallback to C-index
      }
      
      # Brier score
      brier <- mean((pred_complete - obs_complete)^2)
      
      # Discrimination slope
      surv_probs <- pred_complete[obs_complete == 1]
      died_probs <- pred_complete[obs_complete == 0]
      discrim_slope <- mean(surv_probs) - mean(died_probs)
      
      return(list(
        c_index = c_index,
        auc = auc_val,
        brier = brier,
        discrim_slope = discrim_slope
      ))
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Variable importance calculations
  variable_importance <- reactive({
    req(values$current_model, values$current_data, values$current_config)
    
    tryCatch({
      # Get complete cases
      complete_cases <- complete.cases(values$current_data)
      if(sum(complete_cases) < 10) return(NULL)
      
      complete_data <- values$current_data[complete_cases, ]
      baseline_pred <- predict(values$current_model, complete_data, type = "fitted")
      baseline_c <- somers2(baseline_pred, complete_data[[values$current_config$outcome_var]])['C']
      
      # Variables to test
      vars_to_test <- values$current_config$predictors
      importance_results <- data.frame(
        variable = vars_to_test,
        importance = numeric(length(vars_to_test)),
        c_index_drop = numeric(length(vars_to_test)),
        stringsAsFactors = FALSE
      )
      
      for(i in seq_along(vars_to_test)) {
        var <- vars_to_test[i]
        
        # Create permuted data
        perm_data <- complete_data
        perm_data[[var]] <- sample(perm_data[[var]])
        
        # Get predictions with permuted variable
        perm_pred <- predict(values$current_model, perm_data, type = "fitted")
        perm_c <- somers2(perm_pred, perm_data[[values$current_config$outcome_var]])['C']
        
        # Calculate importance as drop in C-index
        importance_results$c_index_drop[i] <- baseline_c - perm_c
        importance_results$importance[i] <- (baseline_c - perm_c) / baseline_c * 100
      }
      
      # Sort by importance
      importance_results <- importance_results[order(importance_results$importance, decreasing = TRUE), ]
      
      return(importance_results)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Output: Performance metrics
  output$c_index <- renderText({
    metrics <- performance_metrics()
    if(is.null(metrics)) return("N/A")
    paste0(round(metrics$c_index, 3))
  })
  
  output$auc_value <- renderText({
    metrics <- performance_metrics()
    if(is.null(metrics)) return("N/A")
    paste0(round(metrics$auc, 3))
  })
  
  output$brier_score <- renderText({
    metrics <- performance_metrics()
    if(is.null(metrics)) return("N/A")
    paste0(round(metrics$brier, 3))
  })
  
  output$discrim_slope <- renderText({
    metrics <- performance_metrics()
    if(is.null(metrics)) return("N/A")
    paste0(round(metrics$discrim_slope, 3))
  })
  
  output$model_quality <- renderText({
    metrics <- performance_metrics()
    if(is.null(metrics)) return("Model not ready")
    
    c_idx <- metrics$c_index
    brier <- metrics$brier
    
    if(c_idx > 0.8 && brier < 0.25) {
      "🟢 Excellent Model Performance"
    } else if(c_idx > 0.7 && brier < 0.3) {
      "🟡 Good Model Performance"
    } else if(c_idx > 0.6) {
      "🟠 Fair Model Performance"
    } else {
      "🔴 Poor Model Performance"
    }
  })
  
  # Output: Variable importance plot
  output$variable_importance <- renderPlot({
    importance_data <- variable_importance()
    if(is.null(importance_data)) {
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Variable importance\nnot available") +
        theme_void())
    }
    
    # Create importance plot
    ggplot(importance_data, aes(x = reorder(.data$variable, .data$importance), y = .data$importance)) +
      geom_col(fill = "#3498db", alpha = 0.8) +
      geom_text(aes(label = paste0(round(.data$importance, 1), "%")), 
                hjust = -0.1, size = 3.5) +
      coord_flip() +
      labs(title = "Variable Importance",
           subtitle = "% drop in C-index when randomized",
           x = "Variable", y = "Importance (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "#7f8c8d"),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
}

# Display the app
shinyApp(ui = ui, server = server, options = list(height = "1200px"))

