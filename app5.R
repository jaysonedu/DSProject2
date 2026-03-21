################################################################################
# Project 2 - R Shiny Data Explorer
# Parts completed:
#   1) Data Upload & Preview
#   2) Cleaning & Preprocessing
################################################################################

library(shiny)
library(DT)
library(readxl)
library(jsonlite)
library(dplyr)
library(readr)

# =========================
# App paths
# =========================
app_dir <- getwd()
data_dir <- file.path(app_dir, "data")

# =========================
# Helper functions
# =========================

read_uploaded_data <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  
  if (ext %in% c("csv", "txt")) {
    # Try comma first, then semicolon, then tab
    df1 <- tryCatch(
      read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.null(df1) && ncol(df1) > 1) return(as.data.frame(df1, check.names = FALSE))
    
    df2 <- tryCatch(
      read.csv(path, sep = ";", stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.null(df2) && ncol(df2) > 1) return(as.data.frame(df2, check.names = FALSE))
    
    df3 <- tryCatch(
      read.delim(path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.null(df3)) return(as.data.frame(df3, check.names = FALSE))
    
    stop("Unable to parse the CSV/TXT file. Please check the delimiter.")
  }
  
  if (ext %in% c("xlsx", "xls")) {
    return(as.data.frame(readxl::read_excel(path), check.names = FALSE))
  }
  
  if (ext == "json") {
    out <- jsonlite::fromJSON(txt = path, simplifyVector = TRUE)
    return(as.data.frame(out, check.names = FALSE))
  }
  
  if (ext == "rds") {
    out <- readRDS(path)
    return(as.data.frame(out, check.names = FALSE))
  }
  
  stop("Unsupported file format. Please upload a CSV, XLSX, JSON, or RDS file.")
}

clean_char_na <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.factor(x)) x <- as.character(x)
    
    if (is.character(x)) {
      x <- trimws(x)
      x[x == ""] <- NA
      x[tolower(x) %in% c("na", "null", "unknown", "?")] <- NA
    }
    x
  })
  as.data.frame(df, check.names = FALSE)
}

coerce_numeric_if_possible <- function(df, min_numeric_ratio = 0.7) {
  df <- as.data.frame(df, check.names = FALSE)
  
  for (nm in names(df)) {
    if (is.character(df[[nm]]) || is.logical(df[[nm]])) {
      parsed <- suppressWarnings(readr::parse_number(as.character(df[[nm]])))
      ratio <- mean(!is.na(parsed))
      if (!is.na(ratio) && ratio >= min_numeric_ratio) {
        df[[nm]] <- parsed
      }
    }
  }
  df
}

get_mode <- function(x) {
  x2 <- x[!is.na(x)]
  if (length(x2) == 0) return(NA)
  ux <- unique(x2)
  ux[which.max(tabulate(match(x2, ux)))]
}

safe_first_non_missing <- function(x) {
  x2 <- x[!is.na(x) & x != ""]
  if (length(x2) == 0) return(NA)
  x2[1]
}

safe_scale <- function(x) {
  if (!is.numeric(x)) return(x)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(x)
  as.numeric(scale(x))
}

# =========================
# UI
# =========================

ui <- navbarPage(
  title = "Project 2 - R Shiny Data Explorer",
  
  tabPanel(
    "User Guide",
    
    helpText("Workflow: Upload your own dataset or select a built-in dataset → Clean & preprocess → Feature engineering → EDA → Export results."),
    
    tags$hr(),
    
    tags$h4("Overview"),
    
    tags$ul(
      tags$li("Loading Datasets: Supports various format uploads and provides built-in datasets (e.g., Bank Marketing and Titanic)."),
      tags$li("Data Cleaning & Preprocessing: Handle inconsistencies, missing values, duplicates, and apply optional scaling and encoding."),
      tags$li("Feature Engineering: Create new features such as age_group, balance_level, contacted_before, and other derived variables."),
      tags$li("EDA (Exploratory Data Analysis): Interactive filtering, visualization, and dynamic statistical summaries."),
      tags$li("Export: Download cleaned and transformed datasets for reproducibility and reporting.")
    )
  ),
  
  tabPanel(
    "1) Data Upload & Preview",
    sidebarLayout(
      sidebarPanel(
        h4("Load Dataset"),
        
        radioButtons(
          "data_source",
          "Choose data source:",
          choices = c("Upload File" = "upload", "Built-in Dataset" = "builtin"),
          selected = "upload"
        ),
        
        conditionalPanel(
          condition = "input.data_source == 'upload'",
          fileInput(
            "upload",
            "Upload dataset (CSV / XLSX / JSON / RDS)",
            accept = c(".csv", ".txt", ".xlsx", ".xls", ".json", ".rds")
          )
        ),
        
        conditionalPanel(
          condition = "input.data_source == 'builtin'",
          selectInput(
            "builtin_choice",
            "Select a built-in dataset:",
            choices = c(
              "Bank Marketing" = "bank-full",
              "Titanic" = "titanic"
            )
          )
        ),
        
        actionButton("load_btn", "Load / Reset", class = "btn-primary"),
        
        tags$hr(),
        
        checkboxInput(
          "strings_to_factor",
          "Convert character columns to factors",
          value = TRUE
        ),
        
        checkboxInput(
          "show_preview",
          "Show data preview",
          value = TRUE
        ),
        
        tags$hr(),
        
        helpText("Click 'Load / Reset' to refresh the dataset and update the downstream tabs.")
      ),
      
      mainPanel(
        h4("Dataset Overview"),
        tableOutput("data_info"),
        
        tags$hr(),
        
        conditionalPanel(
          condition = "input.show_preview == true",
          tagList(
            h4("Raw Data Preview"),
            DTOutput("raw_preview")
          )
        ),
        
        tags$hr(),
        
        h4("Variable Structure & Raw Missing Summary"),
        helpText("Placeholder values such as 'unknown' are treated during the cleaning stage."),
        tableOutput("structure_summary"),
        
        tags$hr(),
        
        verbatimTextOutput("raw_summary")
      )
    )
  ),
  
  tabPanel(
    "2) Cleaning & Preprocessing",
    sidebarLayout(
      sidebarPanel(
        h4("Inconsistency Handling"),
        checkboxInput("unknown_as_na", "Treat 'unknown' / '?' / 'NULL' as missing", value = TRUE),
        checkboxInput("pdays_as_na", "Treat pdays = -1 as missing", value = TRUE),
        checkboxInput("coerce_numeric", "Convert numeric-like text columns to numeric", value = TRUE),
        
        tags$hr(),
        
        h4("Missing Data"),
        sliderInput(
          "missing_thr",
          "Drop columns with missing rate >",
          min = 0.50, max = 0.99, value = 0.95, step = 0.01
        ),
        selectInput(
          "missing_method",
          "Missing value strategy:",
          choices = c(
            "Do nothing",
            "Remove rows with missing values",
            "Median impute numeric columns",
            "Mean impute numeric columns",
            "Mode impute categorical columns"
          ),
          selected = "Median impute numeric columns"
        ),
        
        tags$hr(),
        
        h4("Duplicate / ID Handling"),
        checkboxInput("remove_dup_rows", "Remove duplicate rows", value = FALSE),
        checkboxInput("handle_dup_ids", "Handle duplicate/missing id (if id & position exist)", value = TRUE),
        
        tags$hr(),
        
        h4("Outlier Handling"),
        uiOutput("outlier_var_ui"),
        selectInput(
          "outlier_method",
          "Outlier strategy:",
          choices = c("None", "Remove by IQR", "Cap by IQR", "Log transform"),
          selected = "None"
        ),
        
        tags$hr(),
        
        h4("Transformations"),
        checkboxInput("scale_numeric", "Scale numeric features (z-score)", value = FALSE),
        checkboxInput("encode_cats", "Encode categorical columns (label encode)", value = FALSE),
        uiOutput("cat_cols_ui"),
        
        tags$hr(),
        
        actionButton("clean_btn", "Apply Cleaning", class = "btn-primary")
      ),
      
      mainPanel(
        h4("Before vs After Summary"),
        tableOutput("cleaning_summary"),
        
        tags$hr(),
        
        h4("Cleaned Preview"),
        DTOutput("clean_preview"),
        
        tags$hr(),
        
        h4("Missing Summary After Cleaning"),
        tableOutput("cleaned_missing_summary"),
        
        tags$hr(),
        
        h4("Outlier Preview"),
        plotOutput("outlier_plot", height = "300px"),
        
        tags$hr(),
        
        verbatimTextOutput("clean_summary")
      )
    )
  )
)

# =========================
# Server
# =========================

server <- function(input, output, session) {
  
  # -------------------------
  # Load raw data
  # -------------------------
  raw_data <- eventReactive(input$load_btn, {
    df <- NULL
    
    if (input$data_source == "upload") {
      req(input$upload)
      df <- read_uploaded_data(input$upload$datapath, input$upload$name)
    }
    
    if (input$data_source == "builtin") {
      if (input$builtin_choice == "bank-full") {
        bank_path <- file.path(data_dir, "bank-full.csv")
        shiny::validate(
          shiny::need(file.exists(bank_path), "Built-in Bank Marketing dataset file not found.")
        )
        df <- read.csv(
          bank_path,
          sep = ";",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
      
      if (input$builtin_choice == "titanic") {
        titanic_path <- file.path(data_dir, "titanic.csv")
        shiny::validate(
          shiny::need(file.exists(titanic_path), "Built-in Titanic dataset file not found.")
        )
        df <- read.csv(
          titanic_path,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }
    
    df <- as.data.frame(df, check.names = FALSE)
    
    # Convert empty strings to NA immediately after loading
    df[] <- lapply(df, function(x) {
      if (is.character(x)) {
        x[x == "" | trimws(x) == ""] <- NA
      }
      x
    })
    
    if (isTRUE(input$strings_to_factor)) {
      char_cols <- sapply(df, is.character)
      df[char_cols] <- lapply(df[char_cols], as.factor)
    }
    
    df
  }, ignoreNULL = TRUE)
  
  # -------------------------
  # Data Upload outputs
  # -------------------------
  output$data_info <- renderTable({
    req(raw_data())
    df <- raw_data()
    
    data.frame(
      Metric = c("Number of Rows", "Number of Columns", "Numeric Variables", "Categorical Variables"),
      Value = c(
        nrow(df),
        ncol(df),
        sum(sapply(df, is.numeric)),
        sum(sapply(df, function(x) is.factor(x) || is.character(x)))
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m")
  
  output$raw_preview <- renderDT({
    req(raw_data())
    
    DT::datatable(
      head(raw_data(), 10),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$structure_summary <- renderTable({
    req(raw_data())
    df <- raw_data()
    
    data.frame(
      Variable = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Percent = paste0(
        round(sapply(df, function(x) mean(is.na(x))) * 100, 2),
        "%"
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
  
  output$raw_summary <- renderPrint({
    req(raw_data())
    df <- raw_data()
    
    cat("Rows:", nrow(df), "\n")
    cat("Columns:", ncol(df), "\n\n")
    cat("Summary:\n")
    print(summary(df))
  })
  
  # -------------------------
  # Dynamic UI for cleaning
  # -------------------------
  output$cat_cols_ui <- renderUI({
    req(raw_data())
    cat_cols <- names(raw_data())[sapply(raw_data(), function(x) is.character(x) || is.factor(x))]
    
    if (length(cat_cols) == 0) {
      return(helpText("No categorical columns available for encoding."))
    }
    
    checkboxGroupInput(
      "cat_cols_selected",
      "Categorical columns to encode:",
      choices = cat_cols,
      selected = head(cat_cols, min(length(cat_cols), 5))
    )
  })
  
  output$outlier_var_ui <- renderUI({
    req(raw_data())
    num_cols <- names(raw_data())[sapply(raw_data(), is.numeric)]
    
    if (length(num_cols) == 0) {
      return(helpText("No numeric variables available for outlier handling."))
    }
    
    selectInput(
      "outlier_var",
      "Select numeric variable:",
      choices = num_cols,
      selected = num_cols[1]
    )
  })
  
  # -------------------------
  # Cleaned data
  # -------------------------
  clean_data <- eventReactive(input$clean_btn, {
    req(raw_data())
    
    df <- raw_data()
    df <- as.data.frame(df, check.names = FALSE)
    
    # 1) Inconsistency handling
    df[] <- lapply(df, function(x) {
      if (is.character(x)) {
        x[x == "" | trimws(x) == ""] <- NA
      }
      x
    })
    
    # unknown → NA
    if (isTRUE(input$unknown_as_na)) {
      df <- clean_char_na(df)
    }
    # standardize
    if (isTRUE(input$coerce_numeric)) {
      df <- coerce_numeric_if_possible(df)
    }
    
    if (isTRUE(input$pdays_as_na) && "pdays" %in% names(df) && is.numeric(df$pdays)) {
      df$pdays[df$pdays == -1] <- NA
    }
    
    # 2) Drop columns with high missingness
    miss_rate <- sapply(df, function(x) mean(is.na(x)))
    keep_cols <- miss_rate <= input$missing_thr
    df <- df[, keep_cols, drop = FALSE]
    
    # 3) Remove duplicate rows
    if (isTRUE(input$remove_dup_rows)) {
      df <- df[!duplicated(df), , drop = FALSE]
    }
    
    # 4) Handle duplicate/missing id if id & position exist
    lower_names <- tolower(names(df))
    if (isTRUE(input$handle_dup_ids) &&
        "id" %in% lower_names &&
        "position" %in% lower_names) {
      
      id_col <- names(df)[match("id", lower_names)]
      pos_col <- names(df)[match("position", lower_names)]
      
      df <- df %>%
        group_by(.data[[pos_col]]) %>%
        mutate(
          !!id_col := ifelse(
            is.na(.data[[id_col]]) | .data[[id_col]] == "",
            safe_first_non_missing(.data[[id_col]]),
            .data[[id_col]]
          )
        ) %>%
        ungroup()
      
      df <- df %>%
        distinct(.data[[id_col]], .keep_all = TRUE)
    }
    
    # 5) Missing value handling
    if (input$missing_method == "Remove rows with missing values") {
      df <- na.omit(df)
    }
    
    if (input$missing_method == "Median impute numeric columns") {
      num_cols <- names(df)[sapply(df, is.numeric)]
      if (length(num_cols) > 0) {
        df[num_cols] <- lapply(df[num_cols], function(x) {
          if (all(is.na(x))) return(x)
          x[is.na(x)] <- median(x, na.rm = TRUE)
          x
        })
      }
    }
    
    if (input$missing_method == "Mean impute numeric columns") {
      num_cols <- names(df)[sapply(df, is.numeric)]
      if (length(num_cols) > 0) {
        df[num_cols] <- lapply(df[num_cols], function(x) {
          if (all(is.na(x))) return(x)
          x[is.na(x)] <- mean(x, na.rm = TRUE)
          x
        })
      }
    }
    
    if (input$missing_method == "Mode impute categorical columns") {
      cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      if (length(cat_cols) > 0) {
        for (col in cat_cols) {
          mode_val <- get_mode(df[[col]])
          df[[col]][is.na(df[[col]])] <- mode_val
        }
      }
    }
    
    if (input$missing_method == "Do nothing") {
      cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      if (length(cat_cols) > 0) {
        for (col in cat_cols) {
          df[[col]][is.na(df[[col]])] <- "Unknown"
        }
      }
    }
    # 6) Outlier handling
    if (!is.null(input$outlier_var) &&
        input$outlier_var %in% names(df) &&
        is.numeric(df[[input$outlier_var]]) &&
        input$outlier_method != "None") {
      
      x <- df[[input$outlier_var]]
      
      if (!all(is.na(x))) {
        q1 <- quantile(x, 0.25, na.rm = TRUE)
        q3 <- quantile(x, 0.75, na.rm = TRUE)
        iqr_val <- q3 - q1
        lower <- q1 - 1.5 * iqr_val
        upper <- q3 + 1.5 * iqr_val
        
        if (input$outlier_method == "Remove by IQR") {
          keep <- (x >= lower & x <= upper) | is.na(x)
          df <- df[keep, , drop = FALSE]
        }
        
        if (input$outlier_method == "Cap by IQR") {
          x[x < lower] <- lower
          x[x > upper] <- upper
          df[[input$outlier_var]] <- x
        }
        
        if (input$outlier_method == "Log transform") {
          if (all(x >= 0, na.rm = TRUE)) {
            df[[input$outlier_var]] <- log1p(x)
          }
        }
      }
    }
    
    # 7) Scale numeric columns
    if (isTRUE(input$scale_numeric)) {
      num_cols <- names(df)[sapply(df, is.numeric)]
      if (length(num_cols) > 0) {
        df[num_cols] <- lapply(df[num_cols], safe_scale)
      }
    }
    
    # 8) Encode selected categorical columns
    if (isTRUE(input$encode_cats) && !is.null(input$cat_cols_selected)) {
      selected_cols <- intersect(input$cat_cols_selected, names(df))
      for (col in selected_cols) {
        if (is.character(df[[col]]) || is.factor(df[[col]])) {
          df[[col]] <- as.numeric(as.factor(df[[col]]))
        }
      }
    }
    
    df <- as.data.frame(df, check.names = FALSE)
    df
  }, ignoreNULL = TRUE)
  
  # -------------------------
  # Cleaning outputs
  # -------------------------
  output$cleaning_summary <- renderTable({
    req(raw_data(), clean_data())
    
    before_df <- raw_data()
    after_df  <- clean_data()
    
    data.frame(
      Metric = c("Rows", "Columns", "Total Missing Values", "Duplicate Rows"),
      Before = c(
        nrow(before_df),
        ncol(before_df),
        sum(is.na(before_df)),
        sum(duplicated(before_df))
      ),
      After = c(
        nrow(after_df),
        ncol(after_df),
        sum(is.na(after_df)),
        sum(duplicated(after_df))
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m")
  
  output$clean_preview <- renderDT({
    req(clean_data())
    
    DT::datatable(
      head(clean_data(), 10),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$cleaned_missing_summary <- renderTable({
    req(clean_data())
    df <- clean_data()
    
    data.frame(
      Variable = names(df),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Percent = paste0(round(sapply(df, function(x) mean(is.na(x))) * 100, 2), "%")
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
  
  output$outlier_plot <- renderPlot({
    req(raw_data(), clean_data(), input$outlier_var)
    
    shiny::validate(
      shiny::need(input$outlier_var %in% names(raw_data()), "Please select a valid numeric variable."),
      shiny::need(is.numeric(raw_data()[[input$outlier_var]]), "Selected variable is not numeric.")
    )
    
    par(mfrow = c(1, 2))
    
    boxplot(
      raw_data()[[input$outlier_var]],
      main = "Before Cleaning",
      ylab = input$outlier_var
    )
    
    if (input$outlier_var %in% names(clean_data()) &&
        is.numeric(clean_data()[[input$outlier_var]])) {
      boxplot(
        clean_data()[[input$outlier_var]],
        main = "After Cleaning",
        ylab = input$outlier_var
      )
    } else {
      plot.new()
      text(0.5, 0.5, "Selected variable not available after cleaning.")
    }
  })
  
  output$clean_summary <- renderPrint({
    req(clean_data())
    
    df_before <- raw_data()
    df_after  <- clean_data()
    
    cat("Cleaning completed successfully.\n\n")
    cat("Rows before:", nrow(df_before), "\n")
    cat("Rows after :", nrow(df_after), "\n")
    cat("Columns before:", ncol(df_before), "\n")
    cat("Columns after :", ncol(df_after), "\n")
    cat("Total missing values before:", sum(is.na(df_before)), "\n")
    cat("Total missing values after :", sum(is.na(df_after)), "\n")
    cat("Duplicate rows before:", sum(duplicated(df_before)), "\n")
    cat("Duplicate rows after :", sum(duplicated(df_after)), "\n")
  })
}

shinyApp(ui = ui, server = server)