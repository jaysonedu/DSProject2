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

clean_char_na <- function(df, treat_unknown = TRUE) {
  df[] <- lapply(df, function(x) {
    if (is.factor(x)) x <- as.character(x)
    
    if (is.character(x)) {
      x <- trimws(x)
      x[x == ""] <- NA
      
      missing_tokens <- c("na", "null", "?")
      if (isTRUE(treat_unknown)) {
        missing_tokens <- c(missing_tokens, "unknown")
      }
      
      x[tolower(x) %in% missing_tokens] <- NA
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

missing_summary_all <- function(df) {
  miss_count <- sapply(df, function(x) sum(is.na(x)))
  miss_pct   <- sapply(df, function(x) mean(is.na(x)) * 100)
  
  out <- data.frame(
    Variable = names(df),
    Missing_Count = miss_count,
    Missing_Percent_Num = miss_pct,
    Missing_Percent = paste0(round(miss_pct, 2), "%"),
    stringsAsFactors = FALSE
  )
  
  out <- out[order(-out$Missing_Percent_Num, -out$Missing_Count, out$Variable), , drop = FALSE]
  out$Missing_Percent_Num <- NULL
  rownames(out) <- NULL
  out
}

missing_summary_nonzero <- function(df) {
  miss_count <- sapply(df, function(x) sum(is.na(x)))
  miss_pct   <- sapply(df, function(x) mean(is.na(x)) * 100)
  
  out <- data.frame(
    Variable = names(df),
    Missing_Count = miss_count,
    Missing_Percent_Num = miss_pct,
    Missing_Percent = paste0(round(miss_pct, 2), "%"),
    stringsAsFactors = FALSE
  )
  
  out <- out[out$Missing_Count > 0, , drop = FALSE]
  out <- out[order(-out$Missing_Percent_Num, -out$Missing_Count, out$Variable), , drop = FALSE]
  out$Missing_Percent_Num <- NULL
  rownames(out) <- NULL
  out
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
      tags$li("Loading Datasets: Supports multiple file formats and provides built-in datasets (e.g., Bank Marketing and Titanic)."),
      tags$li("Data Cleaning & Preprocessing: Handle inconsistencies, missing values, duplicates, scaling, encoding, and outliers."),
      tags$li("Feature Engineering: Create new variables and inspect the effect of transformations."),
      tags$li("EDA: Explore distributions, relationships, and interactive summaries."),
      tags$li("Export: Download cleaned and transformed datasets.")
    ),
    
    tags$hr(),
    
    tags$h4("How Missing Data Is Handled in This App"),
    tags$ul(
      tags$li("Step 1: Standardize inconsistent values such as 'unknown', '?', 'NULL', or pdays = -1 into missing values when selected."),
      tags$li("Step 2: Drop very sparse columns above the chosen missing-rate threshold."),
      tags$li("Step 3: Either apply one global missing-value strategy or specify column-specific rules.")
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
        helpText("This table shows the current raw dataset state before cleaning."),
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
        helpText("First remove sparse columns, then choose a global strategy or column-specific treatment for the remaining missing values."),
        sliderInput(
          "missing_thr",
          "Drop columns with missing rate >",
          min = 0.50, max = 0.99, value = 0.95, step = 0.01
        ),
        
        checkboxInput(
          "use_column_missing_rules",
          "Use column-specific missing treatment",
          value = FALSE
        ),
        
        conditionalPanel(
          condition = "input.use_column_missing_rules == false",
          tagList(
            selectInput(
              "missing_method",
              "Missing value strategy:",
              choices = c(
                "Keep remaining missing values as NA" = "keep_na",
                "Remove rows with missing values" = "drop_rows",
                "Median impute numeric columns" = "median_num",
                "Mean impute numeric columns" = "mean_num",
                "Mode impute categorical columns" = "mode_cat",
                "Fill categorical missing with constant value" = "constant_cat"
              ),
              selected = "median_num"
            ),
            conditionalPanel(
              condition = "input.missing_method == 'constant_cat'",
              textInput("missing_constant", "Constant value for categorical missing:", value = "Missing")
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.use_column_missing_rules == true",
          tagList(
            helpText("Select column-specific treatments below. Columns not selected will keep missing values as NA."),
            uiOutput("missing_drop_cols_ui"),
            uiOutput("missing_drop_rows_cols_ui"),
            uiOutput("missing_median_cols_ui"),
            uiOutput("missing_mean_cols_ui"),
            uiOutput("missing_mode_cols_ui"),
            uiOutput("missing_constant_cols_ui"),
            textInput("missing_constant_value", "Constant value:", value = "Missing")
          )
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
        helpText("Only variables with remaining missing values are shown below. All other variables have no missing values."),
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
  # Upload outputs
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
    
    out <- data.frame(
      Variable = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Percent_Num = sapply(df, function(x) mean(is.na(x)) * 100),
      Missing_Percent = paste0(round(sapply(df, function(x) mean(is.na(x)) * 100), 2), "%"),
      stringsAsFactors = FALSE
    )
    
    out <- out[order(-out$Missing_Percent_Num, -out$Missing_Count, out$Variable), , drop = FALSE]
    out$Missing_Percent_Num <- NULL
    rownames(out) <- NULL
    out
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
  # Dynamic UI: encoding / outliers
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
  # Base-cleaned data for dynamic missing UI
  # -------------------------
  pre_missing_data <- reactive({
    req(raw_data())
    
    df <- raw_data()
    df <- as.data.frame(df, check.names = FALSE)
    
    # 1) Standardize inconsistent values
    df[] <- lapply(df, function(x) {
      if (is.character(x)) {
        x[x == "" | trimws(x) == ""] <- NA
      }
      x
    })
    
    df <- clean_char_na(df, treat_unknown = isTRUE(input$unknown_as_na))
    
    if (isTRUE(input$coerce_numeric)) {
      df <- coerce_numeric_if_possible(df)
    }
    
    if (isTRUE(input$pdays_as_na) && "pdays" %in% names(df) && is.numeric(df$pdays)) {
      df$pdays[df$pdays == -1] <- NA
    }
    
    # 2) Drop sparse columns
    miss_rate <- sapply(df, function(x) mean(is.na(x)))
    keep_cols <- miss_rate <= input$missing_thr
    df <- df[, keep_cols, drop = FALSE]
    
    as.data.frame(df, check.names = FALSE)
  })
  
  # -------------------------
  # Dynamic UI: column-specific missing treatment
  # -------------------------
  output$missing_drop_cols_ui <- renderUI({
    req(pre_missing_data())
    df <- pre_missing_data()
    
    miss_cols <- names(df)[sapply(df, function(x) any(is.na(x)))]
    
    if (length(miss_cols) == 0) {
      return(helpText("No columns with remaining missing values are currently available."))
    }
    
    checkboxGroupInput(
      "missing_drop_cols",
      "Columns to drop:",
      choices = miss_cols
    )
  })
  
  output$missing_median_cols_ui <- renderUI({
    req(pre_missing_data())
    df <- pre_missing_data()
    
    num_miss_cols <- names(df)[sapply(df, function(x) is.numeric(x) && any(is.na(x)))]
    
    if (length(num_miss_cols) == 0) {
      return(helpText("No numeric columns with missing values available for median imputation."))
    }
    
    checkboxGroupInput(
      "missing_median_cols",
      "Numeric columns for median imputation:",
      choices = num_miss_cols
    )
  })
  
  output$missing_mean_cols_ui <- renderUI({
    req(pre_missing_data())
    df <- pre_missing_data()
    
    num_miss_cols <- names(df)[sapply(df, function(x) is.numeric(x) && any(is.na(x)))]
    
    if (length(num_miss_cols) == 0) {
      return(helpText("No numeric columns with missing values available for mean imputation."))
    }
    
    checkboxGroupInput(
      "missing_mean_cols",
      "Numeric columns for mean imputation:",
      choices = num_miss_cols
    )
  })
  
  output$missing_mode_cols_ui <- renderUI({
    req(pre_missing_data())
    df <- pre_missing_data()
    
    cat_miss_cols <- names(df)[sapply(df, function(x) (is.character(x) || is.factor(x)) && any(is.na(x)))]
    
    if (length(cat_miss_cols) == 0) {
      return(helpText("No categorical columns with missing values available for mode imputation."))
    }
    
    checkboxGroupInput(
      "missing_mode_cols",
      "Categorical columns for mode imputation:",
      choices = cat_miss_cols
    )
  })
  
  output$missing_constant_cols_ui <- renderUI({
    req(pre_missing_data())
    df <- pre_missing_data()
    
    cat_miss_cols <- names(df)[sapply(df, function(x) (is.character(x) || is.factor(x)) && any(is.na(x)))]
    
    if (length(cat_miss_cols) == 0) {
      return(helpText("No categorical columns with missing values available for constant fill."))
    }
    
    checkboxGroupInput(
      "missing_constant_cols",
      "Categorical columns for constant fill:",
      choices = cat_miss_cols
    )
  })
  
  output$missing_drop_rows_cols_ui <- renderUI({
    req(pre_missing_data())
    df <- pre_missing_data()
    
    miss_cols <- names(df)[sapply(df, function(x) any(is.na(x)))]
    
    if (length(miss_cols) == 0) {
      return(helpText("No columns with remaining missing values available for row removal."))
    }
    
    checkboxGroupInput(
      "missing_drop_rows_cols",
      "Columns for row removal (drop rows if missing in selected columns):",
      choices = miss_cols
    )
  })
  
  # -------------------------
  # Cleaned data
  # -------------------------
  clean_data <- eventReactive(input$clean_btn, {
    req(pre_missing_data())
    
    df <- pre_missing_data()
    df <- as.data.frame(df, check.names = FALSE)
    
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
    if (isTRUE(input$use_column_missing_rules)) {
      
      # 5a Drop selected columns
      drop_cols <- intersect(input$missing_drop_cols, names(df))
      if (length(drop_cols) > 0) {
        df <- df[, setdiff(names(df), drop_cols), drop = FALSE]
      }
      
      # 5b Drop rows if selected columns contain missing values
      drop_rows_cols <- intersect(input$missing_drop_rows_cols, names(df))
      if (length(drop_rows_cols) > 0) {
        keep_idx <- complete.cases(df[, drop_rows_cols, drop = FALSE])
        df <- df[keep_idx, , drop = FALSE]
      }
      
      # 5c Median impute selected numeric columns
      median_cols <- intersect(input$missing_median_cols, names(df))
      for (col in median_cols) {
        if (col %in% names(df) && is.numeric(df[[col]]) && !all(is.na(df[[col]]))) {
          df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
        }
      }
      
      # 5d Mean impute selected numeric columns
      mean_cols <- intersect(input$missing_mean_cols, names(df))
      for (col in mean_cols) {
        if (col %in% names(df) && is.numeric(df[[col]]) && !all(is.na(df[[col]]))) {
          df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
        }
      }
      
      # 5e Mode impute selected categorical columns
      mode_cols <- intersect(input$missing_mode_cols, names(df))
      for (col in mode_cols) {
        if (col %in% names(df) && (is.character(df[[col]]) || is.factor(df[[col]]))) {
          mode_val <- get_mode(df[[col]])
          if (!is.na(mode_val)) {
            if (is.factor(df[[col]])) df[[col]] <- as.character(df[[col]])
            df[[col]][is.na(df[[col]])] <- mode_val
          }
        }
      }
      
      # 5f Constant fill selected categorical columns
      const_cols <- intersect(input$missing_constant_cols, names(df))
      for (col in const_cols) {
        if (col %in% names(df) && (is.character(df[[col]]) || is.factor(df[[col]]))) {
          if (is.factor(df[[col]])) df[[col]] <- as.character(df[[col]])
          df[[col]][is.na(df[[col]])] <- input$missing_constant_value
        }
      }
      
      # Any unselected columns keep NA
      
    } else { 
      
      if (input$missing_method == "drop_rows") {
        df <- na.omit(df)
      }
      
      if (input$missing_method == "median_num") {
        num_cols <- names(df)[sapply(df, is.numeric)]
        if (length(num_cols) > 0) {
          df[num_cols] <- lapply(df[num_cols], function(x) {
            if (all(is.na(x))) return(x)
            x[is.na(x)] <- median(x, na.rm = TRUE)
            x
          })
        }
      }
      
      if (input$missing_method == "mean_num") {
        num_cols <- names(df)[sapply(df, is.numeric)]
        if (length(num_cols) > 0) {
          df[num_cols] <- lapply(df[num_cols], function(x) {
            if (all(is.na(x))) return(x)
            x[is.na(x)] <- mean(x, na.rm = TRUE)
            x
          })
        }
      }
      
      if (input$missing_method == "mode_cat") {
        cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
        if (length(cat_cols) > 0) {
          for (col in cat_cols) {
            mode_val <- get_mode(df[[col]])
            if (!is.na(mode_val)) {
              if (is.factor(df[[col]])) df[[col]] <- as.character(df[[col]])
              df[[col]][is.na(df[[col]])] <- mode_val
            }
          }
        }
      }
      
      if (input$missing_method == "constant_cat") {
        cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
        if (length(cat_cols) > 0) {
          for (col in cat_cols) {
            if (is.factor(df[[col]])) df[[col]] <- as.character(df[[col]])
            df[[col]][is.na(df[[col]])] <- input$missing_constant
          }
        }
      }
      
      # keep_na = intentionally do nothing
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
    
    miss_df <- missing_summary_nonzero(df)
    
    if (nrow(miss_df) == 0) {
      return(data.frame(
        Message = "All other variables have no missing values.",
        check.names = FALSE
      ))
    }
    
    miss_df
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