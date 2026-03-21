library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(jsonlite)
library(ggplot2)
library(scales)
library(digest)

# -----------------------------
# Helpers
# -----------------------------
clean_char_na <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x[x == ""] <- NA
      x[x %in% c("NA", "Na", "na", "null", "NULL", "unknown", "Unknown", "?")] <- NA
    }
    x
  })
  df
}

coerce_numeric_if_possible <- function(df, min_numeric_ratio = 0.7) {
  df <- as.data.frame(df)
  for (nm in names(df)) {
    if (is.character(df[[nm]]) || is.logical(df[[nm]])) {
      parsed <- suppressWarnings(readr::parse_number(df[[nm]]))
      ratio <- mean(!is.na(parsed))
      if (!is.na(ratio) && ratio >= min_numeric_ratio) df[[nm]] <- parsed
    }
  }
  df
}

read_dataset <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))

  if (ext %in% c("csv", "txt")) {
    df <- readr::read_csv(
      path,
      na = c("NA", "na", "", "NULL", "unknown", "Unknown", "?"),
      show_col_types = FALSE
    )
    return(as.data.frame(df))
  }
  if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(path)
    return(as.data.frame(df))
  }
  if (ext %in% c("json")) {
    obj <- jsonlite::fromJSON(path, simplifyVector = TRUE)
    if (is.data.frame(obj)) return(obj)
    return(as.data.frame(obj))
  }
  if (ext %in% c("rds")) {
    df <- readRDS(path)
    return(as.data.frame(df))
  }

  stop("Unsupported file type: ", ext)
}

normalize_hit_column <- function(df) {
  df <- as.data.frame(df)
  if ("hit" %in% names(df)) return(df)

  # Titanic uses Survived (0/1)
  if ("Survived" %in% names(df)) {
    df$hit <- suppressWarnings(readr::parse_number(df$Survived))
    return(df)
  }

  # Bank marketing uses y ('yes'/'no')
  if ("y" %in% names(df)) {
    y <- df$y
    if (is.numeric(y) || is.integer(y)) {
      df$hit <- suppressWarnings(as.integer(y))
    } else {
      y_chr <- tolower(trimws(as.character(y)))
      df$hit <- ifelse(y_chr == "yes", 1, ifelse(y_chr == "no", 0, NA_real_))
    }
    return(df)
  }

  df
}

clean_dataset <- function(
    df,
    missing_threshold = 0.95,
    impute_numeric = TRUE,
    handle_duplicate_ids = TRUE,
    scale_numeric = FALSE,
    encode_categoricals = FALSE,
    categorical_cols = NULL
) {
  df <- clean_char_na(df)
  df <- coerce_numeric_if_possible(df)

  # ---- Duplicate / id consistency ----
  if (handle_duplicate_ids && all(c("id", "position") %in% names(df))) {
    df$position <- suppressWarnings(as.numeric(df$position))

    # Remove id groups that map to multiple distinct positions.
    bad_ids <- df %>%
      filter(!is.na(id) & !is.na(position)) %>%
      group_by(id) %>%
      summarise(n_pos = n_distinct(position), .groups = "drop") %>%
      filter(n_pos > 1) %>%
      pull(id)

    if (length(bad_ids) > 0) df <- df %>% filter(!(id %in% bad_ids))

    # If id is missing, generate deterministic pseudo-id (keeps rows).
    if (all(c("title", "artists", "year") %in% names(df))) {
      missing_id <- is.na(df$id) | df$id == "NA"
      key <- paste0(
        ifelse(is.na(df$title), "", as.character(df$title)),
        "|",
        ifelse(is.na(df$artists), "", as.character(df$artists)),
        "|",
        ifelse(is.na(df$year), "", as.character(df$year))
      )
      df$id[missing_id] <- paste0("missing_", digest(key[missing_id], algo = "sha1"))
    }

    # Keep only one row per id (best chart position = smallest number)
    df <- df %>% arrange(id, position) %>% distinct(id, .keep_all = TRUE)
  }

  # ---- Drop columns with too much missingness ----
  na_frac <- sapply(df, function(x) mean(is.na(x)))
  keep_cols <- names(na_frac)[na_frac <= missing_threshold]
  df <- df[, keep_cols, drop = FALSE]

  # ---- Numeric median imputation ----
  if (impute_numeric) {
    num_cols <- names(df)[sapply(df, is.numeric)]
    for (nm in num_cols) {
      med <- median(df[[nm]], na.rm = TRUE)
      if (!is.na(med)) df[[nm]][is.na(df[[nm]])] <- med
    }
  }

  # ---- Optional scaling ----
  if (scale_numeric) {
    num_cols <- names(df)[sapply(df, is.numeric)]
    for (nm in num_cols) {
      s <- sd(df[[nm]], na.rm = TRUE)
      if (!is.na(s) && s > 0) df[[nm]] <- as.numeric(scale(df[[nm]]))
    }
  }

  # ---- Optional categorical encoding ----
  if (encode_categoricals && !is.null(categorical_cols)) {
    for (nm in categorical_cols) {
      if (nm %in% names(df) && !is.numeric(df[[nm]])) {
        df[[nm]] <- as.integer(factor(df[[nm]]))
      }
    }
  }

  df
}

add_features <- function(
  df,
  add_age_group = TRUE,
  add_family_size = TRUE,
  add_log_time_price = TRUE,
  add_contacted_recent = TRUE,
  add_campaign_prev_ratio = FALSE
) {
  df <- as.data.frame(df)

  get_first_existing <- function(candidates) {
    hits <- candidates[candidates %in% names(df)]
    if (length(hits) == 0) return(NULL)
    hits[1]
  }

  # ---- Age binning (Bank/Titanic both have an age column under different names) ----
  age_col <- get_first_existing(c("age", "Age"))
  if (isTRUE(add_age_group) && !is.null(age_col) && is.numeric(df[[age_col]])) {
    df$age_group <- floor(df[[age_col]] / 10) * 10
  }

  # ---- Titanic family size ----
  sib_col <- get_first_existing(c("SibSp", "sibsp"))
  parch_col <- get_first_existing(c("Parch", "parch"))
  if (
    isTRUE(add_family_size) &&
      !is.null(sib_col) && !is.null(parch_col) &&
      is.numeric(df[[sib_col]]) && is.numeric(df[[parch_col]])
  ) {
    df$family_size <- df[[sib_col]] + df[[parch_col]] + 1
  }

  # ---- Log transform time/price ----
  fare_col <- get_first_existing(c("Fare", "fare"))
  duration_col <- get_first_existing(c("duration", "Duration"))
  if (isTRUE(add_log_time_price)) {
    if (!is.null(fare_col) && is.numeric(df[[fare_col]])) {
      df$log_fare <- log1p(df[[fare_col]])
    }
    if (!is.null(duration_col) && is.numeric(df[[duration_col]])) {
      df$log_duration <- log1p(df[[duration_col]])
    }
  }

  # ---- Bank marketing contacted flag ----
  pdays_col <- get_first_existing(c("pdays", "Pdays"))
  if (isTRUE(add_contacted_recent) && !is.null(pdays_col) && is.numeric(df[[pdays_col]])) {
    df$contacted_recent <- ifelse(df[[pdays_col]] != -1, 1, 0)
  }

  # ---- Ratio feature (Bank marketing) ----
  campaign_col <- get_first_existing(c("campaign", "Campaign"))
  previous_col <- get_first_existing(c("previous", "Previous"))
  if (
    isTRUE(add_campaign_prev_ratio) &&
      !is.null(campaign_col) && !is.null(previous_col) &&
      is.numeric(df[[campaign_col]]) && is.numeric(df[[previous_col]])
  ) {
    df$campaign_prev_ratio <- df[[campaign_col]] / (df[[previous_col]] + 1)
  }

  df
}

# -----------------------------
# UI
# -----------------------------
ui <- page_navbar(
  theme = bs_theme(version = 5),
  title = "Project 2 - R Shiny Data Explorer",
  tabPanel(
    "User Guide",
    helpText("流程：上传数据/选择内置数据 -> 清洗预处理 -> 特征工程 -> 交互式 EDA。"),
    tags$hr(),
    tags$h4("对应 PDF 要求（高层概述）"),
    tags$ul(
      tags$li("Loading Datasets：多格式上传 + 2 个内置数据集"),
      tags$li("Data Cleaning & Preprocessing：缺失处理、重复 id 处理、可选 scaling/编码"),
      tags$li("Feature Engineering：可选生成新特征（age_group / family_size / log_fare/log_duration / contacted_recent / campaign_prev_ratio）"),
      tags$li("EDA：过滤 + 交互式绘图 + 动态统计摘要"),
      tags$li("导出：下载清洗后/特征后数据用于报告复现")
    )
  ),

  tabPanel(
    "1) Data Upload & Preview",
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "upload",
          "Upload dataset (CSV/XLSX/JSON/RDS)",
          accept = c(".csv", ".xlsx", ".json", ".rds")
        ),
        tags$hr(),
        selectInput(
          "builtin_choice",
          "Or use built-in dataset",
          choices = c("Bank Marketing (built-in)" = "bank_marketing", "Titanic (built-in)" = "titanic")
        ),
        actionButton("load_btn", "Load / Reset", class = "btn-primary"),
        tags$hr(),
        helpText("提示：点击一次 `Load / Reset` 后才会刷新后续 tab。")
      ),
      mainPanel(
        h4("Raw Preview"),
        DTOutput("raw_preview"),
        tags$hr(),
        verbatimTextOutput("raw_summary")
      )
    )
  ),

  tabPanel(
    "2) Cleaning & Preprocessing",
    sidebarLayout(
      sidebarPanel(
        sliderInput("missing_thr", "Drop columns with missing rate >", min = 0.5, max = 0.99, value = 0.95, step = 0.01),
        checkboxInput("impute_numeric", "Median impute numeric missing values", value = TRUE),
        checkboxInput("handle_dup_ids", "Handle duplicate/missing id (if id & position exist)", value = TRUE),
        checkboxInput("scale_numeric", "Scale numeric features (z-score)", value = FALSE),
        checkboxInput("encode_cats", "Encode categorical columns (label encode)", value = FALSE),
        uiOutput("cat_cols_ui"),
        tags$hr(),
        actionButton("clean_btn", "Apply Cleaning", class = "btn-primary")
      ),
      mainPanel(
        h4("Cleaned Preview"),
        DTOutput("clean_preview"),
        tags$hr(),
        verbatimTextOutput("clean_summary")
      )
    )
  ),

  tabPanel(
    "3) Feature Engineering",
    sidebarLayout(
      sidebarPanel(
        checkboxInput("add_age_group", "Add age_group (10-year bins) if age exists", value = TRUE),
        checkboxInput("add_family_size", "Add family_size = SibSp + Parch + 1 if Titanic columns exist", value = TRUE),
        checkboxInput("add_log_time_price", "Add log-transformed time/price (log1p(Fare) / log1p(duration))", value = TRUE),
        checkboxInput("add_contacted_recent", "Add contacted_recent = (pdays != -1) if pdays exists", value = TRUE),
        checkboxInput("add_campaign_prev_ratio", "Add campaign_prev_ratio = campaign/(previous+1) if columns exist", value = FALSE),
        tags$hr(),
        actionButton("feat_btn", "Apply Features", class = "btn-primary")
      ),
      mainPanel(
        h4("Featured Preview"),
        DTOutput("feat_preview"),
        tags$hr(),
        verbatimTextOutput("feat_summary")
      )
    )
  ),

  tabPanel(
    "4) EDA (Interactive)",
    sidebarLayout(
      sidebarPanel(
        uiOutput("hit_filter_ui"),
        tags$hr(),
        uiOutput("year_filter_ui"),
        tags$hr(),
        uiOutput("numeric_filter_var_ui"),
        uiOutput("numeric_filter_range_ui"),
        tags$hr(),
        radioButtons(
          "plot_type",
          "Plot type",
          choices = c("Histogram" = "hist", "Scatter" = "scatter", "Boxplot by hit" = "box", "Correlation heatmap" = "corr"),
          selected = "hist"
        ),
        tags$hr(),
        uiOutput("xvar_ui"),
        uiOutput("yvar_ui"),
        uiOutput("corr_cols_ui"),
        tags$hr(),
        helpText("图表根据过滤条件实时更新。数据规模大时建议先在左侧过滤减少点数。")
      ),
      mainPanel(
        h4("EDA Outputs"),
        DTOutput("filtered_preview"),
        tags$hr(),
        plotOutput("main_plot", height = 340),
        tags$hr(),
        verbatimTextOutput("eda_stats")
      )
    )
  ),

  tabPanel(
    "5) Export",
    fluidRow(
      column(
        4,
        downloadButton("download_clean", "Download cleaned CSV", class = "btn-success"),
        br(), br(),
        downloadButton("download_featured", "Download featured CSV", class = "btn-success")
      ),
      column(8, helpText("用于报告/复现分析的数据导出。"))
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  app_dir <- getwd()

  # -------------------------------------------------
  # Built-in datasets (no external files needed)
  # -------------------------------------------------
  make_synthetic_billboard <- function(n = 2000, seed = 42) {
    set.seed(seed)

    years <- sample(1950:2015, n, replace = TRUE)

    # "Audio features" (bounded/continuous)
    energy <- runif(n, 0, 1)
    danceability <- runif(n, 0, 1)
    loudness <- rnorm(n, mean = -6, sd = 2) # typical Spotify loudness range
    valence <- pmin(pmax(energy * 0.6 + runif(n, 0, 0.4), 0), 1)
    acousticness <- pmin(pmax(1 - energy + rnorm(n, 0, 0.08), 0), 1)
    instrumentalness <- pmin(pmax(1 - energy + rnorm(n, 0, 0.12), 0), 1)
    speechiness <- pmin(pmax(runif(n, 0, 0.6) + (1 - energy) * 0.1, 0), 1)
    liveness <- pmin(pmax(runif(n, 0, 1), 0), 1)
    tempo <- runif(n, 60, 180)
    time_signature <- sample(3:7, n, replace = TRUE)
    mode <- sample(0:1, n, replace = TRUE) # 0/1 to simplify

    # Text/readability-like features
    f_k_grade <- pmax(rnorm(n, 2.9, 1.2), 0)
    flesch_index <- pmin(pmax(rnorm(n, 85, 12), 0), 120)
    fog_index <- pmin(pmax(rnorm(n, 6.0, 1.5), 0), 20)
    num_syllables <- pmax(round(rnorm(n, 140, 50)), 1)
    difficult_words <- pmax(round(rnorm(n, 18, 8)), 0)
    num_dupes <- pmax(round(rnorm(n, 8, 4)), 0)
    num_words <- pmax(round(rnorm(n, 420, 180)), 10)
    num_lines <- pmax(round(rnorm(n, 20, 8)), 1)

    # Sentiment decomposition (keep pos/neg separate so net_sentiment is meaningful)
    pos_sentiment <- pmin(pmax(runif(n, 0, 1) * (0.4 + energy * 0.7), 0), 1)
    neg_sentiment <- pmin(pmax(runif(n, 0, 1) * (0.4 + acousticness * 0.6), 0), 1)

    # Categorical fields
    artist_pool <- c("Carpenters", "Patti Page", "Bon Jovi", "Maroon 5", "Erasure", "En Vogue")
    genre_pool <- c("jazz, pop, swing", "country, pop", "rock", "disco", "synthpop", "r&b")
    artists <- sample(artist_pool, n, replace = TRUE)
    title_pool <- c("Always", "Superstar", "Animals", "Hold On", "Don't Be Cruel", "I Wanna Be Loved")
    titles <- sample(title_pool, n, replace = TRUE)
    genre_tags <- sample(genre_pool, n, replace = TRUE)

    # Chart position per year (smaller means better chart)
    # (We'll generate an id + position so cleaning can demonstrate duplicate-id handling.)
    position <- pmin(pmax(round(runif(n, 1, 100)), 1), 100)

    # ids (some missing -> pseudo id generation; some duplicates with multiple positions -> drop)
    id <- paste0("track_", sample(1:(n * 0.9), n, replace = TRUE))
    missing_id_rate <- 0.06
    id[sample(1:n, floor(n * missing_id_rate))] <- NA

    # Create a tiny amount of duplicate-id groups with multiple positions
    dup_rate <- 0.01
    dup_n <- floor(n * dup_rate)
    if (dup_n > 0) {
      chosen <- sample(setdiff(1:n, which(is.na(id))), dup_n, replace = FALSE)
      # force a subset to share the same id but keep different positions
      target_id <- id[chosen[1]]
      id[chosen] <- target_id
      # ensure multiple different positions exist inside that id group
      if (length(chosen) >= 3) {
        position[chosen[1]] <- 5
        position[chosen[2]] <- 25
        position[chosen[3]] <- 70
      }
    }

    # Binary outcome "hit"
    # High energy + higher loudness (less negative) + higher valence -> more likely hit
    z <- 3.0 * energy + 0.4 * valence + 0.35 * (loudness + 6) - 2.2 * acousticness - 0.3 * instrumentalness
    prob <- 1 / (1 + exp(-z + 0.5))
    hit <- rbinom(n, size = 1, prob = prob)

    # explicit (0/1 with missingness)
    explicit <- rbinom(n, 1, 0.15)
    explicit[sample(1:n, floor(n * 0.03))] <- NA

    df <- data.frame(
      year = years,
      position = position,
      title = titles,
      artist = artists,
      pos_sentiment = pos_sentiment,
      neg_sentiment = neg_sentiment,
      neut_sentiment = pmax(pmin(1 - (pos_sentiment + neg_sentiment) / 2, 1), 0),
      compound_sentiment = pos_sentiment - neg_sentiment,
      f_k_grade = f_k_grade,
      flesch_index = flesch_index,
      fog_index = fog_index,
      num_syllables = num_syllables,
      difficult_words = difficult_words,
      num_dupes = num_dupes,
      num_words = num_words,
      num_lines = num_lines,
      genre_tags = genre_tags,
      id = id,
      popularity = pmin(pmax(round(runif(n, 10, 100) + hit * 15), 0), 100),
      duration_ms = round(runif(n, 140000, 240000)),
      explicit = explicit,
      artists = artists,
      id_artists = paste0("artist_", match(artists, artist_pool)),
      danceability = danceability,
      energy = energy,
      key = sample(0:11, n, replace = TRUE),
      loudness = loudness,
      mode = mode,
      speechiness = speechiness,
      acousticness = acousticness,
      instrumentalness = instrumentalness,
      liveness = liveness,
      valence = valence,
      tempo = tempo,
      time_signature = time_signature,
      hit = hit
    )

    # Add missingness to numeric + categorical to demo imputation.
    miss_num <- 0.05
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    for (nm in numeric_cols) {
      idx <- sample.int(n, floor(n * miss_num))
      df[idx, nm] <- NA
    }
    # categorical missingness
    miss_cat <- 0.1
    df$genre_tags[sample.int(n, floor(n * miss_cat))] <- NA

    df
  }

  get_builtin <- function(choice) {
    if (choice == "bank_marketing") {
      return(read_dataset(file.path(app_dir, "data", "bank_marketing.csv"), "bank_marketing.csv"))
    }
    if (choice == "titanic") {
      return(read_dataset(file.path(app_dir, "data", "titanic.csv"), "titanic.csv"))
    }
    stop("Unknown built-in dataset: ", choice)
  }

  raw_data <- eventReactive(input$load_btn, {
    df <- if (!is.null(input$upload)) {
      req(input$upload$datapath, input$upload$name)
      read_dataset(input$upload$datapath, input$upload$name)
    } else {
      get_builtin(input$builtin_choice)
    }
    normalize_hit_column(df)
  }, ignoreNULL = TRUE)

  output$raw_preview <- renderDT({
    req(raw_data())
    datatable(head(raw_data(), 500), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$raw_summary <- renderPrint({
    req(raw_data())
    df <- raw_data()
    cat("rows =", nrow(df), "\n")
    cat("cols =", ncol(df), "\n\n")
    cat("Column classes (top):\n")
    print(head(table(sapply(df, function(x) class(x)[1])), 10))
    cat("\nTop missingness:\n")
    na_frac <- sort(sapply(df, function(x) mean(is.na(x))), decreasing = TRUE)
    print(head(na_frac, 10))
  })

  output$cat_cols_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    cat_cols <- names(df)[sapply(df, function(x) !is.numeric(x))]
    if (length(cat_cols) == 0) {
      return(tags$em("No categorical columns detected."))
    }
    checkboxGroupInput(
      "categorical_cols",
      "Categorical columns to encode (label encode)",
      choices = cat_cols,
      selected = head(cat_cols, 5)
    )
  })

  cleaned_data <- eventReactive(input$clean_btn, {
    req(raw_data())
    df <- raw_data()
    categorical_cols <- if (!is.null(input$categorical_cols)) input$categorical_cols else NULL

    clean_dataset(
      df,
      missing_threshold = input$missing_thr,
      impute_numeric = isTRUE(input$impute_numeric),
      handle_duplicate_ids = isTRUE(input$handle_dup_ids),
      scale_numeric = isTRUE(input$scale_numeric),
      encode_categoricals = isTRUE(input$encode_cats),
      categorical_cols = categorical_cols
    )
  }, ignoreNULL = TRUE)

  output$clean_preview <- renderDT({
    req(cleaned_data())
    datatable(head(cleaned_data(), 500), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$clean_summary <- renderPrint({
    req(cleaned_data())
    df <- cleaned_data()
    cat("rows =", nrow(df), "\n")
    cat("cols =", ncol(df), "\n\n")
    cat("Top missingness after cleaning:\n")
    na_frac <- sort(sapply(df, function(x) mean(is.na(x))), decreasing = TRUE)
    print(head(na_frac, 10))
  })

  featured_data <- eventReactive(input$feat_btn, {
    req(cleaned_data())
    add_features(
      cleaned_data(),
      add_age_group = isTRUE(input$add_age_group),
      add_family_size = isTRUE(input$add_family_size),
      add_log_time_price = isTRUE(input$add_log_time_price),
      add_contacted_recent = isTRUE(input$add_contacted_recent),
      add_campaign_prev_ratio = isTRUE(input$add_campaign_prev_ratio)
    )
  }, ignoreNULL = TRUE)

  output$feat_preview <- renderDT({
    req(featured_data())
    datatable(head(featured_data(), 500), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$feat_summary <- renderPrint({
    req(featured_data())
    df <- featured_data()
    cat("rows =", nrow(df), "\n")
    cat("cols =", ncol(df), "\n\n")
    added <- c("age_group", "family_size", "log_fare", "log_duration", "contacted_recent", "campaign_prev_ratio")
    cat("Added feature columns (if present):\n")
    print(intersect(added, names(df)))
  })

  # ---- EDA helpers / UI ----
  numeric_vars <- reactive({
    req(featured_data())
    df <- featured_data()
    names(df)[sapply(df, is.numeric)]
  })

  output$hit_filter_ui <- renderUI({
    req(featured_data())
    df <- featured_data()
    if (!("hit" %in% names(df))) {
      return(tags$em("No `hit` column found. Boxplot-by-hit is disabled."))
    }
    return(selectInput("hit_filter", "hit filter", choices = c(0, 1), selected = c(0, 1), multiple = TRUE))
  })

  output$year_filter_ui <- renderUI({
    req(featured_data())
    df <- featured_data()
    if (!("year" %in% names(df)) || !is.numeric(df$year)) {
      return(tags$em("No numeric `year` column found; year filter disabled."))
    }
    yr <- df$year
    yr <- yr[!is.na(yr)]
    validate(need(length(unique(yr)) > 1, "year column has too few distinct values"))
    sliderInput("year_range", "Year range", min = min(yr), max = max(yr), value = c(min(yr), max(yr)))
  })

  output$numeric_filter_var_ui <- renderUI({
    req(featured_data())
    df <- featured_data()
    vars <- names(df)[sapply(df, is.numeric)]
    vars <- setdiff(vars, c("hit", "year"))
    if (length(vars) == 0) return(tags$em("No numeric columns available for additional filtering."))
    selectInput("numeric_filter_var", "Numeric filter variable", choices = vars, selected = vars[1])
  })

  output$numeric_filter_range_ui <- renderUI({
    req(featured_data(), input$numeric_filter_var)
    df <- featured_data()
    var <- input$numeric_filter_var
    validate(need(var %in% names(df), "Selected filter variable not found"))
    x <- df[[var]]
    x <- x[!is.na(x)]
    validate(need(length(unique(x)) > 1, "Not enough distinct values for a range filter"))
    x_min <- min(x)
    x_max <- max(x)
    # Default range = 5% ~ 95% quantiles to reduce outlier sensitivity.
    q <- as.numeric(quantile(x, probs = c(0.05, 0.95), na.rm = TRUE))
    step <- (x_max - x_min) / 200
    if (!is.finite(step) || step <= 0) step <- 1
    sliderInput("numeric_filter_range", "Numeric range filter", min = x_min, max = x_max, value = q, step = step)
  })

  output$xvar_ui <- renderUI({
    vars <- numeric_vars()
    if (length(vars) == 0) return(tags$em("No numeric columns available for plotting."))
    selectInput("xvar", "X variable", choices = vars, selected = vars[1])
  })

  output$yvar_ui <- renderUI({
    vars <- numeric_vars()
    if (length(vars) == 0) return(NULL)
    if (isTRUE(input$plot_type == "scatter")) {
      selected <- ifelse(length(vars) >= 2, vars[2], vars[1])
      return(selectInput("yvar", "Y variable", choices = vars, selected = selected))
    }
    return(NULL)
  })

  output$corr_cols_ui <- renderUI({
    vars <- numeric_vars()
    if (length(vars) == 0) return(NULL)
    if (isTRUE(input$plot_type == "corr")) {
      vars <- vars[seq_len(min(length(vars), 12))]
      return(selectInput(
        "corr_cols",
        "Correlation columns (select subset)",
        choices = numeric_vars(),
        selected = vars,
        multiple = TRUE
      ))
    }
    return(NULL)
  })

  filtered <- reactive({
    req(featured_data())
    df <- featured_data()

    if ("hit" %in% names(df) && !is.null(input$hit_filter)) {
      df <- df %>% filter(hit %in% input$hit_filter)
    }

    if ("year" %in% names(df) && !is.null(input$year_range) && is.numeric(df$year)) {
      df <- df %>% filter(year >= input$year_range[1], year <= input$year_range[2])
    }

    if (
      !is.null(input$numeric_filter_var) &&
        !is.null(input$numeric_filter_range) &&
        input$numeric_filter_var %in% names(df) &&
        is.numeric(df[[input$numeric_filter_var]])
    ) {
      rng <- input$numeric_filter_range
      df <- df %>%
        filter(.data[[input$numeric_filter_var]] >= rng[1], .data[[input$numeric_filter_var]] <= rng[2])
    }

    df
  })

  output$filtered_preview <- renderDT({
    req(filtered())
    datatable(head(filtered(), 500), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$main_plot <- renderPlot({
    req(filtered())
    df <- filtered()
    req(input$plot_type)

    if (input$plot_type == "hist") {
      req(input$xvar)
      validate(need(input$xvar %in% names(df), "xvar not found in data"))
      ggplot(df, aes(x = .data[[input$xvar]])) +
        geom_histogram(bins = 30, fill = "#2C7FB8", alpha = 0.8) +
        theme_minimal() +
        labs(x = input$xvar, y = "Count", title = paste("Histogram of", input$xvar))

    } else if (input$plot_type == "scatter") {
      req(input$xvar, input$yvar)
      validate(need(input$xvar %in% names(df) && input$yvar %in% names(df), "variables not found"))
      plot_df <- df
      if (nrow(plot_df) > 20000) plot_df <- plot_df %>% slice_sample(n = 20000)
      ggplot(plot_df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
        geom_point(alpha = 0.25) +
        geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 0.8) +
        theme_minimal() +
        labs(x = input$xvar, y = input$yvar, title = paste(input$xvar, "vs", input$yvar))

    } else if (input$plot_type == "box") {
      req(input$xvar)
      validate(need("hit" %in% names(df), "No `hit` column for boxplot"))
      validate(need(input$xvar %in% names(df), "xvar not found"))
      ggplot(df, aes(x = factor(hit), y = .data[[input$xvar]])) +
        geom_boxplot(fill = "#7FC8A9", alpha = 0.85) +
        theme_minimal() +
        labs(x = "hit", y = input$xvar, title = paste("Boxplot of", input$xvar, "by hit"))

    } else if (input$plot_type == "corr") {
      req(input$corr_cols)
      validate(need(length(input$corr_cols) >= 2, "Select at least 2 numeric columns"))
      cols <- input$corr_cols
      cols <- cols[cols %in% names(df)]
      validate(need(length(cols) >= 2, "Selected columns not found in data"))

      corr <- cor(df[, cols, drop = FALSE], use = "pairwise.complete.obs")
      corr_long <- as.data.frame(as.table(corr))
      colnames(corr_long) <- c("Var1", "Var2", "Corr")
      corr_long$Var1 <- factor(corr_long$Var1, levels = cols)
      corr_long$Var2 <- factor(corr_long$Var2, levels = cols)

      ggplot(corr_long, aes(x = Var1, y = Var2, fill = Corr)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = muted("blue"), mid = "white", high = muted("red"), midpoint = 0, limits = c(-1, 1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = NULL, y = NULL, fill = "Corr", title = "Correlation heatmap")

    } else {
      plot.new()
    }
  })

  output$eda_stats <- renderPrint({
    req(filtered())
    df <- filtered()

    cat("Filtered rows =", nrow(df), "\n\n")
    if ("hit" %in% names(df)) {
      cat("hit distribution:\n")
      print(table(df$hit))
      cat("\n")
    }

    if (input$plot_type %in% c("hist", "box") && !is.null(input$xvar) && input$xvar %in% names(df)) {
      cat("Summary for ", input$xvar, ":\n", sep = "")
      print(summary(df[[input$xvar]]))
      cat("\n")
    }

    if (input$plot_type == "scatter" && !is.null(input$xvar) && !is.null(input$yvar) &&
        input$xvar %in% names(df) && input$yvar %in% names(df) &&
        is.numeric(df[[input$xvar]]) && is.numeric(df[[input$yvar]])) {
      cat("Pearson correlation (x,y):\n")
      print(cor(df[[input$xvar]], df[[input$yvar]], use = "pairwise.complete.obs"))
      cat("\n")
    }
  })

  output$download_clean <- downloadHandler(
    filename = function() paste0("cleaned_", Sys.Date(), ".csv"),
    content = function(file) {
      req(cleaned_data())
      write.csv(cleaned_data(), file, row.names = FALSE)
    }
  )

  output$download_featured <- downloadHandler(
    filename = function() paste0("featured_", Sys.Date(), ".csv"),
    content = function(file) {
      req(featured_data())
      write.csv(featured_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

