# Project 2 — Full R Shiny Data Explorer (PDF-aligned)
# Run: shiny::runApp("app.R")

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
library(plotly)

# bslib tooltips (assignment: guide users through controls)
ti <- function(widget, text, placement = "top") {
  bslib::tooltip(widget, text, placement = placement)
}

# -----------------------------
# Helpers
# -----------------------------
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
      if (!is.na(ratio) && ratio >= min_numeric_ratio) df[[nm]] <- parsed
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

normalize_hit_column <- function(df) {
  df <- as.data.frame(df, check.names = FALSE)
  if ("hit" %in% names(df)) return(df)

  if ("Survived" %in% names(df)) {
    s <- df$Survived
    if (is.numeric(s) || is.integer(s) || is.logical(s)) {
      df$hit <- suppressWarnings(as.numeric(s))
    } else {
      # parse_number() requires character; Survived may be factor after load
      df$hit <- suppressWarnings(readr::parse_number(as.character(s)))
    }
    return(df)
  }

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

# Titanic / uploads often code outcome as 1/2; UI filter used 0/1 -> no rows matched
coerce_hit_to_01 <- function(df) {
  if (!("hit" %in% names(df))) return(df)
  h <- suppressWarnings(as.numeric(as.character(df$hit)))
  u <- sort(unique(stats::na.omit(h)))
  if (length(u) == 2 && all(u %in% c(1, 2))) {
    df$hit <- h - 1L
  } else {
    df$hit <- h
  }
  df
}

handle_outliers <- function(df, var, method) {
  df <- as.data.frame(df, check.names = FALSE)
  if (!var %in% names(df) || !is.numeric(df[[var]]) || method == "None") return(df)

  x <- df[[var]]
  if (all(is.na(x))) return(df)

  q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  lower <- q1 - 1.5 * iqr_val
  upper <- q3 + 1.5 * iqr_val

  if (method == "Remove by IQR") {
    keep <- (x >= lower & x <= upper) | is.na(x)
    df <- df[keep, , drop = FALSE]
  } else if (method == "Cap by IQR") {
    x[x < lower] <- lower
    x[x > upper] <- upper
    df[[var]] <- x
  } else if (method == "Log transform") {
    if (all(x >= 0, na.rm = TRUE)) df[[var]] <- log1p(x)
  }
  df
}

clean_pipeline <- function(
    df,
    unknown_as_na = TRUE,
    pdays_as_na = TRUE,
    coerce_numeric_flag = TRUE,
    missing_thr = 0.95,
    missing_method = "Median impute numeric columns",
    remove_dup_rows = FALSE,
    handle_dup_ids = TRUE,
    outlier_var = NULL,
    outlier_method = "None",
    scale_numeric_flag = FALSE,
    encode_cats = FALSE,
    categorical_cols = NULL
) {
  df <- as.data.frame(df, check.names = FALSE)

  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x[x == "" | trimws(x) == ""] <- NA
    }
    x
  })

  if (unknown_as_na) df <- clean_char_na(df)
  if (coerce_numeric_flag) df <- coerce_numeric_if_possible(df)

  if (pdays_as_na && "pdays" %in% names(df) && is.numeric(df$pdays)) {
    df$pdays[df$pdays == -1] <- NA
  }

  miss_rate <- sapply(df, function(x) mean(is.na(x)))
  keep_cols <- names(miss_rate)[miss_rate <= missing_thr]
  df <- df[, keep_cols, drop = FALSE]

  if (remove_dup_rows) df <- df[!duplicated(df), , drop = FALSE]

  lower_names <- tolower(names(df))
  if (handle_dup_ids && "id" %in% lower_names && "position" %in% lower_names) {
    id_col <- names(df)[match("id", lower_names)]
    pos_col <- names(df)[match("position", lower_names)]
    df[[pos_col]] <- suppressWarnings(as.numeric(df[[pos_col]]))

    bad_ids <- df %>%
      filter(!is.na(.data[[id_col]]) & !is.na(.data[[pos_col]])) %>%
      group_by(.data[[id_col]]) %>%
      summarise(n_pos = dplyr::n_distinct(.data[[pos_col]]), .groups = "drop") %>%
      filter(n_pos > 1) %>%
      pull(.data[[id_col]])

    if (length(bad_ids) > 0) df <- df %>% filter(!(.data[[id_col]] %in% bad_ids))

    if (all(c("title", "artists", "year") %in% names(df))) {
      missing_id <- is.na(df[[id_col]]) | df[[id_col]] == "NA"
      key <- paste0(
        ifelse(is.na(df$title), "", as.character(df$title)), "|",
        ifelse(is.na(df$artists), "", as.character(df$artists)), "|",
        ifelse(is.na(df$year), "", as.character(df$year))
      )
      df[[id_col]][missing_id] <- paste0("missing_", digest(key[missing_id], algo = "sha1"))
    }

    df <- df %>% dplyr::arrange(.data[[id_col]], .data[[pos_col]]) %>%
      dplyr::distinct(.data[[id_col]], .keep_all = TRUE)
  }

  if (missing_method == "Remove rows with missing values") {
    df <- stats::na.omit(df)
  } else if (missing_method == "Median impute numeric columns") {
    num_cols <- names(df)[sapply(df, is.numeric)]
    for (nm in num_cols) {
      med <- median(df[[nm]], na.rm = TRUE)
      if (!is.na(med)) df[[nm]][is.na(df[[nm]])] <- med
    }
  } else if (missing_method == "Mean impute numeric columns") {
    num_cols <- names(df)[sapply(df, is.numeric)]
    for (nm in num_cols) {
      mu <- mean(df[[nm]], na.rm = TRUE)
      if (!is.na(mu)) df[[nm]][is.na(df[[nm]])] <- mu
    }
  } else if (missing_method == "Mode impute categorical columns") {
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    for (nm in cat_cols) {
      mv <- get_mode(df[[nm]])
      df[[nm]][is.na(df[[nm]])] <- mv
    }
  } else if (missing_method == "Do nothing") {
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    for (nm in cat_cols) {
      df[[nm]][is.na(df[[nm]])] <- "Unknown"
    }
  }

  if (!is.null(outlier_var) && outlier_var %in% names(df)) {
    df <- handle_outliers(df, outlier_var, outlier_method)
  }

  if (scale_numeric_flag) {
    num_cols <- names(df)[sapply(df, is.numeric)]
    for (nm in num_cols) df[[nm]] <- safe_scale(df[[nm]])
  }

  if (encode_cats && length(categorical_cols) > 0) {
    for (nm in categorical_cols) {
      if (nm %in% names(df) && !is.numeric(df[[nm]])) {
        df[[nm]] <- as.integer(factor(df[[nm]]))
      }
    }
  }

  as.data.frame(df, check.names = FALSE)
}

add_features <- function(
    df,
    add_age_group = TRUE,
    age_bin_width = 10,
    add_family_size = TRUE,
    add_log_time_price = TRUE,
    add_contacted_recent = TRUE,
    add_campaign_prev_ratio = FALSE
) {
  df <- as.data.frame(df, check.names = FALSE)

  get_first_existing <- function(candidates) {
    hits <- candidates[candidates %in% names(df)]
    if (length(hits) == 0) return(NULL)
    hits[1]
  }

  age_col <- get_first_existing(c("age", "Age"))
  if (isTRUE(add_age_group) && !is.null(age_col) && is.numeric(df[[age_col]])) {
    w <- max(1, suppressWarnings(as.numeric(age_bin_width)))
    if (!is.finite(w)) w <- 10
    df$age_group <- floor(df[[age_col]] / w) * w
  }

  sib_col <- get_first_existing(c("SibSp", "sibsp"))
  parch_col <- get_first_existing(c("Parch", "parch"))
  if (isTRUE(add_family_size) && !is.null(sib_col) && !is.null(parch_col) &&
      is.numeric(df[[sib_col]]) && is.numeric(df[[parch_col]])) {
    df$family_size <- df[[sib_col]] + df[[parch_col]] + 1
  }

  fare_col <- get_first_existing(c("Fare", "fare"))
  duration_col <- get_first_existing(c("duration", "Duration"))
  if (isTRUE(add_log_time_price)) {
    if (!is.null(fare_col) && is.numeric(df[[fare_col]])) df$log_fare <- log1p(df[[fare_col]])
    if (!is.null(duration_col) && is.numeric(df[[duration_col]])) df$log_duration <- log1p(df[[duration_col]])
  }

  pdays_col <- get_first_existing(c("pdays", "Pdays"))
  if (isTRUE(add_contacted_recent) && !is.null(pdays_col) && is.numeric(df[[pdays_col]])) {
    df$contacted_recent <- ifelse(df[[pdays_col]] != -1, 1, 0)
  }

  campaign_col <- get_first_existing(c("campaign", "Campaign"))
  previous_col <- get_first_existing(c("previous", "Previous"))
  if (isTRUE(add_campaign_prev_ratio) && !is.null(campaign_col) && !is.null(previous_col) &&
      is.numeric(df[[campaign_col]]) && is.numeric(df[[previous_col]])) {
    df$campaign_prev_ratio <- df[[campaign_col]] / (df[[previous_col]] + 1)
  }

  df
}

make_synthetic_billboard <- function(n = 2000, seed = 42) {
  set.seed(seed)
  years <- sample(1950:2015, n, replace = TRUE)
  energy <- runif(n, 0, 1)
  loudness <- rnorm(n, mean = -6, sd = 2)
  valence <- pmin(pmax(energy * 0.6 + runif(n, 0, 0.4), 0), 1)
  acousticness <- pmin(pmax(1 - energy + rnorm(n, 0, 0.08), 0), 1)
  instrumentalness <- pmin(pmax(1 - energy + rnorm(n, 0, 0.12), 0), 1)
  danceability <- runif(n, 0, 1)
  speechiness <- pmin(pmax(runif(n, 0, 0.6) + (1 - energy) * 0.1, 0), 1)
  liveness <- runif(n, 0, 1)
  tempo <- runif(n, 60, 180)
  time_signature <- sample(3:7, n, replace = TRUE)
  mode <- sample(0:1, n, replace = TRUE)

  artist_pool <- c("Carpenters", "Patti Page", "Bon Jovi", "Maroon 5", "Erasure", "En Vogue")
  genre_pool <- c("jazz, pop, swing", "country, pop", "rock", "disco", "synthpop", "r&b")
  artists <- sample(artist_pool, n, replace = TRUE)
  title_pool <- c("Always", "Superstar", "Animals", "Hold On", "Don't Be Cruel", "I Wanna Be Loved")
  titles <- sample(title_pool, n, replace = TRUE)
  genre_tags <- sample(genre_pool, n, replace = TRUE)

  position <- pmin(pmax(round(runif(n, 1, 100)), 1), 100)
  id <- paste0("track_", sample(1:floor(n * 0.9), n, replace = TRUE))
  id[sample(1:n, floor(n * 0.06))] <- NA

  dup_n <- floor(n * 0.01)
  if (dup_n > 0) {
    chosen <- sample(setdiff(1:n, which(is.na(id))), dup_n, replace = FALSE)
    target_id <- id[chosen[1]]
    id[chosen] <- target_id
    if (length(chosen) >= 3) {
      position[chosen[1]] <- 5
      position[chosen[2]] <- 25
      position[chosen[3]] <- 70
    }
  }

  z <- 3.0 * energy + 0.4 * valence + 0.35 * (loudness + 6) - 2.2 * acousticness - 0.3 * instrumentalness
  prob <- 1 / (1 + exp(-z + 0.5))
  hit <- rbinom(n, 1, prob)
  explicit <- rbinom(n, 1, 0.15)
  explicit[sample(1:n, floor(n * 0.03))] <- NA

  df <- data.frame(
    year = years,
    position = position,
    title = titles,
    artists = artists,
    pos_sentiment = pmin(pmax(runif(n, 0, 1) * (0.4 + energy * 0.7), 0), 1),
    neg_sentiment = pmin(pmax(runif(n, 0, 1) * (0.4 + acousticness * 0.6), 0), 1),
    genre_tags = genre_tags,
    id = id,
    popularity = pmin(pmax(round(runif(n, 10, 100) + hit * 15), 0), 100),
    duration_ms = round(runif(n, 140000, 240000)),
    explicit = explicit,
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

  numeric_cols <- names(df)[sapply(df, is.numeric)]
  for (nm in numeric_cols) {
    idx <- sample.int(n, floor(n * 0.05))
    df[idx, nm] <- NA
  }
  df$genre_tags[sample.int(n, floor(n * 0.1))] <- NA
  df
}

dataset_info <- function(df) {
  data.frame(
    Metric = c("Number of Rows", "Number of Columns", "Numeric Variables", "Categorical Variables"),
    Value = c(
      nrow(df),
      ncol(df),
      sum(sapply(df, is.numeric)),
      sum(sapply(df, function(x) is.factor(x) || is.character(x)))
    ),
    stringsAsFactors = FALSE
  )
}

structure_summary <- function(df) {
  data.frame(
    Variable = names(df),
    Type = sapply(df, function(x) class(x)[1]),
    Missing_Count = sapply(df, function(x) sum(is.na(x))),
    Missing_Percent = paste0(round(sapply(df, function(x) mean(is.na(x))) * 100, 2), "%"),
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# UI
# -----------------------------
ui <- page_navbar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Project 2 — Data Explorer",
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  tabPanel(
    "User Guide",
    fluidRow(
      column(
        10, offset = 1,
        tags$h3("How to use this application"),
        tags$p(
          "This app follows the Project 2 workflow: ",
          tags$strong("load data"), " → ", tags$strong("clean & preprocess"), " → ",
          tags$strong("engineer features"), " → ", tags$strong("explore (EDA)"), " → ",
          tags$strong("export"), "."
        ),
        tags$hr(),
        tags$h4("1) Loading datasets"),
        tags$ul(
          tags$li("Upload: CSV/TXT (comma, semicolon, or tab), Excel (.xlsx/.xls), JSON, or RDS."),
          tags$li("Built-in: Bank Marketing (Portuguese) and Titanic — files in the ", tags$code("data/"), " folder."),
          tags$li("Synthetic Billboard: generated demo data for testing duplicate-id cleaning and EDA without files.")
        ),
        tags$h4("2) Cleaning & preprocessing"),
        tags$ul(
          tags$li("Treat placeholders as missing, coerce numeric-like text, drop sparse columns, handle duplicates."),
          tags$li("Missing-value strategies, optional IQR outlier actions, z-score scaling, label encoding.")
        ),
        tags$h4("3) Feature engineering"),
        tags$ul(
          tags$li("Optional derived columns (age bins, family size, log transforms, campaign ratios) when columns exist.")
        ),
        tags$h4("4) EDA"),
        tags$ul(
          tags$li("Filter rows, choose plot type, use ", tags$strong("interactive Plotly"), " charts (pan/zoom/hover).")
        ),
        tags$h4("5) Export"),
        tags$ul(
          tags$li("Download cleaned or featured CSV for your report.")
        ),
        tags$h4("Tooltips"),
        tags$p(
          "Throughout the app, hover over controls to see short explanations (Bootstrap tooltips). ",
          "This matches the project requirement for guidance beyond this tab."
        ),
        tags$hr(),
        tags$p(
          class = "text-muted",
          "Tip: Click ", tags$strong("Load / Reset"), " after changing the data source. ",
          "Run ", tags$strong("Apply cleaning"), " — engineered features update automatically. ",
          "Click ", tags$strong("Apply features"), " if you change feature checkboxes without re-cleaning."
        )
      )
    )
  ),

  tabPanel(
    "1) Data Upload & Preview",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        ti(
          radioButtons(
            "data_source",
            "Data source",
            choices = c("Upload file" = "upload", "Built-in / synthetic" = "builtin"),
            selected = "upload"
          ),
          "Switch between your own file and built-in or synthetic demo data."
        ),
        conditionalPanel(
          condition = "input.data_source == 'upload'",
          ti(
            fileInput(
              "upload",
              "Upload dataset",
              accept = c(".csv", ".txt", ".xlsx", ".xls", ".json", ".rds")
            ),
            "CSV/TXT (comma, semicolon, or tab), Excel, JSON, or RDS."
          )
        ),
        conditionalPanel(
          condition = "input.data_source == 'builtin'",
          ti(
            selectInput(
              "builtin_choice",
              "Dataset",
              choices = c(
                "Bank Marketing (data/bank-full.csv)" = "bank",
                "Titanic (data/titanic.csv)" = "titanic",
                "Synthetic Billboard (demo)" = "synthetic"
              )
            ),
            "Bank & Titanic require files in the data/ folder. Synthetic needs no files."
          )
        ),
        ti(
          actionButton("load_btn", "Load / Reset", class = "btn-primary"),
          "Reload the dataset after changing source or file. Required before other tabs use new data."
        ),
        tags$hr(),
        ti(
          checkboxInput("strings_to_factor", "Convert character columns to factors", value = TRUE),
          "Factors help summaries; turn off if you need raw character columns for cleaning."
        ),
        ti(
          checkboxInput("show_preview", "Show raw preview & structure tables", value = TRUE),
          "Toggle the preview table and column missingness summary in the main panel."
        )
      ),
      mainPanel(
        width = 9,
        tableOutput("data_info"),
        tags$hr(),
        conditionalPanel(
          condition = "input.show_preview == true",
          tagList(
            h4("Raw preview"),
            DTOutput("raw_preview"),
            tags$hr(),
            h4("Variable structure & missingness"),
            DTOutput("structure_summary"),
            tags$hr(),
            verbatimTextOutput("raw_summary")
          )
        )
      )
    )
  ),

  tabPanel(
    "2) Cleaning & Preprocessing",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Inconsistency handling"),
        ti(
          checkboxInput("unknown_as_na", "Treat unknown / ? / NULL as missing", TRUE),
          "Maps common placeholder strings to NA before imputation or modeling."
        ),
        ti(
          checkboxInput("pdays_as_na", "Treat pdays = -1 as missing (bank data)", TRUE),
          "In bank marketing data, -1 means 'not contacted' — treat as missing."
        ),
        ti(
          checkboxInput("coerce_numeric", "Coerce numeric-like text to numeric", TRUE),
          "Columns that look mostly numeric (e.g. currency) are converted to numbers."
        ),
        tags$hr(),
        h5("Missing data"),
        ti(
          sliderInput("missing_thr", "Drop columns with missing rate >", 0.5, 0.99, 0.95, step = 0.01),
          "Columns with more missing than this threshold are removed before imputation."
        ),
        ti(
          selectInput(
            "missing_method",
            "Missing value strategy",
            choices = c(
              "Do nothing",
              "Remove rows with missing values",
              "Median impute numeric columns",
              "Mean impute numeric columns",
              "Mode impute categorical columns"
            ),
            selected = "Median impute numeric columns"
          ),
          "How to fill or drop remaining missing values after sparse columns are removed."
        ),
        tags$hr(),
        h5("Duplicates"),
        ti(
          checkboxInput("remove_dup_rows", "Remove duplicate rows", FALSE),
          "Drop identical rows (all columns match)."
        ),
        ti(
          checkboxInput("handle_dup_ids", "Handle id + position (chart / demo data)", TRUE),
          "For charts with id + position: resolve duplicate IDs and missing IDs."
        ),
        tags$hr(),
        h5("Outliers"),
        uiOutput("outlier_var_ui"),
        ti(
          selectInput(
            "outlier_method",
            "Outlier strategy",
            c("None", "Remove by IQR", "Cap by IQR", "Log transform")
          ),
          "IQR: Q1–1.5×IQR to Q3+1.5×IQR. Log requires non-negative values."
        ),
        tags$hr(),
        h5("Transformations"),
        ti(
          checkboxInput("scale_numeric", "Z-score scale numeric columns", FALSE),
          "Subtract mean and divide by SD for each numeric column (after cleaning)."
        ),
        ti(
          checkboxInput("encode_cats", "Label-encode selected categoricals", FALSE),
          "Convert chosen categories to integers 1..K for modeling."
        ),
        uiOutput("cat_cols_ui"),
        tags$hr(),
        ti(
          actionButton("clean_btn", "Apply cleaning", class = "btn-primary"),
          "Run the full pipeline and refresh downstream tables and plots."
        )
      ),
      mainPanel(
        width = 9,
        h4("Before vs after"),
        tableOutput("cleaning_summary"),
        tags$hr(),
        fluidRow(
          column(6, h5("Cleaned preview"), DTOutput("clean_preview")),
          column(6, h5("Outlier comparison (selected variable)"), plotOutput("outlier_plot", height = "280px"))
        ),
        tags$hr(),
        h5("Missingness after cleaning"),
        DTOutput("cleaned_missing_summary"),
        tags$hr(),
        verbatimTextOutput("clean_summary")
      )
    )
  ),

  tabPanel(
    "3) Feature engineering",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        ti(
          checkboxInput("add_age_group", "Age group (bins) if age column exists", TRUE),
          "Creates age_group from lower bound of age bands (width set below)."
        ),
        conditionalPanel(
          condition = "input.add_age_group == true",
          ti(
            numericInput("age_bin_width", "Age bin width (years)", value = 10, min = 1, max = 80, step = 1),
            "e.g. 10 → bands 0, 10, 20, … Change width and click Apply features to see impact in the preview."
          )
        ),
        ti(
          checkboxInput("add_family_size", "Family size (SibSp + Parch + 1) for Titanic-like data", TRUE),
          "Adds family_size when SibSp and Parch columns exist."
        ),
        ti(
          checkboxInput("add_log_time_price", "log1p(Fare) / log1p(duration) when present", TRUE),
          "Stabilizes skewed fare or call duration with log1p."
        ),
        ti(
          checkboxInput("add_contacted_recent", "contacted_recent from pdays", TRUE),
          "Binary flag: pdays not equal to -1 (bank-style data)."
        ),
        ti(
          checkboxInput("add_campaign_prev_ratio", "campaign / (previous + 1)", FALSE),
          "Ratio of contacts to prior contacts; useful for bank marketing."
        ),
        tags$hr(),
        ti(
          actionButton("feat_btn", "Apply features", class = "btn-primary"),
          "Recompute engineered columns from the cleaned table. Use after changing options above."
        )
      ),
      mainPanel(
        width = 9,
        DTOutput("feat_preview"),
        tags$hr(),
        verbatimTextOutput("feat_summary")
      )
    )
  ),

  tabPanel(
    "4) EDA (interactive)",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput("hit_filter_ui"),
        tags$hr(),
        uiOutput("year_filter_ui"),
        tags$hr(),
        uiOutput("numeric_filter_var_ui"),
        uiOutput("numeric_filter_range_ui"),
        tags$hr(),
        ti(
          radioButtons(
            "plot_type",
            "Plot type",
            choices = c(
              "Histogram" = "hist",
              "Scatter" = "scatter",
              "Boxplot" = "box",
              "Correlation heatmap" = "corr"
            ),
            selected = "hist"
          ),
          "Choose chart type; use variable selectors below. Scatter uses Plotly zoom; correlation needs 2+ columns."
        ),
        ti(
          sliderInput("hist_bins", "Histogram bins", min = 5, max = 80, value = 30),
          "Number of bins for the histogram (only affects histogram plot type)."
        ),
        uiOutput("xvar_ui"),
        uiOutput("yvar_ui"),
        uiOutput("box_group_ui"),
        uiOutput("corr_cols_ui"),
        helpText("Plots use Plotly (zoom/pan/hover). Large scatter plots are sampled to 20k points.")
      ),
      mainPanel(
        width = 9,
        h4("Filtered preview"),
        DTOutput("filtered_preview"),
        tags$hr(),
        plotlyOutput("main_plot", height = "420px"),
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
        ti(
          downloadButton("download_clean", "Download cleaned CSV", class = "btn-success"),
          "CSV of the table after Apply cleaning (last cleaning run)."
        ),
        br(), br(),
        ti(
          downloadButton("download_featured", "Download featured CSV", class = "btn-success"),
          "CSV after feature engineering (same columns as the EDA tab)."
        )
      ),
      column(
        8,
        helpText("Exports use the tables produced after clicking Apply cleaning / Apply features.")
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  app_dir <- getwd()
  data_dir <- file.path(app_dir, "data")

  raw_data <- eventReactive(input$load_btn, {
    df <- if (isTRUE(input$data_source == "upload")) {
      req(input$upload)
      read_uploaded_data(input$upload$datapath, input$upload$name)
    } else {
      ch <- input$builtin_choice
      if (ch == "synthetic") {
        make_synthetic_billboard()
      } else if (ch == "bank") {
        p <- file.path(data_dir, "bank-full.csv")
        shiny::validate(
          shiny::need(file.exists(p), "Missing data/bank-full.csv — add it under the data/ folder.")
        )
        read.csv(p, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ch == "titanic") {
        p <- file.path(data_dir, "titanic.csv")
        shiny::validate(
          shiny::need(file.exists(p), "Missing data/titanic.csv — add it under the data/ folder.")
        )
        read_uploaded_data(p, "titanic.csv")
      } else {
        stop("Unknown built-in dataset")
      }
    }
    df <- as.data.frame(df, check.names = FALSE)
    df[] <- lapply(df, function(x) {
      if (is.character(x)) x[x == "" | trimws(x) == ""] <- NA
      x
    })
    if (isTRUE(input$strings_to_factor)) {
      ic <- sapply(df, is.character)
      df[ic] <- lapply(df[ic], factor)
    }
    df <- normalize_hit_column(df)
    df <- coerce_hit_to_01(df)
    df
  }, ignoreNULL = TRUE)

  output$data_info <- renderTable({
    req(raw_data())
    dataset_info(raw_data())
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$raw_preview <- renderDT({
    req(raw_data())
    datatable(head(raw_data(), 500), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$structure_summary <- renderDT({
    req(raw_data())
    datatable(structure_summary(raw_data()), options = list(pageLength = 15, scrollX = TRUE))
  })

  output$raw_summary <- renderPrint({
    req(raw_data())
    df <- raw_data()
    cat("Dimensions:", nrow(df), "x", ncol(df), "\n\n")
    print(summary(df))
  })

  output$cat_cols_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    if (length(cat_cols) == 0) return(helpText("No categorical columns for encoding."))
    ti(
      checkboxGroupInput(
        "cat_cols_selected",
        "Columns to label-encode",
        choices = cat_cols,
        selected = head(cat_cols, min(5, length(cat_cols)))
      ),
      "Only used when Label-encode is checked. Converts categories to integer codes."
    )
  })

  output$outlier_var_ui <- renderUI({
    req(raw_data())
    num_cols <- names(raw_data())[sapply(raw_data(), is.numeric)]
    if (length(num_cols) == 0) return(helpText("No numeric columns for outlier handling."))
    ti(
      selectInput("outlier_var", "Numeric variable for outliers", choices = num_cols, selected = num_cols[1]),
      "Variable used for IQR/cap/log and for the before/after boxplots."
    )
  })

  cleaned_data <- eventReactive(input$clean_btn, {
    req(raw_data())
    df <- raw_data()
    cats <- if (!is.null(input$cat_cols_selected)) input$cat_cols_selected else character(0)
    ov <- input$outlier_var
    if (length(names(df)[sapply(df, is.numeric)]) == 0) ov <- NULL

    clean_pipeline(
      df,
      unknown_as_na = isTRUE(input$unknown_as_na),
      pdays_as_na = isTRUE(input$pdays_as_na),
      coerce_numeric_flag = isTRUE(input$coerce_numeric),
      missing_thr = input$missing_thr,
      missing_method = input$missing_method,
      remove_dup_rows = isTRUE(input$remove_dup_rows),
      handle_dup_ids = isTRUE(input$handle_dup_ids),
      outlier_var = ov,
      outlier_method = input$outlier_method,
      scale_numeric_flag = isTRUE(input$scale_numeric),
      encode_cats = isTRUE(input$encode_cats),
      categorical_cols = cats
    )
  }, ignoreNULL = TRUE)

  output$cleaning_summary <- renderTable({
    req(raw_data(), cleaned_data())
    b <- raw_data()
    a <- cleaned_data()
    data.frame(
      Metric = c("Rows", "Columns", "Total NA", "Duplicate rows"),
      Before = c(nrow(b), ncol(b), sum(is.na(b)), sum(duplicated(b))),
      After = c(nrow(a), ncol(a), sum(is.na(a)), sum(duplicated(a))),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE)

  output$clean_preview <- renderDT({
    req(cleaned_data())
    datatable(head(cleaned_data(), 500), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$cleaned_missing_summary <- renderDT({
    req(cleaned_data())
    datatable(structure_summary(cleaned_data()), options = list(pageLength = 12, scrollX = TRUE))
  })

  output$outlier_plot <- renderPlot({
    req(raw_data(), cleaned_data(), input$outlier_var)
    shiny::validate(shiny::need(input$outlier_var %in% names(raw_data()), "Invalid variable"))
    shiny::validate(shiny::need(is.numeric(raw_data()[[input$outlier_var]]), "Not numeric"))
    par(mfrow = c(1, 2))
    boxplot(raw_data()[[input$outlier_var]], main = "Before", ylab = input$outlier_var)
    if (input$outlier_var %in% names(cleaned_data()) && is.numeric(cleaned_data()[[input$outlier_var]])) {
      boxplot(cleaned_data()[[input$outlier_var]], main = "After", ylab = input$outlier_var)
    } else {
      plot.new()
      text(0.5, 0.5, "Variable dropped or non-numeric after cleaning")
    }
  })

  output$clean_summary <- renderPrint({
    req(cleaned_data())
    cat("Cleaning applied.\n")
    cat("Rows:", nrow(cleaned_data()), " Columns:", ncol(cleaned_data()), "\n")
  })

  # Recompute engineered features whenever cleaning or "Apply features" is clicked
  featured_data <- eventReactive(c(input$clean_btn, input$feat_btn), {
    req(cleaned_data())
    add_features(
      cleaned_data(),
      add_age_group = isTRUE(input$add_age_group),
      age_bin_width = if (is.null(input$age_bin_width)) 10 else input$age_bin_width,
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
    cat("Featured data: ", nrow(df), " rows x ", ncol(df), " cols\n", sep = "")
    added <- c("age_group", "family_size", "log_fare", "log_duration", "contacted_recent", "campaign_prev_ratio")
    cat("New columns present:", paste(intersect(added, names(df)), collapse = ", "), "\n")
    if (isTRUE(input$add_age_group) && "age_group" %in% names(df)) {
      w <- if (is.null(input$age_bin_width)) 10 else input$age_bin_width
      cat("Age bin width used:", w, "years\n")
    }
  })

  numeric_vars <- reactive({
    req(featured_data())
    names(featured_data())[sapply(featured_data(), is.numeric)]
  })

  categorical_vars <- reactive({
    req(featured_data())
    names(featured_data())[sapply(featured_data(), function(x) is.character(x) || is.factor(x))]
  })

  output$hit_filter_ui <- renderUI({
    req(featured_data())
    df <- featured_data()
    if (!("hit" %in% names(df))) {
      return(helpText("No binary `hit` column — boxplot uses another grouping variable below."))
    }
    uh <- sort(unique(stats::na.omit(suppressWarnings(as.numeric(as.character(df$hit))))))
    if (length(uh) == 0) {
      return(helpText("Column `hit` has no non-NA values to filter."))
    }
    ti(
      selectInput(
        "hit_filter",
        "hit filter (values present in data)",
        choices = uh,
        selected = uh,
        multiple = TRUE
      ),
      "Subset rows by outcome class. Leave both selected for all rows; empty selection shows all (no filter)."
    )
  })

  output$year_filter_ui <- renderUI({
    req(featured_data())
    df <- featured_data()
    if (!("year" %in% names(df)) || !is.numeric(df$year)) {
      return(helpText("No numeric `year` column — year filter hidden."))
    }
    yr <- df$year[!is.na(df$year)]
    shiny::validate(shiny::need(length(unique(yr)) > 1, "Year filter unavailable."))
    ti(
      sliderInput("year_range", "Year range", min = min(yr), max = max(yr), value = c(min(yr), max(yr))),
      "Restrict EDA to a range of years (e.g. Billboard-style data)."
    )
  })

  output$numeric_filter_var_ui <- renderUI({
    req(featured_data())
    df <- featured_data()
    vars <- names(df)[sapply(df, is.numeric)]
    vars <- setdiff(vars, c("hit", "year"))
    if (length(vars) == 0) return(helpText("No extra numeric filter."))
    ti(
      selectInput("numeric_filter_var", "Numeric filter variable", choices = vars, selected = vars[1]),
      "Choose which numeric column drives the range slider below."
    )
  })

  output$numeric_filter_range_ui <- renderUI({
    req(featured_data(), input$numeric_filter_var)
    df <- featured_data()
    var <- input$numeric_filter_var
    shiny::validate(shiny::need(var %in% names(df), "Bad filter var"))
    x <- df[[var]][!is.na(df[[var]])]
    shiny::validate(shiny::need(length(unique(x)) > 1, "Not enough values"))
    rng <- as.numeric(stats::quantile(x, c(0.05, 0.95), na.rm = TRUE))
    ti(
      sliderInput(
        "numeric_filter_range",
        "Numeric range",
        min = min(x),
        max = max(x),
        value = rng,
        step = max(diff(range(x)) / 200, .Machine$double.eps)
      ),
      "Keep rows where this variable falls in the interval. NAs in that column are kept."
    )
  })

  output$xvar_ui <- renderUI({
    vars <- numeric_vars()
    if (length(vars) == 0) return(helpText("No numeric columns to plot."))
    ti(
      selectInput("xvar", "X / numeric variable", choices = vars, selected = vars[1]),
      "Horizontal axis for scatter; numeric column for histogram and boxplot."
    )
  })

  output$yvar_ui <- renderUI({
    vars <- numeric_vars()
    if (length(vars) < 1) return(NULL)
    if (identical(input$plot_type, "scatter")) {
      sel <- if (length(vars) >= 2) vars[2] else vars[1]
      ti(
        selectInput("yvar", "Y variable", choices = vars, selected = sel),
        "Vertical axis for scatter plot (with optional regression line)."
      )
    } else NULL
  })

  output$box_group_ui <- renderUI({
    req(featured_data())
    if (!identical(input$plot_type, "box")) return(NULL)
    df <- featured_data()
    if ("hit" %in% names(df)) return(NULL)
    cv <- categorical_vars()
    cv <- setdiff(cv, names(df)[sapply(df, function(x) length(unique(stats::na.omit(x))) > 50)])
    if (length(cv) == 0) {
      return(helpText("No suitable categorical column for boxplot grouping."))
    }
    ti(
      selectInput("box_group", "Group by (categorical)", choices = cv, selected = cv[1]),
      "Used when no `hit` column: boxplots of X by this category."
    )
  })

  output$corr_cols_ui <- renderUI({
    if (!identical(input$plot_type, "corr")) return(NULL)
    vars <- numeric_vars()
    if (length(vars) < 2) return(helpText("Need ≥2 numeric columns for correlation."))
    sel <- vars[seq_len(min(length(vars), 12))]
    ti(
      selectInput("corr_cols", "Numeric columns", choices = vars, selected = sel, multiple = TRUE),
      "Pick 2+ variables for Pearson correlation heatmap. Too many columns can look crowded."
    )
  })

  filtered <- reactive({
    req(featured_data())
    df <- featured_data()

    # Multi-select with nothing chosen => hf is length 0 => %in% matches no rows (0 data bug)
    if ("hit" %in% names(df) && length(input$hit_filter) > 0) {
      hf <- suppressWarnings(as.numeric(input$hit_filter))
      hf <- hf[!is.na(hf)]
      if (length(hf) > 0) {
        df <- df %>%
          dplyr::mutate(.hitn = suppressWarnings(as.numeric(as.character(hit)))) %>%
          dplyr::filter(.hitn %in% hf | is.na(.hitn)) %>%
          dplyr::select(-.hitn)
      }
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
      v <- input$numeric_filter_var
      # Keep NA outside the slider range so one bad column cannot zero the table
      df <- df %>%
        filter(
          is.na(.data[[v]]) |
            (.data[[v]] >= rng[1] & .data[[v]] <= rng[2])
        )
    }

    df
  })

  output$filtered_preview <- renderDT({
    req(filtered())
    datatable(head(filtered(), 500), options = list(pageLength = 8, scrollX = TRUE))
  })

  output$main_plot <- renderPlotly({
    req(filtered())
    df <- filtered()
    pt <- input$plot_type
    shiny::validate(shiny::need(pt %in% c("hist", "scatter", "box", "corr"), "Select plot type"))

    if (pt == "hist") {
      req(input$xvar)
      shiny::validate(shiny::need(input$xvar %in% names(df), "Choose X"))
      bins <- input$hist_bins
      p <- ggplot(df, aes(x = .data[[input$xvar]])) +
        geom_histogram(fill = "#2C7FB8", color = "white", bins = bins) +
        theme_minimal() +
        labs(title = paste("Histogram:", input$xvar), x = input$xvar, y = "Count")
      ggplotly(p, source = "eda") %>% layout(hovermode = "closest")
    } else if (pt == "scatter") {
      req(input$xvar, input$yvar)
      shiny::validate(shiny::need(nrow(df) >= 2, "Select a filter with at least 2 rows."))
      plot_df <- df
      if (nrow(plot_df) > 20000) plot_df <- dplyr::slice_sample(plot_df, n = 20000)
      p <- ggplot(plot_df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
        geom_point(alpha = 0.3, size = 0.6) +
        theme_minimal() +
        labs(title = paste(input$xvar, "vs", input$yvar))
      if (nrow(plot_df) >= 10) {
        p <- p + geom_smooth(method = "lm", se = TRUE, color = "firebrick", linewidth = 0.6)
      }
      ggplotly(p) %>% layout(hovermode = "closest")
    } else if (pt == "box") {
      req(input$xvar)
      shiny::validate(shiny::need(input$xvar %in% names(df), "Choose numeric variable"))
      if ("hit" %in% names(df)) {
        p <- ggplot(df, aes(x = factor(hit), y = .data[[input$xvar]])) +
          geom_boxplot(fill = "#7FC8A9") +
          theme_minimal() +
          labs(title = paste(input$xvar, "by hit"), x = "hit")
      } else {
        req(input$box_group)
        shiny::validate(shiny::need(input$box_group %in% names(df), "Group column"))
        p <- ggplot(df, aes(x = factor(.data[[input$box_group]]), y = .data[[input$xvar]])) +
          geom_boxplot(fill = "#7FC8A9") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
          labs(title = paste(input$xvar, "by", input$box_group), x = input$box_group)
      }
      ggplotly(p)
    } else if (pt == "corr") {
      req(input$corr_cols)
      shiny::validate(shiny::need(length(input$corr_cols) >= 2, "Pick at least 2 columns"))
      cols <- input$corr_cols[input$corr_cols %in% names(df)]
      shiny::validate(shiny::need(length(cols) >= 2, "Columns missing"))
      cm <- stats::cor(df[, cols, drop = FALSE], use = "pairwise.complete.obs")
      cl <- as.data.frame(as.table(cm))
      colnames(cl) <- c("Var1", "Var2", "Corr")
      cl$Var1 <- factor(cl$Var1, levels = cols)
      cl$Var2 <- factor(cl$Var2, levels = cols)
      p <- ggplot(cl, aes(x = Var1, y = Var2, fill = Corr)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = muted("blue"), high = muted("red"), mid = "white", midpoint = 0, limits = c(-1, 1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Correlation matrix", x = NULL, y = NULL)
      ggplotly(p)
    }
  })

  output$eda_stats <- renderPrint({
    req(filtered())
    df <- filtered()
    cat("Filtered rows:", nrow(df), "\n\n")
    if ("hit" %in% names(df)) {
      cat("hit table:\n")
      print(table(df$hit, useNA = "ifany"))
      cat("\n")
    }
    pt <- input$plot_type
    if (pt %in% c("hist", "box") && !is.null(input$xvar) && input$xvar %in% names(df)) {
      cat("Summary:", input$xvar, "\n")
      print(summary(df[[input$xvar]]))
    }
    if (pt == "scatter" && !is.null(input$xvar) && !is.null(input$yvar) &&
        input$xvar %in% names(df) && input$yvar %in% names(df)) {
      cat("\nPearson r:\n")
      print(stats::cor(df[[input$xvar]], df[[input$yvar]], use = "pairwise.complete.obs"))
    }
  })

  output$download_clean <- downloadHandler(
    filename = function() paste0("cleaned_", Sys.Date(), ".csv"),
    content = function(file) {
      req(cleaned_data())
      utils::write.csv(cleaned_data(), file, row.names = FALSE)
    }
  )

  output$download_featured <- downloadHandler(
    filename = function() paste0("featured_", Sys.Date(), ".csv"),
    content = function(file) {
      req(featured_data())
      utils::write.csv(featured_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
