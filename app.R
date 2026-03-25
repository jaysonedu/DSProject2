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

read_json_as_data_frame <- function(path) {
  # Read text explicitly (avoids fromJSON mis-reading temp paths on some platforms/versions).
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  txt <- paste(lines, collapse = "\n")
  if (!nzchar(trimws(txt))) {
    stop("JSON file is empty.", call. = FALSE)
  }
  if (startsWith(txt, "\ufeff")) {
    txt <- sub("^\ufeff", "", txt)
  }
  normalize_parsed_json <- function(out, depth = 0L) {
    if (depth > 6L) {
      stop("JSON is too deeply nested to flatten into one table.", call. = FALSE)
    }
    if (is.data.frame(out)) {
      return(as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE))
    }
    if (is.matrix(out)) {
      return(as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE))
    }
    if (is.atomic(out) && !is.null(dim(out))) {
      return(as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE))
    }
    if (is.atomic(out)) {
      return(data.frame(value = out, check.names = FALSE, stringsAsFactors = FALSE))
    }
    if (!is.list(out)) {
      stop("Unsupported JSON root type for tabular import.", call. = FALSE)
    }
    if (length(out) == 0L) {
      return(as.data.frame(list()))
    }
    # {"data":[...]} / single wrapper
    if (length(out) == 1L) {
      inner <- out[[1L]]
      if (is.data.frame(inner)) {
        return(as.data.frame(inner, check.names = FALSE, stringsAsFactors = FALSE))
      }
      if (is.matrix(inner)) {
        return(as.data.frame(inner, check.names = FALSE, stringsAsFactors = FALSE))
      }
      return(normalize_parsed_json(inner, depth + 1L))
    }
    # {"col":[...], ...} same-length columns
    lens <- vapply(out, function(x) {
      if (is.null(x)) return(0L)
      if (is.atomic(x)) return(length(x))
      NA_integer_
    }, integer(1))
    if (!anyNA(lens) && length(unique(lens)) == 1L && lens[1L] > 0L) {
      return(as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE))
    }
    br <- tryCatch(dplyr::bind_rows(out), error = function(e) NULL)
    if (!is.null(br) && ncol(br) > 0L) {
      return(as.data.frame(br, check.names = FALSE, stringsAsFactors = FALSE))
    }
    stop("Unsupported JSON layout. Use an array of objects, column arrays, or JSON Lines.", call. = FALSE)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = TRUE),
    error = function(e) e
  )
  if (!inherits(parsed, "error")) {
    return(normalize_parsed_json(parsed))
  }
  # JSON Lines: one JSON object per line
  nonempty <- nzchar(trimws(lines))
  lines <- lines[nonempty]
  if (length(lines) < 1L) {
    stop("Could not parse JSON: ", conditionMessage(parsed), call. = FALSE)
  }
  rows <- vector("list", length(lines))
  ok <- TRUE
  for (i in seq_along(lines)) {
    row <- tryCatch(
      jsonlite::fromJSON(trimws(lines[i]), simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(row)) {
      ok <- FALSE
      break
    }
    rows[[i]] <- row
  }
  if (!ok) {
    stop("Could not parse JSON: ", conditionMessage(parsed), call. = FALSE)
  }
  br <- tryCatch(dplyr::bind_rows(rows), error = function(e) NULL)
  if (is.null(br) || ncol(br) == 0L) {
    stop("JSON Lines parsed but rows could not be combined into a table.", call. = FALSE)
  }
  as.data.frame(br, check.names = FALSE, stringsAsFactors = FALSE)
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
    return(read_json_as_data_frame(path))
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

append_post_pipeline_features <- function(
    df,
    add_family_size = FALSE,
    add_log_time_price = FALSE,
    add_campaign_prev_ratio = FALSE
) {
  df <- as.data.frame(df, check.names = FALSE)

  get_first_existing <- function(candidates) {
    hits <- candidates[candidates %in% names(df)]
    if (length(hits) == 0) return(NULL)
    hits[1]
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

  campaign_col <- get_first_existing(c("campaign", "Campaign"))
  previous_col <- get_first_existing(c("previous", "Previous"))
  if (isTRUE(add_campaign_prev_ratio) && !is.null(campaign_col) && !is.null(previous_col) &&
      is.numeric(df[[campaign_col]]) && is.numeric(df[[previous_col]])) {
    df$campaign_prev_ratio <- df[[campaign_col]] / (df[[previous_col]] + 1)
  }

  df
}

# Python EDA.py–style generic feature engineering (column pickers in UI)
feature_engineering_pipeline <- function(
    df,
    create_date_parts = FALSE,
    date_var = NULL,
    create_age_group = FALSE,
    age_var = NULL,
    create_balance_level = FALSE,
    balance_var = NULL,
    create_contacted_before = FALSE,
    pdays_var = NULL,
    create_log_feature = FALSE,
    log_var = NULL,
    create_sqrt_feature = FALSE,
    sqrt_var = NULL,
    create_square_feature = FALSE,
    square_var = NULL,
    create_ratio_feature = FALSE,
    ratio_num = NULL,
    ratio_den = NULL,
    create_sum_feature = FALSE,
    sum_var1 = NULL,
    sum_var2 = NULL,
    create_diff_feature = FALSE,
    diff_var1 = NULL,
    diff_var2 = NULL,
    create_product_feature = FALSE,
    prod_var1 = NULL,
    prod_var2 = NULL,
    create_indicator_feature = FALSE,
    indicator_var = NULL,
    indicator_threshold = NULL,
    create_missing_indicator = FALSE,
    missing_var = NULL,
    create_frequency_feature = FALSE,
    freq_var = NULL,
    create_group_mean_feature = FALSE,
    group_var = NULL,
    target_var = NULL,
    create_rank_feature = FALSE,
    rank_var = NULL,
    create_text_length = FALSE,
    text_var = NULL
) {
  df <- as.data.frame(df, check.names = FALSE)

  has_num <- function(nm) !is.null(nm) && nm %in% names(df) && is.numeric(df[[nm]])
  has_col <- function(nm) !is.null(nm) && nm %in% names(df)

  if (isTRUE(create_date_parts) && has_col(date_var)) {
    dt_raw <- suppressWarnings(as.POSIXlt(as.character(df[[date_var]]), tz = "UTC"))
    df[[paste0(date_var, "_year")]] <- dt_raw$year + 1900L
    df[[paste0(date_var, "_month")]] <- dt_raw$mon + 1L
    df[[paste0(date_var, "_day")]] <- dt_raw$mday
    d_date <- as.Date(dt_raw)
    df[[paste0(date_var, "_weekday")]] <- weekdays(d_date)
    mon <- dt_raw$mon + 1L
    df[[paste0(date_var, "_quarter")]] <- as.integer((mon - 1L) %/% 3L + 1L)
    wday <- dt_raw$wday
    df[[paste0(date_var, "_is_weekend")]] <- as.numeric(wday %in% c(0L, 6L))
  }

  if (isTRUE(create_age_group) && has_num(age_var)) {
    ag <- df[[age_var]]
    df$age_group <- cut(
      ag,
      breaks = c(-Inf, 25, 40, 60, Inf),
      labels = c("Young", "Adult", "Middle", "Senior"),
      include.lowest = TRUE,
      right = TRUE
    )
  }

  if (isTRUE(create_balance_level) && has_num(balance_var)) {
    x <- df[[balance_var]][!is.na(df[[balance_var]])]
    if (length(x) >= 3 && length(unique(x)) >= 3) {
      q1 <- stats::quantile(df[[balance_var]], 0.33, na.rm = TRUE)
      q2 <- stats::quantile(df[[balance_var]], 0.67, na.rm = TRUE)
      if (!is.na(q1) && !is.na(q2) && q1 < q2) {
        df$balance_level <- cut(
          df[[balance_var]],
          breaks = c(-Inf, q1, q2, Inf),
          labels = c("Low", "Medium", "High"),
          include.lowest = TRUE
        )
      }
    }
  }

  if (isTRUE(create_contacted_before) && has_num(pdays_var)) {
    p <- df[[pdays_var]]
    p[is.na(p)] <- -1
    df$contacted_before <- ifelse(p >= 0, "Yes", "No")
  }

  if (isTRUE(create_log_feature) && has_num(log_var)) {
    x <- df[[log_var]]
    if (length(x[!is.na(x)]) > 0 && all(x[!is.na(x)] >= 0)) {
      df[[paste0(log_var, "_log")]] <- log1p(x)
    }
  }

  if (isTRUE(create_sqrt_feature) && has_num(sqrt_var)) {
    x <- df[[sqrt_var]]
    if (length(x[!is.na(x)]) > 0 && all(x[!is.na(x)] >= 0)) {
      df[[paste0(sqrt_var, "_sqrt")]] <- sqrt(x)
    }
  }

  if (isTRUE(create_square_feature) && has_num(square_var)) {
    df[[paste0(square_var, "_sq")]] <- df[[square_var]]^2
  }

  if (isTRUE(create_ratio_feature) && has_num(ratio_num) && has_num(ratio_den)) {
    den <- df[[ratio_den]]
    den[den == 0 & !is.na(den)] <- NA
    df[[paste0(ratio_num, "_to_", ratio_den, "_ratio")]] <- df[[ratio_num]] / den
  }

  if (isTRUE(create_sum_feature) && has_num(sum_var1) && has_num(sum_var2)) {
    df[[paste0(sum_var1, "_plus_", sum_var2)]] <- df[[sum_var1]] + df[[sum_var2]]
  }

  if (isTRUE(create_diff_feature) && has_num(diff_var1) && has_num(diff_var2)) {
    df[[paste0(diff_var1, "_minus_", diff_var2)]] <- df[[diff_var1]] - df[[diff_var2]]
  }

  if (isTRUE(create_product_feature) && has_num(prod_var1) && has_num(prod_var2)) {
    df[[paste0(prod_var1, "_x_", prod_var2)]] <- df[[prod_var1]] * df[[prod_var2]]
  }

  if (isTRUE(create_indicator_feature) && has_num(indicator_var) && !is.null(indicator_threshold)) {
    thr <- suppressWarnings(as.numeric(indicator_threshold))
    if (!is.na(thr)) {
      safe_thr <- gsub(".", "_", as.character(thr), fixed = TRUE)
      nm <- paste0(indicator_var, "_gt_", safe_thr)
      df[[nm]] <- as.integer(!is.na(df[[indicator_var]]) & df[[indicator_var]] > thr)
    }
  }

  if (isTRUE(create_missing_indicator) && has_col(missing_var)) {
    df[[paste0(missing_var, "_missing")]] <- as.integer(is.na(df[[missing_var]]))
  }

  if (isTRUE(create_frequency_feature) && has_col(freq_var)) {
    fv <- df[[freq_var]]
    ux <- unique(fv)
    cnt <- vapply(ux, FUN = function(u) {
      sum(fv == u | (is.na(fv) & is.na(u)))
    }, FUN.VALUE = numeric(1))
    df[[paste0(freq_var, "_freq")]] <- cnt[match(fv, ux)]
  }

  if (isTRUE(create_group_mean_feature) && has_col(group_var) && has_num(target_var)) {
    nm <- paste0(target_var, "_mean_by_", group_var)
    g <- df[[group_var]]
    t <- df[[target_var]]
    df[[nm]] <- ave(t, g, FUN = function(z) mean(z, na.rm = TRUE))
  }

  if (isTRUE(create_rank_feature) && has_num(rank_var)) {
    df[[paste0(rank_var, "_rank")]] <- rank(df[[rank_var]], ties.method = "average", na.last = "keep")
  }

  if (isTRUE(create_text_length) && has_col(text_var)) {
    df[[paste0(text_var, "_length")]] <- nchar(as.character(df[[text_var]]), type = "chars", allowNA = TRUE)
  }

  as.data.frame(df, check.names = FALSE)
}

apply_one_click_transform <- function(df, col, method) {
  df <- as.data.frame(df, check.names = FALSE)
  if (!col %in% names(df)) return(df)
  method <- match.arg(method, c(
    "log", "sqrt", "square", "rank", "missing", "text_length", "frequency", "zscore"
  ))
  switch(method,
    log = {
      if (!is.numeric(df[[col]])) return(df)
      x <- df[[col]]
      ok <- stats::na.omit(x)
      if (length(ok) && all(ok >= 0)) df[[paste0(col, "_log")]] <- log1p(x)
    },
    sqrt = {
      if (!is.numeric(df[[col]])) return(df)
      x <- df[[col]]
      ok <- stats::na.omit(x)
      if (length(ok) && all(ok >= 0)) df[[paste0(col, "_sqrt")]] <- sqrt(x)
    },
    square = {
      if (!is.numeric(df[[col]])) return(df)
      df[[paste0(col, "_sq")]] <- df[[col]]^2
    },
    rank = {
      if (!is.numeric(df[[col]])) return(df)
      df[[paste0(col, "_rank")]] <- rank(df[[col]], ties.method = "average", na.last = "keep")
    },
    missing = {
      df[[paste0(col, "_missing")]] <- as.integer(is.na(df[[col]]))
    },
    text_length = {
      df[[paste0(col, "_length")]] <- nchar(as.character(df[[col]]), type = "chars", allowNA = TRUE)
    },
    frequency = {
      fv <- df[[col]]
      ux <- unique(fv)
      cnt <- vapply(ux, function(u) sum(fv == u | (is.na(fv) & is.na(u))), numeric(1))
      df[[paste0(col, "_freq")]] <- cnt[match(fv, ux)]
    },
    zscore = {
      if (!is.numeric(df[[col]])) return(df)
      x <- df[[col]]
      s <- stats::sd(x, na.rm = TRUE)
      if (!is.na(s) && s > 0) df[[paste0(col, "_z")]] <- as.numeric(scale(x))
    }
  )
  as.data.frame(df, check.names = FALSE)
}

extract_datetime_selected <- function(df, col, year, month, day, weekday, quarter) {
  df <- as.data.frame(df, check.names = FALSE)
  if (!col %in% names(df)) return(df)
  if (!isTRUE(year) && !isTRUE(month) && !isTRUE(day) && !isTRUE(weekday) && !isTRUE(quarter)) return(df)
  dt_raw <- suppressWarnings(as.POSIXlt(as.character(df[[col]]), tz = "UTC"))
  if (all(is.na(as.POSIXct(dt_raw)))) return(df)
  d_date <- as.Date(dt_raw)
  mon <- dt_raw$mon + 1L
  if (isTRUE(year)) df[[paste0(col, "_year")]] <- dt_raw$year + 1900L
  if (isTRUE(month)) df[[paste0(col, "_month")]] <- mon
  if (isTRUE(day)) df[[paste0(col, "_day")]] <- dt_raw$mday
  if (isTRUE(weekday)) df[[paste0(col, "_weekday")]] <- weekdays(d_date)
  if (isTRUE(quarter)) df[[paste0(col, "_quarter")]] <- as.integer((mon - 1L) %/% 3L + 1L)
  as.data.frame(df, check.names = FALSE)
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

# EDA.py-style missing table (Variable, Missing_Count, Missing_Percent only)
eda_missing_summary_df <- function(df) {
  data.frame(
    Variable = names(df),
    Missing_Count = sapply(df, function(x) sum(is.na(x))),
    Missing_Percent = paste0(round(sapply(df, function(x) mean(is.na(x)) * 100), 2), "%"),
    stringsAsFactors = FALSE
  )
}

# EDA.py-style numeric summary (pandas describe().T)
eda_numeric_describe_df <- function(df) {
  nm <- names(df)[sapply(df, is.numeric)]
  if (length(nm) == 0) {
    return(data.frame(Message = "No numeric columns in filtered data.", stringsAsFactors = FALSE))
  }
  sm <- do.call(rbind, lapply(nm, function(col) {
    x <- df[[col]]
    x <- x[!is.na(x)]
    if (length(x) == 0L) {
      return(data.frame(
        Variable = col, count = 0L, mean = NA_real_, std = NA_real_, min = NA_real_,
        `25%` = NA_real_, `50%` = NA_real_, `75%` = NA_real_, max = NA_real_,
        check.names = FALSE, stringsAsFactors = FALSE
      ))
    }
    qs <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE, na.rm = TRUE)
    data.frame(
      Variable = col,
      count = length(x),
      mean = mean(x),
      std = stats::sd(x),
      min = min(x),
      `25%` = qs[1L],
      `50%` = qs[2L],
      `75%` = qs[3L],
      max = max(x),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
  rownames(sm) <- NULL
  sm
}

apply_eda_filters <- function(df, inp) {
  if ("hit" %in% names(df) && !is.null(inp$hit_filter) && length(inp$hit_filter) > 0) {
    hf <- suppressWarnings(as.numeric(inp$hit_filter))
    hf <- hf[!is.na(hf)]
    if (length(hf) > 0) {
      df <- df %>%
        dplyr::mutate(.hitn = suppressWarnings(as.numeric(as.character(hit)))) %>%
        dplyr::filter(.hitn %in% hf | is.na(.hitn)) %>%
        dplyr::select(-.hitn)
    }
  }
  if ("year" %in% names(df) && !is.null(inp$year_range) && is.numeric(df$year)) {
    df <- df %>% dplyr::filter(year >= inp$year_range[1], year <= inp$year_range[2])
  }
  nv <- inp$numeric_filter_var
  if (
    !is.null(nv) && !identical(nv, "None") && nv %in% names(df) &&
      !is.null(inp$numeric_filter_range) && is.numeric(df[[nv]])
  ) {
    rng <- inp$numeric_filter_range
    df <- df %>%
      dplyr::filter(
        is.na(.data[[nv]]) |
          (.data[[nv]] >= rng[1] & .data[[nv]] <= rng[2])
      )
  }
  if (
    !is.null(inp$eda_cat_var) && !identical(inp$eda_cat_var, "None") &&
      inp$eda_cat_var %in% names(df) &&
      !is.null(inp$eda_cat_val) && !identical(inp$eda_cat_val, "All")
  ) {
    df <- df %>% dplyr::filter(as.character(.data[[inp$eda_cat_var]]) == inp$eda_cat_val)
  }
  df
}

# Noir chic — black & white monochrome (bslib)
app_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  primary = "#fafafa",
  secondary = "#737373",
  success = "#d4d4d4",
  info = "#a3a3a3",
  warning = "#a3a3a3",
  danger = "#dc2626",
  `enable-rounded` = TRUE,
  `enable-shadows` = TRUE
)

app_head <- tags$head(
  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
  tags$meta(name = "theme-color", content = "#050505"),
  tags$link(
    href = "https://fonts.googleapis.com/css2?family=Cormorant+Garamond:ital,wght@0,500;0,600;0,700;1,500&family=DM+Sans:ital,wght@0,400;0,500;0,600;0,700;1,400&family=IBM+Plex+Mono:wght@400;600&display=swap",
    rel = "stylesheet"
  ),
  tags$style(HTML("
    body {
      font-family: 'DM Sans', system-ui, sans-serif !important;
      background: #050505 !important;
      color: #e5e5e5 !important;
    }
    code, .shiny-text-output, pre { font-family: 'IBM Plex Mono', monospace !important; }
    .navbar {
      padding-top: 0.65rem !important; padding-bottom: 0.65rem !important;
      background: #000000 !important;
      box-shadow: 0 1px 0 rgba(255,255,255,.12), 0 12px 40px rgba(0,0,0,.6);
      border-bottom: 1px solid rgba(255,255,255,.14) !important;
    }
    .navbar-brand {
      font-family: 'Cormorant Garamond', 'Times New Roman', serif !important;
      font-weight: 600 !important;
      font-size: 1.35rem !important;
      letter-spacing: 0.04em;
    }
    .brand-mark { color: #ffffff; margin-right: .45rem; font-size: 0.9rem; opacity: .9; }
    .brand-title { color: #fafafa !important; font-weight: 600; letter-spacing: 0.06em; }
    .brand-subtitle {
      font-family: 'DM Sans', sans-serif !important;
      font-size: .72rem !important; font-weight: 500;
      letter-spacing: .2em; text-transform: uppercase;
      color: rgba(255,255,255,.55) !important; margin-left: .75rem;
    }
    .navbar-nav .nav-link {
      font-family: 'DM Sans', sans-serif !important; font-weight: 500 !important;
      font-size: .88rem !important; letter-spacing: .03em;
      color: rgba(255,255,255,.75) !important;
      border-radius: .35rem !important; margin: 0 .1rem !important;
      padding: .45rem .9rem !important; white-space: nowrap;
      transition: color .15s, background .15s !important;
    }
    .navbar-nav .nav-link:hover {
      color: #ffffff !important; background: rgba(255,255,255,.08) !important;
    }
    .navbar-nav .nav-link.active {
      color: #000000 !important; background: #fafafa !important; font-weight: 600 !important;
    }
    .hero-panel {
      background: linear-gradient(160deg, #121212 0%, #050505 55%, #0a0a0a 100%);
      border: 1px solid rgba(255,255,255,.1); border-radius: 1rem;
      padding: 2rem 2.25rem; margin-bottom: 1.5rem;
      box-shadow: 0 24px 60px rgba(0,0,0,.65);
    }
    .hero-panel h1 {
      font-family: 'Cormorant Garamond', serif !important;
      color: #fafafa; font-weight: 600; letter-spacing: 0.02em; font-size: 2.15rem;
    }
    .hero-lead { color: #a3a3a3; font-size: 1.05rem; max-width: 40rem; line-height: 1.55; }
    .noir-badge {
      border: 1px solid rgba(255,255,255,.22) !important; color: #e5e5e5 !important;
      background: rgba(255,255,255,.05) !important; font-size: .72rem !important;
      letter-spacing: .1em; text-transform: uppercase; font-weight: 600;
    }
    .wow-card {
      background: #101010 !important; border: 1px solid rgba(255,255,255,.1) !important;
      border-radius: .75rem !important; box-shadow: 0 16px 48px rgba(0,0,0,.45) !important;
      transition: transform .16s ease, border-color .16s ease;
    }
    .wow-card:hover {
      transform: translateY(-2px); border-color: rgba(255,255,255,.22) !important;
    }
    .wow-card .card-header {
      background: rgba(255,255,255,.04) !important;
      border-bottom: 1px solid rgba(255,255,255,.1) !important;
      font-weight: 600; color: #fafafa !important; font-size: .95rem; letter-spacing: .04em;
    }
    .wow-card .card-body { color: #d4d4d4; }
    .well, .shiny-input-container { margin-bottom: .85rem; }
    div[class*='col-sm'] > .well {
      background: #0c0c0c !important; border: 1px solid rgba(255,255,255,.1) !important;
      border-radius: .75rem !important; padding: 1.2rem !important;
    }
    .btn-primary {
      font-weight: 600 !important; letter-spacing: .12em; text-transform: uppercase;
      font-size: .72rem !important; padding: .55rem 1.15rem !important; border-radius: .35rem !important;
      background: #fafafa !important; color: #050505 !important; border: 1px solid #fafafa !important;
      box-shadow: none !important;
    }
    .btn-primary:hover {
      background: #e5e5e5 !important; color: #000 !important; border-color: #e5e5e5 !important;
    }
    .btn-success {
      font-weight: 600 !important; letter-spacing: .1em; text-transform: uppercase;
      font-size: .72rem !important;
      background: transparent !important; color: #fafafa !important;
      border: 1px solid rgba(255,255,255,.45) !important; box-shadow: none !important;
    }
    .btn-success:hover {
      background: rgba(255,255,255,.1) !important; color: #fff !important; border-color: #fff !important;
    }
    h4, h5, h6, .control-label { color: #f5f5f5 !important; }
    .help-block, .text-muted { color: #737373 !important; }
    .feature-sidebar { max-height: 88vh; overflow-y: auto; padding-right: .35rem; }
    .feature-sidebar::-webkit-scrollbar { width: 5px; }
    .feature-sidebar::-webkit-scrollbar-thumb { background: rgba(255,255,255,.25); border-radius: 4px; }
    table.dataTable { color: #e5e5e5 !important; }
    table.dataTable thead th {
      background: rgba(255,255,255,.06) !important; color: #fafafa !important;
      border-bottom: 1px solid rgba(255,255,255,.12) !important;
    }
    table.dataTable tbody td { border-color: rgba(255,255,255,.08) !important; }
    .export-zone {
      background: #0a0a0a; border-radius: 1rem; border: 1px solid rgba(255,255,255,.12);
      padding: 2rem; box-shadow: 0 20px 60px rgba(0,0,0,.5);
    }
    .section-pill {
      font-size: 0.62rem; text-transform: uppercase; letter-spacing: .16em; font-weight: 600;
      margin: 0.35rem 0 0.15rem 0; color: rgba(255,255,255,.45) !important;
    }
    /* One card per table — spacing + horizontal scroll so tables never overlap */
    .table-slot-card { margin-bottom: 1.35rem !important; }
    .table-slot-card .card-body {
      overflow-x: auto;
      overflow-y: visible;
      padding-bottom: 1rem;
    }
    .table-slot-card .dataTables_wrapper { width: 100% !important; }
    .main-panel-stack { display: flex; flex-direction: column; gap: 0; }
    .main-panel-stack > .row { margin-bottom: 1.35rem; }
    /* Feature tab: collapsible sections */
    .feature-sidebar .accordion { --bs-accordion-border-color: rgba(255,255,255,.12); }
    .feature-sidebar .accordion-item {
      background: #0f0f0f !important;
      border: 1px solid rgba(255,255,255,.12) !important;
      margin-bottom: 0.45rem;
      border-radius: 0.5rem !important;
      overflow: hidden;
    }
    .feature-sidebar .accordion-button {
      background: rgba(255,255,255,.06) !important;
      color: #fafafa !important;
      font-size: 0.82rem;
      font-weight: 600;
      letter-spacing: 0.02em;
      padding: 0.55rem 0.9rem;
      box-shadow: none !important;
    }
    .feature-sidebar .accordion-button:not(.collapsed) {
      background: rgba(255,255,255,.11) !important;
      color: #ffffff !important;
    }
    .feature-sidebar .accordion-button::after { filter: invert(1) grayscale(1); opacity: 0.65; }
    .feature-sidebar .accordion-body {
      background: #0a0a0a;
      padding: 0.65rem 0.9rem 0.85rem;
      border-top: 1px solid rgba(255,255,255,.08);
    }
    .feature-sidebar .accordion-body > .shiny-input-container:last-child { margin-bottom: 0; }
    /* Features tab — mockup-style blue accordions */
    .feat-mock-accordion { --bs-accordion-border-color: rgba(59,130,246,.35); }
    .feat-mock-accordion .accordion-item {
      background: #0c1624 !important;
      border: 1px solid rgba(59,130,246,.25) !important;
      margin-bottom: 0.5rem;
      border-radius: 0.45rem !important;
      overflow: hidden;
    }
    .feat-mock-accordion .accordion-button {
      background: linear-gradient(180deg, #1e3a5f 0%, #172554 100%) !important;
      color: #93c5fd !important;
      font-size: 0.88rem;
      font-weight: 600;
      padding: 0.6rem 1rem;
      box-shadow: none !important;
    }
    .feat-mock-accordion .accordion-button:not(.collapsed) {
      background: linear-gradient(180deg, #1d4ed8 0%, #1e3a8a 100%) !important;
      color: #ffffff !important;
    }
    .feat-mock-accordion .accordion-button::after { filter: none; opacity: 0.9; }
    .feat-mock-accordion .accordion-body {
      background: #0a1020;
      padding: 0.75rem 1rem 1rem;
      border-top: 1px solid rgba(59,130,246,.2);
    }
    .feat-mock-accordion .btn-primary {
      margin-top: 0.5rem;
      background: #2563eb !important;
      color: #fff !important;
      border-color: #3b82f6 !important;
    }
    .feat-mock-accordion .btn-primary:hover {
      background: #1d4ed8 !important;
      border-color: #60a5fa !important;
      color: #fff !important;
    }
  "))
)

guide_step_card <- function(num, title, ...) {
  card(
    class = "wow-card border-0 h-100 mb-3",
    card_header(
      tags$span(class = "badge rounded-pill noir-badge me-2", num),
      title
    ),
    card_body(...)
  )
}

# -----------------------------
# UI
# -----------------------------
ui <- page_navbar(
  title = tags$span(
    tags$span(class = "brand-title", "ADS Project 2"),
    tags$span(class = "brand-subtitle d-none d-lg-inline", "Applied Data Science")
  ),
  theme = app_theme,
  fillable = TRUE,
  header = app_head,
  tabPanel(
    title = "Guide",
    fluidPage(
      fluidRow(
        column(
          12,
          div(
            class = "hero-panel",
            tags$h1("A concise workflow for real-world tables"),
            tags$p(
              class = "hero-lead mb-2",
              "Ingest, clean, engineer features, and explore with ",
              tags$strong("interactive Plotly", style = "color: #fafafa;"), " — monochrome UI, full-spectrum analysis."
            ),
            tags$div(
              class = "d-flex flex-wrap gap-2 mt-3",
              tags$span(class = "badge rounded-pill noir-badge px-3 py-2", "Load"),
              tags$span(class = "badge rounded-pill noir-badge px-3 py-2", "Clean"),
              tags$span(class = "badge rounded-pill noir-badge px-3 py-2", "Features"),
              tags$span(class = "badge rounded-pill noir-badge px-3 py-2", "EDA"),
              tags$span(class = "badge rounded-pill noir-badge px-3 py-2", "Export")
            )
          ),
          fluidRow(
            column(4, guide_step_card(
              "1",
              "Load datasets",
              tags$p(
                class = "mb-2",
                "Upload ", tags$code("CSV/TXT"), " (comma, semicolon, tab), ", tags$code("Excel"), ", ",
                tags$code("JSON"), ", or ", tags$code("RDS"), "."
              ),
              tags$p(
                class = "small mb-0",
                "Or use Bank Marketing or Titanic (see ",
                tags$code("data/"), ")."
              )
            )),
            column(4, guide_step_card(
              "2",
              "Clean & preprocess",
              tags$ul(
                class = "small ps-3 mb-0",
                tags$li("Placeholders → missing, numeric coercion, sparse-column drop."),
                tags$li("Duplicate rows; id + position logic for chart-style data."),
                tags$li("IQR outliers, z-score scaling, label encoding.")
              )
            )),
            column(4, guide_step_card(
              "3",
              "Feature engineering",
              tags$p(class = "small mb-2", "Features: one-click transforms and datetime extraction (plus advanced options)."),
              tags$p(class = "small mb-0", style = "color: rgba(255,255,255,.5);", "Apply each section’s button to update the engineered table.")
            ))
          ),
          fluidRow(
            column(4, guide_step_card(
              "4",
              "EDA (interactive)",
              tags$p(class = "small mb-2", "Filters for hit, year, numeric range, and any categorical column."),
              tags$p(class = "small mb-0", tags$strong("Plotly:"), " pan, zoom, hover — plus summaries & missingness tables.")
            )),
            column(4, guide_step_card(
              "5",
              "Export",
              tags$p(class = "small mb-0", "Download cleaned and featured CSVs for reports and reproducibility.")
            )),
            column(4, guide_step_card(
              "Tip",
              "Tooltips",
              tags$p(
                class = "small mb-0",
                "Hover controls for in-app guidance. Use ",
                tags$strong("Load / Reset"),
                ", then ",
                tags$strong("Apply cleaning"),
                " & ",
                tags$strong("Apply features"),
                " in order."
              )
            ))
          )
        )
      )
    )
  ),

  tabPanel(
    title = "Upload & preview",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$div(class = "section-pill", "Source"),
        ti(
          radioButtons(
            "data_source",
            "Data source",
            choices = c("Upload file" = "upload", "Built-in dataset" = "builtin"),
            selected = "upload"
          ),
          "Switch between your own file and built-in datasets in data/."
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
                "Titanic (data/titanic.csv)" = "titanic"
              )
            ),
            "Both options require the corresponding CSV in the data/ folder."
          )
        ),
        ti(
          actionButton("load_btn", "Load / Reset", class = "btn-primary"),
          "Reload the dataset after changing source or file. Required before other tabs use new data."
        ),
        tags$hr(),
        tags$div(class = "section-pill", "Options"),
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
        tags$div(
          class = "main-panel-stack",
          card(
            class = "wow-card border-0 table-slot-card",
            card_header("Dataset snapshot"),
            card_body(tableOutput("data_info"))
          ),
          conditionalPanel(
            condition = "input.show_preview == true",
            tagList(
              card(
                class = "wow-card border-0 table-slot-card",
                card_header("Raw preview"),
                card_body(DTOutput("raw_preview"))
              ),
              card(
                class = "wow-card border-0 table-slot-card",
                card_header("Variable structure & missingness"),
                card_body(DTOutput("structure_summary"))
              ),
              card(
                class = "wow-card border-0 table-slot-card",
                card_header("Raw summary"),
                card_body(verbatimTextOutput("raw_summary"))
              )
            )
          )
        )
      )
    )
  ),

  tabPanel(
    title = "Cleaning",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$div(class = "section-pill", "Quality"),
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
        tags$div(
          class = "main-panel-stack",
          card(
            class = "wow-card border-0 table-slot-card",
            card_header("Before vs after"),
            card_body(tableOutput("cleaning_summary"))
          ),
          fluidRow(
            column(
              6,
              card(
                class = "wow-card border-0 table-slot-card h-100",
                card_header("Cleaned preview"),
                card_body(DTOutput("clean_preview"))
              )
            ),
            column(
              6,
              card(
                class = "wow-card border-0 table-slot-card h-100",
                card_header("Outlier comparison"),
                card_body(plotOutput("outlier_plot", height = "280px"))
              )
            )
          ),
          card(
            class = "wow-card border-0 table-slot-card",
            card_header("Missingness after cleaning"),
            card_body(DTOutput("cleaned_missing_summary"))
          ),
          card(
            class = "wow-card border-0 table-slot-card",
            card_header("Cleaning summary"),
            card_body(verbatimTextOutput("clean_summary"))
          )
        )
      )
    )
  ),

  tabPanel(
    title = "Features",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        class = "feature-sidebar",
        tags$div(class = "section-pill", "Transforms"),
        tags$p(
          class = "small",
          style = "color: rgba(255,255,255,.5); margin-bottom: .75rem;",
          "Pick a column and action, then use the blue buttons inside each section. Cleaning resets the engineered table."
        ),
        bslib::accordion(
          id = "feat_accordion",
          class = "mb-2 feat-mock-accordion",
          multiple = TRUE,
          open = c("feat_oct", "feat_dt"),
          bslib::accordion_panel(
            value = "feat_oct",
            title = "One-Click Transforms",
            uiOutput("oct_col_ui"),
            selectInput(
              "oct_method",
              "Method",
              choices = c(
                "Log" = "log",
                "Square root" = "sqrt",
                "Square" = "square",
                "Rank" = "rank",
                "Missing indicator (0/1)" = "missing",
                "Text length" = "text_length",
                "Category frequency" = "frequency",
                "Z-score (standardize)" = "zscore"
              ),
              selected = "log"
            ),
            actionButton("oct_apply_btn", "Apply Transform", class = "btn-primary w-100")
          ),
          bslib::accordion_panel(
            value = "feat_dt",
            title = "Datetime Extraction",
            uiOutput("dt_col_ui"),
            tags$label(class = "form-label mb-1", style = "color:#a3a3a3;", "Extract"),
            checkboxInput("dt_ext_year", "Year", TRUE),
            checkboxInput("dt_ext_month", "Month", TRUE),
            checkboxInput("dt_ext_day", "Day", TRUE),
            checkboxInput("dt_ext_weekday", "Weekday", TRUE),
            checkboxInput("dt_ext_quarter", "Quarter", TRUE),
            actionButton("dt_extract_btn", "Extract", class = "btn-primary w-100")
          ),
          bslib::accordion_panel(
            value = "feat_adv",
            title = "Advanced & shortcuts",
            ti(
              checkboxInput("add_family_size", "Family size (SibSp + Parch + 1)", TRUE),
              "Titanic-style column when SibSp & Parch exist."
            ),
            ti(
              checkboxInput("add_log_time_price", "log1p(Fare) / log1p(duration) auto", TRUE),
              "Adds log_fare / log_duration when present."
            ),
            ti(
              checkboxInput("add_campaign_prev_ratio", "campaign / (previous + 1)", FALSE),
              "Bank-style ratio when columns exist."
            ),
            tags$hr(),
            selectInput(
              "banding_feature",
              "Bands / levels",
              choices = c(
                "None" = "none",
                "Age bands (Young / Adult / Middle / Senior)" = "age_group",
                "Balance level (Low / Medium / High)" = "balance_level"
              ),
              selected = "none"
            ),
            uiOutput("banding_var_ui"),
            ti(checkboxInput("create_contacted_before", "contacted_before from pdays", FALSE), ""),
            uiOutput("pdays_var_ui"),
            tags$hr(),
            selectInput(
              "arith_feature",
              "Two-column arithmetic",
              choices = c(
                "None" = "none",
                "Ratio of two numeric columns" = "ratio",
                "Sum of two numeric columns" = "sum",
                "Difference of two numeric columns" = "diff",
                "Product of two numeric columns" = "product"
              ),
              selected = "none"
            ),
            uiOutput("arith_vars_ui"),
            tags$hr(),
            ti(checkboxInput("create_indicator_feature", "Indicator: 1 if value > threshold", FALSE), ""),
            uiOutput("indicator_var_ui"),
            uiOutput("indicator_threshold_ui"),
            tags$hr(),
            ti(checkboxInput("create_group_mean_feature", "Group mean of numeric target", FALSE), ""),
            uiOutput("group_var_ui"),
            uiOutput("target_var_ui"),
            tags$hr(),
            actionButton("feat_btn", "Apply advanced options", class = "btn-primary w-100")
          )
        )
      ),
      mainPanel(
        width = 9,
        tags$div(
          class = "main-panel-stack",
          fluidRow(
            column(
              6,
              card(
                class = "wow-card border-0 table-slot-card h-100",
                card_header("Engineered preview"),
                card_body(DTOutput("feat_preview"))
              )
            ),
            column(
              6,
              card(
                class = "wow-card border-0 table-slot-card h-100",
                card_header("First new column"),
                card_body(plotOutput("feat_plot", height = "280px"))
              )
            )
          ),
          card(
            class = "wow-card border-0 table-slot-card",
            card_header("Feature summary"),
            card_body(verbatimTextOutput("feat_summary"))
          )
        )
      )
    )
  ),

  tabPanel(
    title = "EDA",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$div(class = "section-pill", "Filters"),
        uiOutput("hit_filter_ui"),
        tags$hr(),
        uiOutput("year_filter_ui"),
        tags$hr(),
        uiOutput("numeric_filter_var_ui"),
        uiOutput("numeric_filter_range_ui"),
        tags$hr(),
        uiOutput("eda_cat_filter_var_ui"),
        uiOutput("eda_cat_filter_val_ui"),
        tags$hr(),
        ti(
          actionButton("eda_apply_btn", "Apply EDA", class = "btn-primary w-100"),
          tagList(
            "EDA.py-style: click after changing filters to refresh tables, correlation matrix, and plots. ",
            "Until the first click, the full featured table is shown."
          )
        ),
        tags$hr(),
        ti(
          radioButtons(
            "plot_type",
            "Plot type",
            choices = c(
              "Histogram" = "hist",
              "Scatter" = "scatter",
              "Boxplot" = "box",
              "Bar chart (counts)" = "bar",
              "Correlation heatmap" = "corr"
            ),
            selected = "hist"
          ),
          "Bar chart needs a categorical column; correlation heatmap needs 2+ numeric columns."
        ),
        ti(
          sliderInput("hist_bins", "Histogram bins", min = 5, max = 80, value = 30),
          "Bins for the histogram only."
        ),
        uiOutput("xvar_ui"),
        uiOutput("yvar_ui"),
        uiOutput("plot_group_ui"),
        uiOutput("bar_cat_ui"),
        uiOutput("box_group_ui"),
        uiOutput("corr_cols_ui"),
        helpText("Plots use Plotly. Large scatters sample to 20k points.")
      ),
      mainPanel(
        width = 9,
        tags$div(
          class = "main-panel-stack",
          card(
            class = "wow-card border-0 table-slot-card",
            card_header("Filtered data preview"),
            card_body(DTOutput("filtered_preview"))
          ),
          card(
            class = "wow-card border-0 table-slot-card",
            card_header("Numeric summary (describe)"),
            card_body(DTOutput("eda_numeric_summary"))
          ),
          card(
            class = "wow-card border-0 table-slot-card",
            card_header("Missingness"),
            card_body(DTOutput("eda_missing_summary"))
          ),
          card(
            class = "wow-card border-0 table-slot-card",
            card_header("Correlation matrix (Pearson)"),
            card_body(DTOutput("eda_corr_table"))
          ),
          card(
            class = "wow-card border-0 table-slot-card",
            card_header("Visualization"),
            card_body(
              plotlyOutput("main_plot", height = "440px"),
              tags$hr(),
              verbatimTextOutput("eda_stats")
            )
          )
        )
      )
    )
  ),

  tabPanel(
    title = "Export",
    fluidRow(
      column(
        12,
        offset = 0,
        div(
          class = "export-zone",
          tags$h3(class = "mb-3", style = "font-family: 'Cormorant Garamond', serif; color: #fafafa;", "Export"),
          fluidRow(
            column(
              5,
              ti(
                downloadButton("download_clean", "Download cleaned CSV", class = "btn-success w-100 py-3"),
                tagList("Table from the last ", tags$strong("Apply cleaning"), " run.")
              )
            ),
            column(
              5,
              ti(
                downloadButton("download_featured", "Download featured CSV", class = "btn-success w-100 py-3"),
                tagList("Same columns as the EDA tab (after ", tags$strong("Apply features"), ").")
              )
            ),
            column(
              2,
              tags$div(
                class = "text-center text-muted small",
                style = "border-left: 1px solid rgba(148,163,184,.2); padding-left: 1rem;",
                tags$div(class = "mb-2", style = "font-size: 2rem; opacity: .6;", "⬇"),
                "CSV · UTF-8"
              )
            )
          ),
          tags$hr(class = "border-secondary opacity-25"),
          helpText("Run cleaning and feature steps first so exports are up to date.")
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  ggplot2::theme_set(
    ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#141414", color = NA),
        panel.background = ggplot2::element_rect(fill = "#141414", color = NA),
        panel.grid.major = ggplot2::element_line(color = "rgba(255,255,255,0.08)"),
        panel.grid.minor = ggplot2::element_line(color = "rgba(255,255,255,0.04)"),
        text = ggplot2::element_text(color = "#e5e5e5"),
        axis.text = ggplot2::element_text(color = "#a3a3a3"),
        plot.title = ggplot2::element_text(face = "bold", color = "#fafafa")
      )
  )

  app_dir <- getwd()
  data_dir <- file.path(app_dir, "data")

  raw_data <- eventReactive(input$load_btn, {
    df <- if (isTRUE(input$data_source == "upload")) {
      req(input$upload)
      read_uploaded_data(input$upload$datapath, input$upload$name)
    } else {
      ch <- input$builtin_choice
      if (ch == "bank") {
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

  feat_accum <- reactiveVal(NULL)

  observeEvent(input$clean_btn, {
    feat_accum(NULL)
  }, priority = 1)

  observeEvent(input$load_btn, {
    feat_accum(NULL)
  }, priority = 1)

  featured_data <- reactive({
    req(cleaned_data())
    acc <- feat_accum()
    if (is.null(acc)) cleaned_data() else acc
  })

  observeEvent(input$oct_apply_btn, {
    req(cleaned_data(), input$oct_col, input$oct_method)
    base <- if (is.null(isolate(feat_accum()))) cleaned_data() else isolate(feat_accum())
    shiny::validate(shiny::need(input$oct_col %in% names(base), "Choose a valid column."))
    out <- apply_one_click_transform(base, input$oct_col, input$oct_method)
    if (identical(out, base)) {
      showNotification("Transform could not be applied (check column type vs method).", type = "warning", duration = 4)
      return(invisible(NULL))
    }
    feat_accum(out)
    showNotification("Transform applied.", type = "message", duration = 2)
  })

  observeEvent(input$dt_extract_btn, {
    req(cleaned_data(), input$dt_col)
    base <- if (is.null(isolate(feat_accum()))) cleaned_data() else isolate(feat_accum())
    shiny::validate(shiny::need(input$dt_col %in% names(base), "Choose a datetime column."))
    out <- extract_datetime_selected(
      base,
      input$dt_col,
      year = isTRUE(input$dt_ext_year),
      month = isTRUE(input$dt_ext_month),
      day = isTRUE(input$dt_ext_day),
      weekday = isTRUE(input$dt_ext_weekday),
      quarter = isTRUE(input$dt_ext_quarter)
    )
    if (identical(out, base)) {
      showNotification("No datetime parts added (check parsing or select at least one Extract option).", type = "warning", duration = 4)
      return(invisible(NULL))
    }
    feat_accum(out)
    showNotification("Datetime features extracted.", type = "message", duration = 2)
  })

  observeEvent(input$feat_btn, {
    req(cleaned_data())
    base <- if (is.null(isolate(feat_accum()))) cleaned_data() else isolate(feat_accum())
    banding_choice <- if (is.null(input$banding_feature)) "none" else input$banding_feature
    arith_choice <- if (is.null(input$arith_feature)) "none" else input$arith_feature
    thr <- NULL
    if (isTRUE(input$create_indicator_feature) && !is.null(input$indicator_threshold)) {
      thr <- suppressWarnings(as.numeric(input$indicator_threshold))
    }
    d <- feature_engineering_pipeline(
      base,
      create_date_parts = FALSE,
      date_var = NULL,
      create_age_group = identical(banding_choice, "age_group"),
      age_var = if (identical(banding_choice, "age_group")) input$banding_var else NULL,
      create_balance_level = identical(banding_choice, "balance_level"),
      balance_var = if (identical(banding_choice, "balance_level")) input$banding_var else NULL,
      create_contacted_before = isTRUE(input$create_contacted_before),
      pdays_var = if (isTRUE(input$create_contacted_before)) input$pdays_var else NULL,
      create_log_feature = FALSE,
      log_var = NULL,
      create_sqrt_feature = FALSE,
      sqrt_var = NULL,
      create_square_feature = FALSE,
      square_var = NULL,
      create_ratio_feature = identical(arith_choice, "ratio"),
      ratio_num = if (identical(arith_choice, "ratio")) input$arith_var1 else NULL,
      ratio_den = if (identical(arith_choice, "ratio")) input$arith_var2 else NULL,
      create_sum_feature = identical(arith_choice, "sum"),
      sum_var1 = if (identical(arith_choice, "sum")) input$arith_var1 else NULL,
      sum_var2 = if (identical(arith_choice, "sum")) input$arith_var2 else NULL,
      create_diff_feature = identical(arith_choice, "diff"),
      diff_var1 = if (identical(arith_choice, "diff")) input$arith_var1 else NULL,
      diff_var2 = if (identical(arith_choice, "diff")) input$arith_var2 else NULL,
      create_product_feature = identical(arith_choice, "product"),
      prod_var1 = if (identical(arith_choice, "product")) input$arith_var1 else NULL,
      prod_var2 = if (identical(arith_choice, "product")) input$arith_var2 else NULL,
      create_indicator_feature = isTRUE(input$create_indicator_feature),
      indicator_var = if (isTRUE(input$create_indicator_feature)) input$indicator_var else NULL,
      indicator_threshold = thr,
      create_missing_indicator = FALSE,
      missing_var = NULL,
      create_frequency_feature = FALSE,
      freq_var = NULL,
      create_group_mean_feature = isTRUE(input$create_group_mean_feature),
      group_var = if (isTRUE(input$create_group_mean_feature)) input$group_var else NULL,
      target_var = if (isTRUE(input$create_group_mean_feature)) input$target_var else NULL,
      create_rank_feature = FALSE,
      rank_var = NULL,
      create_text_length = FALSE,
      text_var = NULL
    )
    d <- append_post_pipeline_features(
      d,
      add_family_size = isTRUE(input$add_family_size),
      add_log_time_price = isTRUE(input$add_log_time_price),
      add_campaign_prev_ratio = isTRUE(input$add_campaign_prev_ratio)
    )
    feat_accum(d)
    showNotification("Advanced options applied.", type = "message", duration = 2)
  })

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
    par(mfrow = c(1, 2), bg = "#141414", col.axis = "#a3a3a3", col.lab = "#d4d4d4", col.main = "#fafafa", fg = "#525252")
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

  output$oct_col_ui <- renderUI({
    df <- featured_data()
    req(df)
    cols <- names(df)
    if (length(cols) == 0) return(helpText("Run cleaning first, then load columns here."))
    selectInput("oct_col", "Column", choices = cols, selected = cols[1])
  })

  output$dt_col_ui <- renderUI({
    df <- featured_data()
    req(df)
    cols <- names(df)
    if (length(cols) == 0) return(helpText("Run cleaning first."))
    ti(
      selectInput("dt_col", "Datetime column", choices = cols, selected = cols[1]),
      "Parsed with as.POSIXlt (UTC). Check only the parts you want."
    )
  })

  output$banding_var_ui <- renderUI({
    req(cleaned_data())
    choice <- if (is.null(input$banding_feature)) "none" else input$banding_feature
    if (identical(choice, "none")) return(NULL)
    df <- featured_data()
    nm <- names(df)[sapply(df, is.numeric)]
    if (length(nm) == 0) return(helpText("No numeric columns."))
    if (identical(choice, "age_group")) {
      sel <- if ("age" %in% nm) "age" else nm[1]
      return(selectInput("banding_var", "Age column", choices = nm, selected = sel))
    }
    sel <- if ("balance" %in% nm) "balance" else nm[1]
    selectInput("banding_var", "Balance column", choices = nm, selected = sel)
  })

  output$pdays_var_ui <- renderUI({
    req(cleaned_data())
    if (!isTRUE(input$create_contacted_before)) return(NULL)
    df <- featured_data()
    nm <- names(df)[sapply(df, is.numeric)]
    if (length(nm) == 0) return(helpText("No numeric columns."))
    sel <- if ("pdays" %in% nm) "pdays" else nm[1]
    selectInput("pdays_var", "pdays column", choices = nm, selected = sel)
  })

  output$arith_vars_ui <- renderUI({
    req(cleaned_data())
    if (is.null(input$arith_feature) || identical(input$arith_feature, "none")) return(NULL)
    df <- featured_data()
    nm <- names(df)[sapply(df, is.numeric)]
    if (length(nm) < 2) return(helpText("Need ≥2 numeric columns."))
    second_sel <- if (length(nm) >= 2) nm[2] else nm[1]
    first_label <- if (identical(input$arith_feature, "ratio")) "Numerator" else "Column A"
    second_label <- if (identical(input$arith_feature, "ratio")) "Denominator" else "Column B"
    tagList(
      selectInput("arith_var1", first_label, choices = nm, selected = nm[1]),
      selectInput("arith_var2", second_label, choices = nm, selected = second_sel)
    )
  })

  output$indicator_var_ui <- renderUI({
    req(cleaned_data())
    if (!isTRUE(input$create_indicator_feature)) return(NULL)
    df <- featured_data()
    nm <- names(df)[sapply(df, is.numeric)]
    if (length(nm) == 0) return(helpText("No numeric columns."))
    selectInput("indicator_var", "Numeric column", choices = nm, selected = nm[1])
  })

  output$indicator_threshold_ui <- renderUI({
    req(cleaned_data())
    if (!isTRUE(input$create_indicator_feature)) return(NULL)
    df <- featured_data()
    nm <- names(df)[sapply(df, is.numeric)]
    if (length(nm) == 0) return(NULL)
    iv <- if (!is.null(input$indicator_var) && input$indicator_var %in% names(df)) input$indicator_var else nm[1]
    xs <- df[[iv]][!is.na(df[[iv]])]
    if (length(xs) == 0) return(helpText("No non-NA values for threshold."))
    md <- stats::median(xs)
    numericInput("indicator_threshold", "Threshold", value = md, step = (max(xs) - min(xs)) / 100)
  })

  output$group_var_ui <- renderUI({
    req(cleaned_data())
    if (!isTRUE(input$create_group_mean_feature)) return(NULL)
    df <- featured_data()
    cols <- names(df)
    if (length(cols) == 0) return(helpText("No columns."))
    selectInput("group_var", "Group column", choices = cols, selected = cols[1])
  })

  output$target_var_ui <- renderUI({
    req(cleaned_data())
    if (!isTRUE(input$create_group_mean_feature)) return(NULL)
    df <- featured_data()
    nm <- names(df)[sapply(df, is.numeric)]
    if (length(nm) == 0) return(helpText("No numeric target column."))
    selectInput("target_var", "Numeric target (mean by group)", choices = nm, selected = nm[1])
  })

  last_feat_digest <- reactiveVal(NULL)
  observeEvent(featured_data(), {
    fd <- featured_data()
    req(fd)
    d <- digest::digest(list(nr = nrow(fd), nc = ncol(fd), cols = names(fd)))
    prev <- last_feat_digest()
    if (!is.null(prev) && !identical(prev, d)) {
      showNotification(
        "Featured data changed — click Apply EDA to refresh filtered views.",
        type = "message",
        duration = 5
      )
    }
    last_feat_digest(d)
  }, ignoreNULL = TRUE)

  output$feat_preview <- renderDT({
    req(featured_data())
    datatable(head(featured_data(), 500), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$feat_plot <- renderPlot({
    req(featured_data(), cleaned_data())
    par(bg = "#141414", col.axis = "#a3a3a3", col.lab = "#d4d4d4", col.main = "#fafafa", fg = "#525252")
    fd <- featured_data()
    before <- names(cleaned_data())
    newc <- setdiff(names(fd), before)
    if (length(newc) == 0) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "No new columns vs cleaned data", col = "#737373")
      return(invisible(NULL))
    }
    v <- newc[1]
    x <- fd[[v]]
    if (is.numeric(x)) {
      graphics::hist(x, main = paste("Distribution:", v), xlab = v, col = "#a3a3a3", border = "#0a0a0a")
    } else {
      tb <- sort(table(as.character(x), useNA = "ifany"), decreasing = TRUE)
      tb <- head(tb, 20)
      graphics::barplot(tb, las = 2, main = paste("Counts:", v), col = "#737373", border = "#0a0a0a")
    }
  })

  output$feat_summary <- renderPrint({
    req(featured_data(), cleaned_data())
    df <- featured_data()
    cat("Featured data: ", nrow(df), " rows x ", ncol(df), " cols\n", sep = "")
    newc <- setdiff(names(df), names(cleaned_data()))
    if (length(newc) == 0) {
      cat("No new columns vs cleaned table.\n")
    } else {
      cat("New columns:", paste(newc, collapse = ", "), "\n")
    }
    short <- intersect(
      c("family_size", "log_fare", "log_duration", "campaign_prev_ratio"),
      names(df)
    )
    if (length(short) > 0) {
      cat("Shortcut columns present:", paste(short, collapse = ", "), "\n")
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
    if (length(unique(yr)) <= 1) {
      return(helpText("Single year value — year range filter not needed."))
    }
    ti(
      sliderInput("year_range", "Year range", min = min(yr), max = max(yr), value = c(min(yr), max(yr))),
      "Restrict EDA when the table has a numeric year column."
    )
  })

  output$numeric_filter_var_ui <- renderUI({
    req(featured_data())
    df <- featured_data()
    vars <- names(df)[sapply(df, is.numeric)]
    vars <- setdiff(vars, c("hit", "year"))
    ch <- if (length(vars) > 0L) {
      c("None" = "None", stats::setNames(vars, vars))
    } else {
      c("None" = "None")
    }
    ti(
      selectInput("numeric_filter_var", "Numeric filter variable", choices = ch, selected = "None"),
      "EDA.py-style optional range filter. None = do not filter by a numeric column."
    )
  })

  output$numeric_filter_range_ui <- renderUI({
    req(featured_data())
    if (is.null(input$numeric_filter_var) || identical(input$numeric_filter_var, "None")) {
      return(helpText("No numeric range filter — select a variable above or leave as None."))
    }
    df <- featured_data()
    var <- input$numeric_filter_var
    shiny::validate(shiny::need(var %in% names(df), "Bad filter var"))
    x <- df[[var]][!is.na(df[[var]])]
    shiny::validate(shiny::need(length(unique(x)) > 1, "Not enough distinct values for a range."))
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
    if (identical(input$plot_type, "bar")) return(NULL)
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

  output$plot_group_ui <- renderUI({
    req(featured_data())
    pt <- input$plot_type
    if (!pt %in% c("scatter", "box")) return(NULL)
    cv <- categorical_vars()
    if (length(cv) == 0L) {
      return(helpText("No categorical column for group / color (EDA.py-style)."))
    }
    ch <- c("None" = "None", stats::setNames(cv, cv))
    ti(
      selectInput("plot_group_var", "Group / color (optional)", choices = ch, selected = "None"),
      tagList(
        tags$strong("Scatter:"), " color points. ",
        tags$strong("Boxplot:"), " group along x-axis when set (overrides hit / manual group below)."
      )
    )
  })

  output$bar_cat_ui <- renderUI({
    if (!identical(input$plot_type, "bar")) return(NULL)
    req(featured_data())
    cv <- categorical_vars()
    if (length(cv) == 0) return(helpText("No categorical column for bar chart."))
    ti(
      selectInput("bar_cat_var", "Category variable (bar chart)", choices = cv, selected = cv[1]),
      "Shows counts for the top 20 levels."
    )
  })

  output$eda_cat_filter_var_ui <- renderUI({
    req(featured_data())
    cv <- categorical_vars()
    if (length(cv) == 0) {
      return(helpText("No categorical columns — generic category filter hidden."))
    }
    ch <- c("None" = "None", stats::setNames(cv, cv))
    ti(
      selectInput("eda_cat_var", "Filter by categorical column (optional)", choices = ch, selected = "None"),
      "Subset rows where the column equals the chosen level."
    )
  })

  output$eda_cat_filter_val_ui <- renderUI({
    req(featured_data())
    if (is.null(input$eda_cat_var) || input$eda_cat_var == "None") {
      return(helpText("Pick a column above to choose a level, or leave as None."))
    }
    df <- featured_data()
    v <- input$eda_cat_var
    if (!v %in% names(df)) return(NULL)
    vals <- sort(unique(as.character(df[[v]][!is.na(df[[v]])])))
    ch <- c("All" = "All", stats::setNames(vals, vals))
    ti(selectInput("eda_cat_val", "Category level", choices = ch, selected = "All"), "All = no filter on this column.")
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

  filtered_eda_applied <- eventReactive(input$eda_apply_btn, {
    req(featured_data())
    apply_eda_filters(featured_data(), input)
  })

  # EDA.py: filters take effect after "Apply EDA"; before first click, show full featured table.
  filtered <- reactive({
    req(featured_data())
    if (is.null(input$eda_apply_btn) || input$eda_apply_btn < 1L) {
      return(featured_data())
    }
    filtered_eda_applied()
  })

  output$filtered_preview <- renderDT({
    req(filtered())
    datatable(head(filtered(), 15), options = list(pageLength = 8, scrollX = TRUE))
  })

  output$eda_numeric_summary <- renderDT({
    req(filtered())
    sm <- eda_numeric_describe_df(filtered())
    if ("Message" %in% names(sm)) {
      return(datatable(sm, options = list(dom = "t"), rownames = FALSE))
    }
    datatable(sm, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$eda_missing_summary <- renderDT({
    req(filtered())
    datatable(eda_missing_summary_df(filtered()), options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$eda_corr_table <- renderDT({
    req(filtered())
    df <- filtered()
    num_df <- df[, sapply(df, is.numeric), drop = FALSE]
    if (ncol(num_df) < 2L) {
      return(datatable(
        data.frame(Message = "Need at least two numeric columns for correlation."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    cm <- round(stats::cor(num_df, use = "pairwise.complete.obs"), 3)
    out <- cbind(Variable = rownames(cm), as.data.frame(cm, stringsAsFactors = FALSE))
    rownames(out) <- NULL
    datatable(out, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  output$main_plot <- renderPlotly({
    req(filtered())
    df <- filtered()
    pt <- input$plot_type
    shiny::validate(shiny::need(pt %in% c("hist", "scatter", "box", "bar", "corr"), "Select plot type"))

    if (pt == "hist") {
      req(input$xvar)
      shiny::validate(shiny::need(input$xvar %in% names(df), "Choose X"))
      bins <- input$hist_bins
      p <- ggplot(df, aes(x = .data[[input$xvar]])) +
        geom_histogram(fill = alpha("#a3a3a3", 0.88), color = alpha("#fafafa", 0.25), bins = bins) +
        theme_minimal() +
        labs(title = paste("Histogram:", input$xvar), x = input$xvar, y = "Count")
      ggplotly(p, source = "eda") %>% layout(hovermode = "closest")
    } else if (pt == "scatter") {
      req(input$xvar, input$yvar)
      shiny::validate(shiny::need(nrow(df) >= 2, "Select a filter with at least 2 rows."))
      plot_df <- df
      if (nrow(plot_df) > 20000) plot_df <- dplyr::slice_sample(plot_df, n = 20000)
      pg <- input$plot_group_var
      has_group <- !is.null(pg) && !identical(pg, "None") && pg %in% names(plot_df)
      if (has_group) {
        p <- ggplot(
          plot_df,
          aes(
            x = .data[[input$xvar]], y = .data[[input$yvar]],
            color = factor(.data[[pg]])
          )
        ) +
          geom_point(alpha = 0.5, size = 0.72) +
          scale_color_discrete(name = pg) +
          theme_minimal() +
          labs(title = paste(input$yvar, "vs", input$xvar, "by", pg))
      } else {
        p <- ggplot(plot_df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
          geom_point(alpha = 0.4, size = 0.65, color = alpha("#fafafa", 0.35)) +
          theme_minimal() +
          labs(title = paste(input$xvar, "vs", input$yvar))
      }
      if (nrow(plot_df) >= 10) {
        if (has_group) {
          p <- p + geom_smooth(method = "lm", se = TRUE, linewidth = 0.65, alpha = 0.35)
        } else {
          p <- p + geom_smooth(
            method = "lm", se = TRUE,
            color = alpha("#fafafa", 0.75), linewidth = 0.65,
            fill = alpha("#737373", 0.35)
          )
        }
      }
      ggplotly(p) %>% layout(hovermode = "closest")
    } else if (pt == "box") {
      req(input$xvar)
      shiny::validate(shiny::need(input$xvar %in% names(df), "Choose numeric variable"))
      ycol <- input$xvar
      pg <- input$plot_group_var
      if (!is.null(pg) && !identical(pg, "None") && pg %in% names(df)) {
        p <- ggplot(df, aes(x = factor(.data[[pg]]), y = .data[[ycol]])) +
          geom_boxplot(fill = alpha("#525252", 0.75), color = alpha("#fafafa", 0.2)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
          labs(title = paste(ycol, "by", pg), x = pg)
      } else if ("hit" %in% names(df)) {
        p <- ggplot(df, aes(x = factor(hit), y = .data[[ycol]])) +
          geom_boxplot(fill = alpha("#737373", 0.65), color = alpha("#fafafa", 0.2)) +
          theme_minimal() +
          labs(title = paste(ycol, "by hit"), x = "hit")
      } else {
        req(input$box_group)
        shiny::validate(shiny::need(input$box_group %in% names(df), "Group column"))
        p <- ggplot(df, aes(x = factor(.data[[input$box_group]]), y = .data[[ycol]])) +
          geom_boxplot(fill = alpha("#525252", 0.75), color = alpha("#fafafa", 0.2)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
          labs(title = paste(ycol, "by", input$box_group), x = input$box_group)
      }
      ggplotly(p)
    } else if (pt == "bar") {
      req(input$bar_cat_var)
      shiny::validate(shiny::need(input$bar_cat_var %in% names(df), "Choose a category column"))
      v <- input$bar_cat_var
      vc <- df[[v]]
      cc <- as.character(vc)
      cc[is.na(vc)] <- "(NA)"
      tb <- sort(table(cc, useNA = "no"), decreasing = TRUE)
      k <- min(20L, length(tb))
      dplot <- data.frame(
        Level = names(tb)[seq_len(k)],
        Count = as.numeric(tb[seq_len(k)]),
        stringsAsFactors = FALSE
      )
      p <- ggplot(dplot, aes(x = stats::reorder(Level, Count), y = Count)) +
        geom_col(fill = alpha("#d4d4d4", 0.85), color = alpha("#fafafa", 0.15)) +
        coord_flip() +
        theme_minimal() +
        labs(title = paste("Bar chart:", v, "(top", k, "levels)"), x = NULL, y = "Count")
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
        geom_tile(color = alpha("#fafafa", 0.12)) +
        scale_fill_gradient2(
          low = "#171717", mid = "#737373", high = "#fafafa",
          midpoint = 0, limits = c(-1, 1)
        ) +
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

shinyApp(ui, server, options = list(title = "ADS Project 2"))
