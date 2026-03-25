# ADS Project 2 — Data workflow app (R Shiny)

Course project: an **R Shiny** app (**ADS Project 2**) for ingesting tabular data, cleaning and imputing, engineering features, interactive EDA with Plotly, and exporting CSVs. The UI uses a **noir / monochrome** theme (bslib), with **tooltips** on controls and **separate card** panels so tables and plots do not overlap.

## Requirements

- **R 4.x**
- Working directory = this project folder when you run the app.

## Install packages

```r
install.packages(c(
  "shiny", "bslib", "DT", "dplyr", "readr", "readxl", "jsonlite",
  "ggplot2", "scales", "digest", "plotly"
))
```

## Run locally

In RStudio, open `app.R` and click **Run App**, or from the project root:

```r
shiny::runApp("app.R")
```

With R on your PATH:

```bash
Rscript -e "shiny::runApp('app.R', launch.browser = TRUE)"
```

The browser window title is **“ADS Project 2”**.

## App workflow (tabs)

| Tab | What it does |
|-----|----------------|
| **Guide** | Short overview of the pipeline and controls. |
| **Upload & preview** | Load data from a file or from built-ins; dataset snapshot, preview, column types & missingness, `summary()`. |
| **Cleaning** | Placeholders → NA, numeric coercion, sparse columns, missing-value strategies (global or **column-specific rules**), duplicates, **id + position** handling for chart-style rows, IQR outliers, z-score scaling, optional label encoding. |
| **Features** | Accordion sections: one-click transforms, datetime extraction, **two-column arithmetic** (own panel), and advanced options (banding, pdays, indicator, group mean, shortcuts). |
| **EDA** | **Apply EDA** applies filters (year, optional numeric range, optional category level) and refreshes tables and charts. Numeric **describe**-style summary, missingness table, **Pearson correlation matrix** (table), Plotly plots (histogram, scatter with optional group/color, boxplot, bar counts, correlation heatmap). |
| **Export** | Download cleaned and **featured** CSVs (UTF-8). |

## Data sources

**Upload** supports CSV/TXT (comma, semicolon, or tab), Excel (`.xlsx`/`.xls`), **JSON** (table-shaped arrays, wrapped objects, or JSON Lines), and **RDS**.

**Built-in** datasets (files must exist under `data/`):

- `data/bank-full.csv` — Bank Marketing (semicolon-separated)
- `data/titanic.csv` — Titanic

There is **no** synthetic/demo generator; both built-ins need the corresponding CSV.

## Technical notes

- **JSON**: Loaded by reading the file as UTF-8 text (avoids path/encoding edge cases), with optional BOM strip, single-key unwrapping (e.g. `{"data":[...]}`), and **JSON Lines** fallback.
- **EDA behavior**: Matches the spirit of the reference Python app in `EDA.py` (filters + describe + missingness + correlation table + grouped scatter/boxplots), with extra R features (Plotly, year/numeric/category filters, correlation heatmap). Until the first **Apply EDA** click, the EDA views use the full featured table; after that, filters refresh only when you click **Apply EDA** again.
- **Deployment** (e.g. [shinyapps.io](https://www.shinyapps.io)): use `rsconnect::deployApp()` with `appPrimaryDoc = "app.R"`. **Do not commit API tokens or secrets**; configure `rsconnect` via the documented account workflow or environment variables, not hard-coded keys in the repo.

## Repo layout

| Path | Notes |
|------|--------|
| `app.R` | **Main application** — full pipeline, noir UI, cards per table. |
| `data/` | `bank-full.csv`, `titanic.csv` for built-in loading. |
| `setup_shinyapps.R` | Example deploy script — **rotate credentials** if this file ever contained live tokens and use a safe pattern going forward. |
| `EDA.py` | Reference / notebook export for the original Python Shiny specification (full script content may live in your notebook). |
| `app.py` | Python Shiny variant (scope may differ from `app.R`). |
| `app5.R`, `draft.R` | Older or partial R drafts; use **`app.R`** for the current app. |
| `EDA.ipynb` | Notebook tied to Python workflow. |

## Hand-in (course)

The assignment typically asks for a short report, a **deployed app** URL (e.g. shinyapps.io), and this repo with runnable code. Put the live URL and team contribution notes in the report; follow Courseworks for deadlines.
