# Project 2 — Web Application (DS)

This repository contains coursework for **Project 2: Web Application Development and Deployment** (see `Project 2.pdf`). The team is building an interactive data app with upload, cleaning, feature engineering, EDA, and export.

## Primary app (PDF-aligned)

| File | Role |
|------|------|
| **`app.R`** | **Main R Shiny application** — user guide, multi-format upload, built-ins (`data/bank-full.csv`, `data/titanic.csv`) plus a **synthetic Billboard** demo, full cleaning/preprocessing, feature engineering, **interactive Plotly EDA**, CSV export. Run with `shiny::runApp("app.R")`. |

**R packages:** `shiny`, `bslib`, `DT`, `dplyr`, `readr`, `readxl`, `jsonlite`, `ggplot2`, `scales`, `digest`, **`plotly`**.

## Other files in this repository

| File | Role |
|------|------|
| `Project 2.pdf` | Assignment specification, rubric, and deliverables. |
| `app5.R` | Earlier **R Shiny** snapshot: upload + cleaning only (superseded by `app.R` for full workflow). |
| `app.py` | **Python Shiny** app: upload + cleaning only. |
| `draft.R` | Earlier full **R** prototype (ggplot static EDA); `app.R` replaces it with Plotly and expanded cleaning. |
| `EDA.ipynb` | Notebook that **writes** a Python Shiny script for iterative development. |
| `data/` | Built-in datasets: **`bank-full.csv`**, **`titanic.csv`**. |

---

## What each app implements (detail)

### `app5.R` (R Shiny — partial)

**Completed (per file header):**

1. **Loading data** — Upload: CSV/TXT (comma, semicolon, tab), Excel, JSON, RDS. **Built-in:** Bank Marketing and Titanic loaded from `data/bank-full.csv` (semicolon-separated) and `data/titanic.csv`.
2. **Cleaning & preprocessing** — Treats placeholder strings as missing, optional `pdays == -1` as NA, numeric coercion, drop high-missing columns, duplicate rows, optional `id`/`position` duplicate handling, missing-value strategies (drop rows, median/mean/mode impute, or “do nothing” with categorical fill), IQR outlier options, z-score scaling, label encoding for selected categoricals. **Before/after** summary table, cleaned preview, missing summary, side-by-side boxplot (before vs after).

**Not present in this file:** feature engineering tab, EDA/visualization tab, export/download.

---

### `app.py` (Python Shiny — partial)

**Implemented:**

- **User Guide** tab describing the full intended workflow (including feature engineering and EDA), even though those tabs are **not** in the UI yet.
- **Data Upload & Preview** — Same multi-format idea as R: CSV/TXT delimiters, Excel (`pandas`), JSON, RDS (`pyreadr`). Built-ins: `bank-full` and `titanic` from `DATA_DIR` (`data/` under the working directory).
- **Cleaning & Preprocessing** — Pipeline `clean_data_pipeline()` mirrors the R logic: string cleaning, optional `pdays`, column missingness threshold, duplicates, id/position handling, missing strategies, outliers (IQR remove/cap/log), scaling (`sklearn`), categorical label encoding. Outputs: summaries, `DataGrid` previews, matplotlib **before/after boxplot** for the selected numeric variable.

**Not implemented in the UI:** feature engineering, EDA, export (the guide text is **aspirational** relative to the current `navset_tab`).

---

### `draft.R` (R Shiny — fuller prototype)

**Implemented:**

- **User Guide** (bilingual notes: English + Chinese workflow summary).
- **Data upload** or **built-in** choice: Bank Marketing / Titanic via `read_dataset()`; paths expect `data/bank_marketing.csv` and `data/titanic.csv` (**note:** other files use `bank-full.csv` — see *Data paths* below).
- **`clean_dataset()`** — Custom cleaning tuned for demo data (including **Billboard-style** `id` / `position` duplicate logic, median imputation, optional scaling/encoding).
- **`add_features()`** — Optional derived columns: `age_group`, `family_size`, `log_fare` / `log_duration`, `contacted_recent`, `campaign_prev_ratio` when relevant columns exist.
- **Synthetic built-in dataset** — `make_synthetic_billboard()` generates a large fake chart/audio-feature dataset with `hit`, `year`, etc., for demonstrating cleaning and EDA without external files.
- **`normalize_hit_column()`** — Maps `Survived` (Titanic) or `y` (Bank) to a binary `hit` for filters and plots.
- **EDA tab (4)** — **Filters:** `hit`, `year` range, numeric range on a chosen variable. **Plots (ggplot2, static `plotOutput`):** histogram, scatter (with `geom_smooth` + optional row sampling for size), boxplot by `hit`, correlation heatmap from selected numeric columns. **Printed stats:** row counts, `hit` table, `summary()` for chosen axis, Pearson correlation for scatter.
- **Export tab (5)** — Download cleaned CSV and featured CSV.

**Gaps / caveats:** EDA plots are **ggplot2 static** images, not Plotly/interactive; built-in Bank filename differs from `app5.R` / `app.py`.

---

### `EDA.ipynb`

- Contains cells that **write** a Python Shiny application file (e.g. `%%writefile STAT5243_PROJECT_2.py`) — a **notebook-driven workflow** to develop the same Python Shiny stack as `app.py`, not a separate analysis-only report.

---

## Data paths and layout

- **`app.R`**, **`app5.R`**, and **`app.py`** use **`data/bank-full.csv`** (semicolon-separated) and **`data/titanic.csv`** for built-in datasets.
- **`draft.R`** still references `data/bank_marketing.csv` — prefer **`app.R`** for a consistent layout.

---

## How to run (local)

**R — main app (`app.R`):**

```r
install.packages(c("shiny", "bslib", "DT", "dplyr", "readr", "readxl", "jsonlite",
                   "ggplot2", "scales", "digest", "plotly"))
shiny::runApp("app.R")
```

Set the working directory to the project folder (or open the project in RStudio and run the app).

**R — older prototypes:** `shiny::runApp("app5.R")` or `shiny::runApp("draft.R")`.

**Python — `app.py`:**

```bash
shiny run app.py
```

Dependencies include `shiny`, `pandas`, `numpy`, `pyreadr`, `openpyxl` (or `xlrd` for older Excel), `scikit-learn`, `matplotlib`.

---

## Suggested next steps for the team

- Deploy **`app.R`** to **shinyapps.io** (or keep Python `app.py` in sync if you submit both).
- Optionally port the full workflow from `app.py` to match `app.R` for parity.

---

## Assignment deliverables (from PDF)

Short report (1–2 pages), deployment link (e.g. **shinyapps.io**), GitHub with **commented** source and README — team should confirm current Courseworks requirements and deadlines.
