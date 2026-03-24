# Project 2 — Shiny data explorer

Course project for *Web Application Development and Deployment*. This repo is an R Shiny app for loading data, cleaning it, adding a few engineered features, exploring it with plots, and exporting CSVs.

## Running the main app

Use R 4.x with the working directory set to this folder (RStudio: open `app.R` and click **Run App**, or use the terminal from the project root).

Install packages once:

```r
install.packages(c(
  "shiny", "bslib", "DT", "dplyr", "readr", "readxl", "jsonlite",
  "ggplot2", "scales", "digest", "plotly"
))
```

Then:

```r
shiny::runApp("app.R")
```

`Rscript` works too if R is on your PATH:

```bash
Rscript -e "shiny::runApp('app.R', launch.browser = TRUE)"
```

Built-in datasets expect `data/bank-full.csv` and `data/titanic.csv`. You can also use the synthetic “Billboard” option in the app, which needs no files.

## What’s in the repo

| Path | Notes |
|------|--------|
| `app.R` | Main app: upload or built-ins, cleaning, feature engineering, Plotly EDA, export. |
| `data/` | Bank marketing (`bank-full.csv`, semicolon-separated) and Titanic (`titanic.csv`) for built-ins. |
| `app5.R` | Older R version — upload and cleaning only. |
| `draft.R` | Older full R draft; `app.R` is the one to run. |
| `app.py` | Python Shiny app — upload and cleaning only; not the full workflow. |
| `EDA.ipynb` | Notebook used to generate/edit Python Shiny code. |

## Hand-in (course)

The assignment asks for a short report, a deployed app link (e.g. shinyapps.io), and this repo with runnable code. Put the live URL and team contribution notes in the report; check Courseworks for the current deadline.
