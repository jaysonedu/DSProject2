from shiny import App, ui, reactive, render
import pandas as pd
import numpy as np
import os
import json
import tempfile
import pyreadr
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt

# =========================================================
# App paths
# =========================================================
APP_DIR = os.getcwd()
DATA_DIR = os.path.join(APP_DIR, "data")

# =========================================================
# Helper functions
# =========================================================
def read_uploaded_data(fileinfo):
    path = fileinfo["datapath"]
    name = fileinfo["name"].lower()
    ext = name.split(".")[-1]

    if ext in ["csv", "txt"]:
        # try comma, then semicolon, then tab
        try:
            df = pd.read_csv(path)
            if df.shape[1] > 1:
                return df
        except Exception:
            pass

        try:
            df = pd.read_csv(path, sep=";")
            if df.shape[1] > 1:
                return df
        except Exception:
            pass

        try:
            df = pd.read_csv(path, sep="\t")
            return df
        except Exception:
            pass

        raise ValueError("Unable to parse the CSV/TXT file. Please check the delimiter.")

    if ext in ["xlsx", "xls"]:
        return pd.read_excel(path)

    if ext == "json":
        with open(path, "r", encoding="utf-8") as f:
            obj = json.load(f)
        return pd.DataFrame(obj)

    if ext == "rds":
        result = pyreadr.read_r(path)
        return list(result.values())[0]

    raise ValueError("Unsupported file format. Please upload a CSV, XLSX, JSON, or RDS file.")


def get_builtin(choice: str) -> pd.DataFrame:
    if choice == "bank-full":
        path = os.path.join(DATA_DIR, "bank-full.csv")
        if not os.path.exists(path):
            raise FileNotFoundError("Built-in Bank Marketing dataset file not found.")
        return pd.read_csv(path, sep=";")

    if choice == "titanic":
        path = os.path.join(DATA_DIR, "titanic.csv")
        if not os.path.exists(path):
            raise FileNotFoundError("Built-in Titanic dataset file not found.")
        return pd.read_csv(path)

    raise ValueError("Unknown built-in dataset.")


def clean_char_na(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    for col in df.columns:
        if pd.api.types.is_object_dtype(df[col]) or pd.api.types.is_string_dtype(df[col]):
            s = df[col].astype("string").str.strip()
            s = s.replace("", pd.NA)
            s = s.replace(
                {
                    "na": pd.NA,
                    "NA": pd.NA,
                    "null": pd.NA,
                    "NULL": pd.NA,
                    "unknown": pd.NA,
                    "Unknown": pd.NA,
                    "?": pd.NA,
                }
            )
            df[col] = s
    return df


def coerce_numeric_if_possible(df: pd.DataFrame, min_numeric_ratio: float = 0.7) -> pd.DataFrame:
    df = df.copy()
    for col in df.columns:
        if (
            pd.api.types.is_object_dtype(df[col])
            or pd.api.types.is_string_dtype(df[col])
            or pd.api.types.is_bool_dtype(df[col])
        ):
            parsed = pd.to_numeric(
                df[col].astype(str).str.replace(r"[^0-9\.\-]", "", regex=True),
                errors="coerce",
            )
            ratio = parsed.notna().mean()
            if pd.notna(ratio) and ratio >= min_numeric_ratio:
                df[col] = parsed
    return df


def get_mode(series: pd.Series):
    s = series.dropna()
    if s.empty:
        return np.nan
    return s.mode(dropna=True).iloc[0]


def safe_first_non_missing(series: pd.Series):
    s = series.dropna()
    if s.empty:
        return np.nan
    s = s[s.astype(str).str.strip() != ""]
    if s.empty:
        return np.nan
    return s.iloc[0]


def safe_scale(series: pd.Series):
    if not pd.api.types.is_numeric_dtype(series):
        return series
    if series.dropna().nunique() <= 1:
        return series
    scaler = StandardScaler()
    out = series.copy()
    mask = out.notna()
    out.loc[mask] = scaler.fit_transform(out.loc[mask].values.reshape(-1, 1)).flatten()
    return out


def dataset_info(df: pd.DataFrame) -> pd.DataFrame:
    num_vars = sum(pd.api.types.is_numeric_dtype(df[c]) for c in df.columns)
    cat_vars = df.shape[1] - num_vars
    return pd.DataFrame(
        {
            "Metric": [
                "Number of Rows",
                "Number of Columns",
                "Numeric Variables",
                "Categorical Variables",
            ],
            "Value": [df.shape[0], df.shape[1], num_vars, cat_vars],
        }
    )

def make_structure_summary(df: pd.DataFrame) -> pd.DataFrame:
    return pd.DataFrame(
        {
            "Variable": df.columns,
            "Type": [str(df[c].dtype) for c in df.columns],
            "Missing_Count": [int(df[c].isna().sum()) for c in df.columns],
            "Missing_Percent": [f"{df[c].isna().mean() * 100:.2f}%" for c in df.columns],
        }
    )

def handle_outliers(df: pd.DataFrame, var: str, method: str) -> pd.DataFrame:
    df = df.copy()
    if var not in df.columns or not pd.api.types.is_numeric_dtype(df[var]) or method == "None":
        return df

    x = df[var]
    if x.dropna().empty:
        return df

    q1 = x.quantile(0.25)
    q3 = x.quantile(0.75)
    iqr_val = q3 - q1
    lower = q1 - 1.5 * iqr_val
    upper = q3 + 1.5 * iqr_val

    if method == "Remove by IQR":
        df = df[(x.between(lower, upper)) | (x.isna())]
    elif method == "Cap by IQR":
        df[var] = x.clip(lower=lower, upper=upper)
    elif method == "Log transform":
        if (x.dropna() >= 0).all():
            df[var] = np.log1p(x)

    return df


def clean_data_pipeline(
    raw_df: pd.DataFrame,
    unknown_as_na: bool,
    pdays_as_na: bool,
    coerce_numeric_flag: bool,
    missing_thr: float,
    missing_method: str,
    remove_dup_rows: bool,
    handle_dup_ids: bool,
    outlier_var: str | None,
    outlier_method: str,
    scale_numeric_flag: bool,
    encode_cats: bool,
    cat_cols_selected: list[str],
) -> pd.DataFrame:
    df = raw_df.copy()

    # 1) Inconsistency handling
    for col in df.columns:
        if pd.api.types.is_object_dtype(df[col]) or pd.api.types.is_string_dtype(df[col]):
            df[col] = df[col].replace(r"^\s*$", np.nan, regex=True)

    if unknown_as_na:
        df = clean_char_na(df)

    if coerce_numeric_flag:
        df = coerce_numeric_if_possible(df)

    if pdays_as_na and "pdays" in df.columns and pd.api.types.is_numeric_dtype(df["pdays"]):
        df.loc[df["pdays"] == -1, "pdays"] = np.nan

    # 2) Drop columns with high missingness
    miss_rate = df.isna().mean()
    keep_cols = miss_rate[miss_rate <= missing_thr].index
    df = df[keep_cols].copy()

    # 3) Remove duplicate rows
    if remove_dup_rows:
        df = df.drop_duplicates()

    # 4) Handle duplicate/missing id if id & position exist
    lower_map = {c.lower(): c for c in df.columns}
    if handle_dup_ids and "id" in lower_map and "position" in lower_map:
        id_col = lower_map["id"]
        pos_col = lower_map["position"]

        df[id_col] = df.groupby(pos_col)[id_col].transform(
            lambda s: s.fillna(safe_first_non_missing(s))
        )
        df = df.drop_duplicates(subset=[id_col], keep="first")

    # 5) Missing value handling
    if missing_method == "Remove rows with missing values":
        df = df.dropna()

    elif missing_method == "Median impute numeric columns":
        num_cols = [c for c in df.columns if pd.api.types.is_numeric_dtype(df[c])]
        for col in num_cols:
            if not df[col].dropna().empty:
                df[col] = df[col].fillna(df[col].median())

    elif missing_method == "Mean impute numeric columns":
        num_cols = [c for c in df.columns if pd.api.types.is_numeric_dtype(df[c])]
        for col in num_cols:
            if not df[col].dropna().empty:
                df[col] = df[col].fillna(df[col].mean())

    elif missing_method == "Mode impute categorical columns":
        cat_cols = [c for c in df.columns if not pd.api.types.is_numeric_dtype(df[c])]
        for col in cat_cols:
            df[col] = df[col].fillna(get_mode(df[col]))

    elif missing_method == "Do nothing":
        pass

    # 6) Outlier handling
    if outlier_var:
        df = handle_outliers(df, outlier_var, outlier_method)

    # 7) Scale numeric columns
    if scale_numeric_flag:
        num_cols = [c for c in df.columns if pd.api.types.is_numeric_dtype(df[c])]
        for col in num_cols:
            df[col] = safe_scale(df[col])

    # 8) Encode selected categorical columns
    if encode_cats and cat_cols_selected:
        selected_cols = [c for c in cat_cols_selected if c in df.columns]
        for col in selected_cols:
            if not pd.api.types.is_numeric_dtype(df[col]):
                df[col] = pd.factorize(df[col])[0].astype(float)

    return df


# =========================================================
# UI
# =========================================================
app_ui = ui.page_fluid(
    ui.h2("Project 2 - Python Shiny Data Explorer"),

    ui.navset_tab(
        ui.nav_panel(
            "User Guide",
            ui.p(
                "Workflow: Upload your own dataset or select a built-in dataset → "
                "Clean & preprocess → Feature engineering → EDA → Export results."
            ),
            ui.hr(),
            ui.h4("Overview"),
            ui.tags.ul(
                ui.tags.li("Loading Datasets: Supports various format uploads and provides built-in datasets (e.g., Bank Marketing and Titanic)."),
                ui.tags.li("Data Cleaning & Preprocessing: Handle inconsistencies, missing values, duplicates, and apply optional scaling and encoding."),
                ui.tags.li("Feature Engineering: Create new features such as age_group, balance_level, contacted_before, and other derived variables."),
                ui.tags.li("EDA (Exploratory Data Analysis): Interactive filtering, visualization, and dynamic statistical summaries."),
                ui.tags.li("Export: Download cleaned and transformed datasets for reproducibility and reporting."),
            ),
        ),

        ui.nav_panel(
            "1) Data Upload & Preview",
            ui.layout_sidebar(
                ui.sidebar(
                    ui.h4("Load Dataset"),
                    ui.input_radio_buttons(
                        "data_source",
                        "Choose data source:",
                        {"upload": "Upload File", "builtin": "Built-in Dataset"},
                        selected="upload",
                    ),

                    ui.panel_conditional(
                        "input.data_source === 'upload'",
                        ui.input_file(
                            "upload",
                            "Upload dataset (CSV / XLSX / JSON / RDS)",
                            accept=[".csv", ".txt", ".xlsx", ".xls", ".json", ".rds"],
                        ),
                    ),

                    ui.panel_conditional(
                        "input.data_source === 'builtin'",
                        ui.input_select(
                            "builtin_choice",
                            "Select a built-in dataset:",
                            {"bank-full": "Bank Marketing", "titanic": "Titanic"},
                        ),
                    ),

                    ui.input_action_button("load_btn", "Load / Reset"),
                    ui.hr(),
                    ui.input_checkbox("strings_to_factor", "Convert character columns to categorical", True),
                    ui.input_checkbox("show_preview", "Show data preview", True),
                    ui.hr(),
                    ui.p("Click 'Load / Reset' to refresh the dataset and update the downstream tabs."),
                    width=320,
                ),

                ui.h4("Dataset Overview"),
                ui.output_data_frame("data_info"),
                ui.hr(),

                ui.panel_conditional(
                    "input.show_preview === true",
                    ui.h4("Raw Data Preview"),
                    ui.output_data_frame("raw_preview"),
                    ui.hr(),
                ),

                ui.h4("Variable Structure & Raw Missing Summary"),
                ui.p("Placeholder values such as 'unknown' are treated during the cleaning stage."),
                ui.output_data_frame("structure_summary"),
                ui.hr(),
                ui.h4("Raw Summary"),
                ui.output_text_verbatim("raw_summary"),
            ),
        ),

        ui.nav_panel(
            "2) Cleaning & Preprocessing",
            ui.layout_sidebar(
                ui.sidebar(
                    ui.h4("Inconsistency Handling"),
                    ui.input_checkbox("unknown_as_na", "Treat 'unknown' / '?' / 'NULL' as missing", True),
                    ui.input_checkbox("pdays_as_na", "Treat pdays = -1 as missing", True),
                    ui.input_checkbox("coerce_numeric", "Convert numeric-like text columns to numeric", True),

                    ui.hr(),
                    ui.h4("Missing Data"),
                    ui.input_slider("missing_thr", "Drop columns with missing rate >", 0.50, 0.99, 0.95, step=0.01),
                    ui.input_select(
                        "missing_method",
                        "Missing value strategy:",
                        {
                            "Do nothing": "Do nothing",
                            "Remove rows with missing values": "Remove rows with missing values",
                            "Median impute numeric columns": "Median impute numeric columns",
                            "Mean impute numeric columns": "Mean impute numeric columns",
                            "Mode impute categorical columns": "Mode impute categorical columns",
                        },
                    ),

                    ui.hr(),
                    ui.h4("Duplicate / ID Handling"),
                    ui.input_checkbox("remove_dup_rows", "Remove duplicate rows", False),
                    ui.input_checkbox("handle_dup_ids", "Handle duplicate/missing id (if id & position exist)", True),

                    ui.hr(),
                    ui.h4("Outlier Handling"),
                    ui.output_ui("outlier_var_ui"),
                    ui.input_select(
                        "outlier_method",
                        "Outlier strategy:",
                        {
                            "None": "None",
                            "Remove by IQR": "Remove by IQR",
                            "Cap by IQR": "Cap by IQR",
                            "Log transform": "Log transform",
                        },
                    ),

                    ui.hr(),
                    ui.h4("Transformations"),
                    ui.input_checkbox("scale_numeric", "Scale numeric features (z-score)", False),
                    ui.input_checkbox("encode_cats", "Encode categorical columns (label encode)", False),
                    ui.output_ui("cat_cols_ui"),

                    ui.hr(),
                    ui.input_action_button("clean_btn", "Apply Cleaning"),
                    width=320,
                ),

                ui.h4("Before vs After Summary"),
                ui.output_data_frame("cleaning_summary"),
                ui.hr(),

                ui.h4("Cleaned Preview"),
                ui.output_data_frame("clean_preview"),
                ui.hr(),

                ui.h4("Missing Summary After Cleaning"),
                ui.output_data_frame("cleaned_missing_summary"),
                ui.hr(),

                ui.h4("Outlier Preview"),
                ui.output_plot("outlier_plot", height="300px"),
                ui.hr(),

                ui.h4("Cleaning Summary"),
                ui.output_text_verbatim("clean_summary"),
            ),
        ),
    ),
)

# =========================================================
# Server
# =========================================================
def server(input, output, session):
    @reactive.calc
    @reactive.event(input.load_btn)
    def raw_data():
        df = None

        if input.data_source() == "upload":
            files = input.upload()
            if files is None or len(files) == 0:
                raise ValueError("Please upload a file.")
            df = read_uploaded_data(files[0])

        elif input.data_source() == "builtin":
            df = get_builtin(input.builtin_choice())

        # Convert empty strings to NA immediately after loading
        for col in df.columns:
            if pd.api.types.is_object_dtype(df[col]) or pd.api.types.is_string_dtype(df[col]):
                df[col] = df[col].replace(r"^\s*$", np.nan, regex=True)

        if input.strings_to_factor():
            for col in df.columns:
                if pd.api.types.is_object_dtype(df[col]) or pd.api.types.is_string_dtype(df[col]):
                    df[col] = df[col].astype("category")

        return df

    @render.data_frame
    def data_info():
        df = raw_data()
        return render.DataGrid(dataset_info(df), width="100%")

    @render.data_frame
    def raw_preview():
        df = raw_data().head(10)
        return render.DataGrid(df, width="100%")

    @render.data_frame
    def structure_summary():
        df = raw_data()
        return render.DataGrid(make_structure_summary(df), width="100%")

    @render.text
    def raw_summary():
        df = raw_data()
        return f"Rows: {df.shape[0]}\nColumns: {df.shape[1]}\n\n{df.describe(include='all').to_string()}"

    @render.ui
    def cat_cols_ui():
        df = raw_data()
        cat_cols = [c for c in df.columns if not pd.api.types.is_numeric_dtype(df[c])]
        if len(cat_cols) == 0:
            return ui.p("No categorical columns available for encoding.")
        return ui.input_checkbox_group(
            "cat_cols_selected",
            "Categorical columns to encode:",
            choices={c: c for c in cat_cols},
            selected=cat_cols[: min(5, len(cat_cols))],
        )

    @render.ui
    def outlier_var_ui():
        df = raw_data()
        num_cols = [c for c in df.columns if pd.api.types.is_numeric_dtype(df[c])]
        if len(num_cols) == 0:
            return ui.p("No numeric variables available for outlier handling.")
        return ui.input_select(
            "outlier_var",
            "Select numeric variable:",
            {c: c for c in num_cols},
        )

    @reactive.calc
    @reactive.event(input.clean_btn)
    def clean_data():
        df = raw_data().copy()

        outlier_var = input.outlier_var() if len([c for c in df.columns if pd.api.types.is_numeric_dtype(df[c])]) > 0 else None
        cat_cols_selected = input.cat_cols_selected() if input.cat_cols_selected() is not None else []

        return clean_data_pipeline(
            raw_df=df,
            unknown_as_na=input.unknown_as_na(),
            pdays_as_na=input.pdays_as_na(),
            coerce_numeric_flag=input.coerce_numeric(),
            missing_thr=input.missing_thr(),
            missing_method=input.missing_method(),
            remove_dup_rows=input.remove_dup_rows(),
            handle_dup_ids=input.handle_dup_ids(),
            outlier_var=outlier_var,
            outlier_method=input.outlier_method(),
            scale_numeric_flag=input.scale_numeric(),
            encode_cats=input.encode_cats(),
            cat_cols_selected=cat_cols_selected,
        )

    @render.data_frame
    def cleaning_summary():
        before_df = raw_data()
        after_df = clean_data()
        summary_df = pd.DataFrame(
            {
                "Metric": ["Rows", "Columns", "Total Missing Values", "Duplicate Rows"],
                "Before": [
                    before_df.shape[0],
                    before_df.shape[1],
                    int(before_df.isna().sum().sum()),
                    int(before_df.duplicated().sum()),
                ],
                "After": [
                    after_df.shape[0],
                    after_df.shape[1],
                    int(after_df.isna().sum().sum()),
                    int(after_df.duplicated().sum()),
                ],
            }
        )
        return render.DataGrid(summary_df, width="100%")

    @render.data_frame
    def clean_preview():
        df = clean_data().head(10)
        return render.DataGrid(df, width="100%")

    @render.data_frame
    def cleaned_missing_summary():
        df = clean_data()
        return render.DataGrid(make_structure_summary(df), width="100%")

    @render.plot
    def outlier_plot():
        before_df = raw_data()
        after_df = clean_data()

        fig, axes = plt.subplots(1, 2, figsize=(10, 4))

        num_cols = [c for c in before_df.columns if pd.api.types.is_numeric_dtype(before_df[c])]
        if len(num_cols) == 0:
            axes[0].text(0.5, 0.5, "No numeric variables available.", ha="center", va="center")
            axes[0].set_axis_off()
            axes[1].set_axis_off()
            return fig

        var = input.outlier_var()
        if var not in before_df.columns or not pd.api.types.is_numeric_dtype(before_df[var]):
            axes[0].text(0.5, 0.5, "Please select a valid numeric variable.", ha="center", va="center")
            axes[0].set_axis_off()
            axes[1].set_axis_off()
            return fig

        axes[0].boxplot(before_df[var].dropna())
        axes[0].set_title("Before Cleaning")
        axes[0].set_ylabel(var)

        if var in after_df.columns and pd.api.types.is_numeric_dtype(after_df[var]):
            axes[1].boxplot(after_df[var].dropna())
            axes[1].set_title("After Cleaning")
            axes[1].set_ylabel(var)
        else:
            axes[1].text(0.5, 0.5, "Selected variable not available after cleaning.", ha="center", va="center")
            axes[1].set_axis_off()

        plt.tight_layout()
        return fig

    @render.text
    def clean_summary():
        before_df = raw_data()
        after_df = clean_data()
        return (
            "Cleaning completed successfully.\n\n"
            f"Rows before: {before_df.shape[0]}\n"
            f"Rows after:  {after_df.shape[0]}\n"
            f"Columns before: {before_df.shape[1]}\n"
            f"Columns after:  {after_df.shape[1]}\n"
            f"Total missing values before: {int(before_df.isna().sum().sum())}\n"
            f"Total missing values after:  {int(after_df.isna().sum().sum())}\n"
            f"Duplicate rows before: {int(before_df.duplicated().sum())}\n"
            f"Duplicate rows after:  {int(after_df.duplicated().sum())}"
        )


app = App(app_ui, server)