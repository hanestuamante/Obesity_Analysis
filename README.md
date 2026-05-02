<div align="center">
  <img src="https://raw.githubusercontent.com/hanestuamante/Obesity_Analysis/7fdf0f0beaa42dd7d169c69f135632cfc5d4b082/images/descripe.jpeg" alt="Project Overview" width="400">
</div>


# Obesity Analysis

Professionalized R data science project for analyzing obesity status and building an ordinal logistic regression model.

## Project Structure

```text
Obesity_Analysis/
├── data/                  # Raw input data
│   └── raw_data.csv
├── R/                     # Reusable R modules
│   ├── config.R
│   ├── utils.R
│   ├── data_preprocessing.R
│   ├── eda.R
│   ├── statistical_tests.R
│   ├── modeling.R
│   └── assumptions.R
├── scripts/               # Executable scripts
│   ├── run_pipeline.R
│   └── analysis.R
├── plots/                 # Generated figures
├── outputs/               # Generated tables, models, and text reports
├── legacy/                # Original monolithic source for reference
└── report/                # Final report PDF
```

## How to Run

From the project root:

```r
source("scripts/run_pipeline.R")
```

Or from terminal:

```bash
Rscript scripts/run_pipeline.R
```

## Main Outputs

- `outputs/clean_obesity_data.csv`: cleaned analysis dataset
- `outputs/statistical_results.txt`: descriptive and inferential statistics
- `outputs/model_performance.txt`: ordinal logistic regression summary and confusion matrix
- `outputs/assumptions_check.txt`: VIF, Brant test when available, and Durbin-Watson test
- `outputs/ordinal_logistic_model.rds`: fitted model object
- `plots/`: EDA plots, correlation matrix, ROC curves, and log-odds diagnostic plot

## Modeling Approach

The target variable `NObeyesdad` is collapsed into three ordered classes:

- `Normal`
- `Overweight`
- `Obesity`

The main model is ordinal logistic regression (`MASS::polr`) with a 70/30 stratified train-test split and seed `100` for reproducibility.

## Required R Packages

`dplyr`, `ggplot2`, `corrplot`, `car`, `FSA`, `dunn.test`, `MASS`, `caret`, `pROC`, `lmtest`, `forcats`, `tibble`, `scales`.

The `brant` package is optional. If it is not installed, the pipeline skips the Brant test and records that in `outputs/assumptions_check.txt`.

## Notes

The original monolithic script was preserved at `legacy/BTL_original.r`. The modular pipeline should be treated as the maintained version.
