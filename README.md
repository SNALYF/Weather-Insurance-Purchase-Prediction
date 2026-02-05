# China Village Insurance Take-up Analysis

This project analyzes the determinants of insurance take-up in Chinese villages using various econometric and machine learning models, including Logit regression, Lasso regularization, and Random Forest.

## Project Structure

The project relies on an automated pipeline to manage data, scripts, and outputs.

```text
project_root/
├── data/                 # Contains data files (.dta, .csv, .RData)
├── scripts/              # R analysis scripts
│   ├── China village insurance take up.r
│   ├── Test.r
│   └── ECON 124 Empirical Project first try.r
├── docs/                 # Documentation and references
├── output/               # Generated plots and tables
├── archive/              # Archived old files and logs
├── run_pipeline.R        # Master script to execute the entire analysis
├── setup.R               # Script to install dependencies
└── README.md             # This file
```

## Prerequisites

Ensure you have **R** installed on your system.
The analysis requires several R packages, which can be automatically installed using the setup script.

## How to Run

### 1. Setup Environment
If this is your first time running the project, execute the setup script to install necessary packages (`readstata13`, `gamlr`, `dplyr`, `ranger`, `stargazer`, etc.) and ensure the directory structure is correct.

```bash
Rscript setup.R
```

### 2. Execute Pipeline
To run the full analysis pipeline, execute the master script. This will sequentially run all analysis scripts located in the `scripts/` folder.

```bash
Rscript run_pipeline.R
```

## Output

All results are automatically saved to the `output/` directory.
- **Tables**: Regression results (`.tex`, `.txt`)
- **Plots**: ROC curves, Lasso paths, Variable importance plots (`.png`)

## Scripts Description

- **`China village insurance take up.r`**: Main analysis script performing Data Cleaning, Logit Regression, Lasso (IS/OOS), and Random Forest to predict insurance take-up.
- **`Test.r`**: Similar analysis framework, possibly for robustnes checks or testing on subsets.
- **`ECON 124 Empirical Project first try.r`**: Preliminary analysis script.

## Notes
- Ensure all data files (e.g., `0422analysis.dta`) are present in the `data/` directory before running the pipeline.
