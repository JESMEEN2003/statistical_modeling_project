# Statistical Modelling Project

## Project Overview
This repository contains the RStudio project, dataset, data preparation, exploratory analysis, and subsequent statistical modelling work for the Bank Customer Churn project.

## Project Structure

data/
- churn_prediction.csv              # Original raw dataset
- churn_prediction_clean.csv       # Common cleaned dataset

scripts/
- 01_data_prep.R                    # Common data preparation
- 02_data_exploration.R             # EDA
- Additional scripts                # Individual members' analysis

reports/
- Report-related materials

## Working with the Project

1. Open `statistical_modeling_project.Rproj` in RStudio.
2. Do not modify `data/churn_prediction.csv`.
3. Use `data/churn_prediction_clean.csv` as the common dataset for analysis.
4. Create a separate R script for your own task.
5. Keep scripts inside the `scripts/` folder.
6. Use relative file paths such as:
   `data/churn_prediction_clean.csv`
7. Do not independently change the agreed data-preprocessing decisions without discussing them with the group.
8. Add comments to your code so that other members can understand and reproduce your work.
9. Avoid overwriting another member's script.

## Data Preparation

The common preprocessing is documented in `01_data_prep.R`.

The cleaned dataset should be treated as the shared starting point for subsequent analysis and modelling.

## Collaboration

Each member should work in their own script and commit their changes clearly to GitHub. Changes that affect the common dataset or preprocessing should be discussed with the group before being made.
