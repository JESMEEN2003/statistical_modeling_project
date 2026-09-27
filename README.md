# Statistical Modelling Project

## Project Overview

This repository contains the RStudio project, dataset, data preparation, exploratory analysis, statistical modelling work, and report materials for the Bank Customer Churn Statistical Modelling project.

The repository is organized so that all members can work from the same cleaned dataset while keeping their individual analysis separate.

---

## Project Structure

```text
statistical_modeling_project/
│
├── data/
│   ├── churn_prediction.csv
│   └── churn_prediction_clean.csv
│
├── scripts/
│   ├── 01_data_prep.R
│   ├── 02_data_exploration.R
│   └── [individual member scripts]
│
├── reports/
│
└── statistical_modeling_project.Rproj
```

### Folder purposes

**`data/`**
Contains the original dataset and the common cleaned dataset.

**`scripts/`**
Contains the R scripts used for data preparation, EDA, statistical testing, modelling, and other analysis.

**`reports/`**
Contains report-related documents, figures, tables, and supporting materials.

**`.Rproj` file**
This is the RStudio project file. Always open the project through this file so that the correct working directory and relative file paths are maintained.

---

# How to Use the Project in RStudio

## Step 1 — Download the Repository

Open the GitHub repository and click:

**Code → Download ZIP**

Download the ZIP file to your computer.

## Step 2 — Extract the ZIP

Right-click the downloaded ZIP file and select:

**Extract All**

You will get a folder containing the complete project.

Move this folder somewhere convenient, for example:

```text
Desktop/
└── statistical_modeling_project/
```

Do not move individual files out of their folders.

---

## Step 3 — Open the Project in RStudio

Open the extracted project folder.

Double-click:

```text
statistical_modeling_project.Rproj
```

This should open the project directly in RStudio.

Alternatively, in RStudio you can use:

**File → Open Project → Browse**

and select:

```text
statistical_modeling_project.Rproj
```

### Important

Always open the `.Rproj` file rather than opening individual `.R` files by themselves.

This ensures that paths such as:

```r
data/churn_prediction_clean.csv
```

work correctly.

---

# How to Start Your Own Task

## Step 4 — Get the Cleaned Dataset

Use:

```text
data/churn_prediction_clean.csv
```

as the common dataset for your analysis.

The original file:

```text
data/churn_prediction.csv
```

must not be modified.

The common data preparation is documented in:

```text
scripts/01_data_prep.R
```

Do not independently change the common preprocessing unless the group discusses and agrees on the change.

---

## Step 5 — Create Your Own R Script

Do **not** work directly inside another member's script.

Instead, create your own `.R` file inside:

```text
scripts/
```

For example:

```text
03_statistical_tests.R
04_model_building.R
05_pca_analysis.R
06_bayesian_analysis.R
```

Use a name that clearly identifies your task.

At the beginning of your script, load the cleaned dataset using:

```r
data <- read.csv("data/churn_prediction_clean.csv")
```

Then perform your analysis in your own script.

---

# Rules for Consistency

### 1. Use the same cleaned dataset

Everyone should use:

```text
churn_prediction_clean.csv
```

as the common starting point.

### 2. Do not modify the raw dataset

Never overwrite or edit:

```text
churn_prediction.csv
```

### 3. Do not overwrite another member's script

Each member should work in their own `.R` file.

### 4. Use relative paths

Use:

```r
data/churn_prediction_clean.csv
```

Do not use computer-specific paths such as:

```text
C:/Users/YourName/Desktop/...
```

### 5. Keep your code reproducible

Add comments explaining the purpose of important sections and the decisions made.

### 6. Discuss changes to common preprocessing

If you believe the cleaning or preprocessing needs to change, discuss it with the group before creating a different version of the common dataset.

### 7. Keep report figures and tables organized

Save report-related outputs in the appropriate `reports/` folder rather than scattering files throughout the project.

---

# How to Submit Your Work Back to GitHub

Since we are using manual uploads, you do **not** need to use Git commands or GitHub Desktop.

## Step 1 — Finish Your Analysis

Work normally in RStudio and save your `.R` script.

For example:

```text
scripts/
└── 03_statistical_tests.R
```

## Step 2 — Go to the GitHub Repository

Open the project repository on GitHub.

Go into:

```text
scripts/
```

## Step 3 — Upload Your Script

Click:

**Add file → Upload files**

Select the R script you created.

For example:

```text
03_statistical_tests.R
```

Then click:

**Commit changes**

## Step 4 — Tell the Group

After uploading, send a message in the group chat saying that your script has been added.

Example:

> I've completed my analysis and uploaded `03_statistical_tests.R` to the `scripts` folder on GitHub. The analysis uses the common `churn_prediction_clean.csv` dataset.

---

# Important: Don't Upload Your Whole Project Again

After the initial project setup, members should **not upload the entire RStudio project folder again**.

Only upload the files you personally created or modified.

For example, if you created:

```text
scripts/04_model_building.R
```

upload that script.

Do not upload another copy of:

```text
data/
statistical_modeling_project.Rproj
```

or overwrite other members' scripts.

---

# Recommended Collaboration Workflow

```text
Download latest repository
        ↓
Open .Rproj in RStudio
        ↓
Use churn_prediction_clean.csv
        ↓
Create your own R script
        ↓
Complete your assigned task
        ↓
Save your script
        ↓
Upload only your script to GitHub
        ↓
Inform the group
```

Before starting new work, make sure you are using the **latest version of the repository**, so you don't accidentally work from an outdated copy.

---

# Current Common Files

### Data preparation

```text
01_data_prep.R
```

Contains the agreed missing-value handling and creates the common cleaned dataset.

### Exploratory data analysis

```text
02_data_exploration.R
```

Contains the initial EDA, including distributions, outliers, skewness, correlations, and comparisons by churn status.

These files should be treated as the foundation for the later analysis.

---

# Final Principle

The main goal is to ensure that every member is:

**using the same cleaned data + working in their own script + documenting their work + uploading only their own work back to the shared repository.**
