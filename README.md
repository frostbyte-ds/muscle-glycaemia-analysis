# The Independent Role of Skeletal Muscle in Glycaemic Control: A Cross-Sectional Analysis of the NHANES 2011-2018 Data

**Author:** James Frost **Date:** July 20, 2025

------------------------------------------------------------------------

## Overview

This repository contains the complete R code and documentation for my Master's dissertation. The project investigates whether higher muscle mass, as a distinct component of body composition, is independently associated with better glycaemic control (measured by HbA1c).

The analysis uses data from four cycles of the National Health and Nutrition Examination Survey (NHANES) from 2011 to 2018. The project follows a reproducible workflow, from raw data loading and variable harmonisation to final modelling, diagnostics, and prediction.

------------------------------------------------------------------------

## How to Run This Project

To reproduce the analysis, please follow these steps:

1.  **Clone the Repository:** Clone this repository to your local machine.
2.  **Open the Project:** Open the `Dissertation.Rproj` file in RStudio. This will automatically set the correct working directory.
3.  **Install Dependencies:** Run the `00_packages.R` script to install and load all the required R packages.
4.  **Run the Analysis:** Execute the scripts in their numbered order. You can either run them one by one or use a master script to source them all. The scripts will:
    -   Load and clean the raw NHANES data (`01_load_data.R`).
    -   Create the final analytical variables, including harmonising confounders across survey cycles (`02_variable_creation.R`).
    -   Perform multiple imputation to handle missing data (`03_imputation.R`).
    -   Conduct exploratory data analysis (`04_eda.R`).
    -   Fit the final logistic regression models (`05_modelling.R`).
    -   Run model diagnostics (`06_diagnostics.R`).
    -   Generate summary tables (`07_summaries.R`).
    -   Perform cross-validation (`08_crossvalidation.R`).
    -   Generate final predictions and plots (`09_prediction.R`).
    -   Save all of the final figures and tables for the report (`10_saving_assets`).
5.  **Render the Dissertation:** Open `Dissertation.qmd` and click "Render" to generate the final PDF document of the dissertation, which embeds the results from the analysis.

------------------------------------------------------------------------

## Repository Structure

-   `Dissertation.Rproj`: The RStudio project file. Open this to get started.
-   `Dissertation.qmd`: The Quarto source file for the final dissertation write-up.
-   `Dissertation.pdf`: The final thesis.
-   `README.md`: This file, explaining the project.
-   `References.bib`: The bibliography file for all citations.
-   `.gitignore`: Specifies which files and folders to exclude from version control.
-   `scripts/`: A folder containing all the R scripts for the analysis, numbered in the order they should be run.
-   `template/`: A folder containing LaTeX files needed for the custom title page when rendering the Quarto document.
-   `Uni-Exeter-logo-portrait-1.png`: The University of Exeter logo needed for rendering the custom title page.

------------------------------------------------------------------------

## Data Availability

The data used in this analysis is publicly available from the **National Health and Nutrition Examination Survey (NHANES)**. The scripts in this repository are configured to download data from the respective survey cycles (2011-2012, 2013-2014, 2015-2016, 2017-2018). You can find more information at the [NHANES website](https://www.cdc.gov/nchs/nhanes/index.html).

------------------------------------------------------------------------

## Dependencies

This project requires R and RStudio. All necessary R packages are listed in the `scripts/00_packages.R` script and can be installed by running that file.
