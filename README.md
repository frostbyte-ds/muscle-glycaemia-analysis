# The Independent Role of Skeletal Muscle in Glycaemic Control: A Cross-Sectional Analysis of the NHANES 2011-2018 Data

**Author:** James Frost **Date:** August 28, 2025

[![DOI](https://zenodo.org/badge/1022834638.svg)](https://doi.org/10.5281/zenodo.16990116)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![License: CC BY-NC-ND 4.0](https://img.shields.io/badge/License-CC%20BY--NC--ND%204.0-lightgrey.svg)

## How to Cite
Frost, J.A. (2025). *The Independent and Stage-Specific Role of Skeletal Muscle in Glycaemic Control: A Cross-Sectional Analysis of the NHANES 2011–2018 Data*. MSc Dissertation, University of Exeter. DOI: https://doi.org/10.5281/zenodo.16990116


------------------------------------------------------------------------

## Overview

This repository contains the complete R code and documentation for my Master's dissertation. The project investigates whether higher muscle mass, as a distinct component of body composition, is independently associated with better glycaemic control (measured by HbA1c).

The analysis uses data from four cycles of the National Health and Nutrition Examination Survey (NHANES) from 2011 to 2018. The project follows a reproducible workflow, from raw data loading and variable harmonisation to final modelling, diagnostics, and prediction.

------------------------------------------------------------------------

## How to Run This Project

To reproduce the analysis, please follow these steps:

1.  **Clone the Repository:** Clone this repository to your local machine.
2.  **Open the Project:** Open the `muscle-glycaemia-analysis.Rproj` file in RStudio. This will automatically set the correct working directory.
3.  **Execute the Scripts:** Open and execute the `master.R` script. This will run all scripts required for the analysis (00-10) and render the manuscript as a PDF file.

------------------------------------------------------------------------

## Repository Structure

-   `muscle-glycaemia-analysis.Rproj`: The RStudio project file. Open this to get started.
-   `Manuscript.qmd`: The Quarto source file for the final write-up.
-   `MSc Thesis.pdf`: The final thesis as submitted for my MSc.
-   `README.md`: This file, explaining the project.
-   `References.bib`: The bibliography file for all citations.
-   `.gitignore`: Specifies which files and folders to exclude from version control.
-   `scripts/`: A folder containing all the R scripts for the analysis, numbered in the order they should be run.
-   `template/`: A folder containing LaTeX files needed for the custom title page when rendering the Quarto document.
-   `renv/`: A folder containing the local R library and environment files managed by the renv package, ensuring reproducibility.
-   `.Rprofile`: A configuration file that runs automatically when the R project is opened (used here to initialise the renv environment).
-   `renv.lock`: A lockfile that records the exact versions of all R packages used in this project to guarantee absolute reproducibility.
-   `acm.csl`: A Citation Style Language (CSL) file that defines the formatting style for the bibliography (Association for Computing Machinery style).
-   `Uni-Exeter-logo-portrait-1.png`: The University of Exeter logo, also needed for the custom title page.
-   `onset10resids.png`, `onsetVarResids.png`, `prog4resids.png`, `progVarResids.png`: Residual plots included as figures in the final manuscript.

------------------------------------------------------------------------

## Data Availability

The data used in this analysis is publicly available from the **National Health and Nutrition Examination Survey (NHANES)**. The scripts in this repository are configured to download data from the respective survey cycles (2011-2012, 2013-2014, 2015-2016, 2017-2018). You can find more information at the [NHANES website](https://www.cdc.gov/nchs/nhanes/index.html).

------------------------------------------------------------------------

## Dependencies

This project requires R and RStudio. All necessary R packages are automatically installed and used as needed upon running the analysis via the `master.R` script.

## License

### Code
The code in this repository is licensed under the [MIT License](LICENSE.md).

### Text / Documentation
The text, figures, and documentation in this repository are licensed under [Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International](https://creativecommons.org/licenses/by-nc-nd/4.0/).

This repository contains original work developed as part of my MSc dissertation at the University of Exeter, 2025.
