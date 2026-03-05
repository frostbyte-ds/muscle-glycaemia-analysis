# ------------------------------------- #
# Master script for running the analysis
# ------------------------------------- #

# To run all required analyses, simply execute this script

# Ensuring empty environment
rm(list = ls())

# Ensures all packages needed for the analysis are installed
renv::restore(prompt=FALSE)

# Installs tinytex if not already installed
if (!tinytex::is_tinytex()) {
  message("TinyTeX not found. Installing via Quarto...")
  system("quarto install tinytex")
}

# Now running each script sequentially
message("\n>>> Starting Pipeline: 00 Packages")
source("scripts/00_packages.R")

message(">>> Starting Pipeline: 01 Load Data")
source("scripts/01_load_data.R")

message(">>> Starting Pipeline: 02 Variable Creation")
source("scripts/02_variable_creation.R")

message(">>> Starting Pipeline: 03 Imputation")
source("scripts/03_imputation.R")

message(">>> Starting Pipeline: 04 EDA")
source("scripts/04_eda.R")

message(">>> Starting Pipeline: 05 Modelling")
source("scripts/05_modelling.R")

message(">>> Starting Pipeline: 06 Diagnostics")
source("scripts/06_diagnostics.R")

message(">>> Starting Pipeline: 07 Summaries")
source("scripts/07_summaries.R")

message(">>> Starting Pipeline: 08 Prediction")
source("scripts/08_prediction.R")

message(">>> Starting Pipeline: 09 Cross Validation")
source("scripts/09_crossvalidation.R")

message(">>> Starting Pipeline: 10 Saving Assets")
source("scripts/10_saving_assets.R")

message("\n--- ALL SCRIPTS COMPLETED SUCCESSFULLY ---")

# Running the .qmd file to render the thesis
message(">>> Rendering document...")
quarto_render("Dissertation.qmd")

message("\n--- RENDER COMPLETE ---")
