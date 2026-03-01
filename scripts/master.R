# ------------------------------------- #
# Master script for running the analysis
# ------------------------------------- #

# To run all required scripts, simply execute this script

# Be sure that you have read and executed 00_packages.R prior to this script
# You may need 

# Ensuring empty environment
rm(list = ls())

# Installs renv if you have not already
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Insures all packages needed for the analysis are installed
renv::restore(prompt=FALSE)

# Loads the here package
library(here)

# Now running each script sequentially

message("\n>>> Starting Pipeline: 00 Packages")
source(here("scripts", "00_packages.R"), local = FALSE)

message(">>> Starting Pipeline: 01 Load Data")
source(here("scripts", "01_load_data.R"), local = FALSE)

message(">>> Starting Pipeline: 02 Variable Creation")
source(here("scripts", "02_variable_creation.R"), local = FALSE)

message(">>> Starting Pipeline: 03 Imputation")
source(here("scripts", "03_imputation.R"), local = FALSE)

message(">>> Starting Pipeline: 04 EDA")
source(here("scripts", "04_eda.R"), local = FALSE)

message(">>> Starting Pipeline: 05 Modelling")
source(here("scripts", "05_modelling.R"), local = FALSE)

message(">>> Starting Pipeline: 06 Diagnostics")
source(here("scripts", "06_diagnostics.R"), local = FALSE)

message(">>> Starting Pipeline: 07 Summaries")
source(here("scripts", "07_summaries.R"), local = FALSE)

message(">>> Starting Pipeline: 08 Prediction")
source(here("scripts", "08_prediction.R"), local = FALSE)

message(">>> Starting Pipeline: 09 Cross Validation")
source(here("scripts", "09_crossvalidation.R"), local = FALSE)

message(">>> Starting Pipeline: 10 Saving Assets")
source(here("scripts", "10_saving_assets.R"), local = FALSE)

message("\n--- ALL SCRIPTS COMPLETED SUCCESSFULLY ---")

# Running the .qmd file to render the thesis
message(">>> Rendering document...")
quarto_render(here("Dissertation.qmd"))

message("\n--- RENDER COMPLETE ---")
