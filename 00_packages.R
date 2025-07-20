# ------------------------------------------ #
# Loading required packages for the analysis
# ------------------------------------------ #

library(nhanesA) # For NHANEs data
library(survey) # For incorporation of survey design elements
library(systemfonts) # For fonts
library(showtext) # Also for fonts
library(VIM) # For testing for multicollinearity 
library(Amelia) # For multiple imputation
library(parallel) # For parallel processing
library(mice) # For more imputation tools
library(mitools) # For even MORE imputation tools
library(broom)
library(nnet) # For multinomial regression
library(svydiags) # For survey model diagnostics
library(poliscidata) # For fit.svyglm function
library(tidyverse) # For tidyverse functions
library(heiscore) # For HEI scores
library(car) # For useful analysis tools
library(svydiags)
library(miceadds) # For micombine.F
library(gt) # For tables
library(arm)
library(gridExtra) # For presentation of visualisations
library(WeightedROC) # For survey weighted ROC and AUC calculations 