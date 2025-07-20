# ------------------------------------------ #
# Loading required packages for the analysis
# ------------------------------------------ #

# If you need to install any of the packages first, uncomment the following
# code and run it
# install.packages("nhanesA") 
# install.packages("survey") 
# install.packages("systemfonts") 
# install.packages("showtext") 
# install.packages("Amelia") 
# install.packages("parallel") 
# install.packages("mice") 
# install.packages("mitools") 
# install.packages("svydiags")
# install.packages("poliscidata") 
# install.packages("tidyverse") 
# install.packages("heiscore")
# install.packages("car") 
# install.packages("svydiags") 
# install.packages("miceadds") 
# install.packages("gt") 
# install.packages("arm") 
# install.packages("gridExtra") 
# install.packages("WeightedROC") 

library(nhanesA) # For NHANES data
library(survey) # For incorporation of survey design elements
library(systemfonts) # For fonts
library(showtext) # Also for fonts
library(Amelia) # For multiple imputation
library(parallel) # For parallel processing
library(mice) # For more imputation tools
library(mitools) # For even MORE imputation tools
library(svydiags) # For survey model diagnostics
library(poliscidata) # For fit.svyglm function
library(tidyverse) # For tidyverse functions
library(heiscore) # For HEI scores
library(car) # For useful analysis tools
library(svydiags) # For diagnostics of svyglms
library(miceadds) # For micombine.F
library(gt) # For tables
library(arm) # For binned residual plots
library(gridExtra) # For presentation of visualisations
library(WeightedROC) # For survey weighted ROC and AUC calculations 