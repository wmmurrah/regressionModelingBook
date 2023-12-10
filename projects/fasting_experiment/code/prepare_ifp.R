#------------------------------------------------------------------------
# Title: Prepare Lean 
# Author: William Murrah
# Description: Import and clean data in wide format.
# Created: Sunday, 16 July 2023
# R version: R version 4.3.1 (2023-06-16)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------
library(readxl)
library(stringr)

# Read first two rows with variable names and time point.
ifpcolnames <- read_xlsx("projects/fasting_experiment/data/Lean_Archive_IFP.xlsx", 
                         sheet = 1, col_names = FALSE, n_max = 2)
# Prepend time point to variable names.
colnames <- paste(ifpcolnames[1, ], ifpcolnames[2, ], sep = "_")

# Replace illegal characters in names (most of them)
colnames <- str_replace_all(colnames, "NA_", "")  # remove NA's
colnames <- str_replace_all(colnames, " ", "")    # remove blank spaces
colnames <- str_replace_all(colnames, "-", "").   # remove dashes
colnames <- str_replace_all(colnames, "\\*", ".") # remove asterisks

# import data and assign new variable names
ifp <- read_xlsx("projects/fasting_experiment/data/Lean_Archive_IFP.xlsx",
                 skip = 2, 
                 col_names = colnames, sheet = 1)
# Remove temporary objects
rm(ifpcolnames, colnames)
