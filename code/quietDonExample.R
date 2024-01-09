#------------------------------------------------------------------------
# Title: Not So Quiet Don
# Author: William Murrah
# Description: example from Claeskens and Hjort on who wrote The Quiet Don
# Created: Monday, 01 January 2024
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------


don <- read.table(file = "https://perswww.kuleuven.be/~u0043181/modelselection/datasets/quietdon_2.txt", header = FALSE, col.names = c("id","Shall", "Kall", "TDall"))
