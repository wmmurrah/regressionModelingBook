#------------------------------------------------------------------------
# Title: Model Selection with Boston Housing Data
# Author: William Murrah
# Description: Model selection with Information Theory
# Created: Thursday, 07 December 2023
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------
  
data(BostonHousing2, package = "mlbench")
bhdat <- BostonHousing2
names(bhdat)
?bhdat
?mlbench::BostonHousing2

hist(bhdat$cmedv, breaks = "fd")

pairs(bhdat)

table(bhdat$town)
