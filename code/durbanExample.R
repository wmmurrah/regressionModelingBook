#------------------------------------------------------------------------
# Title: Durban Pesticide Example
# Author: William Murrah
# Description: From Burnham and Anderson 2002
# Created: Thursday, 30 November 2023
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------
library(nlme)
pesticide <- read.table("data/Durban.txt", header = TRUE)

g0 <- gls(durban ~ water * radioactivity + fish, data = pesticide)

summary(g0)
