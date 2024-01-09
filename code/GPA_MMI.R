#------------------------------------------------------------------------
# Title: Model Selection and Multimodel Inference with Linear Regression
# Author: William Murrah
# Description: Example from Burnham and Anderson 2002 using GPA
# Created: Wednesday, 03 January 2024
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------

library(MuMIn)
library(nlme)
library(psych)

data("GPA")
gpa <- GPA
rm(GPA)
hist(gpa$y)
boxplot(gpa$y)

pairs.panels(gpa)

g0 <- gls(y ~ ., gpa, method = "ML")
summary(g0)

dmods <- dredge(global.model = g0)
dmods

round(cor(gpa),2)
