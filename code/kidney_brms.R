#------------------------------------------------------------------------
# Title: Kidney Example
# Author: William Murrah
# Description: Example using BRMS package for Bayesian Regression
# Created: Friday, 12 April 2024
# R version: R version 4.3.3 (2024-02-29)
# Project(working) directory: /Users/wmm0017/Projects/Books/regressionModelingBook
#------------------------------------------------------------------------

library(brms)
library(ggplot2)
data(kidney, package = "brms")

summary(kidney)

hist(kidney$time)




fit1 <- brm(time | cens(censored) ~ age*sex + disease + (1 + age | patient), 
            data = kidney, family = lognormal(),
            prior = c(set_prior("normal (0, 5)", class = "b"),
                      set_prior("cauchy(0,2)", class = "sd"),
                      set_prior("lkj(2)", class = "cor")), warmup = 1000,
            iter = 2000, chains = 4, control = list(adapt_delta = 0.98), 
            save_pars = (all = TRUE))

fit1

loo(fit1)
