#------------------------------------------------------------------------
# Title: brms epilepsy example
# Author: William Murrah
# Description: from https://paul-buerkner.github.io/brms/
# Created: Saturday, 30 March 2024
# R version: R version 4.3.3 (2024-02-29)
# Project(working) directory: /Users/wmm0017/Projects/Books/regressionModelingBook
#------------------------------------------------------------------------

library(brms)
library(ggplot2)
library(shinystan)
data("epilepsy")

hist(epilepsy$count)

ggplot(epilepsy, aes(x = visit, y = count, group = patient)) + geom_line() + 
  facet_grid(~ Trt)


fit1 <- brm(count ~ zAge + zBase * Trt + (1 | patient),
            data = epilepsy, family = poisson())
summary(fit1)
plot(conditional_effects(fit1, effects = "zBase:Trt"))

fit2 <- brm(count ~ zAge + zBase * Trt + (1|patient) + (1|obs),
            data = epilepsy, family = poisson())
summary(fit2)

loo(fit1, fit2)
