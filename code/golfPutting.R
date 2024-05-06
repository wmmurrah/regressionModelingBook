#------------------------------------------------------------------------
# Title: Golf putting data example
# Author: William Murrah
# Description: From Gelman et. al. (2020), Bayesian Workflow article
# Created: Sunday, 31 March 2024
# R version: R version 4.3.3 (2024-02-29)
# Project(working) directory: /Users/wmm0017/Projects/Books/regressionModelingBook
#------------------------------------------------------------------------
library(rstanarm)
golf <- read.table("data/golf_data.txt",
                   header = TRUE)


plot(y/n ~ x, golf)

plot(x ~ y , golf)


logist <- stan_glm(cbind(y, n-y) ~ x, data = golf, family = binomial(link = "logit"))

pred <- predict(logist)
summary(logist, digits = 2)

plot(logist)

plot(golf$x, pred, type = "l")

logit(pred)
exp(pred)
