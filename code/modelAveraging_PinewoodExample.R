#------------------------------------------------------------------------
# Title: Pine Wood Example
# Author: William Murrah
# Description: using AICcmodavg data
# Created: Saturday, 09 December 2023
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------

library(AICcmodavg)
data(pine)

gx <- gls(y ~ x, pine, method = "ML")
gz <- gls(y ~ z, pine, method = "ML")

logLik(gx)
logLik(gz)
summary(gx)
summary(gz)
anova(gx)
car::Anova(gx)
var(resid(gx))
var(resid(gz))
gx$sigma^2
gz$sigma^2
(sum(resid(gx)^2))/42
anova(gx)
anova(gz)
AIC(gx, gz)


mods <- list(gx, gz)

model.sel(mods)
sw(mods)
