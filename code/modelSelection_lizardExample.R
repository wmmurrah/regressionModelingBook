#------------------------------------------------------------------------
# Title: Lizards Example of Model Selection
# Author: William Murrah
# Description: description
# Created: Sunday, 10 December 2023
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------



library(AICcmodavg)
library(epitools)
library(tables)
library(MuMIn)
data("lizards")

lizards$Time <- factor(lizards$Time, levels = c("morning", "midday", "afternoon"))

liz <- lizards[rep(seq_len(nrow(lizards)), lizards$Counts), 1:5]



with(liz, table(Insolation, Diameter, Height, Time, Species))

ftable(xtabs( ~  Insolation + Diameter + Height + Time + Species, data = liz))

tabular(Insolation * Diameter * Height ~  Time * Species, data = liz)

AICc(g0)
g0 <- glm(Counts ~ Diameter + Height + Time + Species, family = poisson,
          data = lizards)
summary(g0)

gbase <- glm(Counts ~ (Diameter + Height + Time + Species)^2, 
            family = poisson, data = lizards)
g3 <- update(gbase, . ~ . - Diameter:Time)
summary(gbase)
AICc(g0, gbase)
AIC(g0, gbase)
AICc(g0)
AICc(gbase)

modlist <- list(g0=g0, gbase=gbase, g3=g3)
aictab(modlist)
model.sel(modlist)
