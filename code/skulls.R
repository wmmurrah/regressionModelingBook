#------------------------------------------------------------------------
# Title: Skulls Example
# Author: William Murrah
# Description: from Claeskens and Hjort 2008
# Created: Thursday, 28 December 2023
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------
library(data.table)
library(MuMIn)
library(emmeans)
library(car)
library(interactions)

skulls <- read.table("https://perswww.kuleuven.be/~u0043181/modelselection/datasets/skulls_data.txt", skip = 7, header = FALSE, 
                   col.names = c("MB", "BH", "BL", "NH", "Year"))

skulls$era <- factor(skulls$Year)

table(skulls$era)


pairs(skulls[skulls$era==-4000,1:4], ylim = c(40, 150), xlim = c(40, 150))
pairs(skulls[skulls$era==150,1:4],ylim = c(40, 150), xlim = c(40, 150)) 
pa

psych::pairs.panels(skulls[skulls$era ==-4000, 1:4])
psych::pairs.panels(skulls[skulls$era ==150, 1:4])

aggregate(MB ~ era, skulls, mean)
aggregate(BH ~ era, skulls, mean)
aggregate(BL ~ era, skulls, mean)
aggregate(NH ~ era, skulls, mean)

skulls$id <- 1:150

skullslong <- melt(data = skulls, measure.vars = c("MB", "BH", "BL", "NH"), 
                   variable.name = "measure", value.name = "cm")
skullslong$t.lin <- as.numeric(skullslong$era)
  
aggregate(cm ~ era, by =  "measure", skullslong, mean, simplify = F)
tables::tabular(cm  * era ~ measure*(mean + sd), data = skullslong)



g0 <- lm(cm ~ era*measure, data = skullslong)
g1 <- lm(cm ~ era + measure, skullslong)
g2 <- lm(cm ~ measure, skullslong)
g3 <- lm(cm ~ measure*Year, skullslong)
mods <- list(g0, g1, g2, g3)

xtabs

modseltab <- model.sel(mods)
anova(mod)
summary(g0)

emmeans(g0, pairwise ~ era | measure)

hist(resid(g0))

plot(resid(g0) ~ predict(g0))

residualPlots(g0)
marginalModelPlots(g0)

emmip(g0, era ~ measure)
emmip(g0, measure ~ era)

cat_plot(g0, pred = "era", modx =  "measure", plot.points = TRUE, geom = "line")

anova(g0, g1)
anova(g1, g0)
anova(g0, g2)
anova(g2,g1)

anova(g2, g1)
anova(g2, g0)
