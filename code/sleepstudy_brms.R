#------------------------------------------------------------------------
# Title: sleepstudy simulation with brms
# Author: William Murrah
# Description: description
# Created: Monday, 18 March 2024
# R version: R version 4.3.3 (2024-02-29)
# Project(working) directory: /Users/wmm0017/Projects/Books/regressionModelingBook
#------------------------------------------------------------------------
library(lme4)
data(sleepstudy)
library(brms)
library(ggplot2)

hist(sleepstudy$Reaction)

psych::describe(sleepstudy)

aggregate(Reaction ~ Days, sleepstudy, function(x) c(Mean = mean(x), SD = sd(x)))

ggplot(sleepstudy, aes(x = Days, y = Reaction, group = Subject)) + geom_line()

form <- bf(Reaction ~ Days + (Days |Subject))
pr <- prior(normal(10, 10), class = "b") + 
  prior(normal(25, 4000), class = "sigma")
pp_mod <- brm(form, data = sleepstudy, prior = pr, sample_prior = "only")
print(pp_mod)
mod
pp_mod <- update(ppmod, formula. = ~. ~, prior = pr)

mod2 <- brm(form, data = sleepstudy, prior = pr)
mod2


postpred <- posterior_predict(pp_mod)

ppdat <- data.frame(
  Subject = sleepstudy$Subject,
  Days = sleepstudy$Days,
  Reaction = sleepstudy$Reaction,
  predReaction = colMeans(postpred)
)

ggplot(ppdat, aes(x = Days, y = predReaction, group = Subject)) + geom_line()

pp_check(mod2, type = "stat_2d")
