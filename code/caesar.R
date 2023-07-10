#------------------------------------------------------------------------
# Title: caesar anthropometric data
# Author: William Murrah
# Description: https://data.world/andy/caesar and
#.             https://www.kaggle.com/datasets/thedevastator/3-d-anthropometry-measurements-of-human-body-sur
# Created: Saturday, 08 July 2023
# R version: R version 4.3.1 (2023-06-16)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------

library(car)
caesar <- read.csv("data/caesar.csv", header = TRUE, 
                   stringsAsFactors = TRUE)
caesar <- caesar[-1, ]

# Calculate BMI
#weight (lb) / [height (in)]2 x 703
caesar$bmi <- with(caesar, weight/height^2*703)
mod <- lm(height ~ reported_height, data = caesar)
summary(mod)




kneemod <- lm(height ~ knee_height + age + gender, data = caesar)
summary(kneemod)

with(caesar, cor(height, knee_height, use = "pairwise.complete.obs"))

plot(height ~ knee_height, data = caesar, col = caesar$gender)

scatterplot(height ~ reported_height, data = caesar, 
            id = list(method = "mahal", n = 12))

caesar[2112, ]


hwmod1 <- lm(height ~ poly(weight, 2, raw = TRUE), data = caesar)
hwmod2 <- lm(height ~ weight, caesar)
anova(hwmod1, hwmod2)
anova(hwmod1)
summary(hwmod1)
summary(hwmod2)

scatterplot(height ~ age, caesar)
