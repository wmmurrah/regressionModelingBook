#------------------------------------------------------------------------
# Title: Weight and Height Example from earnings data
# Author: William Murrah
# Description: using data from Regression and Other stories
# Created: Sunday, 31 March 2024
# R version: R version 4.3.3 (2024-02-29)
# Project(working) directory: /Users/wmm0017/Projects/Books/regressionModelingBook
#------------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv"

earnings <- read.csv(file = url, header = TRUE)
names(earnings)

hist(earnings$height, breaks = "fd")
hist(earnings$weight, breaks = "fd")
hist(earnings$age, breaks = "fd")

plot(height ~ age, earnings)

plot(height ~ weight, earnings)
