#------------------------------------------------------------------------
# Title: Train Tickets MLE example
# Author: William Murrah
# Description: from https://www.analyticsvidhya.com/blog/2018/07/introductory-guide-maximum-likelihood-estimation-case-study-r/
# Created: Thursday, 28 December 2023
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------
library(lubridate)

tickets <- read.csv("data/Train_Tickets.csv", header = TRUE)

tickets$Datetime <- dmy_hm(tickets$Datetime)

hist(tickets$Count, breaks = 50, probability = TRUE, 
     main = "Histogram of Count Variable")
lines(density(tickets$Count), col = "red", lwd = 2)
tickets$probs <- ave(tickets$Count, FUN = function(x) x/length(x))
tickets$norm <- ohenery::normalize(tickets$Count)
tickets$lognorm <- log(tickets$norm)
hist(tickets$probs, breaks = 50)
xprod <- prod(tickets$norm)
xlsum <- sum(log(tickets$norm))
which.max(tickets$norm)
which.max(log(tickets$norm))
plot(norm ~ I(-2*lognorm), tickets)
which.min(-2*tickets$lognorm)
