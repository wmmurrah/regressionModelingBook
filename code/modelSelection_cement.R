#------------------------------------------------------------------------
# Title: Cement data
# Author: William Murrah
# Description: from Burnham and Anderson 2002
# Created: Thursday, 07 December 2023
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------

library(MuMIn)
library(AICcmodavg)
library(plyr)
library(nlme)

data("Cement")
cement <- Cement

# Global model
global <- glm(y ~ ., data = cement, x = TRUE, na.action = na.fail)

# Use dredge() function for all possible models
# NOT RECOMMENDED!!!! FOR PEDAGOGICAL PURPOSES ONLY
cmt_dr <- dredge(global)

# Bootstrap and get model proportions
bw <- bootWeights(get.models(cmt_dr, subset = TRUE), R = 10000, rank = "AICc")
cmt_dr$pi_i <- bw

# replicate Table 4.1 and include CI coverage
cmt_dr[1:8]
cmt_dr$w_cumsum <- cumsum(cmt_dr$weight) 
cmt_dr$pi_i_cumsum <- cumsum(cmt_dr$pi_i)
cmt_dr

# Get variable importance values
g0 <- get.models(cmt_dr, 1)[[1]]
summary(g0)
sw(cmt_dr[1:8])

# New data from textbook
newdat <- data.frame(X1 = 10, 
                     X2=50,
                     X3=10, 
                     X4=20)

y0 <- predict(g0, newdata = newdat, se.fit = TRUE)

# Working toward Table 4.2
mods <- get.models(cmt_dr[1:8], subset =TRUE)

# list of predicted values from first 8 models
preds <- list()
for(i in 1:length(mods)) {
  preds[[i]] <- predict(mods[[i]], newdata = newdat, se.fit = TRUE)
}  

tab <- data.frame(matrix(unlist(preds), ncol = 3, byrow = TRUE))
names(tab) <- c("yhat", "SE", "sigma")



tab$var <- tab$SE^2

print(tab, digits = 4)

modav <- model.avg(cmt_dr[1:5])
bw2 <- boot.wt(get.models(cmt_dr, subset = TRUE), second.ord = TRUE, nsim = 1e4)
tab$Yhatbar <- mean(tab$yhat)
tab$y0_ybar <- (tab$yhat - tab$Yhatbar)^2 # Not correct

print(tab, digits = 4)
summary(modav)

modavg2 <- modavg(mods, parm = c("X1", "X2"))
modavg2
