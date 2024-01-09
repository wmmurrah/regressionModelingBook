#------------------------------------------------------------------------
# Title: Sakamoto 10 simulated data sets 
# Author: William Murrah
# Description: From Sakamoto, Y. Ishiguro, M. & Kitagawa, G. (1986). 
#                  Akaike Information Statistics, Reidel.
# Created: Friday, 22 December 2023
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------

library(nlme)
library(MuMIn)

# Simulation parameters
x = seq(0, 1, .05)
# y = c(.125,.156,.193,-.032,-.075,-.064,.006,
#       -.135,.105,.131,.154,.114,-.094,.215,
#       .035,.327,.061,.383,.357,.605,.499)
errorVar <- .01

# function to simulate sinlge data:
simdata <- function(xvar, sigma = errorVar) {
  y <- rnorm(n = 21, mean = exp((xvar - 0.3)^2) - 1, sd = sigma)
  return(y)
}

# Simulate y values for all 10 data sets
sakamoto <- vector("list", 10)
set.seed(1234)
for(d in 1:10) {
  sakamoto[[d]] <- simdata(xvar = x)
}


# Create data.frames with y and x values for models.

sakamotoSimData <- data.frame(
  d = factor(rep(1:10, each = 21), labels = paste0("d", 1:10)),
  x = rep(x, 10),
  y = unlist(sakamoto)
)

# 
sakamotoSimData <- groupedData(y ~ x | d, data = sakamotoSimData)

# Function to estimate parameters for single model on all 10 data sets

generateModelEstimates <- function(formula, dataList) {
  lapply(dataList, function(df) {
    mod <- glm(formula, data = df)
    return(mod)
  })
}

sakamotoDFList <- split(sakamotoSimData, sakamotoSimData$d)

sakamotoDFList <- lapply(sakamotoDFList, as.data.frame)

modsg0 <- generateModelEstimates(formula = y ~ 1, dataList = sakamotoDFList)
modsg1 <- generateModelEstimates(formula = y ~ x, dataList = sakamotoDFList)
modsg2 <- generateModelEstimates(formula = y ~ poly(x,2,raw=T), dataList = sakamotoDFList)

plotApproxMods <- function(modList, pred = x, outcome = y, ...) {
  plot(exp((pred - 0.3)^2) - 1 ~ pred, type = "l", 
       ylim = c(-.2, .8), xlim = c(0,1))
  for(i in 1:length(modList)) {
    points(fitted(modList[[i]]))
  }
}

plotApproxMods(modsg1)

replicateApproximateModels <- function(dataList, formulaList) {
  ApproxModelList <- lapply(dataList, function(dl, fl) {
    generateModelEstimates(formula = fl, dataList = fl)
    return(ApproxModelList)
  })
}

# Create list of approximate model formulas
ApproxFormulas <- vector("list", 6)

ApproxFormulas[[1]] <- formula("y ~ 1")
ApproxFormulas[[2]] <- formula("y ~ x")
for(i in 2:5) {
  ApproxFormulas[[i+1]] <- formula(paste0("y ~ poly(x, ",i,",raw = TRUE)"))
}


approxmods <- vector("list", 6)
for(i in 1:length(approxmods)) {
  approxmods[i] <- lm(ApproxFormulas[[i]], sakamotoDFList[[i]])
}

g1 <- generateModelEstimates(formula = y ~ poly(x, i, raw = TRUE), dataList = sakamotoDFList)
g1 <- generateModelEstimates(formula = y ~ x, dataList = sakamotoDFList)
approxMod_g1 <- lmList(y ~ x | d, data = sakamotoSim)
plot(approxMod_g1)
plot(sakamotoSimData)
gsummary(sakamotoSimData)



modlist <- mapply(lm, formula = y ~ x, sakamotoDFList)

x <- seq(0, 1, .0001)
plot(exp((x - 0.3)^2) - 1 ~ x, type = "l")

min(exp((x - 0.3)^2))

y <- exp((x - 0.3)^2) -1
plot(y ~ x, type ="l")
min(y)

y[y == min(y)]
x[which.min(y)]
