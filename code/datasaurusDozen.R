#------------------------------------------------------------------------
# Title: Datasaurus Dozen
# Author: William Murrah
# Description: Example of various datasets with same descriptives from 
#              Probabalistic Machine learning (Murphy, 2022)
# Created: Friday, 11 August 2023
# R version: R version 4.3.1 (2023-06-16)
# Project(working) directory: /Users/wmm0017/Projects/Books/
# RegressionModelingBook
#------------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/probml/probml-data/main/data/DatasaurusDozen.tsv"

dino <- read.table(file = url, header = TRUE)



dsetname <- unique(dino$dataset)[-2]
dsetnum <- 1:length(dsetname)

par(mfrow = c(4,4))
for(i in 1:length(dsetnum)) {
  plot(y ~ x, data = dino[dino$dataset == dsetname[i], ],
       main = dsetname[i])
}

par(mfrow = c(1,1))


aggregate(cbind(y,x) ~ dataset, dino, 
          FUN = function(x) c(M = round(mean(x),1), 
                              SD = round(sd(x), 1)
                              )
          )
