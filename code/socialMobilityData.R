#------------------------------------------------------------------------
# Title: Grusky and Hauser 1983 Social Mobility Data 
# Author: William Murrah
# Description: Taken from Raftery 1995.
# Created: Saturday, 16 December 2023
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Projects/Books/RegressionModelingBook
#------------------------------------------------------------------------

# gruskyHauser <- data.frame(
#   mobility = c(292,170,29,290,608,37,81,171,175,
#                497,100,12,300,434,7,102,101,129,
#                2085,1047,74,936,2367,57,592,1255,1587,
#                479,)
# )


socmob <- read.table(file = "data/socmob.tab", 
                     skip = 35)
names(socmob) <- c("father_occ", "son_occ", "fam_struc", "race", "son_firstocc",
                   "son_currocc")

occ_labels <- c("Professional,Self-Employed", "Professional-Salaried", 
                "Manager", "Salesman-Nonretail", "Proprietor", "Clerk", 
                "Salesman-Retail", "Craftsman-Manufacturing", "Craftsmen-Other",
                "Craftsman-Construction", "Service Worker", 
                "Operative-Nonmanufacturing", "Operative-Manufacturing",
                "Laborer-Manufacturing", "Laborer-Nonmanufacturing", 
                "Farmer/Farm Manager", "Farm Laborer")

socmob$father_occ_fac <- factor(socmob$father_occ, levels = 17:1, 
                            labels = occ_labels)
socmob$son_occ_fac <- factor(socmob$son_occ, levels = 17:1, 
                             labels = occ_labels)
