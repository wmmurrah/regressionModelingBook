#------------------------------------------------------------------------
# Title: AU Salaries
# Author: William Murrah
# Description: code to explore AU salaries
# Created: Wednesday, 03 January 2024
# R version: R version 4.3.2 (2023-10-31)
# Project(working) directory: /Users/wmm0017/Library/CloudStorage/Box-Box/7_EFLT_StrategicPlanner
#------------------------------------------------------------------------



salary <- read.fwf(file = "data/AUsalaries2023.txt", 
                   widths = c(7, 46, 30, 80, 20) , 
                   header = FALSE,
                   col.names = c("Posn", "Employee_Name", "Department", 
                                 "Title", "Salary")
                 )


salary$Salary <- gsub("\\$", "", salary$Salary)
salary$Salary <- gsub(",", "", salary$Salary)
