# Coşkun Küçükkaragöz (KCKCOS001)

# Packages and setup ####
rm(list=ls())
library(tidyverse) # call the libraries with the functions we want to use
library(readxl)
cpdat <- read_xls("data/transplants_29May2012.xls", sheet = "dataframe", na = "NA")
cpdat[sapply(cpdat, is.character)] <- lapply(cpdat[sapply(cpdat, is.character)], as.factor)

# Tidying up ####

cpdat %>% group_by(Type) %>% summarize(`Mean abundance in 2994` = mean(`n in 1994`, na.rm = TRUE),`Mean abundance in 2008` = mean(`n in 2008`, na.rm = TRUE),`Mean abundance in 2012` = mean(`n in 2012`, na.rm = TRUE))

str_sub(word(cpdat$Site,3),2,-2)
