# Coşkun Küçükkaragöz (KCKCOS001)

# Packages and setup ####
rm(list=ls())
library(tidyverse)
library(readxl) # call the libraries with the functions we want to use
cpdat_0 <- read_xls("data/transplants_29May2012.xls", sheet = "dataframe", na = "NA") # Read the data with the correct sheet selected, and make sure that all instances of "NA" are treated as proper blanks since this was giving issues previously
cpdat_0[sapply(cpdat_0, is.character)] <- lapply(cpdat_0[sapply(cpdat_0, is.character)], as.factor) # Make all string variables into factors since each has multiple categories that are repeated and can be grouped by


# Tidying up ####

(cpdat <- (cpdat_0 %>% mutate(SiteID = word(Site,2), .before = Site)) %>% mutate(`Surface Type` = str_sub(word(cpdat_0$Site,3),2,-2), .after = SiteID) %>% mutate(Site = NULL)) # Seperating the "Site" column into SiteID + Surface Type so that both of those can be stored as separate variables (Surface type as a descriptor of SiteID rather than as part of the ID)



cpdat %>% pivot_longer(cols = c("cm2 in 1994", "std (cm2) in 1994", "n in 1994","cm2 in 2008", "std (cm2) in 2008", "n in 2008","cm2 in 2012", "std (cm2) in 2012", "n in 2012"),names_to = "variable", values_to = "value") %>% summarise()


cpdat %>% group_by(Type) %>% summarize(`Mean covered area in 1994` = print(paste(mean(`cm2 in 1994`, na.rm = TRUE),"±", mean(`std (cm2) in 1994`))),`Mean abundance in 2008` = mean(`n in 2008`, na.rm = TRUE),`Mean abundance in 2012` = mean(`n in 2012`, na.rm = TRUE))

str_sub(word(cpdat$Site,3),2,-2)



print(paste(cpdat$`cm2 in 1994`[1],",xx"))
