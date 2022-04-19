# Coşkun Küçükkaragöz (KCKCOS001)
# Tidy data practical done using the example "transplants_29May2012.xls" dataset

# Packages and setup ####
  rm(list=ls())
  library(tidyverse)
  library(readxl) # call the libraries with the functions we want to use
  {cpdat_0 <- read_xls("data/transplants_29May2012.xls", sheet = "dataframe", na = "NA") # Read the data with the correct sheet selected, and make sure that all instances of "NA" are treated as proper blanks since this was giving issues previously
  cpdat_0[sapply(cpdat_0, is.character)] <- lapply(cpdat_0[sapply(cpdat_0, is.character)], as.factor) # Make all string variables into factors since each has multiple categories that are repeated and can be grouped by
    } # cpdat_0 is the raw data

# Tidying up ####

  (cpdat <- (cpdat_0 %>% mutate(SiteID = word(Site,2), .before = Site)) %>% mutate(`Surface Type` = str_sub(word(cpdat_0$Site,3),2,-2), .after = SiteID) %>% mutate(Site = NULL)) # Seperating the "Site" column into SiteID + Surface Type so that both of those can be stored as separate variables (Surface type as a descriptor of SiteID rather than as part of the ID)
  # cpdat is the data after the "Sites" column has been split with no other changes

# This whole block of code puts the data into "tidy" format and gives the variables more human friendly names
  {(cpdat_tidy <- cpdat %>% 
      rename(`Area cover in 1994 (cm^2)` = `cm2 in 1994`, `Standard Deviation of area cover in 1994 (cm^2)` = `std (cm2) in 1994`, `Number of individuals in 1994` = `n in 1994`,`Area cover in 2008 (cm^2)` = `cm2 in 2008`, `Standard Deviation of area cover in 2008 (cm^2)` = `std (cm2) in 2008`, `Number of individuals in 2008` = `n in 2008`,`Area cover in 2012 (cm^2)` = `cm2 in 2012`,  `Standard Deviation of area cover in 2012 (cm^2)` = `std (cm2) in 2012`, `Number of individuals in 2012` = `n in 2012`)  %>% # Renaming all necessary columns
      
      pivot_longer(cols = c(`Area cover in 1994 (cm^2)`, `Standard Deviation of area cover in 1994 (cm^2)`, `Number of individuals in 1994`,`Area cover in 2008 (cm^2)`, `Standard Deviation of area cover in 2008 (cm^2)`, `Number of individuals in 2008`,`Area cover in 2012 (cm^2)`, `Standard Deviation of area cover in 2012 (cm^2)`, `Number of individuals in 2012`),names_to = "variable", values_to = "value") %>% relocate(Species) %>% # turning the data into "long format" with the quantitative variables as the selected columns
      
      rename(Measure = variable, Value = value, `Plant growth type` = Type)) %>%  # more renaming
    group_by(Species) %>% # The dataframe is grouped by species
      arrange(Species)# The dataframe is arranged by species, which is visible in the console output but not when using the "View(cpdat_tidy)" function
    
cpdat_tidy[sapply(cpdat_tidy, is.character)] <- lapply(cpdat_tidy[sapply(cpdat_tidy, is.character)], as.factor) # Make all string variables into factors since each has multiple categories that are repeated
cpdat_tidy$Measure <- as.character(cpdat_tidy$Measure)
    }

# User specified tables ####
# In this section the user can enter what parameters they desire to be filtered and selected from the rows, text will need to be entered at various points to continue the process

# Summary will give the names and type of every column so that you can determine which column is interesting for you
cpdat_tidy %>% select_if(is.factor) %>% sapply(levels)


## Enter column you would like to apply your filter to here, with the name EACTLY as printed in the console, meaning you need to include backticks (``) as well. After the semicolon (;) enter the filter within that column you would like to apply (again copied exactly as presented in the console)
 {filter_column <- "`Plant growth type`"; filter_level <- "Tree"
 
 cpdat_tidy %>% filter(noquote(filter_column) == paste(filter_level))
  
}
distinct(cpdat_tidy, Type)



