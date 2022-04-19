# Coşkun Küçükkaragöz (KCKCOS001)
# Tidy data practical done using the example "transplants_29May2012.xls" dataset

# Packages and setup ####
  rm(list=ls())
  library(tidyverse)
  library(data.table)
  library(readxl) # call the libraries with the functions we want to use
  {trplants_0 <- read_xls("data/transplants_29May2012.xls", sheet = "dataframe", na = "NA") # Read the data with the correct sheet selected, and make sure that all instances of "NA" are treated as proper blanks since this was giving issues previously
  trplants_0[sapply(trplants_0, is.character)] <- lapply(trplants_0[sapply(trplants_0, is.character)], as.factor) # Make all string variables into factors since each has multiple categories that are repeated and can be grouped by
    } # trplants_0 is the raw data

# Tidying up ####

  (trplants <- (trplants_0 %>% mutate(SiteID = word(Site,2), .before = Site)) %>% mutate(`Surface Type` = str_sub(word(trplants_0$Site,3),2,-2), .after = SiteID) %>% mutate(Site = NULL)) # Seperating the "Site" column into SiteID + Surface Type so that both of those can be stored as separate variables (Surface type as a descriptor of SiteID rather than as part of the ID)
  # trplants is the data after the "Sites" column has been split with no other changes

# This whole block of code puts the data into "tidy" format and gives the variables more human friendly names
  {(trplants_tidy <- trplants %>% 
      rename(`Area cover in 1994 (cm^2)` = `cm2 in 1994`, `Standard Deviation of area cover in 1994 (cm^2)` = `std (cm2) in 1994`, `Number of individuals in 1994` = `n in 1994`,`Area cover in 2008 (cm^2)` = `cm2 in 2008`, `Standard Deviation of area cover in 2008 (cm^2)` = `std (cm2) in 2008`, `Number of individuals in 2008` = `n in 2008`,`Area cover in 2012 (cm^2)` = `cm2 in 2012`,  `Standard Deviation of area cover in 2012 (cm^2)` = `std (cm2) in 2012`, `Number of individuals in 2012` = `n in 2012`)  %>% # Renaming all necessary columns
      
      pivot_longer(cols = c(`Area cover in 1994 (cm^2)`, `Standard Deviation of area cover in 1994 (cm^2)`, `Number of individuals in 1994`,`Area cover in 2008 (cm^2)`, `Standard Deviation of area cover in 2008 (cm^2)`, `Number of individuals in 2008`,`Area cover in 2012 (cm^2)`, `Standard Deviation of area cover in 2012 (cm^2)`, `Number of individuals in 2012`),names_to = "variable", values_to = "value") %>% relocate(Species) %>% # turning the data into "long format" with the quantitative variables as the selected columns
      
      rename(Quantity = variable, Value = value, `Plant growth type` = Type)) %>%  # more renaming
    group_by(Species) %>% # The dataframe is grouped by species
      arrange(Species)# The dataframe is arranged by species, which is visible in the console output but not when using the "View(trplants_tidy)" function
    
trplants_tidy[sapply(trplants_tidy, is.character)] <- lapply(trplants_tidy[sapply(trplants_tidy, is.character)], as.factor) # Make all string variables into factors since each has multiple categories that are repeated
trplants_tidy$Quantity <- as.character(trplants_tidy$Quantity)
    }

# User specified tables ####
# In this section the user can enter what parameters they desire to be filtered and selected from the rows, text will need to be entered at various points to continue the process

# Summary will give the names and type of every column so that you can determine which column is interesting for you
trplants_tidy %>% select_if(is.factor) %>% sapply(levels)


## Enter column you would like to apply your filter to here, with the name EACTLY as printed in the console, meaning you need to include backticks (``), as well as being case sensitive. After the semicolon (;) enter the filter within that column you would like to apply (again copied exactly as presented in the console)
 {filter_column <- expr("Biome"); filter_level <- "Thicket" # setting the filter parameters
   
   filter_column <- rlang::sym(filter_column) # necessary step to overcome quasiquotation problems in tidy packages
   
 (assign(paste0("trplants_filtered_",filter_column,"_",filter_level), trplants_tidy %>% filter(!!filter_column == paste(filter_level)))) #applies the filer to the dataset and names it according to the provided filter parameters.
}
# The above code should give you the correct filled table in the console as well as the environment under the name "trplants_filtered_*column*_*filter*", if the column name and level name were entered **EXACTLY** correctly

# Some summary tables and figure ####
# Some sample informative summary tables will be created, as well as a figure to conpare all of the measured quantities between the different sites

# Means of all measured quantities separated by biome type
(trplants_biome_mean_all <- trplants_tidy %>% group_by(Biome, Quantity) %>% summarise(`Mean measurements` = mean(Value,na.rm=TRUE)))

# Mean area cover of both protea species
(trplants_species_protea_mean_area <- trplants_tidy %>% group_by(Species) %>% filter(`Plant growth type`== "Protea") %>% summarise(`Mean Protea area cover (cm^2)` = mean(Value[Quantity %like% "Area cover"],na.rm=TRUE)))

# Mean area cover of all restios for each site
(trplants_siteID_restio_mean_area <- trplants_tidy %>% group_by(SiteID) %>% filter(`Plant growth type`== "Restio") %>% summarise(`Mean Restio area cover (cm^2)` = mean(Value[Quantity %like% "Area cover"],na.rm=TRUE)))

# Total number of species found for each surface type
(trplants_surfacetype_species_diversity <- trplants_tidy %>% group_by(`Surface Type`) %>% summarise(`Number of species` = length(levels(Species))))

# Total number of individuals found in each biome
(trplants_planttype_count <- trplants_tidy %>% group_by(`Plant growth type`) %>% summarise(`Total number of individual plants found across the years` = sum(Value[Quantity %like% "individuals"],na.rm=TRUE)))

# Creating a plot showing each of the quantities for each site
(trplants_siteID_all_plot <- trplants_tidy %>% ggplot() +
  geom_boxplot(aes(y = Value, x = SiteID)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(Quantity), scales = "free"))
# Saving the plot to folder
  ggsave("./outputs/trplants_siteID_all_plot.png")
  
# Exporting tables ####
# You can export any of the tables generated here to the "outputs" subfolder of this project if you wish to save them for future use in further analyses.
# Fill in the name of your desired table in the following line (within the "expr()" function)
{desired_table <- expr(trplants_filtered_Biome_Thicket)

desired_table <- rlang::sym(desired_table)

write.table(eval(expr(!!desired_table)), file = paste("./outputs/",paste(desired_table),".csv"))
}
# END ####