# Coşkun Küçükkaragöz (KCKCOS001)
# Tidy data prac test script

# Most comments from here on out are Jasper Slingsby's comments. I will specifically indicate if I leave any comments of my own

# Packages and setup ####
# First let's call the libraries with the functions we want to use
library(tidyverse)
library(GGally)
library(googlesheets4) # while googlesheets4 is part of the tidyverse, and installed when you install the tidyverse, it isn't a core package, so you have to call it separately
library(readxl) #while readxl is part of the tidyverse and installed when you install the tidyverse, it isn't a core package, so you have to call it separately

# Google sheets ####
# Now let's have a look at the names of the worksheets (tabs) within the spreadsheet
sheet_names("https://docs.google.com/spreadsheets/d/1U9yNaJFyd6kb5vlKzHhcgM6sLEzw_xVtQxewpH7k0Ho/edit?usp=sharing")
edat <- read_sheet("https://docs.google.com/spreadsheets/d/1U9yNaJFyd6kb5vlKzHhcgM6sLEzw_xVtQxewpH7k0Ho/edit?usp=sharing", sheet = "Sites") # read in data
edat # print a summary of the data

# Excel
# get list of files in the folder (change to your own)
list.files("data")
# See what sheets are in the Excel workbook
excel_sheets("data/pracdatasheet.xlsx")
# Read in data
edat <- read_xlsx("data/pracdatasheet.xlsx", sheet = "Sites")
# Print a summary 
edat

# C: Range function
read_xlsx("data/pracdatasheet.xlsx", sheet = "Sites", range = "C1:H25") # C: Allows you to define the range of the data you want in classic excel format

# Reading in different columns and binding them together
bind_cols(read_xlsx("data/pracdatasheet.xlsx", sheet = "Sites", range = "A1:B25"),
          read_xlsx("data/pracdatasheet.xlsx", sheet = "Sites", range = "G1:H25"))

# By row
bind_rows(read_xlsx("data/pracdatasheet.xlsx", sheet = "Sites", range = "C1:H15"),
          read_xlsx("data/pracdatasheet.xlsx", sheet = "Sites", range = "C16:H25"))


# Types of data ####
letters
class(letters)
as.factor(letters) # C: Change the vector "letters" to a factor with 26 levels
as.factor(rep(letters[1:3], 8))
as.numeric(as.factor(rep(letters[1:3], 8)))
as.numeric(rep(letters[1:3], 8)) # C: When we try to do "as.numeric" without making a "character" vector into factors first

# Problems with factors ###
hmm <- rep(c(4, "0", 2, 5, 3, 1), 3)
hmm
class(hmm)
as.factor(hmm)
as.numeric(as.factor(hmm))
as.numeric(as.character(as.factor(hmm)))

hmm2 <- rep(c(4, "O", 2, 5, 3, 1), 3)
hmm2
class(hmm2)
as.factor(hmm2)
as.numeric(as.factor(hmm2))
as.numeric(as.character(as.factor(hmm2)))

# Leaarning to speak Tidyverse ####

# “Take the environmental data, group it by the Site labels and calculate the means for a specific set of variables for each group.”
edat %>% 
  group_by(Site) %>%
  summarize(BareSoil = mean(PercentBareSoil), 
            `Soil pH` = mean(SoilPH), 
            Dung = mean(Dung))

# “Take the environmental data, filter it for only the renosterveld site, and select a specific set of variables.”
edat %>% 
  filter(Site == "renosterveld") %>%
  select(Point, PercentBareSoil, SoilPH, Dung)

# “Take the environmental data, filter it for only the renosterveld site, select a specific set of variables, and add a new column that expresses the frequency of dung observed as a proportion of the percentage bare soil exposed.”
edat %>% 
  filter(Site == "renosterveld") %>%
  select(Point, PercentBareSoil, SoilPH, Dung) %>%
  mutate(DungPerSoil = Dung/PercentBareSoil)

# “Take the environmental data, select a specific set of variables, add a new column that expresses the frequency of dung observed as a proportion of the percentage bare soil exposed, and arrange the samples in descending order of the new variable.”
edat %>% 
  select(SitePoint, PercentBareSoil, SoilPH, Dung) %>%
  mutate(DungPerSoil = Dung/PercentBareSoil) %>%
  arrange(desc(DungPerSoil))

# Getting things Tidy ####

# Long format (tidy)
read_xlsx("data/pracdatasheet.xlsx", sheet = "Sites")
read_xlsx("data/pracdatasheet.xlsx", sheet = "Sites") %>%
  pivot_longer(cols = c("PercentBareSoil", "SoilPH", "Dung", "Densiometer"),
               names_to = "variable", values_to = "value") 

# Wide format (not tidy)
read_xlsx("data/pracdatasheet.xlsx", sheet = "Species")
read_xlsx("data/pracdatasheet.xlsx", sheet = "Species") %>% 
  select(Site, Point, WorkingName)
read_xlsx("data/pracdatasheet.xlsx", sheet = "Species") %>% 
  select(Site, Point, WorkingName) %>%
  mutate(Presence = 1) %>% # adds a column called "Presence" filled with "1"
  pivot_wider(names_from = WorkingName, values_from = Presence, values_fill = 0)

read_xlsx("data/pracdatasheet.xlsx", sheet = "Species") %>% 
  unite("SitePoint", Site:Point, sep = "_") %>%
  select(SitePoint, WorkingName) %>%
  mutate(Presence = 1) %>% # adds a column called "Presence" filled with "1"
  pivot_wider(names_from = WorkingName, values_from = Presence, values_fill = 0)

read_xlsx("data/pracdatasheet.xlsx", sheet = "Species") %>% 
  separate(WorkingName, c("Gen", "Sp")) %>%
  select(Gen, Sp)

# Escape from Tidyverse ####
# First rerun our tidyverse code to get as close to a community data matrix as possible
comm <- read_xlsx("data/pracdatasheet.xlsx", sheet = "Species") %>% 
  unite("SitePoint", Site:Point, sep = "_") %>%
  select(SitePoint, WorkingName) %>%
  mutate(Presence = 1) %>% # adds a column called "Presence" filled with "1"
  pivot_wider(names_from = WorkingName, values_from = Presence, values_fill = 0)

# Then some Untidyverse code to
comm <- as.data.frame(unclass(comm)) # get out of the tibble format
rownames(comm) <- comm[,1] # add the first column "SitePoint" as rownames
comm <- comm[,-1] # remove the first column (i.e. "SitePoint")

comm[1:5, 1:3] # view the first 5 rows and 3 columns

# Joining dataframes
# First, we calculate the number of species recorded in each site and save it as an object sr
sr <- read_xlsx("data/pracdatasheet.xlsx", sheet = "Species") %>% 
  unite("SitePoint", Site:Point, sep = "_") %>%
  select(SitePoint, WorkingName) %>% 
  group_by(SitePoint) %>%
  summarize(`Species Number` = n())

sr

# Now let’s read in our site data, selet the columns we want and join sr to it using left_join
read_xlsx("data/pracdatasheet.xlsx", sheet = "Sites") %>%
  select(SitePoint, PercentBareSoil, SoilPH) %>%
  left_join(sr, by = "SitePoint")

# Plot all the things
read_xlsx("data/pracdatasheet.xlsx", sheet = "Sites") %>%
  select(SitePoint, PercentBareSoil, SoilPH) %>%
  left_join(sr, by = "SitePoint") %>%
  GGally::ggpairs(columns = 2:ncol(.))
# Note that if you only plan to use functions from a library a few times in your script it can be more efficient to call the function from the library once off using the syntax library::function (e.g. GGally::ggpairs) than to attach the whole library (i.e. calling library(GGally)) asit saves RAM.