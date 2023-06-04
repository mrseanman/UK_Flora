library(tidyverse)
# re install to update
# devtools::install_github("RBGKew/BIFloraExplorer")
library(BIFloraExplorer)


# data from 2020 internship scraped from ecoflora
data_old <- read_delim("~/Documents/NERCflora/old_work/formFinal/finalFlat.csv",
                     delim = "|",
                     col_types = cols(.default = col_character()))

data_new <- BIFloraExplorer::BI_main
 

species_names_old <- data_old %>%
  pull(species) %>% 
  str_replace_all("_", " ")

species_names_new <- data_new %>% 
  pull(taxon_name_binom) %>% 
  str_to_lower()

# 3274
species_names_old %>% length()
# 3227
species_names_new %>% length()

# overlap
# 2584 (79% of old, 80% of new)
species_names_new %>%
  .[. %in% species_names_old] %>% 
  length()

# old not in new
species_old_not_in_new <- species_names_old %>% 
  .[!(. %in% species_names_new)]
# 690
species_old_not_in_new %>% length()

# new not in old
species_new_not_in_old <- species_names_new %>% 
  .[!(. %in% species_names_old)]
# 643
species_new_not_in_old %>% length()

# old with fert data intersected with new species
# 993
data_old %>% 
  filter(!is.na(myFert3)) %>% 
  filter(!(species %>% str_replace_all("_", " "))
          %in%
            species_names_new) %>% 
  pull(species)

# old with fert data
# 1082
data_old %>% filter(!is.na(myFert3)) %>% 
  nrow()

data_old %>% filter(!is.na(myFert3) &
                      species_names_old %in% species_names_new) %>% 
  nrow()
