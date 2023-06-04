source("src/scrape.R")
source("src/utility_functions.R")
source("src/determine_fertilization_characteristics.R")

Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3")
library(reticulate)
library(readr)

old_data <- read_delim("data/old_data_scrape.csv", delim = "|",
                       col_types = cols(.default = col_character()))

new_data <- read_rds("data/new_data_scrape.rds")

# new scrape data with fert mode computed using new R script
new_data_R_fert_mode <- read_rds("data/new_data_new_fert_mode.rds")

# old scrape data with fert mode computed using old Python script 
old_data_py_fert_mode <- read_delim("data/old_data_old_process.csv", delim = "|",
                                   col_types = cols(.default = col_character()))

# check columns are the same... they are quite different!
# print("Old columns not in new columns:")
# old_data %>% colnames() %>% .[!(. %in% colnames(new_data))] %>% print()
# print("\n\nNew columns not in old columns:")
# new_data %>% colnames() %>% .[!(. %in% colnames(old_data))] %>% print()

# Check old vs new methods of obtaining myFert3 on old scrape data
# NB old method has been baked in to a csv so no need to re run old python
# code here.
old_data_R_fert_mode <- old_data %>%
  basic_cleaning() %>%
  assign_3_group_fertilization()

cat("The following should be the same:\n\nOLD DATA NEW PROCESS\n")
old_data_R_fert_mode %>% count(myFert3) %>% print()
cat("OLD DATA OLD PROCESS\n")
old_data_py_fert_mode %>% count(myFert3) %>% print()

# Check old vs new methods of obtaining myFert3 on new scrape data
# This file is a slightly modified version of the original python code to 
# determine fert mode. There is a bit of script at the bottom to read in 
# the new scrape data (as an rds file!) and apply the old python functions
# to it. We harvest that data in the proceeding line.
py_run_file("sketch_test/old_method.py")
new_data_py_fert_mode = py$new_data_old_process

cat("\n------\nThe following should be the same:\n\n NEW DATA NEW PROCESS\n")
new_data_R_fert_mode %>% count(myFert3) %>% print()
cat("NEW DATA OLD PROCESS\n")
new_data_py_fert_mode %>% count(myFert3) %>% print()

