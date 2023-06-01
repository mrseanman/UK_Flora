library(tidyverse)

old_data <- read_delim("data/old_data_scrape.csv", delim = "|",
                       col_types = cols(.default = col_character()))
new_data_new_process <- read_rds("data/new_data_scrape.rds")

old_data_old_process <- read_delim("data/old_data_old_process.csv", delim = "|",
                                   col_types = cols(.default = col_character()))

# check columns are the same
# print("Old columns not in new columns:")
# old_data %>% colnames() %>% .[!(. %in% colnames(new_data))] %>% print()
# print("\n\nNew columns not in old columns:")
# new_data %>% colnames() %>% .[!(. %in% colnames(old_data))] %>% print()

old_data_new_process <- old_data_old_process %>% assign_3_group_fertilization()
#old_data_new_process <- old_data %>% assign_3_group_fertilization()

old_process_outcrossing <- old_data_old_process %>% 
  pull(myFert3) %>% 
  `==`(., "outcrossing")

new_process_outcrossing <- old_data_new_process %>% 
  pull(myFert3) %>% 
  `==`("outcrossing")


old_data_old_process %>%
  mutate(row_num = row_number()) %>%
  filter(new_process_outcrossing & is.na(myFert3)) %>% 
  select(row_num, myFert3, Dichogamy) %>% View()



old_data_old_process %>% filter(!myFert3=="outcrossing" & new_process_explicitly_outcrossing) %>%
  select(Dicliny) %>% View()
