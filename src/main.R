source("src/scrape.R")
source("src/utility_functions.R")
source("src/determine_fertilization_characteristics.R")

cat("Scraping data:")
scraped_data <- scrape_all_tables()

cat("\nCleaning data...")
scraped_data_clean <- scraped_data %>% basic_cleaning()
scraped_data %>% saveRDS("data/new_data_scrape.rds")

cat("\nAssigning fertilization mode...\n")
data_with_fertilization_mode <- scraped_data_clean %>% 
  assign_3_group_fertilization()

data_with_fertilization_mode %>% saveRDS("data/new_data_new_fert_mode.rds")

