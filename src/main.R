suppressWarnings({
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readr)
  # scrape specific
  library(xml2)
  library(rvest)
})

source("src/scrape.R")
source("src/utility_functions.R")
source("src/join_bio_geo_data.R")
source("src/fertilization_type_broad.R")
source("src/fertilization_characteristics_specific.R")


# Set true to retrieve previously scraped data
skip_scrape <- TRUE

if (skip_scrape){
  cat("Retreiving previously scraped data...\n")
  scraped_data <- readRDS("data/new_data_scrape.rds")
} else {
  cat("Scraping data:")
  scraped_data <- scrape_all_tables()
}

cat("Cleaning data...\n")
#scraped_data_clean <- scraped_data %>% basic_cleaning()
#scraped_data %>% saveRDS("data/new_data_scrape.rds")

cat("Joining Bio Geo data...\n")
scraped_data <- scraped_data %>% join_bio_geo_data()

cat("Assigning fertilization mode...\n")
data_with_fertilization_mode <- scraped_data_clean %>% 
  assign_3_group_fertilization()

data_with_fertilization_mode %>% saveRDS("data/new_data_new_fert_mode.rds")

