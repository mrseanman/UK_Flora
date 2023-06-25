source("src/scrape.R")
source("src/utility_functions.R")
source("src/determine_fertilization_characteristics.R")

library(ggplot2)
library(BIFloraExplorer)

new_data_new_fert_mode <- readRDS("data/new_data_new_fert_mode.rds") %>% 
  mutate(myFert3 = factor(myFert3, c("selfing", "mixed", "outcrossing")))

BIFlora <- BIFloraExplorer::BI_main

BIFlora <- BIFlora %>% 
  mutate(
    total_hectads_1987_1999 =
      Ire_hectads_1987_1999 +
      CI_hectads_1987_1999 +
      GB_Man_hectads_1987_1999,
    
    total_hectads_2000_2009 = 
      Ire_hectads_2000_2009 +
      CI_hectads_2000_2009 +
      GB_Man_hectads_2000_2009,
    
    total_hectads_2010_2019 = 
      Ire_hectads_2010_2019 +
      CI_hectads_2010_2019 +
      GB_Man_hectads_2010_2019
  )


new_data_new_fert_mode_range <- new_data_new_fert_mode %>% 
  left_join(BIFlora %>% select(taxon_name_binom, contains("total_hectads")),
            by=c("main_species_name" = "taxon_name_binom"))

new_data_new_fert_mode_range %>%
  filter(!is.na(myFert3) & !is.na(total_hectads_2010_2019)) %>% 
  ggplot(aes(x=myFert3, y=total_hectads_2010_2019)) + geom_boxplot()
ggsave("figures/prelim_fert_v_range_boxplot.png")

new_data_new_fert_mode_range %>%
  filter(!is.na(myFert3) & !is.na(total_hectads_2010_2019)) %>% 
  ggplot(aes(total_hectads_2010_2019, after_stat(count), fill = myFert3)) +
  geom_density(position = "fill")
ggsave("figures/prelim_fert_v_range_split.png")

new_data_new_fert_mode_range %>%
  filter(!is.na(myFert3) & !is.na(total_hectads_2010_2019)) %>%
  ggplot() + geom_density(aes(x=total_hectads_2010_2019, fill=myFert3), alpha=0.4)
ggsave("figures/prelim_fert_v_range_spread.png")






