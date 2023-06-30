join_bio_geo_data <- function(df){
  bio_geo <- read_csv("data/bio_geo/Database_BioGeo_mating.csv",
                      show_col_types = FALSE)
  
  bio_geo <- bio_geo %>% 
    mutate(genus_species = paste(genus, species))
  
  df <- df %>% 
    left_join(bio_geo %>%
                select(genus_species, si, mean.tm),
              by = c("main_species_name"="genus_species"))
  
  df
}