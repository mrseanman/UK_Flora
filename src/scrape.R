library(xml2)
library(rvest)
library(magrittr)
library(stringr)

# -----------------------------------------------------------------------------

scrape_all_tables <- function(){
  df_unique_ID_species_pairs <-
    get_all_ID_species_pairs_with_flattened_synonyms()
  
  all_data <- df_unique_ID_species_pairs %>% 
     pull(ID) %>% 
     map_df(scrape_table_from_ID, .progress=TRUE)
    
  all_data %>% 
    left_join(df_unique_ID_species_pairs, by="ID") %>% 
    select(ID, main_species_name, other_species_names, everything())
  
}

# -----------------------------------------------------------------------------

scrape_table_from_ID <- function(ID){
  link_stub <- "http://ecoflora.org.uk/search_ecochars.php?plant_no="
  link_text <- paste0(link_stub, ID)
  
  tables_in_page <- link_text %>% 
    read_html(encoding="ASCII") %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
  
  relevant_table <- tables_in_page %>%  
    # Sometimes there are scrappy irrelevant tables floating in the page.
    # This line only selects relevant tables
    keep(~"Name" %in% names(.x)) %>% first()
  
  if (is.null(relevant_table)){
    # No info, just return the ID
    return(tibble(ID = ID))
  }
  
  # Collapse multiple values in to comma separated string
  df_multiple_value_collapse <- relevant_table %>% 
    select(name = Name, value = Value) %>% 
    group_by(name) %>% 
    summarise(name = first(name),
              value = paste(value, collapse=", "),
              .groups="drop")
  
  df_multiple_value_collapse %>% 
    pivot_wider(names_from = name,
                values_from = value) %>% 
    mutate(ID = ID) %>% 
    # move ID to start
    select(ID, everything())
}

# -----------------------------------------------------------------------------

get_all_ID_species_pairs_with_flattened_synonyms <- function(){
  all_ID_species_pairs <- get_all_ID_species_pairs()
  
  # check there is just one "non-synonym" name per ID
  n_non_synonym_names <- all_ID_species_pairs %>% 
    filter(!is_synonym_name) %>% 
    group_by(ID) %>% 
    summarise(n_non_synonym_names = n(), .groups="drop") %>% 
    pull(n_non_synonym_names)
  
  if (max(n_non_synonym_names)>1){
    stop("There is a species ID that corr. to more than 1 non-synonym name.")
  }
  
  all_ID_species_pairs %>% 
    group_by(ID, is_synonym_name) %>%
    summarise(ID = first(ID),
          # If in group where is_synonym_name, the main name is NA
          # and visa-versa
          main_species_name =
            ifelse(first(is_synonym_name),
                   NA,
                   paste(species_name, collapse = ", ")),
          other_species_names =
            ifelse(first(is_synonym_name),
                   paste(species_name, collapse = ", "),
                   NA),
          .groups = "drop") %>% 
    # Then we flatten again to get only one row per ID
    group_by(ID) %>% 
    summarise(
      ID = first(ID),
      main_species_name = main_species_name %>% na.omit() %>% first(),
      other_species_names = other_species_names %>% na.omit %>% first()
    )
}

# -----------------------------------------------------------------------------

get_all_ID_species_pairs <- function(){
  ecoflora_link_list_URL <- 
    "http://ecoflora.org.uk/search_synonyms.php"
  
  link_list_page <- read_html(ecoflora_link_list_URL)
  links <- html_nodes(pg, "a")

  ID_species_df <- links %>% 
    map_df(link_to_ID_and_species)
  
  # Trim all whitespace and remove NA rows
  ID_species_df %>% 
    mutate(
      across(c(ID, species_name), .fns = trimws)) %>% 
    filter(!is.na(ID))
}

# -----------------------------------------------------------------------------

link_to_ID_and_species <- function(link){
  href <- link %>% html_attr("href")
  
  # If the link is not to a relevant page, skip it
  if (!(grepl("no=", href))){
    ID = NA_character_
    species_name = NA_character_
    is_synonym_name = NA
  } else {
    ID <- href %>% str_split("no=") %>% .[[1]] %>% .[2]
    species_name <- link %>% html_text()
    # detect how the text is wrapped in HTML
    # 'synonym' names have no <font> wrapper around the actual species
    # name. So the <a> element having no children corr. to being a synonym.
    # Whereas non-synonnym links have the text wrapped in a <font> elemnet.
    is_synonym_name <- link %>%
      html_children() %>%
      length() %>% 
      `==`(., 0)
  }
  
  list(ID = ID,
       species_name = species_name,
       is_synonym_name = is_synonym_name)
}
