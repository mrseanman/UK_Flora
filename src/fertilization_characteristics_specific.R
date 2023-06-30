# Each function (all bar "determine_inbreeding_rate_band) in this file returns
# a logical vector. One value for each row in the df argument. This logical
# vector represents whether the characteristic in the title of the function
# is satisfied for each row in df


## ===== Outcrossing Characteristics ===========================================

# ------------------------------------------------------------------------------
fertilized_by_insects <- function(df){
  df %>% 
    pull(Fertilization) %>% 
    contains_only_certain_values_in_comma_sep_string("insects")
}

# ------------------------------------------------------------------------------
dioecous <- function(df){
  df %>% 
    pull(Dicliny) %>% 
    contains_only_certain_values_in_comma_sep_string("dioecous")
}

# ------------------------------------------------------------------------------
dichogamous_enough <- function(df){
  df %>% 
    pull(Dichogamy) %>% 
    contains_only_certain_values_in_comma_sep_string(
      c("protogynous",
        "protandrous",
        "markedly protandrous",
        "markedly protogynous",
        "entirely protandrous",
        "entirely protogynous"))
}

# ------------------------------------------------------------------------------
explicitly_self_sterile <- function(df){
  df %>% 
    pull(Fertilization) %>% 
    # NB "contains_any" not "contains_only"
    contains_any_of_certain_values_in_comma_sep_string("self sterile")
}

# ------------------------------------------------------------------------------
has_some_incompatibility_system <- function(df){
  has_some_incompatibility_system <-
    !contains_any_of_certain_values_in_comma_sep_string(
      pull(df, `Incompatibility systems`), "none")
}

# ------------------------------------------------------------------------------
explicitly_described_as_outcrossing <- function(df){
  
  df %>% 
    pull(Fertilization) %>% 
    # These are irrelevant to Fertilization mode
    remove_instances_from_css(c("apomictic", "viviparous")) %>% 
    contains_only_certain_values_in_comma_sep_string(c("obligatory cross",
                                                       "normally cross"))
}

### ===== Selfing Characteristics ==============================================

# ------------------------------------------------------------------------------
explicitly_described_as_selfing <- function(df){
  
  df %>% 
    pull(Fertilization) %>% 
    # These are irrelevant to Fertilization mode
    remove_instances_from_css(c("apomictic", "viviparous")) %>% 
    contains_only_certain_values_in_comma_sep_string("normally self")
  
}

# ------------------------------------------------------------------------------
cleistogamous <- function(df){
  
  df %>% 
    pull(Cleistogamy) %>% 
    contains_only_certain_values_in_comma_sep_string(
      c("pseudo-cleistogamous",
        "entirely cleistogamous",
        "usually cleistogamous"))
}

### ===== Mixed Characteristics ================================================

# ------------------------------------------------------------------------------
explicitly_described_as_mixed <- function(df){
  
  df %>% 
    pull(Fertilization) %>% 
    contains_any_of_certain_values_in_comma_sep_string(
      c("cross and self",
        "cross or automatic self"))
}

### ===== Non-Specific Characteristics =========================================

# ------------------------------------------------------------------------------
# Given a df of species, determines if each species is has a Inbreeding rate
# greater than or equal to 85%. This would then later correspond to selfing.
# Similar functions for other bands of 
# Return type is a logical vector corr. to the rows of df
# Note the special case here.
determine_inbreeding_rate_band <- function(df){
  inbreeding_rate <- df %>% pull(`Inbreeding (%)`) %>% as.double()
  
  is_primula_vulgaris <- df %>% 
    pull(main_species_name) %>% 
    `==`("Primula vulgaris")
  
  case_when(
    inbreeding_rate >= 80 & safe_not(is_primula_vulgaris) ~ "high",
    inbreeding_rate >= 20 & inbreeding_rate < 80 ~ "medium",
    inbreeding_rate < 20 ~ "low",
    TRUE ~ NA_character_
  )
}

# Helper function to fit in with grammar of functions here
has_inbreeding_rate_band <- function(df, band){
  determine_inbreeding_rate_band(df) == band
}












