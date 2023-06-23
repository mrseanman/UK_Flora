suppressWarnings({
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
})

# ==============================================================================
# Acts as a main function collating all the characteristics determined by
# the preceding functions in this file. The functions assigns a column of value 
# "mixed" "selfing" or "outcrossing" corr. to the determined fertilization mode
# of that species.

# Return type is a tibble with extra column corr. to fertilization mode
# called `myFert3`
assign_3_group_fertilization <- function(df){
  
  explicitly_described_as_outcrossing <- explicitly_described_as_outcrossing(df)
  explicitly_described_as_mixed <- explicitly_described_as_mixed(df)
  explicitly_described_as_selfing <- explicitly_described_as_selfing(df)
  
  has_high_inbreeeding_rate <- has_inbreeding_rate_band(df, "high")
  has_medium_inbreeding_rate <- has_inbreeding_rate_band(df, "medium")
  has_low_inbreeding_rate <- has_inbreeding_rate_band(df, "low")
  
  seemingly_strictly_outcrossing <- seemingly_strictly_outcrossing(df)
  cleistogamous <- cleistogamous(df)

  explicitly_or_implicitly_outcrossing <-
    explicitly_described_as_outcrossing | seemingly_strictly_outcrossing
  
  # -------   Main logic of determining fert mode    ---------------------------
  selfing <-
    cleistogamous |
    explicitly_described_as_selfing |
    has_high_inbreeeding_rate
  
  mixed <-
    (explicitly_described_as_mixed |
       has_medium_inbreeding_rate) &
    safe_not(has_high_inbreeeding_rate)
  
  outcrossing <-
    explicitly_or_implicitly_outcrossing &
    safe_not(mixed | selfing)
  # ----------------------------------------------------------------------------
  
  # Note the order of these is very important. Some rows will have
  # mixed == TRUE & selfing == TRUE or similar.
  # The order here ensures that if mixed==TRUE then "mixed" is assigned
  # regardless of the value of selfing or outcrossing.
  df %>% mutate(
    myFert3 = case_when(mixed ~ "mixed",
                        selfing ~ "selfing",
                        outcrossing ~ "outcrossing",
                        TRUE ~ NA_character_)) %>% 
    assign_3_group_fertilization_QA_flag()
}

# ==============================================================================
# Given a df of species, note down where certain extra steps specific to that
# species only, have been taken in assigning myFert3
assign_3_group_fertilization_QA_flag <- function(df){
  is_primula_vulgaris <- df %>% 
    pull(main_species_name) %>% 
    `==`("Primula vulgaris")
  
  df %>% 
    mutate(
      myFert3_FLAG = 
        case_when(
          is_primula_vulgaris ~ "P",
          TRUE ~ NA_character_
        )
    )
}

# ==============================================================================
# Given a df of species, determines if each species is explicitly described
# as being a selfer in the column `Fertilization`.
# Return type is a logical vector corr. to the rows of df
explicitly_described_as_selfing <- function(df){
  
  df %>% 
    pull(Fertilization) %>% 
    # These are irrelevant to Fertilization mode
    remove_instances_from_css(c("apomictic", "viviparous")) %>% 
    contains_only_certain_values_in_comma_sep_string("normally self")

}

# ==============================================================================
# Given a df of species, determines if each species is explicitly described
# as having mixed fertilization type in the column `Fertilization`.
# Return type is a logical vector corr. to the rows of df
explicitly_described_as_mixed <- function(df){
  
  df %>% 
    pull(Fertilization) %>% 
    contains_any_of_certain_values_in_comma_sep_string(
      c("cross and self",
        "cross or automatic self"))
}

# ==============================================================================
# Given a df of species, determines if each species is explicitly described
# as being an outcrosser in the column `Fertilization`.
# Return type is a logical vector corr. to the rows of df
explicitly_described_as_outcrossing <- function(df){
  
  df %>% 
    pull(Fertilization) %>% 
    # These are irrelevant to Fertilization mode
    remove_instances_from_css(c("apomictic", "viviparous")) %>% 
    contains_only_certain_values_in_comma_sep_string(c("obligatory cross",
                                                       "normally cross"))
}

# ==============================================================================
# Given a df of all species and all characteristics, tests certain
# characteristics where if any of these characteristics are TRUE
# then the species is deemed seemingly strictly outcrossing.
# E.g. a dioecous species must be outcrossing regardless of other traits
# Retrun type is a logical vector corr. to the rows of df.
seemingly_strictly_outcrossing <- function(df){
  
  has_low_inbreeding_rate <-
    df %>% has_inbreeding_rate_band("low")
  
  fertilized_by_insects <-
    df %>% 
    pull(Fertilization) %>% 
    contains_only_certain_values_in_comma_sep_string("insects")
  
  explicitly_self_sterile <-
    df %>% 
    pull(Fertilization) %>% 
    # NB "contains_any" not "contains_only"
    contains_any_of_certain_values_in_comma_sep_string("self sterile")
  
  dioecous <-
    df %>% 
    pull(Dicliny) %>% 
    contains_only_certain_values_in_comma_sep_string("dioecous")
  
  dichogamous_enough <-
    df %>% 
    pull(Dichogamy) %>% 
    contains_only_certain_values_in_comma_sep_string(
      c("protogynous",
        "protandrous",
        "markedly protandrous",
        "markedly protogynous",
        "entirely protandrous",
        "entirely protogynous")
    )
  
  has_some_incompatibility_system <-
    !contains_any_of_certain_values_in_comma_sep_string(
      pull(df, `Incompatibility systems`), "none")
  
  # -----
  
  all_outcrossing_traits <- list(
    has_low_inbreeding_rate,
    fertilized_by_insects,
    explicitly_self_sterile,
    dioecous,
    dichogamous_enough,
    has_some_incompatibility_system
  )
  
  # Returns true if any of the above are true at a given row
  # Note that Na | True == True, which is what we want.
  all_outcrossing_traits %>% 
    reduce(`|`)
}

# ==============================================================================
# Given a df of species, determines if each species is cleistogamous.
# Return type is a logical vector corr. to the rows of df
cleistogamous <- function(df){
  
  df %>% 
    pull(Cleistogamy) %>% 
    contains_only_certain_values_in_comma_sep_string(
      c("pseudo-cleistogamous",
        "entirely cleistogamous",
        "usually cleistogamous"))
}


# ==============================================================================
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


