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
  
  seemingly_strictly_outcrossing <- seemingly_strictly_outcrossing(df)
  cleistogamous <- cleistogamous(df)
  has_high_inbreeeding_rate <- has_high_inbreeeding_rate(df)
  
  explicitly_or_implicitly_outcrossing <-
    explicitly_described_as_outcrossing | seemingly_strictly_outcrossing
  
  # -------   Main logic of determining fert mode    ---------------------------
  selfing <-
    cleistogamous |
    explicitly_described_as_selfing |
    has_high_inbreeeding_rate
  
  mixed <-
    (explicitly_described_as_mixed |
       (selfing & explicitly_or_implicitly_outcrossing)) &
    safe_not(has_high_inbreeeding_rate)
  
  # TODO
  # New method, use at a later stage
  # outcrossing <- seemingly_strictly_outcrossing &
  #                   !(replace_na(mixed|selfing, FALSE))
  
  outcrossing <-
    (explicitly_described_as_outcrossing | seemingly_strictly_outcrossing) &
    !safe_not(mixed)
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
  
  contains_only_certain_values_in_comma_sep_string(
    pull(df, Fertilization),
    c("normally self",
# NB including "apomictic" and "viviparous" because these traits have no affect
# on Fertilization mode. If they occur in the record, OK, it makes no difference.
# If they do not occur in a record, but say "normally self" does, then this 
# function will still return TRUE, which is what we want.
      "apomictic",
      "viviparous"))
}

# ==============================================================================
# Given a df of species, determines if each species is explicitly described
# as having mixed fertilization type in the column `Fertilization`.
# Return type is a logical vector corr. to the rows of df
explicitly_described_as_mixed <- function(df){
  
  contains_any_of_certain_values_in_comma_sep_string(
    pull(df, Fertilization),
    c("cross and self",
      "cross or automatic self"))
}

# ==============================================================================
# Given a df of species, determines if each species is explicitly described
# as being an outcrosser in the column `Fertilization`.
# Return type is a logical vector corr. to the rows of df
explicitly_described_as_outcrossing <- function(df){
  
  contains_only_certain_values_in_comma_sep_string(pull(df, Fertilization),
     c("obligatory cross",
       "normally cross",
# NB including "apomictic" and "viviparous" because these traits have no affect
# on Fertilization mode. If they occur in the record, OK, it makes no difference.
# If they do not occur in a record, but say "obligatory cross" does, then this 
# function will still return TRUE, which is what we want.
       "apomictic",
       "viviparous"))
}

# ==============================================================================
# Given a df of all species and all characteristics, tests certain
# characteristics where if any of these characteristics are TRUE
# then the species is deemed seemingly strictly outcrossing.
# E.g. a dioecous species must be outcrossing regardless of other traits
# Retrun type is a logical vector corr. to the rows of df.
seemingly_strictly_outcrossing <- function(df){
  
  fertilized_by_insects <- 
    contains_only_certain_values_in_comma_sep_string(pull(df, Fertilization),
                                                     "insects")
  explicitly_self_sterile <-
    # NB "contains_any" not "contains_only"
    contains_any_of_certain_values_in_comma_sep_string(pull(df, Fertilization),
                                                       "self sterile")
  dioecous <-
    contains_only_certain_values_in_comma_sep_string(pull(df, Dicliny),
                                                     "dioecous")
  dichogamous_enough <-
    contains_only_certain_values_in_comma_sep_string(pull(df, Dichogamy),
      c("protogynous",
        "protandrous",
        "markedly protandrous",
        "markedly protogynous",
        "entirely protandrous",
        "entirely protogynous")
    )
  
  # TODO consider changing from any to only here
  has_some_incompatibility_system <-
    !contains_any_of_certain_values_in_comma_sep_string(
      pull(df, `Incompatibility systems`), "none")
  
  # -----
  
  all_outcrossing_traits <- list(
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
  
  contains_only_certain_values_in_comma_sep_string(pull(df, Cleistogamy),
    c("pseudo-cleistogamous",
      "entirely cleistogamous",
      "usually cleistogamous")
  )
}


# ==============================================================================
# Given a df of species, determines if each species is has a Inbreeding rate
# greater than or equal to 85%. This would then later correspond to 
# Return type is a logical vector corr. to the rows of df
# Note the special case here.
has_high_inbreeeding_rate <- function(df){
  high_inbreeding_rate <- df %>%
    pull(`Inbreeding (%)`) %>% 
    as.double() %>% 
    `>=`(85)
  
  # Known that data for this species is not applicable
  is_primula_vulgaris <- df %>% 
    pull(main_species_name) %>% 
    `==`("Primula vulgaris")
  
  # Only give high inbreeding rate TRUE is not primula vulgaris
  high_inbreeding_rate & !replace_na(is_primula_vulgaris, FALSE)
}

