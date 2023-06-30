# ==============================================================================
# Acts as a main function collating all the characteristics determined by
# the preceding functions in this file. The functions assigns a column of value 
# "mixed" "selfing" or "outcrossing" corr. to the determined fertilization mode
# of that species.

# Return type is a tibble with extra column corr. to fertilization mode
# called `myFert3`
assign_3_group_fertilization <- function(df){
  
  # Outcrossing traits ------------------------------------
  fertilized_by_insects_val <- fertilized_by_insects(df)
  has_low_inbreeding_rate_val <- has_inbreeding_rate_band(df, "low")
  dioecous_val <- dioecous(df)
  dichogamous_enough_val <- dichogamous_enough(df)
  explicitly_self_sterile_val <- explicitly_self_sterile(df)
  has_some_incompatibility_system_val <- has_some_incompatibility_system(df)
  explicitly_described_as_outcrossing_val <- explicitly_described_as_outcrossing(df)
  
  any_outcrossing_trait_val <-
    fertilized_by_insects_val               |
    has_low_inbreeding_rate_val             |
    dioecous_val                            |
    dichogamous_enough_val                  |
    explicitly_self_sterile_val             |
    has_some_incompatibility_system_val     |
    explicitly_described_as_outcrossing_val
  
  
  # Selfing traits ----------------------------------------
  explicitly_described_as_selfing_val <- explicitly_described_as_selfing(df)
  cleistogamous_val <- cleistogamous(df)
  has_high_inbreeeding_rate_val <- has_inbreeding_rate_band(df, "high")
  
  any_selfing_trait_val <-
    explicitly_described_as_selfing_val   |
    cleistogamous_val                     |
    has_high_inbreeeding_rate_val   
  
  
  # Mixed traits ------------------------------------------
  has_medium_inbreeding_rate_val <- has_inbreeding_rate_band(df, "medium")
  explicitly_described_as_mixed_val <- explicitly_described_as_mixed(df)
  
  any_mixed_trait_val <-
    has_medium_inbreeding_rate_val    |
    explicitly_described_as_mixed_val
  
  # Main logic of determining fert mode    ----------------
  mixed_val <-
    any_mixed_trait_val &
    safe_not(has_high_inbreeeding_rate_val)
  
  selfing_val <-
    any_selfing_trait_val &
    safe_not(mixed_val)
  
  outcrossing_val <-
    any_outcrossing_trait_val &
    safe_not(mixed_val) &
    safe_not(selfing_val)
  
  # Check that only one trait is assigned for each species
  total_number_of_assignments <- list(mixed_val,
                            selfing_val,
                            outcrossing_val) %>% 
    reduce(~replace_na(.x, 0) + replace_na(.y, 0), .init = 0)
  
  if (max(total_number_of_assignments) > 1){
    stop("More than one assignment has been given to a signle row.")
  }
  
  # Assign ------------------------------------------------
  # Note order does not matter here since passed check above
  df %>% mutate(
    myFert3 = case_when(mixed_val ~ "mixed",
                        selfing_val ~ "selfing",
                        outcrossing_val ~ "outcrossing",
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


