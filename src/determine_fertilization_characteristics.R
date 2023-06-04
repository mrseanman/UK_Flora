suppressWarnings({
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
})

# ==============================================================================
# Given a df of all species and all characteristics, tests certain
# characteristics where if any of these characteristics are TRUE
# then the species is deemed seemingly strictly outcrossing.
# E.g. a dioecous species must be outcrossing regardless of other traits
# Retrun type is a logical vector corr. to the rows of df.

seemingly_strictly_outcrossing <- function(df){
  
  explicitly_obilcatory_cross <- 
    contains_only_certain_values_in_comma_sep_string(pull(df, Fertilization),
                                                     "obligatory cross")
  
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
  
  # TODO BUG ! should read as below, but has been replaced by buggy code for
  # the moment to immitate original python script
  
  dichogamous_enough <- pull(df, Dichogamy) %in%
          # BUG space \/ here that shouldn't be (imitates python code)
        c("protogynous ",
          "protandrous",
          "markedly protandrous",
          "markedly protogynous",
          "entirely protandrous",
          # BUG space here     \/ that shouldn't be (imitates python code)
          "entirely protogynous ")
  
  # dichogamous_enough <- 
  #   contains_only_certain_values_in_comma_sep_string(pull(df, Dichogamy),
  #     c("protogynous",
  #       "protandrous",
  #       "markedly protandrous",
  #       "markedly protogynous",
  #       "entirely protandrous",
  #       "entirely protogynous") 
  #   )
  
  # TODO consider changing from any to only here
  has_some_incompatibility_system <-
    !contains_any_of_certain_values_in_comma_sep_string(
      pull(df, `Incompatibility systems`), "none")
  # -----
  
  all_outcrossing_traits <- list(
    explicitly_obilcatory_cross,
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
# Given a df of species, determines if each species is apomictic.
# Return type is a logical vector corr. to the rows of df.

apomictic <- function(df){

  contains_only_certain_values_in_comma_sep_string(pull(df, Fertilization),
                                                   "apomictic")
}

# ==============================================================================
# Given a df of species, determines if each species is explicitly described
# as having mixed fertilization type in the column `Fertilization`.
# Return type is a logical vector corr. to the rows of df

explicitly_described_as_mixed <- function(df){

  contains_any_of_certain_values_in_comma_sep_string(
    pull(df, Fertilization),
    c("cross and self",
       "cross or automatic self",
       "normally cross",
       "normally self"))
}

# ==============================================================================
# Acts as a main function collating all the characteristics determined by
# the preceding functions in this file. The functions assigns a column of value 
# "mixed" "selfing" or "outcrossing" corr. to the determined fertilization mode
# of that species.

# Return type is a tibble with extra column corr. to fertilization mode
# called `myFert3`

assign_3_group_fertilization <- function(df){
  
  seemingly_strictly_outcrossing <- seemingly_strictly_outcrossing(df)
  cleistogamous <- cleistogamous(df)
  apomictic <- apomictic(df)
  explicitly_described_as_mixed <- explicitly_described_as_mixed(df)
  
  # -------   Main logic of determining fert mode    ---------------------------
  selfing <- cleistogamous | apomictic
  mixed <- explicitly_described_as_mixed | (selfing &
                                              seemingly_strictly_outcrossing)
  # New method, use at a later stage
  # outcrossing <- seemingly_strictly_outcrossing &
  #                   !(replace_na(mixed|selfing, FALSE))
  
  # Replace NA needed here because True & NA == NA. We want to test that 
  # a True value has not been assigned to mixed
  outcrossing <- seemingly_strictly_outcrossing & !replace_na(mixed, FALSE)
  # ----------------------------------------------------------------------------
  
  # Note the order of these is very important. Some rows will have
  # mixed == TRUE & selfing == TRUE or similar.
  # The order here ensures that if mixed==TRUE then "mixed" is assigned
  # regardless of the value of selfing or outcrossing.
  df %>% mutate(
    myFert3 = case_when(mixed ~ "mixed",
                        selfing ~ "selfing",
                        outcrossing ~ "outcrossing",
                        TRUE ~ NA_character_))
}

