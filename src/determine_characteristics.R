
# -----------------------------------------------------------------------------

seemingly_strictly_outcrossing <- function(df){
  
  explicitly_obilcatory_cross <- 
    contains_only_certain_values_in_comma_sep_string("obligatory cross",
                                     pull(df, Fertilization))
  
  fertilized_by_insects <- 
    contains_only_certain_values_in_comma_sep_string("insects",
                                     pull(df, Fertilization))
  
  explicitly_self_sterile <-
    # NB "contains_any" not "contains_only"
    contains_any_of_certain_values_in_comma_sep_string("self sterile",
                                       pull(df, Fertilization))
  
  dioecous <-
    contains_only_certain_values_in_comma_sep_string("dioecous",
                                     pull(df, Dicliny))
  
  # TODO BUG ! should read as below, but has been replaced by buggy code for
  # the moment to immitate original python script
  
  dichogamous_enough <- pull(df, Dichogamy) %in%
          # BUG space \/ here that shouldn't (immitates python code)
        c("protogynous ",
          "protandrous",
          "markedly protandrous",
          "markedly protogynous",
          "entirely protandrous",
          # BUG space here     \/ that shouldn't (immitates python code)
          "entirely protogynous ")
  
  # dichogamous_enough <- 
  #   contains_only_certain_values_in_comma_sep_string(
  #     c("protogynous",
  #       "protandrous",
  #       "markedly protandrous",
  #       "markedly protogynous",
  #       "entirely protandrous",
  #       "entirely protogynous"),
  #     pull(df, Dichogamy) 
  #   )

  has_some_incompatibility_system <-
    !contains_any_of_certain_values_in_comma_sep_string("none",
                                      pull(df, `Incompatibility systems`))
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

# -----------------------------------------------------------------------------

cleistogamous <- function(df){
  
  contains_only_certain_values_in_comma_sep_string(
    c("pseudo-cleistogamous",
      "entirely cleistogamous",
      "usually cleistogamous"),
    pull(df, Cleistogamy)
  )
}

# -----------------------------------------------------------------------------

apomictic <- function(df){

  contains_only_certain_values_in_comma_sep_string("apomictic",
                                   pull(df, "Fertilization"))
}

# -----------------------------------------------------------------------------

explicitly_described_as_mixed <- function(df){

  contains_any_of_certain_values_in_comma_sep_string(
    c("cross and self",
       "cross or automatic self",
       "normally cross",
       "normally self"),
    pull(df, "Fertilization")
  )
}

# -----------------------------------------------------------------------------
assign_3_group_fertilization <- function(df){
  # TODO remove this and put in a main type function at some later stage
  df <- df %>% basic_cleaning()
  
  seemingly_strictly_outcrossing <- seemingly_strictly_outcrossing(df)
  cleistogamous <- cleistogamous(df)
  apomictic <- apomictic(df)
  explicitly_described_as_mixed <- explicitly_described_as_mixed(df)
  
  selfing <- cleistogamous | apomictic
  mixed <- explicitly_described_as_mixed | (selfing &
                                              seemingly_strictly_outcrossing)
  # New method, use at a later stage
  # outcrossing <- seemingly_strictly_outcrossing &
  #                   !(replace_na(mixed|selfing, FALSE))
  outcrossing <- seemingly_strictly_outcrossing & !replace_na(mixed, FALSE)
  
  df %>% mutate(
    myFert3 = case_when(mixed ~ "mixed",
                        selfing ~ "selfing",
                        outcrossing ~ "outcrossing",
                        TRUE ~ NA_character_))
}

