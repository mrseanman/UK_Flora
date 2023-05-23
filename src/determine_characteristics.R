basic_cleaning <- function(df){
  
}

seemingly_strictly_outcrossing <- function(df){
  
  explicitly_obilcatory_cross <- 
    unique_value_in_comma_sep_string("obligatory cross",
                                     pull(df, Fertilization))
  
  fertilized_by_insects <- 
    unique_value_in_comma_sep_string("insects",
                                     pull(df, Fertilization))
  
  explicitly_self_sterile <-
    # NB "contains" not "unique"
    contains_value_in_comma_sep_string("self sterile",
                                       pull(df, Fertilization))
  
  dioecous <-
    unique_value_in_comma_sep_string("dioecous",
                                     pull(df, Fertilization))
  
  dichogamous_enough <- pull(df, Dichogamy) %in%
    c("protogynous",
      "protoandrous",
      "markedly protoandrous",
      "markedly protogynous",
      "entirely protoandrous",
      "entirely protogynous")
  
  # TODO incompaibility systems onwards
  
  
  
  
  
}

cleistogamous <- function(df){
  
  
}

apomictic <- function(df){
  
  
}

explicitly_described_as_mixed <- function(df){
  
}

