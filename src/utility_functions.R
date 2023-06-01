# -----------------------------------------------------------------------------

contains_only_certain_values_in_comma_sep_string <- function(values, css){
  css_split <- split_css(css)
  
  css_split %>% 
    map_lgl(~all(.x %>% 
      # This behaves like %in% but has desired NA behaviors
      # e.g. NA %in% c("blah") == FALSE whereas we want that to be NA
      map_lgl(~any(.x==values)))
      )
}

# -----------------------------------------------------------------------------

contains_any_of_certain_values_in_comma_sep_string <- function(values, css){
  css_split <- split_css(css)
  
  css_split %>% 
    map_lgl(~any(.x %>% 
       # This behaves like %in% but has desired NA behaviors
       # e.g. NA %in% c("blah") == FALSE whereas we want that to be NA
       map_lgl(~any(.x==values)))
       )
}

# -----------------------------------------------------------------------------

split_css <- function(css){
  # splitting by "," not ", " in case there are some without white space.
  css_split <- str_split(css, ",")
  # clean trailing white space  
  css_split %>% map(trimws)
}

# -----------------------------------------------------------------------------

basic_cleaning <- function(df){
  df %>%
    mutate(across(everything(), make_str_na_actual_na)) %>%
    map_df(trimws)
}

# -----------------------------------------------------------------------------

make_str_na_actual_na <- function(vec){
    # Just list the all lower case options here
    fake_str_nans <- c("nan", "na")
    turn_to_nan_cases <- contains_only_certain_values_in_comma_sep_string(
      tolower(fake_str_nans), vec) |
      is.na(vec) |
      is.nan(vec)
    
    # Annoying branch on the type of the input vector
    # that could maybe be avoided
    if (typeof(vec) == "character"){
      case_when(
        turn_to_nan_cases ~ NA_character_,
        TRUE ~ vec)
    } else if (typeof(vec) == "double"){
      case_when(
        turn_to_nan_cases ~ NA,
        TRUE ~ vec)
    }
}

