unique_value_in_comma_sep_string <- function(value, css){
  # splitting by "," not ", " in case there are some without white space.
  css_split <- str_split(css, ",")
  # clean trailing white space  
  css_split <- css_split %>% map(trimws)
  
  css_split %>%
    map(~all(value == .x))
}

contains_value_in_comma_sep_string <- function(value, css){
  # splitting by "," not ", " in case there are some without white space.
  css_split <- str_split(css, ",")
  # clean trailing white space  
  css_split <- css_split %>% map(trimws)
  
  css_split %>% 
    map(~any(value == .x))
}