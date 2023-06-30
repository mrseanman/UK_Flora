# ==============================================================================
# css = "Comma Separated String"
# values is vector test values, think of these as the RHS of an %in% expression
#     e.g. values = c("foo", "bar", "qux")
# css is vector of comma separated strings 
#     e.g. css <-.c("foo, bar", "foobar, foo, bar", "baz") and a

# This returns a logical vector of length length(css)
#     return[[1]] == TRUE means that ALL of elements of split_css(css[[1]])
#     are in values.
#     The total return for this example is
#     c(TRUE, FALSE, FALSE)

# NB there is an altered version of %in% used where the treatment of NA
# is different
# NB value can be of length 1 and this form of the function is useful.
contains_only_certain_values_in_comma_sep_string <- function(css, values){
  css_split <- split_css(css)
  
  css_split %>% 
    map_lgl(~all(.x %>% 
      # This behaves like %in% but has desired NA behaviors
      # e.g. NA %in% c("blah") == FALSE whereas we want that to be NA
      map_lgl(~any(.x==values)))
      )
}

# ==============================================================================
# css = "Comma Separated String"
# values is vector test values, think of these as the RHS of an %in% expression
#     e.g. values = c("foo", "bar", "qux")
# css is vector of comma separated strings 
#     e.g. css <-.c("foo, bar", "foobar, foo, bar", "baz") and a

# This returns a logical vector of length length(css)
#     return[[1]] == TRUE means that at least one the elements
#     of split_css(css[[1]]) is in values.

#     The total return for this example is
#     c(TRUE, TRUE, FALSE)

# NB there is an altered version of %in% used where the treatment of NA
# is different
# NB value can be of length 1 and this form of the function is useful.
contains_any_of_certain_values_in_comma_sep_string <- function(css, values){
  css_split <- split_css(css)
  
  css_split %>% 
    map_lgl(~any(.x %>% 
       # This behaves like %in% but has desired NA behaviors
       # e.g. NA %in% c("blah") == FALSE whereas we want that to be NA
       map_lgl(~any(.x==values)))
       )
}

# ==============================================================================
# css = "Comma separated string"
# Splits a vector of comma separated strings in to a list of vectors, splitting 
# by commas
# e.g. split_css(c("foo, bar", "foobar, baz", "qux")) is
#         list(c("foo", "bar"),
#              c("foobar", "baz"),
#              c("qux"))

split_css <- function(css){
  # splitting by "," not ", " in case there are some without white space.
  css_split <- str_split(css, ",")
  # clean trailing white space  
  css_split %>% map(trimws)
}

# ==============================================================================
# Vectorised function that turns any entries that are the literal string "na"
# or "NA" etc. in to actual NA
make_str_na_actual_na <- function(vec){
    # Just list the all lower case options here
    fake_str_nans <- c("", "nan", "na")
    turn_to_nan_cases <- contains_only_certain_values_in_comma_sep_string(
      tolower(vec), fake_str_nans)  |
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

# ==============================================================================
# for doing not (!) on a vector where many elements are NA.
# FALSE | NA is NA, but often we want it to be FALSE
safe_not <- function(logi){
  !replace_na(logi, FALSE)
}

remove_instances_from_css <- function(css, items_to_remove){
  css_split <- split_css(css)
  
  css_split %>% 
    map(~.x[!(.x %in% items_to_remove)]) %>%
    map_chr(~paste(.x, collapse=", "))
}

