# install library
library(dplyr)
library(readr)

# clean -> read csv -> change columns name -> unify date/record state/SA or NSA -> filtering -> retrun graph
clean_state_series <- function(file_path, value_col, state_name, type_label) {
  
  df <- read.csv(file_path)
  
  names(df)[names(df) == "observation_date"] <- "date"
  names(df)[names(df) == value_col] <- "value"
  
  df <- df %>%
    mutate(
      date = as.Date(date),
      state = state_name,
      data_type = type_label
    ) %>%
    filter(date >= as.Date("1995-01-01"),
           date <= as.Date("2024-12-01"))
  
  return(df)
}

