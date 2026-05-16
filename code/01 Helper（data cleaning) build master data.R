# ===============
# Step 0 Part 1: install functions_cleaning r files
# ===============

source("functions_cleaning.R")
library(dplyr)

# ===============
# Step 0 Part 2: clean 7 states
# ===============

# Kansas
kansas_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/KSUR.csv",
  value_col = "KSUR",
  state_name = "Kansas",
  type_label = "SA"
)
kansas_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/KSURN.csv",
  value_col = "KSURN",
  state_name = "Kansas",
  type_label = "NSA"
)
kansas_both <- bind_rows(kansas_sa, kansas_nsa)
head(kansas_both)
table(kansas_both$data_type)

#Arkansas
arkansas_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/ARUR.csv",
  value_col = "ARUR",
  state_name = "Arkansas",
  type_label = "SA"
)
arkansas_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/ARURN.csv",
  value_col = "ARURN",
  state_name = "Arkansas",
  type_label = "NSA"
)
arkansas_both <- bind_rows(arkansas_sa, arkansas_nsa)
head(arkansas_both)
table(arkansas_both$data_type)

#Nebraska
nebraska_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/NEUR.csv",
  value_col = "NEUR",
  state_name = "Nebraska",
  type_label = "SA"
)
nebraska_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/NEURN.csv",
  value_col = "NEURN",
  state_name = "Nebraska",
  type_label = "NSA"
)
nebraska_both <- bind_rows(nebraska_sa, nebraska_nsa)
head(nebraska_both)
table(nebraska_both$data_type)

#New Mexico
newmexico_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/NMUR.csv",
  value_col = "NMUR",
  state_name = "New Mexico",
  type_label = "SA"
)
newmexico_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/NMURN.csv",
  value_col = "NMURN",
  state_name = "New Mexico",
  type_label = "NSA"
)
newmexico_both <- bind_rows(newmexico_sa, newmexico_nsa)
head(newmexico_both)
table(newmexico_both$data_type)

#Missouri
missouri_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/MOUR.csv",
  value_col = "MOUR",
  state_name = "Missouri",
  type_label = "SA"
)
missouri_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/MOURN.csv",
  value_col = "MOURN",
  state_name = "Missouri",
  type_label = "NSA"
)
missouri_both <- bind_rows(missouri_sa, missouri_nsa)
head(missouri_both)
table(missouri_both$data_type)

#Oklahoma
oklahoma_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/OKUR.csv",
  value_col = "OKUR",
  state_name = "Oklahoma",
  type_label = "SA"
)
oklahoma_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/OKURN.csv",
  value_col = "OKURN",
  state_name = "Oklahoma",
  type_label = "NSA"
)
oklahoma_both <- bind_rows(oklahoma_sa, oklahoma_nsa)
head(oklahoma_both)
table(oklahoma_both$data_type)

#Texas
texas_sa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state SA csv/TXUR.csv",
  value_col = "TXUR",
  state_name = "Texas",
  type_label = "SA"
)
texas_nsa <- clean_state_series(
  file_path = "/Users/jackytian/Desktop/JHU/Financial/group project/7 state NSA csv/TXURN.csv",
  value_col = "TXURN",
  state_name = "Texas",
  type_label = "NSA"
)
texas_both <- bind_rows(texas_sa, texas_nsa)
head(texas_both)
table(texas_both$data_type)

# =========
# Step 0 Part 3: build master dataset
# =========
all_states_both <- bind_rows(
  arkansas_both,
  kansas_both,
  missouri_both,
  nebraska_both,
  newmexico_both,
  oklahoma_both,
  texas_both
)
all_states_sa <- all_states_both %>%
  filter(data_type == "SA")

head(all_states_both)
head(all_states_sa)
unique(all_states_both$state)
table(all_states_both$data_type, useNA = "ifany")
all_states_both %>% count(state, data_type)
range(all_states_both$date)
