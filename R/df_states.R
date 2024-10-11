

df_states <- data.frame(State = state.name, Abbr = state.abb) %>%
  dplyr::add_row(State = "United States", Abbr = "US") %>%
  dplyr::add_row(State = "District of Columbia", Abbr = "DC") 

