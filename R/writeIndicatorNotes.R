
print("Create fresh logs for various info: df_log_sources df_log_notes df_log_definitions")
df_log_sources <- df_log_notes <- df_log_definitions <- data.frame()


writeIndicatorNotes <- function(sheetUrl,
         oldYear = "2021", newYear = "2022",
         bool_logNotes = FALSE) {

  # Read in the existing notes:
  df_this_notes <- sheetUrl |>
    googlesheets4::read_sheet(sheet = "Notes") |>
    mutate(Index = row_number())

  # Where is the various fields?
  df_sources <- df_this_notes |>
    filter(Type == "Sources")
  df_notes <-  df_this_notes |>
    filter(Type == "Notes")
  df_definitions <-  df_this_notes |>
    filter(Type == "Definitions")


  patternToRemove_1 <- "For 2020 data please refer to the \\[[-a-zA-Z 0-9\\(\\)]*\\]\\([a-zA-Z0-9:/.-]*\\)\\. We will continue to report data from the ACS for 2021 and later."
  patternToRemove_2 = " We will continue to report data from the ACS for 2021 and later. Estimates from the CPS are not comparable to estimates from ACS.\n"
  patternToRemove_3 <- "Estimates from the CPS are not comparable to estimates from ACS."
  newNoteText = "\n"

  df_thisNotes_cleaned <- df_this_notes |>
    mutate(Note = case_when(
      # Swap years in the notes
      Type == "Sources" ~ Note |> lapply(function(thisString) gsub(oldYear, newYear, thisString)),
      TRUE ~ Note)) |>
    # Remove text from notes (there are a few types of patterns to be removed)
    mutate(Note = case_when(
            Type == "Notes" ~ Note |> lapply(function(thisString) gsub(patternToRemove_1, newNoteText, thisString)),
            TRUE ~ Note)) |>
    mutate(Note = case_when(
      Type == "Notes" ~ Note |> lapply(function(thisString) gsub(patternToRemove_2, newNoteText, thisString)),
      TRUE ~ Note)) |>
    mutate(Note = case_when(
      Type == "Notes" ~ Note |> lapply(function(thisString) gsub(patternToRemove_3, newNoteText, thisString)),
      TRUE ~ Note)) |>
    print()

  df_notes$Note
  df_thisNotes_cleaned$Note
  # Log the data if requested
  if(bool_logNotes){

    # Where is the various fields?
    df_sources <- df_thisNotes_cleaned |>
      filter(Type == "Sources")
    df_notes <-  df_thisNotes_cleaned |>
      filter(Type == "Notes")
    df_definitions <-  df_thisNotes_cleaned |>
      filter(Type == "Definitions")

    if(nrow(df_sources) > 0){
    df_log_sources <<- df_log_sources |>
      bind_rows(df_sources)
    }
    if(nrow(df_notes) > 0){
    df_log_notes <<- df_log_notes |>
      bind_rows(df_notes)
    }
    if(nrow(df_definitions) > 0){
    df_log_definitions <<- df_log_definitions |>
      bind_rows(df_definitions)
    }
  }

  # Write the new note info to the google sheet
  df_thisNotes_cleaned %>%
    select(-Index) %>%
    googlesheets4::range_write(data = .,
                               ss = sheetUrl,
                               sheet = "Notes",
                               range = "A2",
                               col_names = FALSE,
                               reformat = FALSE
                                )

  return(sheetUrl)
}
