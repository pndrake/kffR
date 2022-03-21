# Used to clean SHF google sheets as read in by googlesheets4::read_sheet()


read_sheets <- function(sheetURL,
                        indicatorName,
                        includeNotes = FALSE,
                        sheetName = "Year",
                        naString = "N/A"){

  tryCatch({
  # Read in a list of all the sheets in this google sheet
  vec_sheetNames <- googlesheets4::sheet_names(sheetURL)

  # Remove notes sheet if requested:
  if(!includeNotes){
    vec_sheetNames <- vec_sheetNames[!grepl("^Notes", vec_sheetNames)]
  }

  # Read in the data on every sheet
  list_sheetData <- lapply(vec_sheetNames,
                           function(x){
                             df_googleSheet <- googlesheets4::read_sheet(sheetURL,
                                                                         sheet = x,
                                                                         na = naString,
                                                                         skip = 0,
                                                                         # Note: can we get range working? Default checks 5000000 records.
                                                                         #range = "'2019'!'2:1000'"
                             )

                             # Extract the first row to properly label rows (percent vs number vs . . .):
                             # Note: have to bring in
                             vec_headerRow <- df_googleSheet[1,] %>%
                               unlist()


                             # Clean up the dataframe returned by google sheets:
                             df_googleSheet %>%
                               # Remove the first row (used above for column names)
                               dplyr::slice(2:dplyr::n()) %>%
                               # Add the name of the sheet to each data set (usually adding year)
                               #      And the indicator name
                               # Note: using GLUE to create column name from variable
                               dplyr::mutate("{sheetName}" := x,
                                      Indicator = indicatorName) %>%
                               dplyr::rename(State = "...1") %>%
                               # Clean the sheet
                               kffR::clean_sheet() %>%
                               # Reorder sheet so all the columns to pivot are on the right
                               dplyr::relocate(State, Year, Indicator) %>%
                               # Convert the table from Wide to Long format:
                               dplyr::group_by(State, !!as.name(sheetName), Indicator) %>%
                               tidyr::pivot_longer(cols = names(.)[4:ncol(.)],
                                            names_to = "Category",
                                            values_to = "Value"
                               ) %>%
                               dplyr::rowwise() %>%
                               # Add in the modifier we extracted from the first row of the google sheet:
                               dplyr::mutate(Type = vec_headerRow[which(Category == names(vec_headerRow))]) %>%
                               # Clean out text added by R to the category column:
                               dplyr::mutate(Category = gsub("\\.\\.\\.[0-9]*", "", Category)) %>%

                               # Unlist the value column
                               # Note: this is from reading in the google sheet with multiple variable types in each column.
                               #     R handles this by wrapping the column in lists (a column of lists is made up of one variable type i.e. lists, even if the lists contain numbers and characters)
                               dplyr::mutate(Value = as.numeric(Value)) %>%
                               # Note:it seems just using as.numeric works better
                               # dplyr::mutate(Value = dplyr::case_when(
                               #   is.na(Value) | is.null(Value)~ NA_real_,
                               #   unlist(Value) %in% c("NR") ~ NA_real_,
                               #   is.na(as.numeric(unlist(Value))) ~ NA_real_,
                               #   TRUE ~ unlist(Value))) %>%
                               # Move the new columns to the left next to State
                               dplyr::relocate(State, Year, Indicator, Category, Type)  %>%
                               # Remove any grouping/rowwise settings from this dataframe
                               dplyr::ungroup() %>%
                               return()
                           })

  # Convert the list of dataframes into one combined dataframe (long format)
  df_sheetData <- do.call(rbind, list_sheetData)

  # Return our combined dataframe:
  return(df_sheetData)

  },
  error=function(cond) {
    message("Error: please contact SHF team for support")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  })#,
  # warning=function(cond) {
  #   message("Warning: please contact SHF for support")
  #   message(cond)
  #   # Choose a return value in case of warning
  #   return(NULL)
  # })

}



