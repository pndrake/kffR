# Used to clean SHF google sheets as read in by googlesheets4::read_sheet()

#"Total Monthly Medicaid and CHIP Enrollment"
read_sheets <- function(docURL,
                        indicatorName,
                        includeNotes = FALSE,
                        sheetName = "Year",
                        numberOfSheets = 0,
                        naString = "N/A"){

  tryCatch({
  # Read in a list of all the sheets in this google sheet
  vec_sheetNames <- googlesheets4::sheet_names(docURL)

  # Remove notes sheet if requested:
  if(!includeNotes){
    vec_sheetNames <- vec_sheetNames[!grepl("^Notes", vec_sheetNames)]
  }

  # Subset sheet names to user selection:
  if(numberOfSheets > 0){

    n <- length(vec_sheetNames)

    # If the user has selected more sheets than exist, return all that exist
    if(numberOfSheets > n){
      numberOfSheets <- n
    }

    index_startSheet <- (n + 1) - numberOfSheets
    index_endSheet   <- n

    index_startSheet <- 1
    index_endSheet   <- index_startSheet + (numberOfSheets - 1)

    vec_sheetNames <- vec_sheetNames[index_startSheet:index_endSheet]
  }


  # Read in the data on every sheet
  list_sheetData <- lapply(vec_sheetNames,
                           function(x){
                             df_googleSheet <- googlesheets4::read_sheet(docURL,
                                                                         sheet = x,
                                                                         na = naString,
                                                                         skip = 0,
                                                                         # Note: can we get range working? Default checks 5000000 records.
                                                                         #range = "'2019'!'2:1000'"
                             )


                             # Which row starts the data?
                             # Note: categories and datatypes usually take 1 or 2 rows
                             firstRowOfData <- which(df_googleSheet$...1 != "")[1]
                             # Note: currently I have coded these for 1 and 2 rows
                             if(!firstRowOfData %in% c(2,3)) print("first row of data not in programmed conditions")

                             # If there is a subcategory (aka first row of data =3) we need to pull that out and add it to the description
                             if(firstRowOfData == 3){
                               vec_subcategoryText = df_googleSheet[1,] %>%
                                 unlist()
                             } else {
                                # Note: if there is no subcategory I am making these empty strings
                                #         We'll paste these values to the main categories later
                                vec_subcategoryText = rep("", ncol(df_googleSheet))
                             }


                             # Add the subcategory text to the names values of the header row
                             #     The names are used to define the category variable when we pivot longer
                             value_seperator = "___"
                             names(df_googleSheet) <- paste(names(df_googleSheet), vec_subcategoryText, sep = value_seperator)


                             # Extract the first row to properly label rows (percent vs number vs . . .):
                             # Note: have to bring in
                             vec_headerRow <- df_googleSheet[(firstRowOfData - 1),] %>%
                               unlist()

                             # Clean up the dataframe returned by google sheets:
                             df_raw <- df_googleSheet %>%
                               # Remove the first row (used above for column names)
                               dplyr::slice(firstRowOfData:dplyr::n()) %>%
                               # Add the name of the sheet to each data set (usually adding year)
                               #      And the indicator name
                               # Note: using GLUE to create column name from variable
                               dplyr::mutate("{sheetName}" := x,
                                      Indicator = indicatorName) %>%
                               dplyr::rename(State = paste0("...1", value_seperator)) %>%
                               # Clean the sheet
                               kffR::clean_sheet() %>%
                               # Remove any null values if they are present:
                               rowwise() %>%
                               mutate_all(function(x){
                                 ifelse(
                                   x %>%
                                    unlist() %>%
                                     is.null(),
                                   # Note: making these NA_character_. This could impact numeric columns later on
                                   #    na.omit() below should allow the column type checker to handle this properly.
                                   #  We don't want to convert a numeric column with a NA_character_ to character if we
                                   #    don't have to.
                                   list(NA_character_), list(x)) %>%
                                   return()
                               }) %>%
                               ungroup() %>%
                               # Determine what type of columns are present then unlist the data
                               # Note: columns should be character if any of their rows are characters, and numeric otherwise
                               mutate_all(function(x){
                                 if(any(is.character(na.omit(unlist(x))))){
                                   x %>%
                                     unlist() %>%
                                     as.character() %>%
                                     return()
                                 } else {
                                   x %>%
                                     unlist() %>%
                                     as.numeric() %>%
                                     return()
                                 }
                               }) %>%
                               ungroup() %>%
                               # Reorder sheet so all the columns to pivot are on the right
                               dplyr::relocate(State, Year, Indicator)

                             # Which columns are numeric or character ?
                             # We need to unlist the lists, then consider the numbers/characters separately
                             df_data <- df_raw %>% select(-State, -Year, -Indicator)

                             columns_character <- sapply(df_data, function(x) typeof(x) == "character")
                             columns_numeric <- sapply(df_data, function(x) typeof(x) %in% c("integer", "double", "complex"))
                             # Attach the character columns to the state/year/indicator
                             df_characters <- df_raw %>%
                               select(State, Year, Indicator) %>%
                               cbind(df_data[columns_character])
                             df_numbers <- df_raw %>%
                               select(State, Year, Indicator) %>%
                               cbind(df_data[columns_numeric])


                             # Combine the dataframes that have actual data
                             # Note: reference data = state, year, indicator at the moment
                             numColsReferenceData <- 3
                             df_all <- data.frame()

                             if(ncol(df_characters) > numColsReferenceData){
                               df_all <- bind_rows(df_all,
                                                   df_characters %>%
                                                     dplyr::group_by(State, !!as.name(sheetName), Indicator) %>%
                                                     tidyr::pivot_longer(cols = names(.)[4:ncol(.)],
                                                                         names_to = "Category",
                                                                         values_to = "Value_character"
                                                     )
                               )
                             }
                             if(ncol(df_numbers) > numColsReferenceData) {
                               df_all <- bind_rows(df_all,
                                                   df_numbers %>%
                                 dplyr::group_by(State, !!as.name(sheetName), Indicator) %>%
                                 tidyr::pivot_longer(cols = names(.)[4:ncol(.)],
                                                     names_to = "Category",
                                                     values_to = "Value"
                                 )
                               )
                             }
                             # Combine the character and numeric data frames
                            df_forOut <-  df_all %>%
                               dplyr::rowwise() %>%
                               # Add in the modifier we extracted from the first row of the google sheet:
                               dplyr::mutate(Type = vec_headerRow[which(Category == names(vec_headerRow))]) %>%
                               # Clean out text added by R to the category column:
                               dplyr::mutate(Category = gsub("\\.\\.\\.[0-9]*", "", Category)) %>%
                               dplyr::mutate(Category = gsub(paste0("^", value_seperator), "", Category)) %>%
                               dplyr::mutate(Category = gsub(paste0(value_seperator,"$"), "", Category))

                            # If all records in this dataset are characters or numbers, add the other type for consistency
                            if(!"Value" %in% names(df_forOut)) df_forOut$Value <- NA
                            if(!"Value_character" %in% names(df_forOut)) df_forOut$Value_character <- NA

                            df_forOut %>%
                               # Move the new columns to the left next to State
                               dplyr::relocate(State, Year, Indicator, Category, Type, Value)  %>%
                               # Remove any grouping/rowwise settings from this dataframe
                               dplyr::ungroup() %>%
                               return()

                           })

  # Convert the list of dataframes into one combined dataframe (long format)
  df_sheetData <- do.call(bind_rows, list_sheetData)

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



