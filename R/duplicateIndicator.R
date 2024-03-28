# loadIndicatorList()


duplicateIndicator <- function(Indicator,
                               # Duplicate the latest year's data in new tab?
                               makeNewSheet = TRUE,
                               nameNewSheet = "2022", # What to call the new sheet?
                               cleanNewSheet = TRUE, # Should function remove data from new sheet? Note: this is responsive to data size, but does expect SHF style layout
                               # Name of google sheet
                               cleanName = TRUE,
                               cleanName_year = "2022", # Code edits "Indicator 2008-2021" -> "Indicator 2008-2022"
                               deleteOldSheet = FALSE,
                               publishToWeb = TRUE,
                               ArchiveOldSheet = FALSE,
                               sheetName_duplicationLog = "Duplicated indicators",
                               log_id = "1y3uBO1qMHexOrtV5MeOKJPF0F2Pl8Eu3tRgwnq-v6Ps"){

  # Extract URL of the Indicators existing google sheet and the source
  df_log_thisIndicator <- df_indicators %>%
    filter(Indicator == !!(Indicator))
  this_docURL <- df_log_thisIndicator %>%
    pull(DocURL)
  this_source <- df_log_thisIndicator %>%
    pull(Source) %>% unique()

  # Check that the indicator information was loaded correctly
  # And that the requested indicator exists in the Indicator Log
  if(is.na(thisDocURL)) return("error: load tidyverse")
  if(length(thisDocURL) == 0) return("error: Indicator not found")
  if(length(thisDocURL) > 1) return("error: Multiple indicators found")


  # Convert the url to a google id
  id_oldSheet <- googledrive::as_id(this_docURL)

  # Find the ids for the current folder, and the archive folder
  driveInfo_oldSheet <- googledrive::drive_get(id_oldSheet)
  id_parentFolder <- googledrive::as_id(driveInfo_oldSheet$drive_resource[[1]]$parents[[1]])


  df_itemsInParentFolder <- googledrive::drive_ls(id_parentFolder)
  # Create the archive folder (if it doesn't exist)
  if(!("Archive" %in% df_itemsInParentFolder$name)){
    # Make the new folder
    googledrive::drive_mkdir(name = "Archive", path = id_parentFolder)
  }

  df_itemsInParentFolder <- googledrive::drive_ls(id_parentFolder)

  id_archiveFolder <-  df_itemsInParentFolder|>
    filter(name == "Archive") |>
    pull(id)

  # Copy the existing google sheet
  df_newSheetInfo <- googledrive::drive_cp(file = id_oldSheet,
                                           path = id_parentFolder)

  # Add the google sheet url to the new sheet's info----
  toURL <- function(thisId){
    paste0("https://docs.google.com/spreadsheets/d/", thisId)
  }

  df_newSheetInfo <- df_newSheetInfo %>%
    mutate(Source = this_source,
           docURL = toURL(id))

  # Extract the google id of the new duplicated sheet:
  #id_newSheet <- apiResponse$id

  if(makeNewSheet){
    # Name of most recent sheet?
    df_newSheetInfo$sheetNames <- list(googlesheets4::sheet_names(df_newSheetInfo$id))

    # Copy the most recent sheet:
    googlesheets4::sheet_copy(from_ss = df_newSheetInfo$id,
                              from_sheet = df_newSheetInfo$sheetNames[[1]][1],
                              to_sheet = nameNewSheet)

    # if(deleteOldSheet){
    #   googlesheets4::sheet_delete(ss = df_newSheetInfo$id,
    #                               df_newSheetInfo$sheetNames[[1]][1])
    # }

    vec_sheetNames <- googlesheets4::sheet_names(df_newSheetInfo$id)
    name_copiedSheet <- vec_sheetNames |> head(n = 1)

    if(cleanNewSheet){

      # Remove data from sheet from the original copied sheet
      df_copiedSheet <- googlesheets4::read_sheet(df_newSheetInfo$id,
                                                  sheet = name_copiedSheet)

      # Which columns
      names_df_copiedSheet <- names(df_copiedSheet)
      index_columnsToClean <- 2:length(names_df_copiedSheet)
      names_columnsToClean <- names_df_copiedSheet[index_columnsToClean]

      # Which rows
      rows_withData <- 3:max(which(!is.na(df_copiedSheet[[names_df_copiedSheet[1]]])))

      # Which columns:
      cols_withData <- 2:length(names_df_copiedSheet)
      # Convert the colums to letters:
      cols_withData_LETTERS <- LETTERS[cols_withData]

      range_forDeletion <- paste0(
        cols_withData_LETTERS[1], rows_withData[1],
        ":",
        cols_withData_LETTERS[length(cols_withData_LETTERS)], rows_withData[length(rows_withData)] + 1
      )
      # Create a range to delete data from:
      range(rows_withData)[1]
      googlesheets4::range_clear(df_newSheetInfo$id,
                                 sheet = name_copiedSheet,
                                 range = range_forDeletion)


    }
  }


  # Rename the new sheet as requested
  # googlesheets4::sheet_rename(df_newSheetInfo$id,
  #                             sheet = name_copiedSheet,
  #                             new_name = nameNewSheet)



  # Move the old sheet to the archive:
  # googledrive::drive_cp(file = id_oldSheet,
  #                       path = id_archiveFolder,
  #                       name = driveInfo_oldSheet$name)
  if(ArchiveOldSheet){
    # Note:does this work?
    googledrive::drive_mv(file = id_oldSheet,
                          path = id_archiveFolder)
  }



  # Clean the indicator title
  if(cleanName){

    cleanedName <- df_newSheetInfo %>%
      pull(name) %>%
      gsub("^Copy of ", "", .) %>%
      gsub("[0-9]{4}$", cleanName_year, .)

    googledrive::drive_rename(file = df_newSheetInfo$id,
                              cleanedName)

    df_newSheetInfo$name[1] <- cleanedName

  }

  # Publish the duplicate to the web----
  if(publishToWeb){
    googledrive::drive_publish(df_newSheetInfo$id)
  }

  # Change ownership of the file to KFF Commons ----
  # new_owner_email = "kffcommon@gmail.com"

  # Log the metadata of the new sheet to the google sheet

  df_newSheetInfo %>%
    mutate(`Old URL` = toURL(id_oldSheet),
           Date = Sys.Date()) %>%
    select(Date, Source, Indicator = name, `New URL` = docURL, `Old URL`) %>%
    googlesheets4::sheet_append(ss = log_id ,
                                data =  .,
                                sheet = sheetName_duplicationLog)
  return(df_newSheetInfo)

}
