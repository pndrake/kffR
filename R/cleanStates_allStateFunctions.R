library(pdftools)

medicaidStateClean <- list()
medicaidStateClean$Illinois <- function() {

  # Setup
  StorageFilePath = "./State Data Downloads/Illinois"


  # Latest file name:
  allFiles <- list.files(StorageFilePath)

  # Loop through each pdf and create an organized dataframe:
  list_df_enrollment <- lapply(allFiles, function(thisFile){

  # Load the file into R
  vec_rows <- pdftools::pdf_text(paste0(StorageFilePath, "/", thisFile)) %>%
    strsplit("\n") %>% unlist()

  # Extract the time period of this pdf

  index_date <- which(grepl("Enrollments", vec_rows, ignore.case = TRUE))[1]
  # Note the Thru: date - date row is on all pages of the pdf
  date_row <- vec_rows[index_date] %>%
    stringr::str_trim()


  month_name <- stringr::str_extract(date_row, month.name) %>% na.omit()
  year_data <- stringr::str_extract(date_row, "[0-9]{4}") %>% as.numeric()

  # Find the value row ("Total")
  total_value <- vec_rows[tail(grep("Total", vec_rows), n = 1)] %>%
    strsplit(x = ., split = " ") %>%
    unlist() %>%
    gsub(",", "", .) %>%
    as.numeric() %>%
    na.omit() %>%
    tail(n = 1)

  # Combine everything into an output dataset:
  df_thisPDF <- cbind(month_name,
                         total_value) %>%
    as.data.frame(row.names = FALSE) %>%
    # Clean up the months and years:
    rowwise() %>%
    mutate(Month = which(month.name %in% month_name),
           Year = year_data) %>%
    mutate(State = "Illinois",
           Abb = "IL") %>%
    select(State, Abb, Month, Year, Value = total_value)

  return(df_thisPDF)
})

# Create one large dataframe from this list of dataframes
  df_enrollment <- do.call(rbind, list_df_enrollment)


  return(df_enrollment)
}
medicaidStateClean$Florida <- function() {
  # Setup
  targetSheetName <- "MEDICAID"
  StorageFilePath = "./State Data Downloads/Florida"


  # Latest file name:
  thisFile <- list.files(StorageFilePath) %>% tail(n = 1)

  # Load the file into R
  df_data <-
    readxl::read_xls(paste0(StorageFilePath, "/", thisFile), sheet = targetSheetName)

  # Which row has enrollment totals?
  row_enrollmentTotals <-
    which(df_data$...1 == "FLORIDA_MEDICAID ENROLLMENT TOTAL")

  if (length(row_enrollmentTotals) == 0)
    return("Failed: no row with totals")
  if (length(row_enrollmentTotals) > 1)
    return("Failed: too many enrollment total rows")

  # Which row has months:
  row_months <- which(df_data$...1 == "PLAN_NAME")[1]

  # Extract months and totals:
  months_raw <- df_data[row_months, ] %>% unlist()
  values_raw <-
    df_data[row_enrollmentTotals, ] %>% unlist() %>% as.numeric()

  index_ofData <- grepl("[0-9]{4}", months_raw)


  df_enrollment <- cbind(months_raw[index_ofData],
                         values_raw[index_ofData]) %>%
    as.data.frame(row.names = FALSE) %>%
    # Clean up the months and years:
    rowwise() %>%
    mutate(Month = which(tolower(strsplit(V1, split = "_")[[1]][1]) == tolower(month.name)),
           Year = as.numeric(strsplit(V1, split = "_")[[1]][2])) %>%
    mutate(State = "Florida",
           Abb = "FL") %>%
    select(State, Abb, Month, Year, Value = V2)

  return(df_enrollment)
}
medicaidStateClean$NewMexico <- function() {

  # Setup
  StorageFilePath = "./State Data Downloads/New Mexico"


  # Latest file name:
  allFiles <- list.files(StorageFilePath)

  i = 1
  thisFile <- allFiles[i]
  # Load the file into R
  vec_rows <- pdftools::pdf_text(paste0(StorageFilePath, "/", thisFile)) %>%
    strsplit("\n") %>% unlist()

  # Extract the time period of this pdf
  vec_rows[52]
  index_date <- which(grepl("thru:", vec_rows, ignore.case = TRUE))[1]
  # Note the Thru: date - date row is on all pages of the pdf
  date_row <- vec_rows[index_date] %>%
    stringr::str_trim() %>%
    stringr::str_extract_all("[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
    unlist()

  date_start <- date_row[1]
  date_end <- date_row[2]

  month_start <- stringr::str_extract(date_start, "[0-9]{2}")
  month_end <- stringr::str_extract(date_end, "[0-9]{2}")
  year_data <- stringr::str_extract(date_end, "[0-9]{4}") %>% as.numeric()
  # Grab those months' names:
  month_name <- month.name[as.numeric(month_start)]


  # Are the start/end months the same?
  if(month_start != month_end) break

  # Find the value row ("Total")
  total_value <- vec_rows[tail(grep("Grand Total", vec_rows), n = 1)] %>%
    strsplit(x = ., split = " ") %>%
    unlist() %>%
    gsub(",", "", .) %>%
    as.numeric() %>%
    na.omit() %>%
    tail(n = 1)

  # Combine everything into an output dataset:
  df_enrollment <- cbind(month_name,
                         total_value) %>%
    as.data.frame(row.names = FALSE) %>%
    # Clean up the months and years:
    rowwise() %>%
    mutate(Month = month_start,
           Year = year_data) %>%
    mutate(State = "New Mexico",
           Abb = "NM") %>%
    select(State, Abb, Month, Year, Value = total_value)

  return(df_enrollment)
}
medicaidStateClean$NewYork <- function() {

  # Setup
  StorageFilePath = "./State Data Downloads/New York"


  # Latest file name:
  allFiles <- list.files(StorageFilePath)
  # Loop through each pdf and create an organized dataframe:
  list_df_enrollment <- lapply(allFiles, function(thisFile){

    # Read in and rename columns of thisFile
    df_raw_enrollment <- read_csv(paste0(StorageFilePath, "/", thisFile))
    names(df_raw_enrollment) <- c("Date", "NYC", "Rest", "Enrollment")


    stringr::str_extract("October 2021", month.name) %>% na.omit()
    df_raw_enrollment %>%
      # Remove the header row
      slice(2:(n()-1)) %>%
      rowwise() %>%
      # Extract out the month and year from the date:
      mutate(Year = stringr::str_extract(Date, "[0-9]{4}"),
             Month = which(month.name %in% strsplit(Date, split = " ")[[1]][1]))  %>%
      # Convert Enrollment to numeric
      mutate(Value = as.numeric(gsub(",", "", Enrollment))) %>%
      mutate(State = "New York",
             Abb = "NY") %>%
      select(State, Abb, Month, Year, Value) %>%
      return()

  })

  # Create one large dataframe from this list of dataframes
  df_enrollment <- do.call(rbind, list_df_enrollment)


  return(df_enrollment)
}
medicaidStateClean$WestVirginia <- function() {

  # Setup
  StorageFilePath = "./State Data Downloads/West Virginia"


  # Latest file name:
  thisFile <- list.files(StorageFilePath) %>% tail(n = 1)

  # Load the file into R
  vec_rows <- pdftools::pdf_text(paste0(StorageFilePath, "/", thisFile)) %>%
    strsplit("\n") %>% unlist()

  # Find the value row ("Total")
  total_row <- vec_rows[tail(grep("Total", vec_rows), n = 1)] %>%
    strsplit(x = ., split = " ") %>%
    unlist() %>%
    gsub(",", "", .) %>%
    as.numeric() %>%
    na.omit()

  # How many months of data do we have?
  numberMonths <- length(total_row)

  # Grab those months' names:
  month_row <- month.name[1:numberMonths]

  # What year are these data from?
  thisYear <-   stringr::str_extract(thisFile, "[0-9]{4}.pdf") %>%
    gsub(".pdf", "", .) %>%
    as.numeric()



  df_enrollment <- cbind(month_row,
                         total_row) %>%
    as.data.frame(row.names = FALSE) %>%
    # Clean up the months and years:
    rowwise() %>%
    mutate(Month = which(month_row == month.name),
           Year = thisYear) %>%
    mutate(State = "West Virginia",
           Abb = "WV") %>%
    select(State, Abb, Month, Year, Value = total_row)

  return(df_enrollment)
}
medicaidStateClean$Washington <- function() {

  # Setup
  StorageFilePath = "./State Data Downloads/Washington"


  # Latest file name:
  thisFile <- list.files(StorageFilePath) %>% tail(n = 1)

  # Load the file into R
  df_data <-
    readxl::read_xlsx(paste0(StorageFilePath, "/", thisFile))


  # Which row has enrollment totals?
  row_enrollmentTotals <-df_data[which(df_data[,c(1)] == "Grand Total"),] %>% unlist()

  # Which row has months:
  row_months <- df_data[5,] %>% unlist()

  # Extract months and totals:
  months_raw <- na.omit(str_extract(row_months, "[0-9]{6}"))
  values_raw <- row_enrollmentTotals[row_months %in% months_raw] %>% unlist() %>% as.numeric()

  df_enrollment <- cbind(months_raw,
                         values_raw) %>%
    as.data.frame(row.names = FALSE) %>%
    # Clean up the months and years:
    rowwise() %>%
    mutate(Month = as.numeric(str_extract(months_raw, "[0-9]{2}$")),
           Year = as.numeric(str_extract(months_raw, "^[0-9]{2}"))) %>%
    mutate(State = "Washington",
           Abb = "WA") %>%
    select(State, Abb, Month, Year, Value = values_raw)

  return(df_enrollment)

}

