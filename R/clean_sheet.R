

clean_sheet <- function(df_fromGoogleSheet){


  # Note: we removed the need for this functionality:
  # Remove the first row (specifies data type for website)
  #df_toFormat <- df_fromGoogleSheet %>%
  #  slice(2:n())


  #
  # Reformat any "N/A" values to NA (i.e., is.na("N/A") = FALSE while is.na(NA) = TRUE)
  #df_toFormat[df_toFormat == naString] <- NA

  #str(df_fromGoogleSheet)

return(df_fromGoogleSheet)

}
