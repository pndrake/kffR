

fetchIndicator <- function(indicatorName,
                           sheetName = "Year",
                           numberOfSheets = 0,
                           printURL = FALSE){

  # Load indicator table
  # Note: only does this the first time of this session:
  if((!exists("df_indicators"))) {
    df_indicators <<- kffR::shf_listIndicators()
  }

  # Does the requested indicator exist?
  if(!indicatorName %in% df_indicators$Indicator) return(NA)
  # Does the requested indicator have a URL:
  if(is.na(df_indicators$DocURL[which(df_indicators$Indicator == indicatorName)])) return(NA)

  thisDocURL <- df_indicators %>%
    filter(Indicator %in% c(indicatorName)) %>%
    pull(DocURL)

  if(printURL) print(thisDocURL)

  # Load the requested table:
  df_thisIndicator <- thisDocURL %>%
    kffR::read_sheets(indicatorName = indicatorName,
                      numberOfSheets = numberOfSheets,
                      sheetName = sheetName)

  # Mark that indicators don't need to be reloaded
  reloadIndicators <<- FALSE

  # Return the requested indicators data
  return(df_thisIndicator)
}
