#
# Note: will need to set up permissions of your google account to read/write spreadsheets
shf_listIndicators <- function(){

  url_sheet <- "https://docs.google.com/spreadsheets/d/12y734scgISKLaduk3g2QMtkKeA9ii31-w1q5JAY3pvQ/edit#gid=0"

  df_sheet <- googlesheets4::read_sheet(url_sheet)

  df_indicatorList <- df_sheet %>%
    select(Indicator = `SHF Link`,
           Category,
           Subcategory,
           Source,
           Date,
           YearLastUpdated = `Data Year of Last Update`,
           Team, Notes,
           DocURL = `Current Gdoc`,
           Updated, Reviewed, Posted)

  return(df_indicatorList)

}





# 'loadIndicatorList'
# Input: none
# Output: loads indicator list to global environment (if not already present)

loadIndicatorList <- function(){

  # Load indicator table
  # Note: only does this the first time of this session:
  if((!exists("df_indicators"))) {
    df_indicators <<- kffR::shf_listIndicators()
  }

}
