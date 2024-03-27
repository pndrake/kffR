# 'loadIndicatorList'
# Input: none
# Output: loads indicator list to global environment (if not already present)

loadIndicatorList <- function(){

  # Load indicator table
  # Note: only does this the first time of this session:
  if((!exists("df_indicators"))) {
    df_indicators <<- shf_listIndicators()
  }

}
