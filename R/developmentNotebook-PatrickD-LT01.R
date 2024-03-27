
# working document for duplicateIndicator.R
# Can delete once finished
source("./R/duplicateIndicator.R")

copyID <- "1WbdlAnxWfKtCE09W-u2IHX_MM48sJXdneJ75T7vC5U8"

# df_indicators_bhs <- df_indicators %>%
#   filter(grepl("Medicaid Behavioral Health Services", Indicator))
df_indicators_bhs_raw <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/15zVQLY6oqY5xdtb8vs6nI-cExadQqMhXwFdjupO9EiU/edit#gid=0")

df_indicators_bhs <- df_indicators_bhs_raw %>%
  filter(!grepl("DELETE", Notes))

df_indicators_bhs$`SHF URL` <- NA

# Finished
c(1:3)
c(40, 41)



#
# fixed: Broke on i = 41 # Cause = "'" in title on GS
for(i in 21:nrow(df_indicators_bhs)){



thisIndicator <- df_indicators_bhs$Indicators[i] |> print()

thisYearsToInclude <- df_indicators_bhs$Years[i] |> print()

# Should we delete the old sheet?
deleteOldSheet <- !thisYearsToInclude %in% c("Both")

# Should we create a new indicator GS for the selected Indicator?
createFromTemplate <- thisYearsToInclude %in% c("2022 only")

if(createFromTemplate){

  print("creating from template")
    # "2022 only"
    id_template <-  googledrive::as_id("1QDJq4bgCpFx77A1nDGb5lMCC2pAG2aX2ZbsriivgsWM")
    id_folder <- googledrive::as_id("1CT6TJHH62BE-cGCic3qE9BFHWGLMfdHF")
    # Copy the existing google sheet
    df_newSheetInfo <- googledrive::drive_cp(name = thisIndicator,
                                             file = id_template,
                                             path = id_folder)

    googledrive::drive_publish(df_newSheetInfo$id)

    df_indicators_bhs$`SHF URL`[i] <- paste0("https://docs.google.com/spreadsheets/d/", df_newSheetInfo$id)
}

if(FALSE) {

#
  print("duplicating this indicator")
if(!deleteOldSheet){

newNameYearLabel <- ifelse(deleteOldSheet, "2022", "2018-2022")


df_thisDuplicate <- duplicateIndicator(Indicator = thisIndicator,
                                       makeNewSheet = TRUE,
                                       nameNewSheet = "2022",
                                       cleanName = TRUE,
                                       cleanName_year = newNameYearLabel,
                                       deleteOldSheet = deleteOldSheet,
                                       publishToWeb = TRUE)


df_indicators_bhs$`SHF URL`[i] <- df_thisDuplicate$docURL
# df_thisDuplicate$docURL
# df_thisDuplicate$drive_resource[[1]]$parents[[1]]

}



}
}



# Write urls to the steering google sheet----

# googlesheets4::sheet_add("https://docs.google.com/spreadsheets/d/15zVQLY6oqY5xdtb8vs6nI-cExadQqMhXwFdjupO9EiU/edit#gid=0",
#                          "temp")

googlesheets4::sheet_write(data = df_indicators_bhs,
                           ss = "https://docs.google.com/spreadsheets/d/15zVQLY6oqY5xdtb8vs6nI-cExadQqMhXwFdjupO9EiU/edit#gid=0",
                           sheet = "temp")
df_indicators_bhs


