# library(tidyverse)
#
#
# # working document for duplicateIndicator.R
# # Can delete once finihed
# source("./R/duplicateIndicator.R")
#
#
#
# # duplicateIndicator("Population Distribution of Nonelderly Adults by Race/Ethnicity")
#
# sourceName = 'WONDER'
#
#
# vec_indicatorsToDuplicate <- df_indicators %>%
#   filter(grepl("cancer inc", Indicator, ignore.case = TRUE)) |>
#   pull(Indicator) |>
#   print()
#
# for (i  in 1:length(vec_indicatorsToDuplicate)) {
#   thisIndicator <- vec_indicatorsToDuplicate[i] |> print()
#   duplicateIndicator(thisIndicator)
# }
#
# # fixed: Broke on i = 41 # Cause = "'" in title on GS
# for (i in 21:nrow(df_indicators_bhs)) {
#   thisIndicator <- df_indicators_bhs$Indicators[i] |> print()
#
#   thisYearsToInclude <- df_indicators_bhs$Years[i] |> print()
#
#   # Should we delete the old sheet?
#   deleteOldSheet <- !thisYearsToInclude %in% c("Both")
#
#   # Should we create a new indicator GS for the selected Indicator?
#   createFromTemplate <- thisYearsToInclude %in% c("2022 only")
#
#   if (createFromTemplate) {
#     print("creating from template")
#     # "2022 only"
#     id_template <-
#       googledrive::as_id("1QDJq4bgCpFx77A1nDGb5lMCC2pAG2aX2ZbsriivgsWM")
#     id_folder <-
#       googledrive::as_id("1CT6TJHH62BE-cGCic3qE9BFHWGLMfdHF")
#     # Copy the existing google sheet
#     df_newSheetInfo <- googledrive::drive_cp(name = thisIndicator,
#                                              file = id_template,
#                                              path = id_folder)
#
#     googledrive::drive_publish(df_newSheetInfo$id)
#
#     df_indicators_bhs$`SHF URL`[i] <-
#       paste0("https://docs.google.com/spreadsheets/d/",
#              df_newSheetInfo$id)
#   }
#
#   if (FALSE) {
#     #
#     print("duplicating this indicator")
#     if (!deleteOldSheet) {
#       newNameYearLabel <- ifelse(deleteOldSheet, "2022", "2018-2022")
#
#
#       df_thisDuplicate <- duplicateIndicator(
#         Indicator = thisIndicator,
#         makeNewSheet = TRUE,
#         nameNewSheet = "2022",
#         cleanName = TRUE,
#         cleanName_year = newNameYearLabel,
#         deleteOldSheet = deleteOldSheet,
#         publishToWeb = TRUE
#       )
#
#
#       df_indicators_bhs$`SHF URL`[i] <- df_thisDuplicate$docURL
#       # df_thisDuplicate$docURL
#       # df_thisDuplicate$drive_resource[[1]]$parents[[1]]
#
#     }
#
#
#
#   }
# }



# Write urls to the steering google sheet----

# googlesheets4::sheet_add("https://docs.google.com/spreadsheets/d/15zVQLY6oqY5xdtb8vs6nI-cExadQqMhXwFdjupO9EiU/edit#gid=0",
#                          "temp")

# googlesheets4::sheet_write(data = df_indicators_bhs,
#                            ss = "https://docs.google.com/spreadsheets/d/15zVQLY6oqY5xdtb8vs6nI-cExadQqMhXwFdjupO9EiU/edit#gid=0",
#                            sheet = "temp")
# df_indicators_bhs
