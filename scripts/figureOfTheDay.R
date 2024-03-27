
# Load resources:
#library(kffR)
library(tidyverse)


# Load the list of indicators in SHFs
df_indicators <- shf_listIndicators()


# Attempt 1: Home and community based care for the elderly----


# Medicaid Benefits: Nursing Facility Services, Other Than in an Institution for Mental Disease, Age 21+
possibleMatches <- c("Medicaid Benefits: Program of All-Inclusive Care for the Elderly (PACE)",
                     "Medicaid Behavioral Health Services: Assertive Community Treatment",
                     "Medicaid Benefits: Home Health Services – Nursing Services, Home Health Aides, and Medical Supplies/Equipment",
                     "Medicaid Section 1915(c) Home and Community-Based Service Waiver Expenditures, by Type of Waiver ($, in thousands)",
                     "Total Aged and Aged/Disabled Medicaid Section 1915(c) Home and Community-Based Services Waivers Expenditures ($, in thousands)",
                     "Total Aged and Aged/Disabled Medicaid Section 1915(c) Home and Community-Based Services Waivers Participants",
                     "Total Number of Medicaid Section 1915(c) Home and Community-Based Services Waivers",
                     "Medicaid Home Health Participants",
                     "Medicare Service Use: Home Health Services")


#
df_indicators %>%
  filter(Indicator %in% c(possibleMatches))

read_sheets()

ggplot +
  theme_KFF()




