library(tidyverse)
# Load the indicator list:
df_indicators <- kffR::shf_listIndicators()

googlesheets4::gs4_auth()
kffcommon@gmail.com
# Metadata for created indicators is stored:
# https://docs.google.com/spreadsheets/d/1y3uBO1qMHexOrtV5MeOKJPF0F2Pl8Eu3tRgwnq-v6Ps/edit#gid=0

#
# duplicateIndicator("Health Insurance Coverage of Children 0-18")
# duplicateIndicator("Health Insurance Coverage of the Nonelderly (0-64) with Incomes below 200% Federal Poverty Level (FPL)")
# duplicateIndicator("States with Firearm Laws Designed to Protect Children", nameNewSheet = "2023")

vec_indicatorsToDuplicate_ACS <- df_indicators |>
  filter(Source == "ACS") |>
  pull(Indicator) |>
  print()
vec_indicatorsToDuplicate_BRFSS <- df_indicators |>
  filter(Source == "BRFSS") |>
  filter(DocURL != "-") |>
  filter(is.na(Notes) | Notes != "odd year") |>
  pull(Indicator) |> print()
vec_indicatorsToDuplicate_BRFSS[1]


source("./R/duplicateIndicator.R")

i_start = 51# 11
i_end = 79# 30
for(i in i_start:i_end){
  print(i)
    duplicateIndicator(Indicator = vec_indicatorsToDuplicate_ACS[i],
                       nameNewSheet = "2022",
                       cleanName = TRUE,
                       cleanName_year = "2022")
  Sys.sleep(3)
}


