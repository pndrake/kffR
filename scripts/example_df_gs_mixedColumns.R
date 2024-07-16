library(tidyverse)

gs_id = "1ogCBlAsNl0x3Yjaf8CWzGZqZPkcE4hZz6z7fRGgdQhc"

# Load the data
data("iris")

# Example set "Suppressed"
maxValue = 5
suppressionText = "Suppressed"

# Create a new version of Sepal.Length that is of type "list" (lists can have mixed types)
# Note: everything in R is a list of lists
df_forOut <- iris |>
  rowwise() |>
  mutate(Sepal.Length_list = case_when(
    Sepal.Length > maxValue ~ list(suppressionText),
    TRUE ~ list(Sepal.Length)
  ),
  Sepal.Length_char= case_when(
    Sepal.Length > maxValue ~ as.character(suppressionText),
    TRUE ~ as.character(Sepal.Length)
  ),

  Sepal.Length_num = case_when(
    Sepal.Length > maxValue ~ as.numeric(suppressionText),
    TRUE ~ as.numeric(Sepal.Length)
  )

  ) |>
  ungroup() |>
  select(Original = Sepal.Length,
         as_char = Sepal.Length_char,
        as_num = Sepal.Length_num,
         as_list = Sepal.Length_list) |>
  print()



  googlesheets4::write_sheet(ss = gs_id,
                             data = df_forOut, sheet = "output")

df_sepalLength <-   googlesheets4::read_sheet(ss = gs_id, sheet = "output")
