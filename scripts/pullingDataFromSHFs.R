# To do -----------

# 1. why did
# "https://docs.google.com/spreadsheets/d/1ZJBWBMfs7QrRNq53hMrFzMHM8DvAVhHbMFN-0L65DUY/edit#gid=1194362909"
# return NA
# from read_sheets(x)


library(future.apply)
plan(multicore)

future::

Snow



# Load a complete list of State Health Facts Indicators-----
df_indicators <- shf_listIndicators()
future_mapply()



n = 3
system.time({
  list_df_shf <- future_mapply(function(x,y){
    read_sheets(x, y)
  },
  df_indicators$DocURL[1:n],
  df_indicators$Indicator[1:n]
  )
})


# How long does it take to load and clean 10 indicators?
# . . . About seven minutes


# Load a complete list of State Health Facts Indicators-----
df_indicators <- shf_listIndicators()

n = 4

indicatorInfo <- list()
for(i in 1:n){
  indicatorInfo[[i]] <- c(df_indicators$DocURL[i],
                          df_indicators$Indicator[i])
}

system.time({
    list_df_shf <- lapply(indicatorInfo,
                          function(x){
      read_sheets(x[1], x[2]) %>%
        return()
    })
})


# Multithreaded version -----
plan(multisession, workers = 6)
n = 100

indicatorInfo <- list()
for(i in 1:n){
  indicatorInfo[[i]] <- c(df_indicators$DocURL[i],
                          df_indicators$Indicator[i])
}


system.time({
  list_df_shf <- future.apply::future_lapply(
    indicatorInfo,
    function(x){
      print(x)
      read_sheets(x[1], x[2]) %>%
        return()
    },
    future.seed = NULL

    )
})

# Errors on
  c(10, 13, 18)
# errors on:
  c(49, 62)

  # Strange tables:
  c("https://docs.google.com/spreadsheets/d/1JRcjCZClOqo0KHPw7LERqUrY5F3rrjkwRbiDG4ICgbk/edit#gid=1")

list_shf <- list()
system.time({
for(i in 1:n){
  print(i)
  x = indicatorInfo[[i]]
  list_shf[[i]] <- read_sheets(x[1], x[2])
}
})
list_df_shf


# Data quality checks -----

# Load an example of some SHFs data

list_df_nodes <- readRDS("./objects/shf_1to100.rds")

df_node <- list_df_nodes[[2]] %>%
  print()



# Creating a complete data frame of all shf indicators:
df_shf <- do.call(rbind, list_df_nodes)

# How many missing values?

df_shf %>%
  rowwise() %>%
  filter(!is.na(Indicator)) %>%
  group_by(Indicator, Year) %>%
    summarise(
              Number = n(),
              NumMissing = sum(is.na(Value), na.rm = TRUE),
              PerMissing = paste0(round(100*NumMissing/Number, 1), "%")
              ) %>%
  arrange(desc(NumMissing/Number)) %>%
  left_join(df_indicatorList %>%
              select(Indicator, DocURL),
            by = c("Indicator")) %>%
  write_csv("~/Data/shf_indicatorsMissingness.csv")

sum(is.na(df_node), na.rm = TRUE)/length(unlist(df_node))

# Simple regression looking for outliers:
library(modelr)
library(rstandard)

model_timeTrend <- function(df) {
  if(sum(is.na(df$Value)) == 0) return(NA)
  if(sum(is.na(df$Year)) == 0) return(NA)
  tryCatch(

    lm(Value ~ Year, data = df) %>%
      return(),


  error=function(cond) {
    message("Error: please contact SHF team for support")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  })


}
calculateStandardResidual <- function(df) {
  if(sum(is.na(df$Value)) == length(df$Value)) return(NA)
  if(sum(is.na(df$Year)) == length(df$Year)) return(NA)
  tryCatch(

    lm(Value ~ Year, data = df) %>%
      rstandard() %>%
      return(),


    error=function(cond) {
      message("Error: please contact SHF team for support")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    })


}

df_models <- df_shf %>%
  rowwise() %>%
  # Convert year to a numeric for use in our models
  mutate(Year = as.numeric(Year)) %>%
  # A few indicators didn't get read in-- remove them
  filter(!is.na(Indicator)) %>%
  # Organize a small dataframe for every indicator category combination
  group_by(State, Indicator, Category, Type) %>%
    nest() %>%
  # Fit the model to each category of data for each indicator:
  mutate(model = map(data, model_timeTrend),
         # Add residuals for each model
         resids_standardized = map(data, calculateStandardResidual))

# Extract the residuals form our nested dataframe
df_resids_max <- df_models %>%
  unnest( resids_standardized) %>%
  arrange(desc(abs(resids_standardized))) %>%
  #group_by(Indicator, State, Category, Type) %>%
  #summarise(MaxResid = max(abs(resids_standardized), na.rm = TRUE)) %>%
  left_join(df_indicatorList %>%
              select(Indicator, DocURL),
            by = "Indicator")


df_shf %>%
  filter(Indicator == "" &
           Category == "" &
           State == "Rhode Island")


df_resids_max %>%
  head(n = 100) %>%
  View()

# Plot these standarized residuals
df_resids_max %>%
  pull(resids_standardized) %>% plot()


df_resids %>%
  filter(grepl("Distribution of Allopathic Medical School Graduates by Gender", Indicator)) %>%
  View()


df_node
