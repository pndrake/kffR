library(tidyverse)
library(plotly)
library(viridis)



#url = url_totalHospitals
url = "https://docs.google.com/spreadsheets/d/1UDQCMZrtMut213aoge-eQt7g_C0UYckBnbcQmRTries/edit#gid=1329887062"

compareYears <- function(Indicator, url = NA, makePlot = TRUE){


  if(is.na(url)){ # if requested, read in a live SHF indicator
    df_thisIndicator_raw <- fetchIndicator(Indicato )
  } else { # Otherwise, pull in data from the url of a non-live SHF sheet
    df_thisIndicator_raw <- fetchIndicator(indicatorName = Indicator,
                                                 URL = url )
  }



  df_thisIndicator <- df_thisIndicator_raw |>
    # Attempt to combine the character and numeric values
    mutate(Value = ifelse(is.na(Value), as.numeric(Value_character), Value)) |>
    print()


  df_thisIndicator <- df_thisIndicator %>%
    arrange(State, Year) %>%
    group_by(State, Category) %>%
    mutate(
      Numerical_Difference = (Value - lag(Value, default = NA)),
      Proportional_Difference = round(100*Numerical_Difference/lag(Value, default = 0), 3)
      ) |>
    ungroup() |>
    print()


  # Plot
  if(makePlot){
    plot <- df_thisIndicator %>%
      arrange(Proportional_Difference) |>
      mutate(Index = row_number()) |>
      ggplot(aes(x = Index   , y = Proportional_Difference, label = State, label1 = Category,label2 = Year)) +
      geom_point(aes(size = Numerical_Difference, color = Year)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      scale_color_viridis(discrete = TRUE) +
      xlab("Index") +
      ylab("Proportional Difference") +
      labs(title = Indicator) +
      theme_minimal() +
        theme(legend.position = "none")
    # Convert ggplot object to Plotly
    plotly_plot <- ggplotly(plot)

    # Display the Plotly plot
    print(plotly_plot)


    plot <-  plot  +
      facet_wrap(~Year)
    plotly_plot <- ggplotly(plot)

    # Display the Plotly plot
    print(plotly_plot)


  }


  return(df_thisIndicator)

}


system.time({
df_totalHospitals <- compareYears(Indicator = "Total hospitals",
                                  url =  "https://docs.google.com/spreadsheets/d/1pRO8Ah5FO2Gr5UlDiyUoyixeK3nr3DssFlsAH4SfZdA/edit#gid=1123279521")

df_totalHospitalBeds <- compareYears(Indicator = "Total Hospital Beds",
                                     url = "https://docs.google.com/spreadsheets/d/1xSuDF_wx0UMHYf-EJklEzsFYLQHqdBpGBO_VWQsNQXU/edit#gid=1515794933")

df_expenses_perInpatientDay <- compareYears(Indicator = "Hospital Adjusted Expenses per Inpatient Day",
                                            url = "https://docs.google.com/spreadsheets/d/1HEsPG3iQ2R3B6wMP9jNTfx1CJH0zbfBkZ__yt-5WhBY/edit#gid=2138425478")

df_ERvisits_byOwnership <- compareYears(Indicator = "ER visits by ownership",
                                            url = "https://docs.google.com/spreadsheets/d/1r65yCLzDM34zaWMWmpz9WQWd6OUwit228jF0tVggbfI/edit#gid=723846441")

df_ERvisits_byOwnership <- compareYears(Indicator = "ER visits by ownership",
                                        url = "https://docs.google.com/spreadsheets/d/1r65yCLzDM34zaWMWmpz9WQWd6OUwit228jF0tVggbfI/edit#gid=723846441")

df_ERvisits_byOwnership <- compareYears(Indicator = "ER visits by ownership",
                                        url = "https://docs.google.com/spreadsheets/d/1r65yCLzDM34zaWMWmpz9WQWd6OUwit228jF0tVggbfI/edit#gid=723846441")



})
