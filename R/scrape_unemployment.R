# scrape_unemployment scrapes weekly unemployment data from:
# web address: https://oui.doleta.gov/unemploy/claims.asp
# and outputs it as a dataframe:
# state (chr)
# week (date)
# InitialClaims (dbl)
# ContinuedClaims (dbl)
# CoveredEmployment (dbl)
# InsuredUnemploymentRate (dbl)
# Note: SHFs needs only the latest week's value, setting latestWeekOnly = FALSE pulls in all data available from the webform


scrape_unemployment <- function(level = "state",
                                startYear = 2022,
                                endYear = 2022,
                                latestWeekOnly = TRUE) {

  # Webpage information ----
  form_url       <- "https://oui.doleta.gov/unemploy/claims.asp"
  form_xpath     <- "//*[@id='content']/table/form"
  response_xpath <- "//*[@id='content']/table" # note: not being used at the moment
  stateList_xpath <- "//*[@id='states']/option"


  # note: (To find a div's xpath, or id, or class)
  # look through DOM for the form/id name class or xpath
  # CSS ids are referenced via # (e.g #elementID) , and classes are referenced via . (e.g. .elementClass)
  # note: xpath can be found in the inspector via 1. right click 2. copy 3. copy xpath


  # Create the session for this webpage----
  session <- session(form_url)


  # Select and fill in the form----
  # Select the form from the html webpage:
  form_blank <- session %>%
    xml2::read_html() %>%
    # Select the nodelist for the form:
    rvest::html_node(xpath = form_xpath) %>%
    # Convert that nodelist to a form
    rvest::html_form()

  # Note: you can see the fields of the form like this:
  # form_blank$fields
  # form_blank$fields$`states[]`


  # Extract from the webpage all states in the state selector
  # Note: despite displaying state names, the form is expecting the abbreviations
  stateList_v <- session %>%
    html_node("#states") %>%
    html_nodes("option") %>%
    html_attr("value")




  # Set values for the selections
  form_filled <- form_blank %>%
    html_form_set(
      "level" = level,
      "strtdate" = as.character(startYear),
      "enddate" = as.character(endYear),
      "states[]" = stateList_v
    )

  # Submit the form ----
  form_response <-
    rvest::html_form_submit(form = form_filled, submit = "submit")

  # Extract the text values from the response ----
  df_response_raw <- form_response %>%
    # Extract the content of the response as text
    httr::content(., as = "text") %>%
    # Break out each row of the response:
    strsplit(x = ., split = "<week>") %>%
    as.data.frame() %>%
    as_tibble()

  # Reset the name of the first column
  columnName <- "response_raw"
  names(df_response_raw) <- columnName

  # Parse values from from the website's response text -----
  # Create patterns to match for each variable:
  # Note: see this primer on regex (and stringr) https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf
  divName_state = "stateName"
  divName_week = "weekEnded"
  divName_value_1 = "InitialClaims"
  divName_value_2 = "ContinuedClaims"
  divName_value_3 = "CoveredEmployment"
  divName_value_4 = "InsuredUnemploymentRate"

  divName_values = c("InitialClaims",
                     "ContinuedClaims",
                     "CoveredEmployment",
                     "InsuredUnemploymentRate"
                     )


  # this function creates a pattern to match from the html text output
  createRegex <- function(divName) {
    paste0("<", divName, ">", ".*", "</", divName, ">") %>%
      return()

  }


  # Note: here are some example text to match for each variable:
  testText_state = "\t<stateName>District of Columbia</stateName>\n"
  testText_week = "\t<weekEnded>01/23/2021</weekEnded>\n"
  testText_value = "t\t<InitialClaims>1,292</InitialClaims>\n\t"

  # Check that our patterns work:
  stringr::str_extract(testText_state, createRegex(divName_state))
  stringr::str_extract(testText_week, createRegex(divName_week))
  stringr::str_extract(testText_value, createRegex(divName_value))



  df_response_clean <- df_response_raw %>%
    # Remove the first row (artifact of reading in the data, is not really a table row)
    slice(2:n()) %>%
    # Create each column of the data by extracting pattern matches from the strings:
    # Note: how to do this by calling str_extract on my vector above "divName_values"?
    mutate(
      state_raw = stringr::str_extract(
        string = (!!as.name(columnName)),
        pattern =  createRegex(divName_state)
      ),
      week_raw = stringr::str_extract(
        string = (!!as.name(columnName)),
        pattern = createRegex(divName_week)
      ),
      value_raw_1 = stringr::str_extract(
        string = (!!as.name(columnName)),
        pattern = createRegex(divName_value_1)
      ),
      value_raw_2 = stringr::str_extract(
        string = (!!as.name(columnName)),
        pattern = createRegex(divName_value_2)
      ),
      value_raw_3 = stringr::str_extract(
        string = (!!as.name(columnName)),
        pattern = createRegex(divName_value_3)
      ),
      value_raw_4 = stringr::str_extract(
        string = (!!as.name(columnName)),
        pattern = createRegex(divName_value_4)
      )
    ) %>%
    # Drop the column of raw text
    select(-!!as.name(columnName)) %>%
    # Remove leftover html bits (e.g. <div> and </div>)
    mutate(
      state = gsub("</?[a-zA-Z0-9]*>", "", state_raw),
      week = gsub("</?[a-zA-Z]*>", "", week_raw),
      InitialClaims = gsub("</?[a-zA-Z]*>", "", value_raw_1),
      ContinuedClaims = gsub("</?[a-zA-Z]*>", "", value_raw_2),
      CoveredEmployment = gsub("</?[a-zA-Z]*>", "", value_raw_3),
      InsuredUnemploymentRate = gsub("</?[a-zA-Z]*>", "", value_raw_4),

    ) %>%
    # Format value and week:
    mutate(week = as.Date(week, format = "%m/%d/%Y"),
           InitialClaims = as.numeric(gsub(",", "", InitialClaims)),
           ContinuedClaims = as.numeric(gsub(",", "", ContinuedClaims)),
           CoveredEmployment = as.numeric(gsub(",", "", CoveredEmployment)),
           InsuredUnemploymentRate = as.numeric(gsub(",", "", InsuredUnemploymentRate))
           ) %>%
    # Remove extra columns, leaving our clean dataset:
    select(state,
           week,
           InitialClaims,
           ContinuedClaims,
           CoveredEmployment,
           InsuredUnemploymentRate
           )



  if(latestWeekOnly){

    # Pull out only the last week reported in the data for each state
    df_response_clean %>%
      group_by(state) %>%
        arrange(desc(week)) %>%
        slice(1:1) %>%
      return()

  } else {

    return(df_response_clean)

  }

}

