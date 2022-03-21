# scrape_wonder_births scrapes weekly unemployment data from:
# web address:
# and outputs it as a dataframe:


scrape_wonder_births <- function(){

  # Webpage information ----
  form_url       <- "https://wonder.cdc.gov/natality-expanded-current.html"

  form_xpath     <- "//*[@id='wonderform']"
  #response_xpath <- "//*[@id='content']/table" # note: not being used at the moment
  #stateList_xpath <- "//*[@id='states']/option"


  # note: (To find a div's xpath, or id, or class)
  # look through DOM for the form/id name class or xpath
  # CSS ids are referenced via # (e.g #elementID) , and classes are referenced via . (e.g. .elementClass)
  # note: xpath can be found in the inspector via 1. right click 2. copy 3. copy xpath


  # Create the session for this webpage----
  session <- session(form_url)


  # Submit the WONDER data use consent form:
  response <- session %>%
    xml2::read_html() %>%
    # Select the nodelist for the form
    rvest::html_node(xpath = form_xpath) %>%
    # Convert that nodelist to a form
    rvest::html_form() %>%
    # Press the consent button
    rvest::html_form_submit(submit = "action-I Agree")

  # Select and fill in the form----

  # Select the form from the html webpage:
  form_blank <- response %>%
    xml2::read_html() %>%
    # Select the nodelist for the form:
    rvest::html_node(xpath = form_xpath) %>%
    # Convert that nodelist to a form
    rvest::html_form()


  # Wonder selection field ids:


  # Geography
  # B_1 = Grouping variable
  #   includes geographies, demo, and health characteristics
  wonder_grouping <- "SB_1"

  # Set values for the selections
  form_filled <- form_blank %>%
    html_form_set(
      "B_1" = "D149.V21-level1"
    )

  # Submit the form ----
  form_response <-
    rvest::html_form_submit(form = form_filled,
                            submit = "action-Send")

  rvest::html_form_submit(form = form_filled,
                          submit = "action-Send")

  rvest::session_submit(response)

  session_response <- rvest::session_submit(session,
                        form_blank,
                        submit = "action-Send")
session_response %>%
  rvest::html_table()



# Code below doesn't work, but returns a good request (aka 200) after a session timeout
session <- session(form_url)

session <- rvest::session_submit(session, session %>%
                                   xml2::read_html() %>%
                                   # Select the nodelist for the form
                                   rvest::html_node(xpath = form_xpath) %>%
                                   # Convert that nodelist to a form
                                   rvest::html_form(), submit = "action-I Agree")


# Select the form from the html webpage:
form_blank <- session %>%
  xml2::read_html() %>%
  # Select the nodelist for the form:
  rvest::html_node(xpath = form_xpath) %>%
  # Convert that nodelist to a form
  rvest::html_form()


response <- rvest::html_form_submit(form_blank, submit = "action-Send")

response %>%
  xml2::read_html() %>%
  html_text()
html_table()


}

scrape_wonder_deaths <- function(){


}


