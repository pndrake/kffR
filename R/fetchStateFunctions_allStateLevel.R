library(rvest)


text_success <- "success"
text_alreadyExists <- "file already exists"
text_failed <- "error"

# Create a list to contain our state level functions
medicaidStateFetch <- list()

medicaidStateFetch$Illinois <- function(stateURL = "https://www2.illinois.gov/hfs/MedicalProviders/cc/Pages/TotalCCEnrollmentforAllPrograms.aspx",
                                        fetchAll = FALSE) {
  tryCatch({
    StorageFilePath = "./State Data Downloads/Illinois"

    # Direction of new links added to the page:
    # Newest data are at the top

    session <- rvest::session(stateURL)

    # Obtain a list of all nodes that have links (aka href) present on this page:
    nodeset_links <- rvest::html_elements(css = "a", session)

    # Extract the links form those nodes:
    vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
    vec_text <- nodeset_links %>% rvest::html_text()

    # Note: some months are called MER for some reason (e.g. May's file is called "MER-By-County-1")

    patternToMatch <-"Enro[l]{1,}ment as of "

    index_stateLevelMonthlyReport <- grepl(patternToMatch, vec_text)


    # Subset to a list of relevant links:
    vec_hres_enrollment <- vec_hrefs[index_stateLevelMonthlyReport]

    # Subset to a list of relevant texts:
    vec_text_enrollment <- vec_text[index_stateLevelMonthlyReport] %>%
      gsub(linkPatternToMatch, "", .) %>%
      gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

    if(fetchAll){

      for(i in 1:length(vec_hres_enrollment)){
      # Pull out latest link:
      latest_link <- vec_hres_enrollment[i]

      # Grab the file name for saving
      latest_fileName <- stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*.pdf") %>%
        gsub("/", "", .)

      # Follow that link to download the file (if it isn't already there)
      #isAlreadyDownloaded <-
      #  latest_fileName %in% list.files(StorageFilePath)

      # if (!isAlreadyDownloaded) {
      rvest::session_jump_to(session, latest_link)$url %>%
        download.file(destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName), mode = "wb")

      }

    } else {
      # Pull out latest link:
      latest_link <- vec_hres_enrollment[1]

      # Grab the file name for saving
      latest_fileName <- stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*.pdf") %>%
        gsub("/", "", .)

      # Follow that link to download the file (if it isn't already there)
      #isAlreadyDownloaded <-
      #  latest_fileName %in% list.files(StorageFilePath)

      # if (!isAlreadyDownloaded) {
      rvest::session_jump_to(session, latest_link)$url %>%
        download.file(destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName), mode = "wb")

      # Return the status of this function:
      return(text_success)

    }
  },
  error = function(cond) {
    return(text_failed)
  })
}
medicaidStateFetch$Florida <- function() {
  tryCatch({
    StorageFilePath = "./State Data Downloads/Florida"
    linkPatternToMatch <- "ENR"

    # Direction of new links added to the page:
    # Newest data are at the top

    # Set up the html session:
    stateURL <-
      "https://ahca.myflorida.com/Medicaid/Finance/data_analytics/enrollment_report/index.shtml"
    session <- rvest::session(stateURL)

    # Obtain a list of all nodes that have links (aka href) present on this page:
    nodeset_links <- rvest::html_elements(css = "a", session)

    # Extract the links form those nodes:
    vec_hrefs <- nodeset_links %>% html_attr("href")

    # Subset to a list of relevant links:
    vec_hres_enrollment <-
      vec_hrefs[grep(linkPatternToMatch, vec_hrefs)]

    # Pull out latest link:
    latest_link <- vec_hres_enrollment[1]

    # Grab the file name for saving
    latest_fileName <- tail(strsplit(latest_link, "/")[[1]], 1)

    # Follow that link to download the file (if it isn't already there)
    isAlreadyDownloaded <-
      latest_fileName %in% list.files(StorageFilePath)

    if (!isAlreadyDownloaded) {
      rvest::session_jump_to(session, latest_link)$url %>%
        download.file(destfile = paste0(StorageFilePath, "/", latest_fileName),
                      mode = "wb")

      # Return the status of this function:
      return(text_success)
    } else {
      return(text_alreadyExists)
    }
  },
  error = function(cond) {
    return(text_failed)
  })
}
medicaidStateFetch$NexMexico <- function(fetchAll = FALSE) {
  tryCatch({
    StorageFilePath = "./State Data Downloads/New Mexico"

    #linkPatternToMatch <- "Enrollment"

    # Direction of new links added to the page:
    # Newest data are at the top

    # Set up the html session:
    stateURL <- "https://www.hsd.state.nm.us/medicaid-eligibility-reports/"
    session <- rvest::session(stateURL)

    # Obtain a list of all nodes that have links (aka href) present on this page:
    nodeset_links <- rvest::html_elements(css = "a", session)

    # Extract the links form those nodes:
    vec_hrefs <- nodeset_links %>% html_attr("href")
    vec_text <- nodeset_links %>% html_text()

    # Note: some months are called MER for some reason (e.g. May's file is called "MER-By-County-1")

    linkPatternToMatch <-"[-_ –]{1,}By County"

    index_stateLevelMonthlyReport <- sapply(c(month.name, "MER"), function(y) grepl(paste0(y, linkPatternToMatch), vec_text)) %>%
      as.data.frame() %>%
      mutate(keep = if_any(),
             index = row_number()) %>%
      filter(keep) %>%
      pull(index)


    # Subset to a list of relevant links:
    vec_hres_enrollment <- vec_hrefs[index_stateLevelMonthlyReport]

    # Subset to a list of relevant texts:
    vec_text_enrollment <- vec_text[index_stateLevelMonthlyReport] %>%
      gsub(linkPatternToMatch, "", .)


    if(fetchAll){

      for(i in 1:length(vec_hres_enrollment)){

        latest_link <- vec_hres_enrollment[i]
        latest_fileName <- vec_text_enrollment[i]
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(destfile = paste0(StorageFilePath, "/", latest_fileName, "_", i, ".pdf"), mode = "wb")

      }
    } else {
    # Pull out latest link:
    latest_link <- vec_hres_enrollment[1]

    # Grab the file name for saving
    latest_fileName <- vec_text_enrollment[1]

    # Follow that link to download the file (if it isn't already there)
    #isAlreadyDownloaded <-
    #  latest_fileName %in% list.files(StorageFilePath)

   # if (!isAlreadyDownloaded) {
    rvest::session_jump_to(session, latest_link)$url %>%
      download.file(destfile = paste0(StorageFilePath, "/", Sys.Date(), "_", latest_fileName, ".pdf"), mode = "wb")

      # Return the status of this function:
      return(text_success)
    #} else {
    #  return(text_alreadyExists)
    #}

    }
  },
  error = function(cond) {
    return(text_failed)
  })
}
medicaidStateFetch$NewYork <- function(stateURL = "https://www.health.ny.gov/health_care/medicaid/enrollment/historical/all_months.htm") {
  tryCatch({
    StorageFilePath = "./State Data Downloads/New York"

    # Direction of new links added to the page:
    # Newest data are at the top

    session <- rvest::session(stateURL)


    # Extract the table from the webpage:
    list_tables <-session %>% rvest::html_table()



      # if (!isAlreadyDownloaded) {
    list_tables[[1]] %>%
      write_csv(file = paste0(StorageFilePath, "/", Sys.Date(), "all_months.csv"))

      # Return the status of this function:
      return(text_success)


  },
  error = function(cond) {
    return(text_failed)
  })
}
medicaidStateFetch$WestVirginia <- function() {
  tryCatch({
    StorageFilePath = "./State Data Downloads/West Virginia"

    linkPatternToMatch <- "Enrollment"

    # Direction of new links added to the page:
    # Newest data are at the top

    # Set up the html session:
    stateURL <-
      "https://dhhr.wv.gov/bms/Members/Managed%20Care/MCOreports/Pages/default.aspx"
    session <- rvest::session(stateURL)

    # Obtain a list of all nodes that have links (aka href) present on this page:
    nodeset_links <- rvest::html_elements(css = "a", session)

    # Extract the links form those nodes:
    vec_hrefs <- nodeset_links %>% html_attr("href")

    # Subset to a list of relevant links:
    vec_hres_enrollment <-
      vec_hrefs[grepl(linkPatternToMatch, vec_hrefs) &
                  grepl(".pdf", vec_hrefs)]


    # Pull out latest link:
    latest_link <- vec_hres_enrollment[1]

    # Grab the file name for saving
    latest_fileName <- tail(strsplit(latest_link, "/")[[1]], 1)

    # Follow that link to download the file (if it isn't already there)
    isAlreadyDownloaded <-
      latest_fileName %in% list.files(StorageFilePath)

    if (!isAlreadyDownloaded) {
      rvest::session_jump_to(session, latest_link)$url %>%
        download.file(destfile = paste0(StorageFilePath, "/", latest_fileName),
                      mode = "wb")

      # Return the status of this function:
      return(text_success)
    } else {
      return(text_alreadyExists)
    }
  },
  error = function(cond) {
    return(text_failed)
  })
}
medicaidStateFetch$Washington <- function() {
  tryCatch({
    StorageFilePath = "./State Data Downloads/Washington"

    linkPatternToMatch <- "program/12monthsummary.xlsx"

    # Direction of new links added to the page:
    # Newest data are at the top

    # Set up the html session:
    stateURL <-
      "https://www.hca.wa.gov/about-hca/apple-health-medicaid-and-managed-care-reports#managed-care-enrollment"
    session <- rvest::session(stateURL)

    # Obtain a list of all nodes that have links (aka href) present on this page:
    nodeset_links <- rvest::html_elements(css = "a", session)

    # Extract the links form those nodes:
    vec_hrefs <- nodeset_links %>% html_attr("href")

    # Subset to a list of relevant links:
    vec_hres_enrollment <-
      vec_hrefs[grepl(linkPatternToMatch, vec_hrefs)]


    # Pull out latest link:
    latest_link <- vec_hres_enrollment[1]

    # Grab the file name for saving
    latest_fileName <- tail(strsplit(latest_link, "/")[[1]], 1)

    # Follow that link to download the file (if it isn't already there)
    isAlreadyDownloaded <-
      latest_fileName %in% list.files(StorageFilePath)

    if (!isAlreadyDownloaded) {
      rvest::session_jump_to(session, latest_link)$url %>%
        download.file(destfile = paste0(StorageFilePath, "/", latest_fileName),
                      mode = "wb")

      # Return the status of this function:
      return(text_success)
    } else {
      return(text_alreadyExists)
    }
  },
  error = function(cond) {
    return(text_failed)
  })
}
