library(tidyverse)
library(rvest)

text_success <- "success"
text_alreadyExists <- "file already exists"
text_failed <- "error"

# Create a list to contain our state level functions
medicaidStateFetch <- list()

# States with no monthly enrollment data?
c("Alabama")

# States with Tableau dashboards
c("Ohio")

medicaidStateFetch$Alaska <-
  function(stateURL = "https://health.alaska.gov/healthyalaska/pages/dashboard.aspx") {
    tryCatch({
      StorageFilePath = "./State Data Downloads/Alaska"

      linkPatternToMatch <- "Medicaid-Dashboard-text-only"


      # Set up the html session:
      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% html_attr("href")
      vec_text <- nodeset_links %>% html_text()

      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .)



      # Pull out latest link:
      latest_link <- vec_hres_enrollment[1]

      # Grab the file name for saving
      latest_fileName <- vec_text_enrollment[1]

      # Follow that link to download the file (if it isn't already there)
      #isAlreadyDownloaded <-
      #  latest_fileName %in% list.files(StorageFilePath)

      # if (!isAlreadyDownloaded) {
      rvest::session_jump_to(session, latest_link)$url %>%
        download.file(
          destfile = paste0(
            StorageFilePath,
            "/",
            Sys.Date(),
            "_",
            latest_fileName,
            ".pdf"
          ),
          mode = "wb"
        )

      # Return the status of this function:
      return(text_success)
      #} else {
      #  return(text_alreadyExists)
      #}


    },
    error = function(cond) {
      return(text_failed)
    })

  }

# Work in progress
# Note: the links are present on the page, but can't find them through rvest session
medicaidStateFetch$Arkansas <-
  function(stateURL = "https://www.azahcccs.gov/Resources/Reports/population.html",
           fetchAll = FALSE) {
    linkPatternToMatch <- "Monthly\\-Enrollment\\-and"
    tryCatch({
      StorageFilePath = "./State Data Downloads/Arkansas"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Attempting to find the right objects in browser:
      # Note only the oldest links appear in this list . . .
      # rvest::html_elements(css = "#table_1", session) %>% rvest::html_text()

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      # Note: some months are called MER for some reason (e.g. May's file is called "MER-By-County-1")
      grepl("Monthly",
            vec_hrefs)  %>%
        which()
      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*.pdf") %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        # Note: first link here is the most recent
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*.pdf") %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })

  }
medicaidStateFetch$Arizona <-
  function(stateURL =  "https://www.azahcccs.gov/Resources/Reports/population.html") {

    fetchAll = FALSE
    linkPatternToMatch <- "Highlights"
    filePatternToMatch <- "/[a-zA-Z0-9_ -.]*.pdf"

    tryCatch({

      StorageFilePath = "./State Data Downloads/Arizona"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)

      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }


medicaidStateFetch$Colorado <-
  function(stateURL = "https://hcpf.colorado.gov/premiums-expenditures-and-caseload-reports",
           fetchAll = FALSE) {

    # Note: This link structure has changed as of July, probably will change again
    linkPatternToMatch <- "%20Joint%20Budget%20Committee%20Monthly%20Premiums%20Report.xlsx"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.xlsx"

    tryCatch({

      StorageFilePath = "./State Data Downloads/Colorado"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()


      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }

medicaidStateFetch$Connecticut <-
  function(stateURL = "https://data.ct.gov/widgets/sx77-vjbh") {
    tryCatch({
      StorageFilePath = "./State Data Downloads/Connecticut"

      linkPatternToMatch <- "Medicaid-Dashboard-text-only"


      # Set up the html session:
      session <- rvest::session(stateURL)

      # rvest::html_elements(xpath = "/html/body/div[2]/div[3]/div[1]/div/div[1]/div[2]/div/div[4]/div[1]/div/table", session)
      # rvest::html_elements(xpath = css = ".mainMenuButton", session)


      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% html_attr("href")
      vec_text <- nodeset_links %>% html_text()

      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .)



      # Pull out latest link:
      latest_link <- vec_hres_enrollment[1]

      # Grab the file name for saving
      latest_fileName <- vec_text_enrollment[1]

      # Follow that link to download the file (if it isn't already there)
      #isAlreadyDownloaded <-
      #  latest_fileName %in% list.files(StorageFilePath)

      # if (!isAlreadyDownloaded) {
      rvest::session_jump_to(session, latest_link)$url %>%
        download.file(
          destfile = paste0(
            StorageFilePath,
            "/",
            Sys.Date(),
            "_",
            latest_fileName,
            ".pdf"
          ),
          mode = "wb"
        )

      # Return the status of this function:
      return(text_success)
      #} else {
      #  return(text_alreadyExists)
      #}


    },
    error = function(cond) {
      return(text_failed)
    })

  }
medicaidStateFetch$Delaware <-
  function(stateURL = "https://data.delaware.gov/Health/Medicaid-Enrollment/xhfg-cwx7") {

    tryCatch({
    df_enrollment <- jsonlite::read_json(
     "https://data.delaware.gov/resource/xhfg-cwx7.json",
     simplifyVector = TRUE) %>%
      mutate(Date = as.Date(date),
             State = "Delaware",
             Abbr = "DE") %>%
      arrange(desc(Date)) %>%
      as_tibble()

      write_csv(df_enrollment, file = paste0("./State Data Downloads./Delaware/delawareEnrollment_",Sys.Date(), ".csv"))

      return(text_success)

    },
    error = function(cond) {
      return(text_failed)
    })
  }
# Work in progress
# This site links to a separate page that then has a link to the report
medicaidStateFetch$DistrictOfColumbia <-
  function(stateURL = "https://dhcf.dc.gov/page/monthly-medicaid-and-alliance-enrollment-reports",
           fetchAll = FALSE) {

    return(text_failed)

    linkPatternToMatch <- "Enrollment Report"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*$"


    tryCatch({

      StorageFilePath = "./State Data Downloads/District of Columbia/"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()


      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_text)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      vec_hres_enrollment %>%
        rvest::session_follow_link()
      session %>%
        rvest::session_follow_link(x = ., css)

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }
medicaidStateFetch$Hawaii <-
  function(stateURL = "https://medquest.hawaii.gov/en/resources/reports.html",
           fetchAll = FALSE) {


    linkPatternToMatch <- "Medicaid Managed Care Enrollment"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.pdf"

    tryCatch({

      StorageFilePath = "./State Data Downloads/Hawaii"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()


      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_text)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

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

medicaidStateFetch$Illinois <-
  function(stateURL = "https://www2.illinois.gov/hfs/MedicalProviders/cc/Pages/TotalCCEnrollmentforAllPrograms.aspx",
           fetchAll = FALSE) {
    linkPatternToMatch <- "MCOAGGREGATEDEnrollmentReportForWebsite"
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

      patternToMatch <- "Enro[l]{1,}ment as of "

      index_stateLevelMonthlyReport <-
        grepl(patternToMatch, vec_text)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*.pdf") %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*.pdf") %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }
medicaidStateFetch$Indiana <-
  function(stateURL = "https://www.in.gov/fssa/ompp/forms-documents-and-tools2/medicaid-monthly-enrollment-reports/",
           fetchAll = FALSE) {
    linkPatternToMatch <- "Monthly\\-Enrollment\\-Report"
    tryCatch({
      StorageFilePath = "./State Data Downloads/Indiana"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()


      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*.xlsx") %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*.xlsx") %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }
medicaidStateFetch$Iowa <-
  function(stateURL = "https://dhs.iowa.gov/ime/about/performance-data/MC-monthly-reports",
           fetchAll = FALSE) {
    linkPatternToMatch <- "MCO[_%]counts[_%]"
    tryCatch({

      StorageFilePath = "./State Data Downloads/Iowa"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*.pdf") %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*.pdf") %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }

medicaidStateFetch$Kansas <-
  function(stateURL = "https://www.kancare.ks.gov/policies-and-reports/medical-assistance-report",
           fetchAll = FALSE) {

    linkPatternToMatch <- "MAR FY"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.pdf"

    tryCatch({
      StorageFilePath = "./State Data Downloads/Kansas"

      # Direction of new links added to the page:
      # Newest data are at the top
      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()


      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_text)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }


medicaidStateFetch$Kentucky <-
  function(stateURL = "https://chfs.ky.gov/agencies/dms/dafm/Pages/statistics.aspx?View=2022%20Reports%20by%20County&Title=Table%20Viewer%20Webpart",
           fetchAll = FALSE) {

    linkPatternToMatch <- "KYDWMMCC"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.pdf"

    tryCatch({

      StorageFilePath = "./State Data Downloads/Kentucky"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()


      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }

# Do manually
medicaidStateFetch$Louisiana <-
  function(stateURL = "https://ldh.la.gov/HealthyLaDashboard/",
           fetchAll = FALSE) {

    return(text_failed)

    linkPatternToMatch <- "KYDWMMCC"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.pdf"

    tryCatch({

      StorageFilePath = "./State Data Downloads/Louisiana"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()


      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }


medicaidStateFetch$Maine<-
  function(stateURL = "https://www.maine.gov/dhhs/ofi/about-us/data-reports",
           fetchAll = FALSE) {


    linkPatternToMatch <- "overflow"
    filePatternToMatch <- "/[\\(\\)%a-zA-Z0-9_ -]*.pdf"

    tryCatch({

      StorageFilePath = "./State Data Downloads/Maine"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      # Match on text or links?
      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs, ignore.case = TRUE)

      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {

        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }

# Note: file names are unique for MA, copy another function for creating new states
medicaidStateFetch$Massachusetts <-
  function(stateURL = "https://www.mass.gov/lists/masshealth-enrollment-and-caseload-metrics#2022-masshealth-monthly-caseload-reports-",
           fetchAll = FALSE) {
    linkPatternToMatch <-
      "masshealth[%-]caseload[%-]snapshot[%-]and[%-]enrollment[%-]summary"
    tryCatch({
      StorageFilePath = "./State Data Downloads/Massachusetts"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*/download") %>%
            gsub("/download", "", .) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(
                StorageFilePath,
                "/",
                Sys.Date(),
                latest_fileName,
                ".pdf"
              ),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, "/[a-zA-Z0-9_ -]*/download") %>%
          gsub("/download", "", .) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(
              StorageFilePath,
              "/",
              Sys.Date(),
              latest_fileName,
              ".pdf"
            ),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }
medicaidStateFetch$Michigan<-
  function(stateURL = "https://www.michigan.gov/mdhhs/assistance-programs/medicaid/portalhome/reports/medicaid-and-healthy-michigan-plan-health-plan-enrollment-report",
           fetchAll = FALSE) {



    linkPatternToMatch <- "Enrollment data"
    filePatternToMatch <- "/[\\(\\)%a-zA-Z0-9_ -]*.pdf"

    tryCatch({

      StorageFilePath = "./State Data Downloads/Michigan"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      # Match on text or links?
      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_text)

      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {

        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }

# do this one manually
#https://medicaid.ms.gov/resources/


medicaidStateFetch$Minnesota<-
  function(stateURL = "https://www.dhs.state.mn.us/main/idcplg?IdcService=GET_DYNAMIC_CONVERSION&RevisionSelectionMethod=LatestReleased&dDocName=DHS16_141529",
           fetchAll = FALSE) {

    return(text_failed)

    linkPatternToMatch <- "enrollment figures,"
    filePatternToMatch <- "/[\\(\\)%a-zA-Z0-9_ -]*"

    tryCatch({

      StorageFilePath = "./State Data Downloads/Minnesota"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      # Match on text or links?
      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_text)

      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {

        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }


medicaidStateFetch$Mississippi<-
  function(stateURL = "https://medicaid.ms.gov/resources/",
           fetchAll = FALSE) {



    linkPatternToMatch <- "Medicaid 20[0-9]{2} calendar year"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.pdf"



    tryCatch({

      StorageFilePath = "./State Data Downloads/Mississippi"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      # Match on text or links?
      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_text)

      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {

        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }


medicaidStateFetch$Nebraska <-
    function(stateURL = "https://dhhs.ne.gov/Documents/HeritageHealthDashData.pdf",
             fetchAll = FALSE) {


        StorageFilePath = "./State Data Downloads/Nebraska"
        latest_fileName <- "HeritageHealthDashData.pdf"

        download.file(
          url = stateURL,
          destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
          mode = "wb"
        )

          # Return the status of this function:
          return(text_success)

    }
  medicaidStateFetch$NewHampshire <-
    function(stateURL = "https://www.dhhs.nh.gov/programs-services/medicaid"             ) {

      fetchAll = FALSE

      linkPatternToMatch <- "Medicaid Enrollment"
      filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.pdf"



      tryCatch({

        StorageFilePath = "./State Data Downloads/New Hampshire"

        # Direction of new links added to the page:
        # Newest data are at the top

        session <- rvest::session(stateURL)

        # Obtain a list of all nodes that have links (aka href) present on this page:
        nodeset_links <- rvest::html_elements(css = "a", session)

        # Extract the links form those nodes:
        vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
        vec_text <- nodeset_links %>% rvest::html_text()

        # Match on text or links?
        index_stateLevelMonthlyReport <-
          grepl(linkPatternToMatch, vec_text)

        # Subset to a list of relevant links:
        vec_hres_enrollment <-
          vec_hrefs[index_stateLevelMonthlyReport]

        # Subset to a list of relevant texts:
        vec_text_enrollment <-
          vec_text[index_stateLevelMonthlyReport] %>%
          gsub(linkPatternToMatch, "", .) %>%
          gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

        if (fetchAll) {
          for (i in 1:length(vec_hres_enrollment)) {
            # Pull out latest link:
            latest_link <- vec_hres_enrollment[i]

            # Grab the file name for saving
            latest_fileName <-
              stringr::str_extract(latest_link, filePatternToMatch) %>%
              gsub("/", "", .)

            # Follow that link to download the file (if it isn't already there)
            #isAlreadyDownloaded <-
            #  latest_fileName %in% list.files(StorageFilePath)

            # if (!isAlreadyDownloaded) {
            rvest::session_jump_to(session, latest_link)$url %>%
              download.file(
                destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
                mode = "wb"
              )

          }

        } else {

          # Pull out latest link:
          latest_link <- vec_hres_enrollment[1]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

          # Return the status of this function:
          return(text_success)

        }
      },
      error = function(cond) {
        return(text_failed)
      })
    }
  medicaidStateFetch$NewJersey <-
    function(stateURL = "https://www.state.nj.us/humanservices/dmahs/news/reports/",
             fetchAll = FALSE) {



      linkPatternToMatch <- "enrollment_"
      filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.pdf"



      tryCatch({

        StorageFilePath = "./State Data Downloads/New Jersey"

        # Direction of new links added to the page:
        # Newest data are at the top

        session <- rvest::session(stateURL)

        # Obtain a list of all nodes that have links (aka href) present on this page:
        nodeset_links <- rvest::html_elements(css = "a", session)

        # Extract the links form those nodes:
        vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
        vec_text <- nodeset_links %>% rvest::html_text()

        # Match on text or links?
        index_stateLevelMonthlyReport <-
          grepl(linkPatternToMatch, vec_hrefs)

        # Subset to a list of relevant links:
        vec_hres_enrollment <-
          vec_hrefs[index_stateLevelMonthlyReport]

        # Subset to a list of relevant texts:
        vec_text_enrollment <-
          vec_text[index_stateLevelMonthlyReport] %>%
          gsub(linkPatternToMatch, "", .) %>%
          gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

        if (fetchAll) {
          for (i in 1:length(vec_hres_enrollment)) {
            # Pull out latest link:
            latest_link <- vec_hres_enrollment[i]

            # Grab the file name for saving
            latest_fileName <-
              stringr::str_extract(latest_link, filePatternToMatch) %>%
              gsub("/", "", .)

            # Follow that link to download the file (if it isn't already there)
            #isAlreadyDownloaded <-
            #  latest_fileName %in% list.files(StorageFilePath)

            # if (!isAlreadyDownloaded) {
            rvest::session_jump_to(session, latest_link)$url %>%
              download.file(
                destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
                mode = "wb"
              )

          }

        } else {

          # Pull out latest link:
          latest_link <- vec_hres_enrollment[1]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

          # Return the status of this function:
          return(text_success)

        }
      },
      error = function(cond) {
        return(text_failed)
      })
    }

# Work in progress: need to figure out how to find latest month file
medicaidStateFetch$NewMexico <- function(stateURL = "https://www.hsd.state.nm.us/medicaid-eligibility-reports/",
                                          fetchAll = FALSE) {
  tryCatch({
    StorageFilePath = "./State Data Downloads/New Mexico"

    #linkPatternToMatch <- "Enrollment"

    # Direction of new links added to the page:
    # Newest data are at the top

    # Set up the html session:

    session <- rvest::session(stateURL)

    # Obtain a list of all nodes that have links (aka href) present on this page:
    nodeset_links <- rvest::html_elements(css = "a", session)

    # Extract the links form those nodes:
    vec_hrefs <- nodeset_links %>% html_attr("href")
    vec_text <- nodeset_links %>% html_text()

    # Note: some months are called MER for some reason (e.g. May's file is called "MER-By-County-1")

    linkPatternToMatch <- "[-_ –]{1,}By County"

    index_stateLevelMonthlyReport <-
      sapply(c(month.name, "MER"), function(y)
        grepl(paste0(y, linkPatternToMatch), vec_text)) %>%
      as.data.frame() %>%
      mutate(keep = if_any(),
             index = row_number()) %>%
      filter(keep) %>%
      pull(index)


    # Subset to a list of relevant links:
    vec_hres_enrollment <- vec_hrefs[index_stateLevelMonthlyReport]

    # Subset to a list of relevant texts:
    vec_text_enrollment <-
      vec_text[index_stateLevelMonthlyReport] %>%
      gsub(linkPatternToMatch, "", .)


    if (fetchAll) {
      for (i in 1:length(vec_hres_enrollment)) {
        latest_link <- vec_hres_enrollment[i]
        latest_fileName <- vec_text_enrollment[i]
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", latest_fileName, "_", i, ".pdf"),
            mode = "wb"
          )

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
        download.file(
          destfile = paste0(
            StorageFilePath,
            "/",
            Sys.Date(),
            "_",
            latest_fileName,
            ".pdf"
          ),
          mode = "wb"
        )

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
medicaidStateFetch$NewYork <-
  function(stateURL = "https://www.health.ny.gov/health_care/medicaid/enrollment/historical/all_months.htm") {
    tryCatch({
      StorageFilePath = "./State Data Downloads/New York"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)


      # Extract the table from the webpage:
      list_tables <- session %>% rvest::html_table()



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

# need to use selenium
medicaidStateFetch$Ohio <-
  function(stateURL = "https://medicaid.ohio.gov/stakeholders-and-partners/reports-and-research/caseload-reports/caseload-reports",
           fetchAll = FALSE
           ) {

   return(text_failed)

    linkPatternToMatch <- "Total Enrollment$"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.pdf"

    tryCatch({

      StorageFilePath = "./State Data Downloads/Ohio"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      # Match on text or links?
      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_text)

      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {

        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }



medicaidStateFetch$Oklahoma <-
  function(stateURL = "https://oklahoma.gov/ohca/research/data-and-reports.html"
          ) {

    fetchAll = FALSE

    linkPatternToMatch <- "Total Enrollment$"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.pdf"

    tryCatch({

      StorageFilePath = "./State Data Downloads/Oklahoma"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      # Match on text or links?
      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_text)

      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {

        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }

medicaidStateFetch$Pennsylvania <-
  function(stateURL = "https://www.dhs.pa.gov/about/Pages/Data-Dashboards.aspx") {
    linkPatternToMatch <- "Department of Human Service Monthly Data"
    tryCatch({
      StorageFilePath = "./State Data Downloads/Pennsylvania"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_text)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]


      # Pull out latest link:
      latest_link <- vec_hres_enrollment[1]

      # Grab the file name for saving
      latest_fileName <-
        stringr::str_extract(latest_link, "/[a-zA-Z0-9_ %-]*.pdf") %>%
        gsub("/", "", .)

      # Follow that link to download the file (if it isn't already there)
      #isAlreadyDownloaded <-
      #  latest_fileName %in% list.files(StorageFilePath)

      # if (!isAlreadyDownloaded) {
      rvest::session_jump_to(session, latest_link)$url %>%
        download.file(
          destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
          mode = "wb"
        )

      # Return the status of this function:
      return(text_success)


    },
    error = function(cond) {
      return(text_failed)
    })
  }
medicaidStateFetch$SouthCarolina <-
  function(stateURL = "https://msp.scdhhs.gov/managedcare/site-page/reports"
           ) {
    fetchAll = FALSE

    linkPatternToMatch <- "Current%20Medicaid%20Enrollment"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.xlsx"
   tryCatch({
      StorageFilePath = "./State Data Downloads/South Carolina"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()


      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }

# this one's file pattern is different
medicaidStateFetch$SouthDakota <-
  function(stateURL = "https://dss.sd.gov/keyresources/statistics.aspx",
           fetchAll = TRUE
  ) {


    linkPatternToMatch <- "med_eligibility_data"
    filePatternToMatch <- "[0-9]{4}/[%a-zA-Z0-9_ -]*.pdf"

    tryCatch({

      StorageFilePath = "./State Data Downloads/South Dakota"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      # Match on text or links?
      index_stateLevelMonthlyReport <- grepl(linkPatternToMatch, vec_hrefs)

      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "_", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {

        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }


medicaidStateFetch$Tennessee <-
  function(stateURL = "https://www.tn.gov/tenncare/information-statistics/enrollment-data.html",
           fetchAll = FALSE) {

    linkPatternToMatch <- "fte_[0-9]*.xlsx"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.xlsx"

    tryCatch({
      StorageFilePath = "./State Data Downloads/Tennessee"

      # Direction of new links added to the page:
      # Newest data are at the top
      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()


      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
    },
    error = function(cond) {
      return(text_failed)
    })
  }



medicaidStateFetch$Virginia <-
  function(stateURL = "https://www.dmas.virginia.gov/data/enrollment-reports/"
           ) {

    fetchAll = FALSE
    linkPatternToMatch <- "DMAS Enrollment Report [a-zA-Z 0-9]*\\[xlsx\\]"
    filePatternToMatch <- "/[%a-zA-Z0-9_ -]*.xlsx"


    tryCatch({

      StorageFilePath = "./State Data Downloads/Virginia"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()


      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_text)


      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        stringr::str_trim()

      if (fetchAll) {
        for (i in 1:length(vec_hres_enrollment)) {
          # Pull out latest link:
          latest_link <- vec_hres_enrollment[i]

          # Grab the file name for saving
          latest_fileName <-
            stringr::str_extract(latest_link, filePatternToMatch) %>%
            gsub("/", "", .)

          # Follow that link to download the file (if it isn't already there)
          #isAlreadyDownloaded <-
          #  latest_fileName %in% list.files(StorageFilePath)

          # if (!isAlreadyDownloaded) {
          rvest::session_jump_to(session, latest_link)$url %>%
            download.file(
              destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
              mode = "wb"
            )

        }

      } else {
        # Pull out latest link:
        latest_link <- vec_hres_enrollment[1]

        # Grab the file name for saving
        latest_fileName <-
          stringr::str_extract(latest_link, filePatternToMatch) %>%
          gsub("/", "", .)

        # Follow that link to download the file (if it isn't already there)
        #isAlreadyDownloaded <-
        #  latest_fileName %in% list.files(StorageFilePath)

        # if (!isAlreadyDownloaded) {
        rvest::session_jump_to(session, latest_link)$url %>%
          download.file(
            destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
            mode = "wb"
          )

        # Return the status of this function:
        return(text_success)

      }
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
medicaidStateFetch$Wisconsin <-
  function(stateURL = "https://www.forwardhealth.wi.gov/wiportal/Content/Member/caseloads/enrollment/enrollment.htm.spage") {
    linkPatternToMatch <- "MonthlyEnrollment.pdf"

    tryCatch({
      StorageFilePath = "./State Data Downloads/Wisconsin"

      # Direction of new links added to the page:
      # Newest data are at the top

      session <- rvest::session(stateURL)

      # Obtain a list of all nodes that have links (aka href) present on this page:
      nodeset_links <- rvest::html_elements(css = "a", session)

      # Extract the links form those nodes:
      vec_hrefs <- nodeset_links %>% rvest::html_attr("href")
      vec_text <- nodeset_links %>% rvest::html_text()

      index_stateLevelMonthlyReport <-
        grepl(linkPatternToMatch, vec_hrefs)

      # Subset to a list of relevant links:
      vec_hres_enrollment <-
        vec_hrefs[index_stateLevelMonthlyReport]

      # Subset to a list of relevant texts:
      vec_text_enrollment <-
        vec_text[index_stateLevelMonthlyReport] %>%
        gsub(linkPatternToMatch, "", .) %>%
        gsub("\\(pdf\\)", "", .) %>% stringr::str_trim()


      # Pull out latest link:
      # Note: first link here is the most recent
      latest_link <- vec_hres_enrollment[1]

      # Grab the file name for saving
      latest_fileName <-
        stringr::str_extract(latest_link, "[a-zA-Z0-9_ -]*.pdf")

      # Follow that link to download the file (if it isn't already there)
      #isAlreadyDownloaded <-
      #  latest_fileName %in% list.files(StorageFilePath)

      # if (!isAlreadyDownloaded) {
      rvest::session_jump_to(session, latest_link)$url %>%
        download.file(
          destfile = paste0(StorageFilePath, "/", Sys.Date(), latest_fileName),
          mode = "wb"
        )

      # Return the status of this function:
      return(text_success)


    },
    error = function(cond) {
      return(text_failed)
    })
  }
