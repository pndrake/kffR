# initialize Google Api account information:

# Common gmail account
# User : kff.shf.user@gmail.com
# Pwd  : Kaiser2022

setAPIUser <- function(){

  googlesheets4::gs4_auth(
      email = gargle::gargle_oauth_email(),
       path = NULL,
       scopes = "https://www.googleapis.com/auth/spreadsheets",
       cache = gargle::gargle_oauth_cache(),
       use_oob = gargle::gargle_oob_default(),
       token = NULL
   )

}

# User login information
# googledrive::drive_user()
# googledrive::drive_auth_configure()


# API Indicator Log
# A google sheet that stores information about all api activities

 # id_apiIndicatorLog = "1i0EjDfV0E2lkj56gCJ6bVPPkeSfH_Keb15zSf8XqlUk" %>%
 #   googledrive::as_id()


# Sheet names
#apiLog_sheetName = "Copies"

