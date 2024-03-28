# Set up an instance of firefox with a remote driver

selenium_setup <- function(){
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  rD <- RSelenium::rsDriver(browser="firefox", port=4545L, verbose=F)
  remDr <- rD[["client"]]
  return(remDr)
}

