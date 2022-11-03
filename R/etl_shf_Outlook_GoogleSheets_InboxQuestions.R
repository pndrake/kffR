

etl_outlookToGS  <- function(){

# Parameters ----
# How many emails do we want to read in?
numberEmails = 200
# What is the sender name for contact us questions?
senderName_toLookUp <- "KFF Contact Us"

# Setup this instance of R/outlook ----
outLook <- Microsoft365R::get_business_outlook()

# Retrieve the latest emails from my inbox ----
list_latestEmails <- outLook$list_emails(n = numberEmails)


# Subset emails to inbox questions ----
list_inboxQuestions <- list()
list_inboxUserNames <- list()

# Question:
# Is there an R equivelent to JS nodeList.filter(d => d.sender.emailAddress == "KFF Contact Us")
# For now I will just use a for-loop

for(i in 1:length(list_latestEmails)){

  this_email <- list_latestEmails[[i]]

  # if this email is from "KFF Contact Us" add it to our output list
  if(this_email$properties$sender$emailAddress$name == senderName_toLookUp){

    list_inboxQuestions <-  append(list_inboxQuestions, this_email)
  }

  list_inboxUserNames <- append(list_inboxUserNames, this_email$properties$sender$emailAddress$name)

}

list_inboxQuestions

}
