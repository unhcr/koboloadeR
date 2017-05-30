#' @name kobo_projectconfig
#' @rdname kobo_projectconfig
#' @title  Project configuration file
#'
#' @description  Write a small configuration file to access your kobo project
#'
#' @return A file with all elements to get your data & form.
#'
#' @author Edouard Legoupil
#'
#' @export kobo_projectconfig
#'
#' @examples
#' kobo_projectconfig()
#'


kobo_projectconfig <- function() {
  mainDir <- getwd()



  ## Now we can create the configuration file for the project
  cat("Now we can create the configuration file for the project.\n")

  apichoose <- readline("Select the server you want to use - Enter 1 for UNHCR server, 2 for OCHA server, 3 for HHI server and 4 for ONA server: ")
  username <- readline("Provide your username for the server you selected: ")
  password <- readline("Provide your password for the server you selected: ")
  user <- paste(username,password,sep=":")
  if (apichoose=='1') {
    api = "https://kobocat.unhcr.org/api/v1/"
  } else if (apichoose=='2') {
    api <- "https://kc.humanitarianresponse.info/api/v1/"
  } else if (apichoose=='3')  {
    api <- "https://kc.kobotoolbox.org/api/v1/"
  } else if (apichoose=='4')  {
    api <-  "https://ona.io/api/v1/"
  } else  {
    cat("Wrong number")
  }
  ## Store password & user name in another file -- not to be shared in github
  #source("perso/username.R")
  library(koboloadeR)

  #kobo_dataset <- kobo_datasets (user = usernamepassword , api = apiurl)

  print(kobo_datasets(user, api))

  formid <- readline("Select the formid your want to pull from the list above: ")

  str(kobo_data_downloader(formid, user, api, check = TRUE))


  destfile="./perso/username.R"
  if (!file.exists(destfile)) {
    fileConn<-file(destfile)
    writeLines(c("# This is the connection to your data collection project on Kobotoolbox",
                 # "usernamepassword <- paste(user,passw,sep=":")",
                 "host <- apiurl",
                 "formid <- formid"), fileConn)
    close(fileConn)
  }



  ## End of it...

  runproject <- readline("Doyou want to run the project now: Yes(Y)/No(N)? ")
  if (runproject=='Y') {
    source("./code/runproject.R")
  } else {
    cat("You can now go to /code/runproject.R in order to run the project.\n")
  }

}
NULL
