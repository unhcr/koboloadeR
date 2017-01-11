#' @name kobo_projectinit
#' @rdname kobo_projectinit
#' @title  Analysis project initiation
#'
#' @description    Create analysis project structure & related analysis scripts
#'
#' @return A structure of directory and scripts in order to set up quickly a project.
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_projectinit()
#'


kobo_projectinit <- function() {
  mainDir <- getwd()


  cat("We will first create various standard folders and copy some analysis script\n")

  subDir <- "perso"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("Perso exists in mainDir and is a directory\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("Perso directory exists in your project directory but is a file")
    # you will probably want to handle this separately
  } else {
    cat("Config directory does not exist in your project directory - creating now! ")
    dir.create(file.path(mainDir, subDir))
  }

  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    # By this point, the directory either existed or has been successfully created
    setwd(file.path(mainDir, subDir))
  } else {
    cat("")
    # Handle this error as appropriate
  }
  destfile="./perso/README.md"
  if (!file.exists(destfile)) {
    fileConn<-file(destfile)
    writeLines(c("### This folder is where your user name, password and config are stored"), fileConn)
    close(fileConn)
  }


  subDir <- "code"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("Code exists in mainDir and is a directory")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("Code directory exists in your project directory but is a file")
    # you will probably want to handle this separately
  } else {
    cat("Code directory does not exist in your project directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }

  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    # By this point, the directory either existed or has been successfully created
    setwd(file.path(mainDir, subDir))
  } else {
    cat("")
    # Handle this error as appropriate
  }
  destfile="./code/README.md"
  if (!file.exists(destfile)) {
    fileConn<-file(destfile)
    writeLines(c("### This folder is where analysis scripts are saved"), fileConn)
    close(fileConn)
  }
  destfile="./code/0-packages.R"
  if (!file.exists(destfile)) {
    file.copy(paste(.libPaths(),"/koboloadeR/script/0-packages.R",sep=""), destfile)
  }
  destfile="./code/1-loaddata.R"
  if (!file.exists(destfile)) {
    file.copy(paste(.libPaths(),"/koboloadeR/script/1-loaddata.R",sep=""), destfile)
  }
  destfile="./code/2-create-graph.R"
  if (!file.exists(destfile)) {
    file.copy(paste(.libPaths(),"/koboloadeR/script/2-create-graph.R",sep=""), destfile)
  }
  destfile="./code/3-create-map.R"
  if (!file.exists(destfile)) {
    file.copy(paste(.libPaths(),"/koboloadeR/script/3-create-map.R",sep=""), destfile)
  }
  destfile="./code/3-compute-indicators.R"
  if (!file.exists(destfile)) {
    file.copy(paste(.libPaths(),"/koboloadeR/script/3-compute-indicators.R",sep=""), destfile)
  }


  subDir <- "data"

  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("Data exists in mainDir and is a directory\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("Data directory exists in your project directory but is a file")
    # you will probably want to handle this separately
  } else {
    cat("Data directory does not exist in your project directory - creating now! \n")
    dir.create(file.path(mainDir, subDir))
  }

  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    # By this point, the directory either existed or has been successfully created
    setwd(file.path(mainDir, subDir))
  } else {
    cat("")
    # Handle this error as appropriate
  }
  destfile="./data/README.md"
  if (!file.exists(destfile)) {
    fileConn<-file(destfile)
    writeLines(c("### This folder is the one where are stored data in CSV format, the form in XLS format and geodata in SHP format",
                 "# BE CAREFUL: DO NOT SHARE PROTECTION SENSITIVE DATA ON GITHUB!",
                 "",
                 "This project is only to keep track of your analysis workflow"), fileConn)
    close(fileConn)
  }


  subDir <- "out"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("Out directory exists in your project directory and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("Ouput exists in your project directory but is a file\n")
    # you will probably want to handle this separately
  } else {
    cat("Out directory does not exist in your project directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }

  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    # By this point, the directory either existed or has been successfully created
    setwd(file.path(mainDir, subDir))
  } else {
    cat("")
    # Handle this error as appropriate
  }
  destfile="./out/README.md"
  if (!file.exists(destfile)) {
    fileConn<-file(destfile)
    writeLines(c("### This folder is where the analysis output will be generated",
                 "# BE CAREFUL: DO NOT SHARE PROTECTION SENSITIVE DATA ON GITHUB!"), fileConn)
    close(fileConn)
  }


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
