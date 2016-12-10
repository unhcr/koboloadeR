#' @name kobo_datasets2
#' @rdname kobo_datasets2
#' @title Lists the Datasets Available including count of submission.
#'
#'
#' @param user Optional. A single string indicating the username and password
#' (in the form of \code{"username:password"}), or a character vector or list,
#' length 2, with the first value being the "username", and the second being
#' the "password".
#' @param api The URL at which the API can be accessed. Defaults to "kobo",
#' which loads the KoBo Toolbox API.
#'
#' @return A data.table containing details about the datasets available,
#' including items like the "title", "id", and "submission".
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_datasets2()
#'
#' @export kobo_datasets2
#'
kobo_datasets2 <- function(user = NULL, api = "unhcr") {

  #data <- kobo_datasets(user = NULL, api = "unhcr")
  #count <- kobo_submission_count(formid, user, api)
}
NULL

#' @name kobo_dico
#' @rdname kobo_dico
#' @title  Produce a data dictionnary based on the form of the project
#'
#'
#'
#' @param formid The ID of the form to be accessed (as a character string).
#' @param user Optional. A single string indicating the username and password
#' (in the form of \code{"username:password"}), or a character vector or list,
#' length 2, with the first value being the "username", and the second being
#' the "password".
#' @param api The URL at which the API can be accessed.
#' Defaults to "unhcr", which loads the UNHCR KoBo Toolbox API.
#' @param check Logical. Should the function first check to see whether the
#' data is available offline.
#'
#'
#' @return A "data.table" with the full data dictionnary. To be used in the rest of the analysis.
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_dico()
#'
#' @export kobo_dico
#' @examples
#' \dontrun{
#' kobo_dico("15051")
#' kobo_dico("31511", user = userpwd, api = "unhcr", check = TRUE)
#' }
#'
#' @export kobo_dico
kobo_dico <- function(formid, user = NULL, api = "unhcr", check = TRUE) {
  locfile <- sprintf(fmt = "data/form_%s", formid)

  if (isTRUE(check)) {
    if (exists(locfile)) {
      message("The form was already downloaded.")
      redownload = FALSE
    } else {
      URL1 <- sprintf(fmt = "%sforms/%s/form.xls", koboloadeR:::host(api), formid)
      form_tmp <- file(paste0("data/form_",formid,".xls"), open = "wb")
      bin <- getBinaryURL(URL1, user , httpauth = 1L, ssl.verifypeer=FALSE  )
      writeBin(bin, form_tmp)
      close(form_tmp)

      ## test with xlsx
      URL2 <- sprintf(fmt = "%sforms/%s/form.xlsx", koboloadeR:::host(api), formid)
      form_tmp2 <- file(paste0("data/form_",formid,".xlsx"), open = "wb")
      bin <- getBinaryURL(URL1, user , httpauth = 1L, ssl.verifypeer=FALSE  )
      writeBin(bin, form_tmp2)
      close(form_tmp2)

    }
  }

  ### Now opening & parsing the form


}
NULL




#' @name kobo_projectinit
#' @rdname kobo_projectinit
#' @title  Create analysis project structure & related analysis scripts
#'
#' @return A structure of directory and scripts in order to set up quickly a project.
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_projectinit()
#'
#' @export kobo_projectinit
kobo_projectinit <- function() {
  mainDir <- getwd()

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

  cat("The analysis plan is a csv file that contains the mapping between\n
                           your questions and your indicators and disaggration variables.\n
      Such file will allow to compute directly additional analysis from your dataset\n")

  analysisplan <- readline("Provide the name of the file where you have your analysis plan: (Press enter if no plan")

  cat("Now creating all folders and analysis script\n")

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
  destfile="./perso/username.R"
  if (!file.exists(destfile)) {
    fileConn<-file(destfile)
    writeLines(c("# This is the connection to your data collection project on Kobotoolbox",
                # "usernamepassword <- paste(user,passw,sep=":")",
                 "host <- apiurl",
                 "formid <- formid"), fileConn)
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

  ## End of it...

  runproject <- readline("Doyou want to run the project now: Yes(Y)/No(N)? ")
  if (runproject=='Y') {
    source("./code/runproject.R")
  } else {
    cat("You can now go to /code/runproject.R in order to run the project.\n")
  }

}
NULL
