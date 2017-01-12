#' @name kobo_projectinit
#' @rdname kobo_projectinit
#' @title  Analysis project initiation
#'
#' @description    Create analysis project structure 
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


  cat("let's create various standard folders and copy some analysis script\n")

  subDir <- "perso"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("Perso exists in mainDir and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("perso directory exists in your project directory but is a file.\n")
    # you will probably want to handle this separately
  } else {
    cat("perso directory does not exist in your project directory - creating now!\n ")
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
    cat("Code exists in mainDir and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("Code directory exists in your project directory but is a file.\n")
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
 


  subDir <- "data"

  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("Data exists in mainDir and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("Data directory exists in your project directory but is a file.\n")
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
    cat("Ouput exists in your project directory but is a file.\n")
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




}
NULL
