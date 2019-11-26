#' @name kobo_projectinit
#' @rdname kobo_projectinit
#' @title  Analysis project initiation
#'
#' @description    Create analysis project structure
#'
#'
#' @return A structure of directory and scripts in order to set up quickly a project.
#'
#' @export kobo_projectinit
#'
#' @author Edouard Legoupil, Elliott MEesseiller
#'
#' @examples
#' \dontrun{
#' kobo_projectinit()
#' }


kobo_projectinit <- function() {
  tryCatch({
    mainDir <- kobo_getMainDirectory()

    cat("Let's create various standard folders and copy some analysis script\n")


    ## doc folder creation ####
    subDir <- "doc"
    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      cat("doc exists in mainDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
      cat("doc directory exists in your project directory but is a file.\n")
      # you will probably want to handle this separately
    } else {
      cat("doc directory does not exist in your project directory - creating now!\n ")
      dir.create(file.path(mainDir, subDir))
    }

    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      # By this point, the directory either existed or has been successfully created
      setwd(file.path(mainDir, subDir))
    } else {
      cat("")
      # Handle this error as appropriate
    }
    destfile = paste0(mainDir,"/doc/README.md")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("### This folder is where your user name, password and config are stored"), fileConn)
      close(fileConn)
    }


    ## Code folder creation ####
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
    destfile = paste0(mainDir,"/code/README.md")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("### This folder is where analysis scripts are saved"), fileConn)
      close(fileConn)
    }

    ## Need to test if we have multiple .libPaths()

    path <- as.data.frame(.libPaths())
    if (nrow(path) == 1) {path_correct <- as.character(path[1,1])
    } else {cat("You have multiple library path! \n")
      if (dir.exists(file.path(path[1,1],"/koboloadeR"))) {
        path_correct <- as.character(path[1,1])
      } else {path_correct <- as.character(path[2,1])}
    }

    # subsubDir <- "script"
    # if (file.exists(paste(mainDir, subDir,"/",subsubDir,"/", sep = "/", collapse = "/"))) {
    #   cat("script exists in subDir and is a directory.\n")
    # } else if (file.exists(paste(mainDir, subDir, subsubDir, sep = "/", collapse = "/"))) {
    #   cat("script directory exists in your project directory.\n")
    #   # you will probably want to handle this separately
    # } else {
    #   cat("script directory does not exist in your project directory - creating now!\n ")
    #   dir.create(file.path(mainDir, subDir,subsubDir))
    # }

    destfile = paste0(mainDir,"/code/run-analysis.R")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/script/run-analysis.R",sep = ""), destfile)
    }else{
      file.remove(destfile)
      file.copy(paste(path_correct,"/koboloadeR/script/run-analysis.R",sep = ""), destfile)
    }


    #
    # destfile = paste0(mainDir,"/code/0-theme.R")
    # if (!file.exists(destfile)) {
    #   file.copy(paste(path_correct,"/koboloadeR/script/0-theme.R", sep = ""), destfile)
    # }
    #
    # destfile = paste0(mainDir,"/code/0-config.R")
    # if (!file.exists(destfile)) {
    #   file.copy(paste(path_correct,"/koboloadeR/script/0-config.R", sep = ""), destfile)
    # }
    #
    # destfile = paste0(mainDir,"/code/1-loaddata.R")
    # if (!file.exists(destfile)) {
    #   file.copy(paste(path_correct,"/koboloadeR/script/1-loaddata.R", sep = ""), destfile)
    # }
    #
    # #  destfile = paste0(mainDir,"/code/2-create-graph.R")
    # #  if (!file.exists(destfile)) {
    # #    file.copy(paste(path_correct,"/koboloadeR/script/2-create-graph.R", sep = ""), destfile)
    # #  }
    #
    # destfile = paste0(mainDir,"/code/2-create-indicators.R")
    # if (!file.exists(destfile)) {
    #   file.copy(paste(path_correct,"/koboloadeR/script/2-create-indicators.R", sep = ""), destfile)
    # }
    #
    # destfile = paste0(mainDir,"/code/3-generate-report.R")
    # if (!file.exists(destfile)) {
    #   file.copy(paste(path_correct,"/koboloadeR/script/3-generate-report.R", sep = ""), destfile)
    # }
    #
    # destfile = paste0(mainDir,"/code/4-generate-prediction.R")
    # if (!file.exists(destfile)) {
    #   file.copy(paste(path_correct,"/koboloadeR/script/4-generate-prediction.R", sep = ""), destfile)
    # }
    #
    # destfile = paste0(mainDir,"/code/report.Rmd")
    # if (!file.exists(destfile)) {
    #   file.copy(paste(path_correct,"/koboloadeR/script/report.Rmd", sep = ""), destfile)
    # }

    destfile = paste0(mainDir,"/code/style-unhcr-portrait.docx")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/script/style-unhcr-portrait.docx", sep = ""), destfile)
    }

    destfile = paste0(mainDir,"/code/report_template.docx")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/script/report_template.docx", sep = ""), destfile)
    }

    # destfile = paste0(mainDir,"/code/XLSform_template.xlsx")
    # if (!file.exists(destfile)) {
    #   file.copy(paste(path_correct,"/koboloadeR/script/XLSform_template.xlsx", sep = ""), destfile)
    # }
    #

    ## shiny_app Subfolder creation ####

    subsubDir <- "shiny_app"
    if (file.exists(paste(mainDir, subDir,"/",subsubDir,"/", sep = "/", collapse = "/"))) {
      cat("shiny_app exists in subDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, subsubDir, sep = "/", collapse = "/"))) {
      cat("shiny_app directory exists in your project directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("shiny_app directory does not exist in your project directory - creating now!\n ")
      dir.create(file.path(mainDir, subDir,subsubDir))
    }

    destfile = paste0(mainDir,"/code/shiny_app/app_koboloadeR.R")
    file.copy(paste(path_correct,"/koboloadeR/shiny_app/app_koboloadeR.R", sep = ""), destfile, overwrite = TRUE)

    destfile = paste0(mainDir,"/code/shiny_app/app_sampling.R")
    file.copy(paste(path_correct,"/koboloadeR/shiny_app/app_sampling.R", sep = ""), destfile, overwrite = TRUE)

    destfile = paste0(mainDir,"/code/shiny_app/app_dataviewer.R")
    file.copy(paste(path_correct,"/koboloadeR/shiny_app/app_dataviewer.R", sep = ""), destfile, overwrite = TRUE)

    destfile = paste0(mainDir,"/code/shiny_app/app_main_koboloadeR.R")
    file.copy(paste(path_correct,"/koboloadeR/shiny_app/app_main_koboloadeR.R", sep = ""), destfile, overwrite = TRUE)

    ## WWW sub subfolder creation ####

    subsubsubDir <- "www"
    if (file.exists(paste(mainDir, subDir,"/",subsubDir,"/",subsubsubDir, "/",sep = "/", collapse = "/"))) {
      cat("www exists in subDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, subsubDir,subsubsubDir, sep = "/", collapse = "/"))) {
      cat("www directory exists in your project directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("www directory does not exist in your project directory - creating now!\n ")
      dir.create(file.path(mainDir, subDir,subsubDir,subsubsubDir))
    }

    destfile = paste0(mainDir,"/code/shiny_app/www/exportformat.png")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/shiny_app/www/exportformat.png",sep = ""), destfile)
    }else{
      file.remove(destfile)
      file.copy(paste(path_correct,"/koboloadeR/shiny_app/www/exportformat.png",sep = ""), destfile)
    }

    destfile = paste0(mainDir,"/code/shiny_app/www/bootstrap.min.css")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/shiny_app/www/bootstrap.min.css",sep = ""), destfile)
    }else{
      file.remove(destfile)
      file.copy(paste(path_correct,"/koboloadeR/shiny_app/www/bootstrap.min.css",sep = ""), destfile)
    }

    destfile = paste0(mainDir,"/code/shiny_app/www/style.css")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/shiny_app/www/style.css",sep = ""), destfile)
    }else{
      file.remove(destfile)
      file.copy(paste(path_correct,"/koboloadeR/shiny_app/www/style.css",sep = ""), destfile)
    }

    destfile = paste0(mainDir,"/code/shiny_app/www/background-lines.png")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/shiny_app/www/background-lines.png",sep = ""), destfile)
    }else{
      file.remove(destfile)
      file.copy(paste(path_correct,"/koboloadeR/shiny_app/www/background-lines.png",sep = ""), destfile)
    }

    destfile = paste0(mainDir,"/code/shiny_app/www/analysis-plan-configuration.png")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/shiny_app/www/analysis-plan-configuration.png",sep = ""), destfile)
    }else{
      file.remove(destfile)
      file.copy(paste(path_correct,"/koboloadeR/shiny_app/www/analysis-plan-configuration.png",sep = ""), destfile)
    }

    ## HTML Template ####

    subsubDir <- "css"
    if (file.exists(paste(mainDir, subDir,"/",subsubDir,"/", sep = "/", collapse = "/"))) {
      cat("css exists in subDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, subsubDir, sep = "/", collapse = "/"))) {
      cat("css directory exists in your project directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("css directory does not exist in your project directory - creating now!\n ")
      dir.create(file.path(mainDir, subDir,subsubDir))
    }

    destfile = paste0(mainDir,"/code/css/bootstrap.css")
    file.copy(paste(path_correct,"/koboloadeR/css/bootstrap.css", sep = ""), destfile, overwrite = TRUE)

    destfile = paste0(mainDir,"/code/css/unhcr-bootstrap.css")
    file.copy(paste(path_correct,"/koboloadeR/css/unhcr-bootstrap.css", sep = ""), destfile, overwrite = TRUE)

    destfile = paste0(mainDir,"/code/css/style.css")
    file.copy(paste(path_correct,"/koboloadeR/css/style.css", sep = ""), destfile, overwrite = TRUE)

    destfile = paste0(mainDir,"/code/css/unhcr-header.css")
    file.copy(paste(path_correct,"/koboloadeR/css/unhcr-header.css", sep = ""), destfile, overwrite = TRUE)

    destfile = paste0(mainDir,"/code/css/header.html")
    file.copy(paste(path_correct,"/koboloadeR/css/header.html", sep = ""), destfile, overwrite = TRUE)


    ## fonts sub subfolder creation ####
    subsubsubDir <- "fonts"
    if (file.exists(paste(mainDir, subDir,"/",subsubDir,"/",subsubsubDir, "/",sep = "/", collapse = "/"))) {
      cat("fonts exists in subDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, subsubDir,subsubsubDir, sep = "/", collapse = "/"))) {
      cat("fonts directory exists in your project directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("fonts directory does not exist in your project directory - creating now!\n ")
      dir.create(file.path(mainDir, subDir,subsubDir,subsubsubDir))
    }
    destfile = paste0(mainDir,"/code/css/fonts/glyphicons-halflings-regular.eot")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/css/fonts/glyphicons-halflings-regular.eot",sep = ""), destfile)
    }
    destfile = paste0(mainDir,"/code/css/fonts/glyphicons-halflings-regular.svg")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/css/fonts/glyphicons-halflings-regular.svg",sep = ""), destfile)
    }
    destfile = paste0(mainDir,"/code/css/fonts/glyphicons-halflings-regular.ttf")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/css/fonts/glyphicons-halflings-regular.ttf",sep = ""), destfile)
    }
    destfile = paste0(mainDir,"/code/css/fonts/glyphicons-halflings-regular.woff")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/css/fonts/glyphicons-halflings-regular.woff",sep = ""), destfile)
    }
    destfile = paste0(mainDir,"/code/css/fonts/glyphicons-halflings-regular.woff2")
    if (!file.exists(destfile)) {
      file.copy(paste(path_correct,"/koboloadeR/css/fonts/glyphicons-halflings-regular.woff2",sep = ""), destfile)
    }

    ## image sub subfolder creation ####
    subsubsubDir <- "image"
    if (file.exists(paste(mainDir, subDir,"/",subsubDir,"/",subsubsubDir, "/",sep = "/", collapse = "/"))) {
      cat("image exists in subDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, subsubDir,subsubsubDir, sep = "/", collapse = "/"))) {
      cat("image directory exists in your project directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("image directory does not exist in your project directory - creating now!\n ")
      dir.create(file.path(mainDir, subDir,subsubDir,subsubsubDir))
    }

    destfile = paste0(mainDir,"/code/css/image/decoded.png")
    if (!file.exists(destfile)) { file.copy(paste(path_correct,"/koboloadeR/css/image/decoded.png",sep = ""), destfile)}

    destfile = paste0(mainDir,"/code/css/image/icon-mbl-nav-arrow.png")
    if (!file.exists(destfile)) { file.copy(paste(path_correct,"/koboloadeR/css/image/icon-mbl-nav-arrow.png",sep = ""), destfile)}

    destfile = paste0(mainDir,"/code/css/image/icon-global-search.png")
    if (!file.exists(destfile)) { file.copy(paste(path_correct,"/koboloadeR/css/image/icon-global-search.png",sep = ""), destfile)}

    destfile = paste0(mainDir,"/code/css/image/icon-global-search.png")
    if (!file.exists(destfile)) { file.copy(paste(path_correct,"/koboloadeR/css/image/icon-global-search.png",sep = ""), destfile)}

    destfile = paste0(mainDir,"/code/css/image/icons-tool.png")
    if (!file.exists(destfile)) { file.copy(paste(path_correct,"/koboloadeR/css/image/icons-tool.png",sep = ""), destfile)}

    destfile = paste0(mainDir,"/code/css/image/icons-key.png")
    if (!file.exists(destfile)) { file.copy(paste(path_correct,"/koboloadeR/css/image/icons-key.png",sep = ""), destfile)}

    destfile = paste0(mainDir,"/code/css/image/icon-help.png")
    if (!file.exists(destfile)) { file.copy(paste(path_correct,"/koboloadeR/css/image/icon-help.png",sep = ""), destfile)}

    destfile = paste0(mainDir,"/code/css/image/icon-close.png")
    if (!file.exists(destfile)) { file.copy(paste(path_correct,"/koboloadeR/css/image/icon-close.png",sep = ""), destfile)}

    destfile = paste0(mainDir,"/code/css/image/icon-burger.png")
    if (!file.exists(destfile)) { file.copy(paste(path_correct,"/koboloadeR/css/image/icon-burger.png",sep = ""), destfile)}

    destfile = paste0(mainDir,"/code/css/image/unhcr-logo.png")
    if (!file.exists(destfile)) { file.copy(paste(path_correct,"/koboloadeR/css/image/unhcr-logo.png",sep = ""), destfile)}

    destfile = paste0(mainDir,"/code/css/image/icon-search.png")
    if (!file.exists(destfile)) { file.copy(paste(path_correct,"/koboloadeR/css/image/icon-search.png",sep = ""), destfile)}


    ## Data folder creation ####
    subDir <- "data"

    if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
      cat("Data exists in mainDir and is a directory.\n")
    } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
      cat("Data directory exists in your project directory.\n")
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
    destfile = paste0(mainDir,"/data/README.md")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("### This folder is the one where are stored data in CSV format, the form in XLS format and geodata in SHP format",
                   "# BE CAREFUL: DO NOT SHARE PROTECTION SENSITIVE DATA ON GITHUB!",
                   "",
                   "This project is only to keep track of your analysis workflow"), fileConn)
      close(fileConn)
    }

    destfile = paste0(mainDir,"/data/form.xls")
     if (!file.exists(destfile)) {
       file.copy(paste(path_correct,"/koboloadeR/script/form.xls", sep = ""), destfile)
     }


    ## Out folder creation ####

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
    destfile = paste0(mainDir,"/out/README.md")
    if (!file.exists(destfile)) {
      fileConn <- file(destfile)
      writeLines(c("### This folder is where the analysis output will be generated",
                   "# BE CAREFUL: DO NOT SHARE PROTECTION SENSITIVE DATA ON GITHUB!"), fileConn)
      close(fileConn)
    }


    ## Out subfolder creation ####

    mainDirectory <- paste0(mainDir,"/out")
    # subDir <- "/bar_multi"
    # if (file.exists(paste(mainDirectory, subDir, "/", sep = "/", collapse = "/"))) {
    #   cat("bar_multi directory exists in out directory and is a directory.\n")
    # } else if (file.exists(paste(mainDirectory, subDir, sep = "/", collapse = "/"))) {
    #   cat("bar_multi directory exists in your out directory.\n")
    #   # you will probably want to handle this separately
    # } else {
    #   cat("bar_multi directory does not exist in your out directory - creating now!\n ")
    #   dir.create(file.path(mainDirectory, subDir))
    # }
    #
    # mainDirectory <- paste0(mainDir,"/out")
    # subDir <- "/disagg_multi"
    # if (file.exists(paste(mainDirectory, subDir, "/", sep = "/", collapse = "/"))) {
    #   cat("disagg_multi directory exists in out directory and is a directory.\n")
    # } else if (file.exists(paste(mainDirectory, subDir, sep = "/", collapse = "/"))) {
    #   cat("disagg_multi directory exists in your out directory.\n")
    #   # you will probably want to handle this separately
    # } else {
    #   cat("disagg_multi directory does not exist in your out directory - creating now!\n ")
    #   dir.create(file.path(mainDirectory, subDir))
    # }
    #
    # mainDirectory <- paste0(mainDir,"/out")
    # subDir <- "/bar_one"
    # if (file.exists(paste(mainDirectory, subDir, "/", sep = "/", collapse = "/"))) {
    #   cat("bar_one directory exists in out directory and is a directory.\n")
    # } else if (file.exists(paste(mainDirectory, subDir, sep = "/", collapse = "/"))) {
    #   cat("bar_one directory exists in your out directory.\n")
    #   # you will probably want to handle this separately
    # } else {
    #   cat("bar_one directory does not exist in your out directory - creating now!\n ")
    #   dir.create(file.path(mainDirectory, subDir))
    # }
    #
    # mainDirectory <- paste0(mainDir,"/out")
    # subDir <- "/disagg_one"
    # if (file.exists(paste(mainDirectory, subDir, "/", sep = "/", collapse = "/"))) {
    #   cat("disagg_one directory exists in out directory and is a directory.\n")
    # } else if (file.exists(paste(mainDirectory, subDir, sep = "/", collapse = "/"))) {
    #   cat("disagg_one directory exists in your out directory.\n")
    #   # you will probably want to handle this separately
    # } else {
    #   cat("disagg_one directory does not exist in your out directory - creating now!\n ")
    #   dir.create(file.path(mainDirectory, subDir))
    # }


    mainDirectory <- paste0(mainDir,"/out")
    subDir <- "/crunching_reports"
    if (file.exists(paste(mainDirectory, subDir, "/", sep = "/", collapse = "/"))) {
      cat("crunching_reports directory exists in out directory and is a directory.\n")
    } else if (file.exists(paste(mainDirectory, subDir, sep = "/", collapse = "/"))) {
      cat("crunching_reports directory exists in your out directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("crunching_reports directory does not exist in your out directory - creating now!\n ")
      dir.create(file.path(mainDirectory, subDir))
    }

    mainDirectory <- paste0(mainDir,"/out")
    subDir <- "/cluster_reports"
    if (file.exists(paste(mainDirectory, subDir, "/", sep = "/", collapse = "/"))) {
      cat("cluster_reports directory exists in out directory and is a directory.\n")
    } else if (file.exists(paste(mainDirectory, subDir, sep = "/", collapse = "/"))) {
      cat("cluster_reports directory exists in your out directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("cluster_reports directory does not exist in your out directory - creating now!\n ")
      dir.create(file.path(mainDirectory, subDir))
    }

    mainDirectory <- paste0(mainDir,"/out")
    subDir <- "/anonymisation_reports"
    if (file.exists(paste(mainDirectory, subDir, "/", sep = "/", collapse = "/"))) {
      cat("anonymisation_reports directory exists in out directory and is a directory.\n")
    } else if (file.exists(paste(mainDirectory, subDir, sep = "/", collapse = "/"))) {
      cat("anonymisation_reports directory exists in your out directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("anonymisation_reports directory does not exist in your out directory - creating now!\n ")
      dir.create(file.path(mainDirectory, subDir))
    }


    mainDirectory <- paste0(mainDir,"/out")
    subDir <- "/ddi"
    if (file.exists(paste(mainDirectory, subDir, "/", sep = "/", collapse = "/"))) {
      cat("ddi directory exists in out directory and is a directory.\n")
    } else if (file.exists(paste(mainDirectory, subDir, sep = "/", collapse = "/"))) {
      cat("ddi directory exists in your out directory.\n")
      # you will probably want to handle this separately
    } else {
      cat("ddi directory does not exist in your out directory - creating now!\n ")
      dir.create(file.path(mainDirectory, subDir))
    }

    ## reset the correct Working directory
    setwd(mainDir)
    cat("Please open now the file called run-analysis.R within the ++code++ folder, configure the xlsform and get your dataset. \n ")

    kobo_load_packages() # Make sure that all the packages necessary are loaded.
  }, error = function(err) {
    print("kobo_projectinit_ERROR")
    return(structure(err, class = "try-error"))
  })
}
NULL
