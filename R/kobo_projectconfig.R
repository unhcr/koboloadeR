#' @name kobo_projectconfig
#' @rdname kobo_projectconfig
#' @title  Project configuration file
#'
#' @description  Write all necessary configuration files for your project
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

  cat(" ##################################################################\n")
  cat(" # Welcome to KoboloadeR !                                        #\n")
  cat(" #an R package to facilitate data crunching of survey  microdata. #\n")
  cat(" ##################################################################\n")
  cat(" \n")
  cat(" The 2 main advantages of KoboloadeR are to: \n")
  cat(" 1. Save time to quicly generate the graphs and analysis you need to discover insights from your dataset.\n")
  cat(" 2. Ensure analysis reproducibilty through a separation of the analysis configuration and the analysis process.\n")
  cat(" \n")
  cat(" For more information: consult https://github.com/Edouard-Legoupil/koboloadeR/blob/master/README.md \n")
  cat(" or send a message to legoupil@unhcr.org\n")
  cat(" \n")
  cat("                     :+o. oo:. \n")
  cat("                   /ooo+  /ooo+.\n")
  cat("                 /oooo/    -oooo+.\n")
  cat("              .+oooo/        :osoo+- \n")
  cat("            :osoo+-             ./oooo/ \n")
  cat("          /oooo:..    -ooso+    . -+ooo+.\n")
  cat("       ./ooo+. .oo/   oooooo.  -so:  /ooo+-\n")
  cat("      /oooo/  .ooo:   :oooo/   .oos:  -oooo+.\n")
  cat("    -ooooo/   +ooo     :oo+     +ooo   -soooo/\n")
  cat("    ooooos-  :soo-   :+oooo+-    ooo+   soooos-\n")
  cat("   .oooooo/.:ooo/  -oooooooooo-  .soo+.-sooooo:\n")
  cat("   .oooooooooooo   /oooooooooo/   /oooosoooooo:\n")
  cat("   .oooooooooos:   /oooooooooo+   .oooooooooos-\n")
  cat("    ooooooooooo    /oooooooooo+    +ooooooooos-\n")
  cat("    oooooooooo:    /oooooooooo+    .ooooooooos.\n")
  cat("    ooooooooo+     /oooooooooo+     :oooooooos.\n")
  cat("    +ooooooo+      /oooooooooo+      /oooooooo \n")
  cat("    /ooooooo.      /oooooooooo+       oooooooo \n")
  cat("    :soooooo       /oooooooooo+       /oooooo+ \n")
  cat("    .soooooo       :soooooooos/       /oooooo: \n")
  cat("     ooooooo          /oooos.         /ooooos- \n")
  cat("     +oooooo          /oooos          /ooooos  \n")
  cat("     +oooooo          /oooos          /oooooo  \n")
  cat("      .----.          -:::::           -----.  \n")
  cat("\n")

  cat(" \n")
  cat(" This script will allow you to write the configuration files for your project.\n")

  cat(" \n")

  cat(" \n")
  cat(" First creating the necessary forlders\n")
  #library("koboloadeR")
  #kobo_projectinit()
  cat(" \n")
  cat(" \n")
  cat(" Now configuring a few elements to write the config file: \n")

  cat(" \n")
  cat("  1.- confirm the name of the main dataframe for the data. this should be a *.csv file with ; as separator \n")
  datafile <- readline("insert the name of the data file: if empty - we use data.csv \n")

  cat(" \n")
  cat("  2.- Confirm the name of the xlsform file for the form. this should be a *.xls file - NOT a *.xlsx file! \n")
  formfile <- readline("insert the name of the form file: if empty - we use form.xls \n")

  cat(" \n")
  cat("  3.- What sampling do you have? \n")
  usedweight <- readline("No sampling(type 1) , Cluster sample (type 2), Stratified sample (type 3), Respondent Driven Sample sample (type 4)? ")
  if (runproject=='Y') {
      source("./code/runproject.R")
    } else {
      cat("You can now go to /code/runproject.R in order to run the project.\n")
    }


  cat(" \n")
  cat("  4.- Do you have data cleaning log? \n")
  usedlog<- readline("Type yes - or leave empty of no datacleaning log ")
  cat(" \n")
  cat("  4.- Do you have indicators definition? \n")
  usedindic <- readline("Type yes - or leave empty of no in ")
  cat(" \n")
  cat(" \n")
  cat(" \n")
  cat(" ###########################################\n")
  cat(" # The initial configuration is completed! #\n")
  cat(" # A config file has been generated!       #\n")
  cat(" ###########################################\n")
  cat(" \n")
  
  cat(" \n")
  cat(" \n")
  cat("  Step 1.- Now generating the dictionnary from the xlsfrom. \n")
  cat(" \n")
  cat("  Step 2.- Now generating the additional indicators. \n")
  cat(" \n")
  cat("  Step 3.-- Now generating a default Rmarkdown template for your report. \n")
  cat(" \n")
  cat("  Step 4.- Now generating the corresponding report in word format. \n")
  cat(" \n")
  cat(" ###################################################################\n")
  cat(" # Check the report and adjust the configuration of your analysis! #\n")
  cat(" # Refer to the package doccumentation for more details            #\n")
  cat(" ###################################################################\n")

  cat(" \n")
  cat("  A- In your xlsform: \n")
  cat("     - Define the chapter to be used for each question \n")
  cat("     - Shorten labels, \n")
  cat("     - Define the variable to be used for disaggregation\n")
  cat("     - Define the variable to be used for correlation. \n")

  cat(" \n")
  cat("  B- Add new indicators in the  list. \n")
  cat(" \n")
  cat("  C- Revise data cleaning log. \n")




  mainDir <- kobo_getMainDirectory()



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
