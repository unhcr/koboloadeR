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


  cat("This script will allow you to write the configuration files for your project.\n")

  cat(" \n")

  cat(" \n")
  cat("First creating the necessary forlders\n")
  kobo_projectinit()
  #load necess
  cat(" \n")
  cat(" \n")
  cat("Now configuring a few elements to write the config file: \n")

  cat(" \n")
  cat("  1.- confirm the name of the main dataframe for the data. this should be a *.xls file  - NOT a *.xlsx file! \n")
  datafile <- readline("insert the name of the data file: if empty - we use data.xls \n")
  if(datafile==""){
    datafile<-"data.xls"
  }
  datafile<-str_replace_all(datafile," ","")
  cat(" \n")
  cat("  2.- Confirm the name of the form. This should be a *.xls file - NOT a *.xlsx file! \n")
  formfile <- readline("Insert the name of the form file: if empty - we use form.xls \n")
  if(formfile==""){
    formfile<-c("form.xls")
  }
  formfile<-str_replace_all(formfile," ","")
  Koboformname <- read_excel(paste0("data/",formfile) , sheet='settings')
  koboformname <- as.character(Koboformname[1,"id_string"])

  cat(" \n")
  cat("  3.- Type of weighting system used? Type the number in brackets().Default is None (2) \n")
  usedweight <- readline("Sampling frame (1), None (2)")
  if(usedweight==""){
    usedweight <- 2
  }
  if (usedweight==1){
    cat("     3.1- Type of sampling used ? Type the number in brackets() \n")
    usedsampling <- readline("Simple random (1), 2 stages random -st1 (2), cluster sampling(3)")

  }
  cat(" \n")

  cat(" \n")
  usedanalysisplan <- readline("4.- Did you use the data analysis plan? Yes (Y) or No (N)")
  cat(" \n")

  cleaneddata <- readline("5.- Did you clean the data? Yes (Y) or No (N)")
  if (cleaneddata=="Y"){
    namecleansheet <- readline("   - What is the name of the cleaned excel sheet? Default: 'cleaned_data'")
    if(namecleansheet==''){
      namecleansheet<- "cleaned_data"
    }
  }
  cat(" \n")


  cat("  5.- Other information \n")
  report_name <- readline("What is the name of the report?\n")
  location <- readline("Where is the report written?\n")
  author <- readline("What is your name (author)?\n")
  organisation <- readline("What is your organisation?\n")

  cat(" \n")
  cat(" \n")
  cat(" \n")
  cat(" \n")
  cat(" ##############################################\n")
  cat(" # The initial configuration is completed!    #\n")
  cat(" # A config file has been generated!          #\n")
  cat(" # Please verify all is good: data/0-config.R #\n")
  cat(" ##############################################\n")
  cat(" \n")
  cat(" \n")
  cat(" \n")
  cat("# When you are ready, run kobo_dico() to continue the analysis")

  ###Config file writing
  #Fetching the directory
  mainDir <- getwd()
  #Path to file
  configfile<-paste(mainDir,"/code/0-config.R",sep="")
  #Writting file
  sink(configfile)
  cat("#### Config file ###\n")
  cat("\n")
  cat("### Can be manualy edited or interactively rewritten using the function kobo_projectconfig() \n")
  cat("\n")
  cat("### 1. Form in xslform format - saved as .xls - not xlsx - in the data folder###\n")
  cat('#Replace "',formfile,'" by the name of the form in the data folder (ex: baseline_form.xls)\n')
  cat(paste("form<-'",formfile,"'",sep=""))
  cat("\n")
  cat(paste('path.to.form <- paste("data/',formfile,'",sep="") \n',sep=""))
  path.to.form <- paste("'data/",formfile,"'",sep="")

  cat("\n")
  cat("\n### 2. Main dataframe for the data. this should be a *.xls file ###\n")
  cat("#Replace 'datafile' by the name of the dataframe in the data folder (ex: baseline_data.xls)\n")
  cat(paste('path.to.data <- paste("data/',datafile,'",sep="") \n',sep=""))
  path.to.data <- paste("'data/",datafile,"'",sep="")
  cat(paste('datafile <-"',datafile,'"',sep=""))
  cat("\n")

  if(cleaneddata=="Y"){
    cat(paste('data <- read_excel(',path.to.data,', sheet="',namecleansheet,'")',sep=""))
    cat("\n")

  }
  if(cleaneddata=="N"){

    cat(paste('data <- read_excel(',path.to.data,', sheet="',koboformname,'")',sep=""))
    cat("\n")

  }


  cat("### 3.- Type of weighting used ###\n")

  if(usedweight==1){
    cat("#From Sampling frame: \n")
    cat(paste('usedweight <- "sampling_frame"',sep=""))
    cat("\n")
    cat("####### 3.1 - Type of sampling used ###\n")

    if(usedsampling==1){
      cat("#Simple random: \n")
      cat(paste('usedsampling <- "simple random"',sep=""))
    }
    if(usedsampling==2){
      cat("#2 stages random - 1st: \n")
      cat(paste('usedsampling <- "2 stages random"',sep=""))
    }
    if(usedsampling==3){
      cat("#Cluster sampling: \n")
      cat(paste('usedsampling <- "cluster sampling"',sep=""))
    }

    cat("\n")

  }else{
    cat(paste0('usedweight <- "none"'))
  }
  cat("\n")
  cat("\n")
  cat("###  4.- Used the data analysis plan\n")
  cat(paste('analysis_plan <-"',usedanalysisplan,'"',sep=""))

  cat("\n")


  cat("\n")
  cat("\n")

  cat("###  5. General info on the project\n")
  cat(paste('report_name <-"',report_name,'"',sep=""))
  cat("\n")
  cat(paste('location <-"',location,'"',sep=""))
  cat("\n")
  cat(paste('author <-"',author,'"',sep=""))
  cat("\n")
  cat(paste('organisation <-"',organisation,'"',sep=""))
  cat("\n")
  sink()
}
NULL
