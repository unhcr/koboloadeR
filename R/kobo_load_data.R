#' @name kobo_load_data
#' @rdname kobo_load_data
#' @title Kobo Load Data
#'
#' @description Load form, building dictionnary, loading all required data into the environment, Check to split select_multiple if data is extracted from ODK, Clean variable if any and Re-encoding data based on the dictionnary
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#' 
#'
#' @return No return, all results will be saved inside new CSV files
#'
#' @author Maher Daoud
#'
#' @examples
#' kobo_load_data()
#'
#' @examples
#' \dontrun{
#' kobo_load_data("myform.xls")
#' }
#'
#' @export kobo_load_data
#'

kobo_load_data <- function(form = "form.xls") {
  mainDir <- kobo_getMainDirectory()
  form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
  ## Load all required packages#############################################
  source("code/0-packages.R")
  source("code/0-config.R")
  source("code/0-theme.R")
  cat("\n\n\n Generate dictionnary from the xlsform \n\n\n\n")
  kobo_dico(form)
  dico <- read.csv(paste("data/dico_",form,".csv",sep = ""), encoding = "UTF-8", na.strings = "")
  
  ## Load data #######################################################################
  cat("\n\n\n Load original dataset \n\n\n\n")
  data.or <- read.csv(path.to.data, sep = ",", encoding = "UTF-8", na.strings = "") 

  ## Check to split select_multiple if data is extracted from ODK ###################
  cat("\n\n\n Now split select_multiple  variables \n\n\n\n")
  household <- kobo_split_multiple(data.or, dico)
  
  ## Clean variable if any ##########################################################
  cat("\n\n\n Clean variable if any \n\n\n\n")
  household <- kobo_clean(household, dico)
  
  
  ## Join with Weight file #########################################
  cat("\n\n\n Adding weight and removing some forms \n\n\n\n")
  settings <- tryCatch({
    as.data.frame(read_excel(form_tmp, sheet = "settings"),
                  stringsAsFactors = FALSE)
  }, error = function(err) {
    data.frame(
      name = character(),
      label = character(),
      value = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
  })
  if(nrow(settings)==0){
    cat("\n\n\n You need to enter the sampling methods and all required parameters in settings sheet before processed  \n\n\n\n")
    return(FALSE)
  }
  if(settings[settings$name=="sample_type", "value"] != "No sampling(type 1)"){
    path <- settings[settings$name=="weights_info", "path"]
    weight <- read.csv(path,stringsAsFactors = F)
    variableName <- settings[settings$name=="variable_name", "value"]
    household <- left_join(x = household, y = weight, by = variableName)
  }
  
  ## Cheking the labels matching... #################################################
  ## household is the default root data componnents to be used -- in order to deal with nested dataset
  cat("\n\n\n Now  labeling variables \n\n\n\n")
  household <- kobo_label(household, dico)
  
  ## Save preliminary version before encoding or adding indicators ##################
  cat("\n\nWrite backup before encoding or indicators calculation..\n")
  write.csv(household,paste(mainDir,"/household.csv",sep = ""), row.names = FALSE, na = "")
  
  ## load all required data files #########################################
  cat("\n\nload all required data files..\n")
  dataBeginRepeat <- kobo_get_begin_repeat()
  dataBeginRepeat <- dataBeginRepeat$names
  for (dbr in dataBeginRepeat) {
    dataFrame <- read.csv(paste(mainDir,"/data/",dbr,".csv",sep = ""),stringsAsFactors = F) 
    dataFrame <- kobo_split_multiple(dataFrame, dico)
    dataFrame <- kobo_clean(dataFrame, dico)
    dataFrame <- kobo_label(dataFrame, dico)
    dataFrame <- left_join(household, dataFrame, by="responseID")
    write.csv(dataFrame,paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""), row.names = FALSE, na = "")
    cat("\n\nload",dbr,"and create all needed files for it..\n")
  }
  
  
  ## Compute indicators if defined ##################################################
  kobo_create_indicators()
  
  dico <- read.csv(paste("data/dico_",form,".csv",sep = ""), encoding = "UTF-8", na.strings = "")
  household <- read.csv(paste(mainDir,"/household.csv",sep = ""), encoding = "UTF-8", na.strings = "NA")
  
  ## Re-encoding data now based on the dictionnary -- ##############################
  ## the xlsform dictionnary can be adjusted this script re-runned till satisfaction
  cat("\n\n\n Now  re-encode data  \n\n\n\n")
  household <- kobo_encode(household, dico)
  for (dbr in dataBeginRepeat) {
    dataFrame <- read.csv(paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""),stringsAsFactors = F) 
    dataFrame <- kobo_encode(dataFrame, dico)
    write.csv(dataFrame,paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""), row.names = FALSE, na = "")
    cat("\n\nload",dbr,"and create all needed files for it..\n")
  }
  
  write.csv(household,paste(mainDir,"/household.csv",sep = ""), row.names = FALSE, na = "")
  
}
