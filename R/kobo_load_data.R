#' @name kobo_load_data
#' @rdname kobo_load_data
#' @title Kobo Load Data
#'
#' @description Load form, building dictionnary, loading all required data into the environment, Check to split select_multiple if data is extracted from ODK, Clean variable if any and Re-encoding data based on the dictionnary
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#' 
#' @param app The place where the function has been executed, the default is the console and the second option is the shiny app
#'
#' @return No return, all results will be saved inside new CSV files
#'
#' @author Edouard Legoupil, Maher Daoud
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

kobo_load_data <- function(form = "form.xls", app="console") {
  tryCatch({
    ## Load all required packages#############################################
    if(app=="shiny"){
      progress <- shiny::Progress$new()
      progress$set(message = "Load Data in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 25
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()
    }
    kobo_load_packages()
    configInfo <- kobo_get_config(form)
    configInfo <- configInfo[!is.na(configInfo$name),]
    mainDir <- kobo_getMainDirectory()
    form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
    
    cat("\n\n\n Generate dictionnary from the xlsform \n\n\n\n")
    kobo_dico(form)
    dico <- read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")
    
    ## Load data #######################################################################
    cat("\n\n\n Load original dataset \n\n\n\n")
    originalData <- read.csv(paste(mainDir,"/data/MainDataFrame.csv",sep = ""), sep = ",", encoding = "UTF-8", na.strings = "") 
    
    ## Check to split select_multiple if data is extracted from ODK ###################
    cat("\n\n\n Now split select_multiple  variables \n\n\n\n")
    if(app=="shiny"){
      progress$set(message = "Splitting Main Data File in progress...")
      updateProgress()
    }
    household <- kobo_split_multiple(originalData, dico)
    
    ## Clean variable if any ##########################################################
    cat("\n\n\n Clean variable if any \n\n\n\n")
    if(app=="shiny"){
      progress$set(message = "Cleaning Main Data File in progress...")
      updateProgress()
    }
    household <- kobo_clean(household, dico)

    ## Join with Weight file #########################################
    cat("\n\n\n Adding weight and removing some forms \n\n\n\n")
    if(nrow(configInfo)==0){
      cat("\n\n\n You need to enter the sampling methods and all required parameters in settings sheet before processed  \n\n\n\n")
      return(FALSE)
    }
    if(configInfo[configInfo$name=="sample_type", "value"] != "No sampling(type 1)"){
      if(app=="shiny"){
        progress$set(message = "Adding weights with Main Data File in progress...")
        updateProgress()
      }
      path <- configInfo[configInfo$name=="weights_info", "path"]
      weight <- read.csv(path,stringsAsFactors = F)
      variableName <- configInfo[configInfo$name=="variable_name", "value"]
      household <- left_join(x = household, y = weight, by = variableName)
    }
    
    ## Cheking the labels matching... #################################################
    ## household is the default root data componnents to be used -- in order to deal with nested dataset
    if(app=="shiny"){
      progress$set(message = "labeling variables for Main Data File in progress...")
      updateProgress()
    }
    cat("\n\n\n Now  labeling variables \n\n\n\n")
    household <- kobo_label(household, dico)

    ## Save preliminary version before encoding or adding indicators ##################
    cat("\n\nWrite backup before encoding or indicators calculation..\n")
    write.csv(household,paste(mainDir,"/data/household.csv",sep = ""), row.names = FALSE, na = "")
    
    ## load all required data files #########################################
    cat("\n\nload all required data files..\n")
    if(app=="shiny"){
      progress$set(message = "loading all required data files in progress...")
      updateProgress()
    }
    
    configInfo <- configInfo[startsWith(tolower(configInfo$name), "instanceid"),]
    levelsOfDF <- kobo_get_dataframes_levels(form)
    levelsOfDF <- levelsOfDF[levelsOfDF$name!="MainDataFrame",]
    if(nrow(levelsOfDF)!=0){
      levelsOfDF[levelsOfDF$parent=="MainDataFrame","parent"] <- "household"
    }
    #ataBeginRepeat <- kobo_get_begin_repeat("form2.xls")
    #dataBeginRepeat <- dataBeginRepeat$names
    
    if(nrow(levelsOfDF)!=0){
    
      for (dbr in levelsOfDF$name) {
        cat("\n\nload all required data files..\n")
        if(app=="shiny"){
          progress$set(message = paste("loading",dbr,"file in progress..."))
          updateProgress()
        }
        dataFrame <- read.csv(paste(mainDir,"/data/",dbr,".csv",sep = ""),stringsAsFactors = F) 
        if(app=="shiny"){
          progress$set(message = paste("Splitting",dbr,"file in progress..."))
          updateProgress()
        }
        dataFrame <- kobo_split_multiple(dataFrame, dico)
        if(app=="shiny"){
          progress$set(message = paste("Cleaning",dbr,"file in progress..."))
          updateProgress()
        }
        dataFrame <- kobo_clean(dataFrame, dico)
        if(app=="shiny"){
          progress$set(message = paste("Labeling",dbr,"file in progress..."))
          updateProgress()
        }
        dataFrame <- kobo_label(dataFrame, dico)
        
        write.csv(dataFrame,paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""), row.names = FALSE, na = "")
        cat("\n\nload",dbr,"and create all needed files for it..\n")
        
      }
      for (dbr in levelsOfDF$name) {
        if(app=="shiny"){
          progress$set(message = paste("loading",dbr,"file in progress..."))
          updateProgress()
        }
        dataFrame <- read.csv(paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""),stringsAsFactors = F) 
        child <- levelsOfDF[levelsOfDF$name==dbr, "name"]
        parent <- levelsOfDF[levelsOfDF$name==dbr, "parent"]
        while (T) {
          instanceIDChild  <- configInfo[tolower(configInfo$name)==tolower(paste0("instanceid_",child,"_",ifelse(parent=="household","MainDataFrame",parent))), "value"]
          instanceIDParent <- configInfo[tolower(configInfo$name)==tolower(paste0("instanceid_",ifelse(parent=="household","MainDataFrame",parent),"_",child)), "value"]
          if(parent=="household"){
            parentDf <- read.csv(paste(mainDir,"/data/",parent,".csv",sep = ""),stringsAsFactors = F)
          }else{
            parentDf <- read.csv(paste(mainDir,"/data/",parent,"-edited.csv",sep = ""),stringsAsFactors = F)
          }
          unColChild <- dataFrame[,instanceIDChild]
          dataFrame <- dataFrame[,colnames(dataFrame)!=instanceIDChild]
          unCN <- colnames(dataFrame)[!colnames(dataFrame) %in% colnames(parentDf)]
          
          if(instanceIDChild != instanceIDParent){
            unCN <- c(instanceIDChild, unCN, "jointemp")
            dataFrame[instanceIDChild] <- unColChild
            dataFrame["jointemp"] <- unColChild
          }else{
            unCN <- c(unCN, "jointemp")
            dataFrame["jointemp"] <- unColChild
          }
          
          parentDf["jointemp"] <- parentDf[,instanceIDParent]
          
          dataFrame <- dataFrame[ unCN ]
          
          dataFrame <- left_join(dataFrame, parentDf, by="jointemp")
          dataFrame["jointemp"] <- NULL
          
          if(parent=="household"){
            break
          }else{
            child <- levelsOfDF[levelsOfDF$name==parent, "name"]
            parent <- levelsOfDF[levelsOfDF$name==parent, "parent"]
          }
        }
        
        write.csv(dataFrame,paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""), row.names = FALSE, na = "")
      }
    
    }
    ## Compute indicators if defined ##################################################
    cat("\n\nCompute indicators if defined..\n")
    if(app=="shiny"){
      progress$set(message = "Computing indicators (if defined) in progress...")
      updateProgress()
    }

    result <-  kobo_create_indicators(form)
    if(class(result) == "try-error"){
      return(structure(result, class = "try-error"))
    }
    
    
    
    dico <- read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")
    household <- read.csv(paste(mainDir,"/data/household.csv",sep = ""), encoding = "UTF-8", na.strings = "NA")

    ## Re-encoding data now based on the dictionnary -- ##############################
    ## the xlsform dictionnary can be adjusted this script re-runned till satisfaction
    cat("\n\n\n Now  re-encode data  \n\n\n\n")
    cat("\n\nCompute indicators if defined..\n")
    if(app=="shiny"){
      progress$set(message = "Re-encoding data now based on the dictionnary in progress...")
      updateProgress()
    }
    
    household <- kobo_encode(household, dico)
    for (dbr in levelsOfDF$name) {
      dataFrame <- read.csv(paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""),stringsAsFactors = F) 
      dataFrame <- kobo_encode(dataFrame, dico)
      write.csv(dataFrame,paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""), row.names = FALSE, na = "")
      cat("\n\nRe-encode",dbr,"..\n")
    }
    if(app=="shiny"){
      updateProgress()
    }
    write.csv(household,paste(mainDir,"/data/household.csv",sep = ""), row.names = FALSE, na = "")
    return(TRUE)
  }, error = function(err) {
    print("kobo_load_data_ERROR")
    return(structure(err, class = "try-error"))
  })
}
