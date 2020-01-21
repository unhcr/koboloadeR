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
#'
#' @examples
#' \dontrun{
#' kobo_load_data("myform.xls")
#' }
#'
#' @export kobo_load_data
#'

kobo_load_data <- function(form = "form.xls", app = "console") {
  tryCatch ( {
    ## Load all required packages#############################################

    if (app == "shiny") {
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

    ## getting project configuration variables
    cat("\n\n\n Getting project configuration variables \n\n\n\n")
    configInfoOrigin <- kobo_get_config(form)
    configInfoOrigin <- configInfoOrigin[!is.na(configInfoOrigin$name),]



    cat("\n\n\n Generate dictionnary from the xlsform \n\n\n\n")
    mainDir <- kobo_getMainDirectory()
    kobo_dico(form)
    dico <- utils::read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")

    ## Load data #######################################################################
    cat("\n\n\n Load original dataset \n\n\n\n")

    originalData <- utils::read.csv(configInfoOrigin[configInfoOrigin$name == "MainDataFrame", "path"], sep = ",", encoding = "UTF-8", na.strings = "")

    if (ncol(originalData) == 1) {
      cat("seems like you file use  ; rather , variable separator.... \n")
      originalData <- utils::read.csv(configInfoOrigin[configInfoOrigin$name == "MainDataFrame", "path"], sep = ";", encoding = "UTF-8", na.strings = "")
    }

    ## Check to split select_multiple if data is extracted from ODK ###################
    if (app == "shiny") {
      progress$set(message = "Splitting Main Data File in progress...")
      updateProgress()
    }

    cat("\n\n\n Now split select_multiple  variables \n\n\n\n")
    MainDataFrame <- kobo_split_multiple(originalData, dico)


    ## Clean variable if any ##########################################################
    if (app == "shiny") {
      progress$set(message = "Cleaning Main Data File in progress...")
      updateProgress()
    }


    cat("\n\n\n Clean variable if any \n\n\n\n")
    MainDataFrame <- kobo_clean(MainDataFrame)



    ## Join with Weight file #########################################
    cat("\n\n\n Set up sampling \n\n\n\n")

    if (nrow(configInfoOrigin) == 0) {
      cat("\n\n\n You need to enter the sampling methods and all required parameters in settings sheet before processed  \n\n No sampling (type 1) \n\n Cluster sample (type 2) \n\n Stratified sample (type 3) \n\n")
      return(FALSE)
    }

    if (length(configInfoOrigin[configInfoOrigin$name == "sample_type", "value"]) != 0) {

      if (configInfoOrigin[configInfoOrigin$name == "sample_type", "value"] != "No sampling (type 1)") {

        if (app == "shiny") {
          progress$set(message = "Adding weights with Main Data File in progress...")
          updateProgress()
        }

        path <- configInfoOrigin[configInfoOrigin$name == "weights_info", "path"]
        weight <- utils::read.csv(path,stringsAsFactors = F)


        variableName <- configInfoOrigin[configInfoOrigin$name == "variable_name", "value"]
        MainDataFrame <- plyr::join(x = MainDataFrame, y = weight, by = variableName, type = "right")


      }
    }


    ## Cheking the labels matching... #################################################


    ## MainDataFrame is the default root data componnents to be used -- in order to deal with nested dataset
    if (app == "shiny") {


      progress$set(message = "labeling variables for Main Data File in progress...")
      updateProgress()
    }
    cat("\n\n\n Now  labeling variables \n\n\n\n")
    MainDataFrame <- kobo_label(MainDataFrame, dico)

    ## Save preliminary version before encoding or adding indicators ##################


    cat("\n\n Write backup before encoding or indicators calculation..\n")
    utils::write.csv(MainDataFrame,paste(mainDir,"/data/MainDataFrame_edited.csv",sep = ""), row.names = FALSE, na = "")



    ## load all required data files #########################################
    cat("\n\nload all required nested data files..\n")
    if (app == "shiny") {
      progress$set(message = "loading all required data files in progress...")
      updateProgress()
    }


    configInfo <- configInfoOrigin[startsWith(tolower(configInfoOrigin$name), "instanceid"),]

    levelsOfDF <- kobo_get_dataframes_levels(form)


    levelsOfDF <- levelsOfDF[levelsOfDF$name != "MainDataFrame",]
    if (nrow(levelsOfDF) != 0) {
    #  levelsOfDF[levelsOfDF$parent == "MainDataFrame","parent"] <- "MainDataFrame"
    #}
    #dataBeginRepeat <- kobo_get_begin_repeat("form2.xls")


    #dataBeginRepeat <- dataBeginRepeat$names


      for (dbr in levelsOfDF$name) {

        if (app == "shiny") {
          progress$set(message = paste("loading",dbr,"file in progress..."))
          updateProgress()
        }
        # dbr <- levelsOfDF$name[1]
        cat("\n\nloading",dbr,"file ..\n")
        dataFrame <- utils::read.csv(configInfoOrigin[configInfoOrigin$name == dbr,"path"], stringsAsFactors = F)

        if (app == "shiny") {
          progress$set(message = paste("Splitting",dbr,"file in progress..."))
          updateProgress()
        }
        cat(paste("Splitting",dbr,"file in progress...\n"))
        dataFrame <- kobo_split_multiple(dataFrame, dico)


        if (app == "shiny") {
          progress$set(message = paste("Cleaning",dbr,"file in progress..."))
          updateProgress()
        }
        cat(paste("Cleaning",dbr,"file in progress...\n"))
        dataFrame <- kobo_clean(dataFrame)


        if (app == "shiny") {
          progress$set(message = paste("Labeling",dbr,"file in progress..."))
          updateProgress()
        }
        cat(paste("Labeling",dbr,"file in progress...\n"))
        dataFrame <- kobo_label(dataFrame, dico)


        cat("\n\n Saving ",dbr,"file as _edited..\n")
        utils::write.csv(dataFrame,paste(mainDir,"/data/",dbr,"_edited.csv",sep = ""), row.names = FALSE, na = "")

      # }
      #
      #
      # cat("\n\n Join hierarchical structured if defined..\n")
      # for (dbr in levelsOfDF$name) {

        # if (app == "shiny") {
        #   progress$set(message = paste("loading",dbr,"file in progress..."))
        #   updateProgress()
        # }
        #
        # dataFrame <- utils::read.csv(paste(mainDir,"/data/",dbr,"_edited.csv",sep = ""),stringsAsFactors = F)
        child <- levelsOfDF[levelsOfDF$name == dbr, "name"]
        parent <- levelsOfDF[levelsOfDF$name == dbr, "parent"]

        cat("\n\n Join hierarchical structure between ", child, " and ", parent, " in order to calculate indicators...\n")

        while (T) {
          instanceIDChild  <- configInfo[tolower(configInfo$name) == tolower(paste0("instanceid_",child,"_",ifelse(parent == "household","MainDataFrame",parent))), "value"]
          instanceIDParent <- configInfo[tolower(configInfo$name) == tolower(paste0("instanceid_",ifelse(parent == "household","MainDataFrame",parent),"_",child)), "value"]

          ## Case MainDataFrame called household
          if (parent %in% c("household", "MainDataFrame")) {
            parentDf <- utils::read.csv(paste(mainDir,"/data/",parent,"_edited.csv",sep = ""),stringsAsFactors = F)

          }else{
            parentDf <- utils::read.csv(paste(mainDir,"/data/",parent,"_edited.csv",sep = ""),stringsAsFactors = F)
          }


          ## Preparing the 2 data frame for a left join - create a common key betwee 2 frames for the left_join
          unColChild <- dataFrame[,instanceIDChild]

          ## Removing this from child
          dataFrame <- dataFrame[  colnames(dataFrame) != instanceIDChild]

          ## get all variables from child that are not in parent

          unCN <- colnames(dataFrame)[!colnames(dataFrame) %in% colnames(parentDf)]

          if (instanceIDChild != instanceIDParent) {
            unCN <- c(instanceIDChild, unCN, "jointemp")
            dataFrame[instanceIDChild] <- unColChild
            dataFrame[ , "jointemp"] <- unColChild
          } else {
            unCN <- c(unCN, "jointemp")
            dataFrame[ , "jointemp"] <- unColChild
          }

          parentDf[, "jointemp"] <- parentDf[,instanceIDParent]

          dataFrame <- dataFrame[ unCN ]

          ### Now ready for a left join
          dataFrame <- plyr::join(dataFrame, parentDf, by = "jointemp", type = "right")
          dataFrame["jointemp"] <- NULL


          if (parent == "MainDataFrame") {

            break
          } else {
            child <- levelsOfDF[levelsOfDF$name == parent, "name"]
            parent <- levelsOfDF[levelsOfDF$name == parent, "parent"]
          }
        }

        cat("\n\n Saving edited version of  ", dbr, " ...\n")
        utils::write.csv(dataFrame,paste(mainDir,"/data/",dbr,"_edited.csv",sep = ""), row.names = FALSE, na = "")

      }

    }



    ## Compute indicators if defined ##################################################
    if (app == "shiny") {
      progress$set(message = "Computing indicators (if defined) in progress...")
      updateProgress()
    }

    cat("\n\n Now computing all calculated indicators  if defined..\n")
    result <-  kobo_create_indicators(form)

    if (class(result) == "try-error") {
      return(structure(result, class = "try-error"))
    }



    dico <- utils::read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")

    MainDataFrame <- utils::read.csv(paste(mainDir,"/data/MainDataFrame_edited.csv",sep = ""), encoding = "UTF-8", na.strings = "NA")


    ## Re-encoding data now based on the dictionnary -- ##############################
    ## the xlsform dictionnary can be adjusted this script re-runned till satisfaction
    cat("\n\n\n Now  re-encode data  \n\n\n\n")
    cat("\n\nCompute indicators if defined..\n")
    if (app == "shiny") {
      progress$set(message = "Re-encoding data now based on the dictionnary in progress...")
      updateProgress()
    }


    MainDataFrame <- kobo_encode(MainDataFrame, dico)

    ## loading nested frame
    for (dbr in levelsOfDF$name) {
      dataFrame <- utils::read.csv(paste(mainDir,"/data/",dbr,"_edited.csv",sep = ""),stringsAsFactors = F)
      dataFrame <- kobo_encode(dataFrame, dico)
      utils::write.csv(dataFrame,paste(mainDir,"/data/",dbr,"_encoded.csv",sep = ""), row.names = FALSE, na = "")

      cat("\n\nRe-encode",dbr,"..\n")
    }
    if (app == "shiny") {
      updateProgress()
    }

    utils::write.csv(MainDataFrame,paste(mainDir,"/data/MainDataFrame_encoded.csv",sep = ""), row.names = FALSE, na = "")

    return(TRUE)
  }, error = function(err) {
    print("There was an error in the data processing step!!! \n\n")
    return(structure(err, class = "try-error"))
  })
}
