#' @name kobo_check_project_configuration
#' @rdname kobo_check_project_configuration
#' @title Check kobo_check_project_configuration
#'
#' @description Check if the project configurations exist and all required files are in the right place.
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#'
#'
#' @return The return will be a list that contains a list that checks all elements of the project configuration and a message of confirmation
#'
#' @author Maher Daoud
#'
#'
#' @examples
#' \dontrun{
#' kobo_check_project_configuration("myform.xls")
#' }
#'
#' @export kobo_check_project_configuration
#'

kobo_check_project_configuration <- function(form = "form.xls") {
  tryCatch({
    result <- list()
    result$flag <- T
    result$message <- ""
    countE <- 0

    mainDir <- kobo_getMainDirectory()
    form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
    configInfoOrigin <- kobo_get_config(form)
    configInfoOrigin <- configInfoOrigin[!is.na(configInfoOrigin$name),]
    #check if the form is exist
    if(!file.exists(form_tmp)){
      result$flag <- F
      countE <- countE+1
      result$xlsformExistence <- paste(countE,"-"," The xlsform file doesn't exist in the data folder with 'form' name", sep = "")
      result$message <- paste(result$message, "\n", result$xlsformExistence, sep = "")
      return(result)
    }
    if(nrow(configInfoOrigin)==0){
      result$flag <- F
      countE <- countE+1
      result$settingsExistence <- paste(countE,"-"," The xlsform file doesn't have any settings", sep = "")
      result$message <- paste(result$message, "\n", result$settingsExistence, sep = "")
      return(result)
    }
    #check if data files are exist
    subDataFrames <- kobo_get_begin_repeat()
    subDataFrames = c("MainDataFrame",subDataFrames$names)

    for (sdf in subDataFrames) {
      if(!sdf %in% configInfoOrigin$name){
        result$flag <- F
        countE <- countE+1
        result[[paste0("Existence_",sdf)]] <- paste(countE,"-", sdf, " does not exist in 'analysisSettings' sheet", sep = "")
        result$message <- paste(result$message, "\n", result[[paste0("Existence_",sdf)]], sep = "")
        next
      }
      path <- configInfoOrigin[configInfoOrigin$name==sdf, "path"]
      val <- configInfoOrigin[configInfoOrigin$name==sdf, "value"]

      if(is.na(val) | trimws(val) == ""){
        countE <- countE+1
        result[[paste0("emptyValue_",sdf)]] <- paste(countE,"-", " The value column for ", sdf," doesn't fill in the 'analysisSettings' sheet", sep = "")
        result$message <- paste(result$message, "\n", result[[paste0("emptyValue_",sdf)]], sep = "")
      }

      if(is.na(path) | trimws(path) == ""){
        countE <- countE+1
        result[[paste0("emptyPath_",sdf)]] <- paste(countE,"-", " The path for ", sdf," doesn't exist in the 'analysisSettings' sheet", sep = "")
        result$message <- paste(result$message, "\n", result[[paste0("emptyPath_",sdf)]], sep = "")
      } else if(!file.exists(path)){
        result$flag <- F
        countE <- countE+1
        result[[sdf]] <- paste(countE,"-",sdf," doesn't exist in the data folder", sep = "")
        result$message <- paste(result$message, "\n", result[[sdf]], sep = "")
      }
    }

    instanceidRows <- configInfoOrigin[startsWith(tolower(configInfoOrigin$name), "instanceid"),]
    instanceidValues <- instanceidRows[,"value"]
    instanceidNames <- instanceidRows[,"name"]


    #check if instanceid Values are filled
    if(sum(is.na(instanceidValues) | trimws(instanceidValues) == "") != 0){
      instanceidNames <- instanceidNames[which(is.na(instanceidValues) | trimws(instanceidValues)=="")]
      instanceidNames <- paste(instanceidNames, collapse = ", ", sep="/")
      result$flag <- F
      countE <- countE+1
      result$instanceId <- paste(countE,"- ", " Please make sure that you fill the instance id in the 'analysisSettings' sheet for ", instanceidNames, sep = "")
      result$message <- paste(result$message, "\n", result$instanceId, sep = "")
    }

    #check if the information of sampling are filled
    if (configInfoOrigin[configInfoOrigin$name == "sample_type", "value"] == "Cluster sample (type 2)") {
      weightsInfoPath <- configInfoOrigin[configInfoOrigin$name == "weights_info", "path"]
      weightsInfoValue <- configInfoOrigin[configInfoOrigin$name == "weights_info", "value"]
      variableName <- configInfoOrigin[configInfoOrigin$name == "variable_name", "value"]
      weightsVariable <- configInfoOrigin[configInfoOrigin$name == "weightsVariable", "value"]
      numberOfClusters <- configInfoOrigin[configInfoOrigin$name == "numberOfClusters", "value"]

      if(is.na(weightsInfoPath) | trimws(weightsInfoPath) == ""){
        result$flag <- F
        countE <- countE+1
        result$weightsInfoPath <- paste(countE,"-", " Please make sure that you fill the path of 'weightsInfo'", sep = "")
        result$message <- paste(result$message, "\n", result$weightsInfoPath, sep = "")
      }
      if(is.na(weightsInfoValue) | trimws(weightsInfoValue) == ""){
        result$flag <- F
        countE <- countE+1
        result$weightsInfoValue <- paste(countE,"-", " Please make sure that you fill the value of 'weightsInfo'", sep = "")
        result$message <- paste(result$message, "\n", result$weightsInfoValue, sep = "")
      }
      if(is.na(variableName) | trimws(variableName) == ""){
        result$flag <- F
        countE <- countE+1
        result$variableName <- paste(countE,"-", " Please make sure that you fill the value of 'variableName'", sep = "")
        result$message <- paste(result$message, "\n", result$variableName, sep = "")
      }
      if(is.na(weightsVariable) | trimws(weightsVariable) == ""){
        result$flag <- F
        countE <- countE+1
        result$weightsVariable <- paste(countE,"-", " Please make sure that you fill the value of 'weightsVariable'", sep = "")
        result$message <- paste(result$message, "\n", result$weightsVariable, sep = "")
      }
      if(is.na(numberOfClusters) | trimws(numberOfClusters) == ""){
        result$flag <- F
        countE <- countE+1
        result$numberOfClusters <- paste(countE,"-", " Please make sure that you fill the value of 'numberOfClusters'", sep = "")
        result$message <- paste(result$message, "\n", result$numberOfClusters, sep = "")
      }
    }else if (configInfoOrigin[configInfoOrigin$name == "sample_type", "value"] == "Stratified sample (type 3)") {
      weightsInfoPath <- configInfoOrigin[configInfoOrigin$name == "weights_info", "path"]
      weightsInfoValue <- configInfoOrigin[configInfoOrigin$name == "weights_info", "value"]
      variableName <- configInfoOrigin[configInfoOrigin$name == "variable_name", "value"]
      weightsVariable <- configInfoOrigin[configInfoOrigin$name == "weightsVariable", "value"]

      if(is.na(weightsInfoPath) | trimws(weightsInfoPath) == ""){
        result$flag <- F
        countE <- countE+1
        result$weightsInfoPath <- paste(countE,"-", " Please make sure that you fill the path of 'weightsInfo'", sep = "")
        result$message <- paste(result$message, "\n", result$weightsInfoPath, sep = "")
      }
      if(is.na(weightsInfoValue) | trimws(weightsInfoValue) == ""){
        result$flag <- F
        countE <- countE+1
        result$weightsInfoValue <- paste(countE,"-", " Please make sure that you fill the value of 'weightsInfo'", sep = "")
        result$message <- paste(result$message, "\n", result$weightsInfoValue, sep = "")
      }
      if(is.na(variableName) | trimws(variableName) == ""){
        result$flag <- F
        countE <- countE+1
        result$variableName <- paste(countE,"-", " Please make sure that you fill the value of 'variableName'", sep = "")
        result$message <- paste(result$message, "\n", result$variableName, sep = "")
      }
      if(is.na(weightsVariable) | trimws(weightsVariable) == ""){
        result$flag <- F
        countE <- countE+1
        result$weightsVariable <- paste(countE,"-", " Please make sure that you fill the value of 'weightsVariable'", sep = "")
        result$message <- paste(result$message, "\n", result$weightsVariable, sep = "")
      }
    }

    #Check if the cluster id is exist###################
    survey <- tryCatch({
      as.data.frame(read_excel(form_tmp, sheet = "survey"),
                    stringsAsFactors = FALSE) #read survey sheet from the form
    }, error = function(err) {
      data.frame( #if it doesn't exist, we need to create empty dataframe with those fields
        type = character(),
        name = character(),
        label = character(),
        labelReport = character(),
        variable = character(),
        disaggregation = character(),
        chapter = character(),
        structuralequation.risk = character(),
        structuralequation.coping = character(),
        structuralequation.resilience = character(),
        anonymise = character(),
        correlate = character(),
        clean = character(),
        cluster = character(),
        predict = character(),
        mappoint = character(),
        mappoly = character(),
        stringsAsFactors = FALSE
      )
    })

    if("cluster" %in% colnames(survey)){
      if(!"id" %in% survey$cluster){
        result$flag <- F
        countE <- countE+1
        result$clusterID <- paste(countE,"-", " The ID variable for cluster report doesn't exist in the survey sheet in the cluster column", sep = "")
        result$message <- paste(result$message, "\n", result$clusterID, sep = "")
      }
    }

    if(result$flag){
      result$message <- "All required information is existed and filled with the right value"
    }

    return(result)
  }, error = function(err) {
    print("kobo_check_project_configuration_ERROR")
    return(structure(err, class = "try-error"))
  })
}
