#' @name kobo_rename_xlsform_dataframes
#' @rdname kobo_rename_xlsform_dataframes
#' @title  Rename xlsform and all dataframes
#'
#' @description Rename xlsform under data file to form.xls and all dataframes to the
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#' @param app The place where the function has been executed, the default is the console and the second option is the shiny app
#'
#' @return no return. only if there is error.
#'
#' @author Maher Daoud
#'
#' @examples
#' kobo_rename_xlsform_dataframes()
#'
#' @export kobo_rename_xlsform_dataframes
#'
#'


kobo_rename_xlsform_dataframes <- function(form = "form.xls", app="console") {
  tryCatch({
    mainDir <- kobo_getMainDirectory()

    if(paste0(mainDir,"/data/",form) != paste0(mainDir,"/data/form.xls")){
        file.copy(paste0(mainDir,"/data/",form), paste0(mainDir,"/data/form.xls"))
    }
    dataFramesNames <- kobo_get_begin_repeat(form)
    dataFramesNames <- c("MainDataFrame",dataFramesNames$names)

    configInfo <- kobo_get_config(form)
    configInfo <- configInfo[configInfo$name %in% dataFramesNames, ]
    if(nrow(configInfo)==0){
      return(structure("Please make sure that you fill the settings sheet with the required information about dataframes", class = "try-error"))
    }
    if(nrow(configInfo) != length(dataFramesNames)){
      return(structure("Please make sure that you fill the settings sheet with the required information about dataframes", class = "try-error"))
    }
    configInfo <- configInfo[c("name","path")]
    if(sum(is.na(configInfo)) > 0){
      return(structure("Please make sure that you fill path column in the settings sheet with the required information", class = "try-error"))
    }
    for (i in 1:nrow(configInfo)) {
      print(configInfo[i,"path"])
      print(paste0(mainDir,"/data/",configInfo[i,"name"],".csv"))
      if(configInfo[i,"path"] != paste0(mainDir,"/data/",configInfo[i,"name"],".csv")){
        file.copy(configInfo[i,"path"], paste0(mainDir,"/data/",configInfo[i,"name"],".csv"))
      }
    }
    return(T)
  }, error = function(err) {
    print("kobo_rename_xlsform_dataframes_ERROR")
    return(structure(err, class = "try-error"))
  })
}
