#' @name kobo_edit_form
#' @rdname kobo_edit_form
#' @title  Edit XLS form with shiny app for configuration
#'
#' @description  This function used to change the data of sheets in the xlsform and apply all required styles for each sheet
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#'
#' @param survey Dataframe that represent the data of survey sheet in the xlsform
#' @param choices Dataframe that represent the data of choices sheet in the xlsform
#' @param indicator Dataframe that represent the data of indicator sheet in the xlsform
#' @param settings Dataframe that represent the data of settings sheet in the xlsform
#' @param analysisSettings Dataframe that represent the data of analysisSettings sheet in the xlsform
#'
#' @return No return, this function edit the original XLSform directly
#'
#' @author Maher Daoud
#'
#'
#' @examples
#' \dontrun{
#' kobo_edit_form("form.xlsx")
#' }
#'
#' @export kobo_edit_form
#'

kobo_edit_form <- function(form = "form.xlsx", survey = NULL, choices = NULL, indicator = NULL, settings = NULL, analysisSettings=NULL) {
  tryCatch({
    wb <- openxlsx::createWorkbook()
    mainDir <- kobo_getMainDirectory()
    form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")

    #################################### survey sheet ######################################
    if(is.null(survey)){
      survey <- tryCatch({
        as.data.frame(readxl::read_excel(form_tmp, sheet = "survey"),
                      stringsAsFactors = FALSE) #read survey sheet from the form
      }, error = function(err) {
        data.frame( #if it doesn't exist, we need to create empty dataframe with those fields
          type = character(),
          name = character(),
          label = character(),
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
    }

    if(!is.null(survey)){
      survey[is.na(survey)] <-  ""
      sheetname <- "survey"
      if(!is.null(openxlsx::getSheets(wb)[[sheetname]]))
        openxlsx::removeSheet(wb, sheetname)
      surveySheet <- openxlsx::createSheet(wb, sheetname) #create survey sheet in wb
      openxlsx::addDataFrame(survey, surveySheet, col.names=TRUE, row.names=FALSE) #add survey dataframe in the survey sheet
    }


    #################################### choices sheet ######################################
    if(is.null(choices)){
      choices <- tryCatch({
        as.data.frame(readxl::read_excel(form_tmp, sheet = "choices"),
                      stringsAsFactors = FALSE) #read survey sheet from the form
      }, error = function(err) {
        data.frame( #if it doesn't exist, we need to create empty dataframe with those fields
          list_name = character(),
          name = character(),
          label = character(),
          order = character(),
          stringsAsFactors = FALSE
        )
      })
    }
    if(!is.null(choices)){
      sheetname <- "choices"
      if(!is.null(openxlsx::getSheets(wb)[[sheetname]]))
        openxlsx::removeSheet(wb, sheetname)
      choicesSheet <- openxlsx::createSheet(wb, sheetName=sheetname)
      openxlsx::addDataFrame(choices, choicesSheet, col.names=TRUE, row.names=FALSE)
    }

    #################################### indicator sheet ######################################
    if(is.null(indicator)){
      indicator <- tryCatch({
        as.data.frame(readxl::read_excel(form_tmp, sheet = "indicator"),stringsAsFactors = FALSE)
      }, error = function(err) {
        data.frame(
          type = character(),
          fullname = character(),
          labelReport = character(),
          hintReport = character(),
          frame = character(),
          listname = character(),
          calculation = character(),
          chapter = character(),
          disaggregation = character(),
          correlate = character(),
          anonymise = character(),
          cluster = character(),
          predict = character(),
          variable = character(),
          mappoint = character(),
          mappoly = character(),
          structuralequation.risk = character(),
          structuralequation.coping = character(),
          structuralequation.resilience = character(),
          stringsAsFactors = FALSE
        )
      })
    }
    if(!is.null(indicator)){
      sheetname <- "indicator"
      if(!is.null(openxlsx::getSheets(wb)[[sheetname]]))
        openxlsx::removeSheet(wb, sheetname)
      indicatorSheet <- openxlsx::createSheet(wb, sheetName=sheetname)
      openxlsx::addDataFrame(indicator, indicatorSheet, col.names=TRUE, row.names=FALSE)
    }



    #################################### settings sheet ######################################
    if(is.null(settings)){
      settings <- tryCatch({
        as.data.frame(readxl::read_excel(form_tmp, sheet = "settings"),
                      stringsAsFactors = FALSE)
      }, error = function(err) {
        data.frame(
          form_title = character(),
          form_id = character(),
          default_language = character(),
          stringsAsFactors = FALSE
        )
      })
    }
    if(!is.null(settings)){
      sheetname <- "settings"

      if(!is.null(openxlsx::getSheets(wb)[[sheetname]]))
        openxlsx::removeSheet(wb, sheetname)
      settingsSheet <- openxlsx::createSheet(wb, sheetName=sheetname) #create sheet with settings name
      openxlsx::addDataFrame(settings, settingsSheet, col.names=TRUE, row.names=FALSE) #add settings data frame to this sheet
    }


    #################################### settings sheet ######################################
    if(is.null(analysisSettings)){
      analysisSettings <- tryCatch({
        as.data.frame(readxl::read_excel(form_tmp, sheet = "analysisSettings"),
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
    }
    if(!is.null(analysisSettings)){
      sheetname <- "analysisSettings"

      if(!is.null(openxlsx::getSheets(wb)[[sheetname]]))
        openxlsx::removeSheet(wb, sheetname)
      settingsSheet <- openxlsx::createSheet(wb, sheetName=sheetname) #create sheet with analysisSettings name
      openxlsx::addDataFrame(analysisSettings, settingsSheet, col.names=TRUE, row.names=FALSE) #add analysisSettings data frame to this sheet
    }
    if (file.exists(form_tmp)) file.remove(form_tmp)
    openxlsx::saveWorkbook(wb, form_tmp)


  }, error = function(err) {
    print("kobo_load_data_ERROR")
    return(structure(err, class = "try-error"))
  })
}
NULL

