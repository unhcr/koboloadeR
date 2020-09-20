#' @name kobo_check_analysis_plan
#' @rdname kobo_check_analysis_plan
#' @title Check Analysis Plan
#'
#' @description Check if the user setup the analysis plan in the right way.
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#'
#'
#' @return The return will be a list that contains a list that checks all elements of the analysis plan and message of confirmation
#'
#' @author Maher Daoud
#'
#'
#' @examples
#' \dontrun{
#' kobo_check_analysis_plan("myform.xls")
#' }
#'
#' @export kobo_check_analysis_plan
#'

kobo_check_analysis_plan <- function(form = "form.xls") {
  tryCatch({
    result <- list()
    result$flag <- T
    result$message <- ""
    countE <- 0

    mainDir <- kobo_getMainDirectory()
    form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")

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

    survey <- survey[!tolower(survey$name) %in% c("begin_repeat","begin repeat","begin-repeat",
                                         "end_repeat", "end repeat", "end-repeat",
                                         "begin_group","begin group","begin-group",
                                         "end_group","end group","end-group"
                                         ),]

    survey <- survey[!is.na(survey$chapter) & !is.na(survey$name),]
    if(nrow(survey) == 0){
      result$flag <- F
      countE <- countE+1
      result$chapterSur <- paste(countE,"-"," Please make sure that you have at least one chapter before loading data and generating reports (survey sheet)", sep = "")
      result$message <- paste(result$message, "\n", result$chapterSur,sep = "")
      return(result)
    }


    if(!"labelReport"%in%colnames(survey)){
      result$flag <- F
      countE <- countE+1
      result$labelReportNotExisSur <- paste(countE,"-"," Survey sheet does not contain labelReport column, you must fist labelReport before Data Processing", sep = "")
      result$message <- paste(result$message, "\n", result$labelReportNotExisSur,sep = "")
    } else {
      temp <- survey[is.na(survey$labelReport),]
      if (nrow(temp) > 0) {
        result$flag <- F
        countE <- countE + 1
        result$labelReportMissedSur <- paste(countE,"-"," Please make sure that you fill all labelReport with a proper label before loading data and generating reports (survey sheet)", sep = "")
        result$message <- paste(result$message, "\n", result$labelReportMissedSur,sep = "")
      }

      if (mean(stringr::str_length(survey$labelReport) <= 85, na.rm = T) != 1) {
        result$flag <- F
        countE <- countE + 1
        temp <- survey[stringr::str_length(survey$labelReport) > 85,"name"]
        temp <- paste(temp, sep = " ", collapse = " ,")
        result$labelReportLengthSur <- paste(countE,"-"," Please make sure that all labelReport length are less than 85 character in survey sheet where name equal: ",temp , sep = "")
        result$message <- paste(result$message, "\n", result$labelReportLengthSur,sep = "")
      }
    }

    choices <- tryCatch({
      as.data.frame(read_excel(form_tmp, sheet = "choices"),
                    stringsAsFactors = FALSE) #read survey sheet from the form
    }, error = function(err) {
      data.frame( #if it doesn't exist, we need to create empty dataframe with those fields
        list_name = character(),
        name = character(),
        label = character(),
        labelReport = character(),
        order = character(),
        stringsAsFactors = FALSE,
        check.names = F
      )
    })
    choices <- choices[!is.na(choices$name) & !is.na(choices$list_name),]


    if (!"labelReport" %in% colnames(choices)) {
      result$flag <- F
      countE <- countE + 1
      result$labelReportNotExisCho <- paste(countE,"-"," Choices sheet does not contain labelReport column, you must fist labelReport before Data Processing", sep = "")
      result$message <- paste(result$message, "\n", result$labelReportNotExisCho,sep = "")
    } else {
      if (mean(stringr::str_length(choices$labelReport) <= 85, na.rm = TRUE) != 1){
        result$flag <- F
        countE <- countE+1
        temp <- choices[stringr::str_length(choices$labelReport) > 85,"name"]
        temp <- paste(temp, sep = " ", collapse = " ,")
        result$labelReportLengthCho <- paste(countE,"-"," Please make sure that all labelReport length are less than 85 character in choices sheet where name equal: ",temp , sep = "")
        result$message <- paste(result$message, "\n", result$labelReportLengthCho,sep = "")
      }
    }



    temp <- survey[!is.na(survey[,"variable"]),]
    temp <- temp[temp[,"variable"]=="ordinal",]
    temp <- temp[startsWith(tolower(temp[,"type"]), "select_one"),]

    varOfOrder <- sapply(temp[,"type"], function(x) {
      strsplit(x," ")[[1]][2]
    }, simplify = TRUE, USE.NAMES = FALSE)

    orderVar <- choices[choices[,"list_name"] %in% varOfOrder, "order"]
    temp <- orderVar[is.na(orderVar)]
    if(length(temp)>0){
      result$flag <- F
      countE <- countE+1
      result$ordinalVariablesCho <- paste(countE,"-"," Please make sure that you fill all ordinal variables with orders in order column in choices sheet", sep = "")
      result$message <- paste(result$message, "\n", result$ordinalVariablesCho,sep = "")
    }

    indicator <- tryCatch({
      as.data.frame(read_excel(form_tmp, sheet = "indicator"),stringsAsFactors = FALSE)
    }, error = function(err) {
      data.frame(
        type = character(),
        fullname = character(),
        labelReport = character(),
        hintReport = character(),
        chapter = character(),
        disaggregation = character(),
        correlate = character(),
        sensitive = character(),
        anonymise = character(),
        cluster = character(),
        predict = character(),
        variable = character(),
        mappoint = character(),
        mappoly = character(),
        structuralequation = character(),
        frame = character(),
        listname = character(),
        calculation = character(),
        stringsAsFactors = FALSE
      )
    })

    indicator <- indicator[!is.na(indicator$fullname),]

    if (nrow(indicator) > 0) {
      if (!"labelReport" %in% colnames(indicator)) {
        result$flag <- F
        countE <- countE + 1
        result$labelReportNotExisInd <- paste(countE,"-"," Indicator sheet does not contain labelReport column, you must fist labelReport before Data Processing", sep = "")
        result$message <- paste(result$message, "\n", result$labelReportNotExisInd,sep = "")
      } else {
        if(mean(stringr::str_length(indicator$labelReport) <= 85, na.rm = TRUE) != 1) {
          result$flag <- F
          countE <- countE+1
          temp <- indicator[stringr::str_length(indicator$labelReport) > 85,"fullname"]
          temp <- paste(temp, sep = " ", collapse = " ,")
          result$labelReportLengthInd <- paste(countE,"-"," Please make sure that all labelReport length are less than 85 character in indicator sheet where fullname equal: ",temp , sep = "")
          result$message <- paste(result$message, "\n", result$labelReportLengthInd,sep = "")
        }
        temp <- indicator[is.na(indicator$labelReport),"labelReport"]
        if(length(temp)>0){
          result$flag <- F
          countE <- countE+1
          result$labelInd <- paste(countE,"-"," Please make sure that you fill all cells of labelReport column in the indicator sheet", sep = "")
          result$message <- paste(result$message, "\n", result$labelInd,sep = "")
        }
      }
      temp <- indicator[is.na(indicator$type) | !indicator$type %in% c("integer","numeric","select_one"),"type"]
      if(length(temp)>0){
        result$flag <- F
        countE <- countE+1
        result$typeInd <- paste(countE,"-"," Please make sure that you fill all cells of type column in the indicator sheet", sep = "")
        result$message <- paste(result$message, "\n", result$typeInd,sep = "")
      }

      temp <- indicator[is.na(indicator$chapter),"chapter"]
      if(length(temp)>0){
        result$flag <- F
        countE <- countE+1
        result$chapterInd <- paste(countE,"-"," Please make sure that you fill all cells of chapter column in the indicator sheet", sep = "")
        result$message <- paste(result$message, "\n", result$chapterInd,sep = "")
      }

      temp <- indicator[is.na(indicator$frame),"frame"]
      if(length(temp)>0){
        result$flag <- F
        countE <- countE+1
        result$frameInd <- paste(countE,"-"," Please make sure that you fill all cells of frame column in the indicator sheet", sep = "")
        result$message <- paste(result$message, "\n", result$frameInd,sep = "")
      }

      temp <- indicator[is.na(indicator$calculation),"calculation"]
      if(length(temp)>0){
        result$flag <- F
        countE <- countE+1
        result$calculationInd <- paste(countE,"-"," Please make sure that you fill all cells of calculation column in the indicator sheet", sep = "")
        result$message <- paste(result$message, "\n", result$calculationInd,sep = "")
      }

      temp <- indicator[indicator$type=="select_one","listname"]
      temp <- temp[is.na(temp)]
      if(length(temp)>0){
        result$flag <- F
        countE <- countE+1
        result$listnameInd <- paste(countE,"-"," Please make sure that you fill all cells of listname column where type is 'select_one' in the indicator sheet", sep = "")
        result$message <- paste(result$message, "\n", result$listnameInd,sep = "")
      }
    }
    return(result)

  }, error = function(err) {
    print("kobo_check_analysis_plan_ERROR")
    return(structure(err, class = "try-error"))
  })
}
