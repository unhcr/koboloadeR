#' @name kobo_edit_form
#' @rdname kobo_edit_form
#' @title  Edit XLS form
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
#'
#' @return No return, this function edit the original XLSform directly
#'
#' @author Maher Daoud
#'
#' @examples
#' kobo_edit_form()
#'
#' @examples
#' \dontrun{
#' kobo_edit_form("myform.xls")
#' }
#'
#' @export kobo_edit_form
#' 

kobo_edit_form <- function(form = "form.xls", survey = NULL, choices = NULL, indicator = NULL, settings = NULL) {
  
  #we need to define the below dataframe for Styling purposes.
  #So, If we add a filters for sheet we have to set the range for addAutoFilter() function
  #EX: addAutoFilter(sheet1, "A1:P1") --> add a filter on the 1rd row, columns A:P
  #this dataframe will help us to find 'P' based on number of columns in the sheet
  #if number of columns equal to 6 then the range will be A1:dfref[6,] -- A1:F1
  dfref <- data.frame( 
    key = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256),
    val = c('A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1', 'H1', 'I1', 'J1', 'K1', 'L1', 'M1', 'N1', 'O1', 'P1', 'Q1', 'R1', 'S1', 'T1', 'U1', 'V1', 'W1', 'X1', 'Y1', 'Z1', 'AA1', 'AB1', 'AC1', 'AD1', 'AE1', 'AF1', 'AG1', 'AH1', 'AI1', 'AJ1', 'AK1', 'AL1', 'AM1', 'AN1', 'AO1', 'AP1', 'AQ1', 'AR1', 'AS1', 'AT1', 'AU1', 'AV1', 'AW1', 'AX1', 'AY1', 'AZ1', 'BA1', 'BB1', 'BC1', 'BD1', 'BE1', 'BF1', 'BG1', 'BH1', 'BI1', 'BJ1', 'BK1', 'BL1', 'BM1', 'BN1', 'BO1', 'BP1', 'BQ1', 'BR1', 'BS1', 'BT1', 'BU1', 'BV1', 'BW1', 'BX1', 'BY1', 'BZ1', 'CA1', 'CB1', 'CC1', 'CD1', 'CE1', 'CF1', 'CG1', 'CH1', 'CI1', 'CJ1', 'CK1', 'CL1', 'CM1', 'CN1', 'CO1', 'CP1', 'CQ1', 'CR1', 'CS1', 'CT1', 'CU1', 'CV1', 'CW1', 'CX1', 'CY1', 'CZ1', 'DA1', 'DB1', 'DC1', 'DD1', 'DE1', 'DF1', 'DG1', 'DH1', 'DI1', 'DJ1', 'DK1', 'DL1', 'DM1', 'DN1', 'DO1', 'DP1', 'DQ1', 'DR1', 'DS1', 'DT1', 'DU1', 'DV1', 'DW1', 'DX1', 'DY1', 'DZ1', 'EA1', 'EB1', 'EC1', 'ED1', 'EE1', 'EF1', 'EG1', 'EH1', 'EI1', 'EJ1', 'EK1', 'EL1', 'EM1', 'EN1', 'EO1', 'EP1', 'EQ1', 'ER1', 'ES1', 'ET1', 'EU1', 'EV1', 'EW1', 'EX1', 'EY1', 'EZ1', 'FA1', 'FB1', 'FC1', 'FD1', 'FE1', 'FF1', 'FG1', 'FH1', 'FI1', 'FJ1', 'FK1', 'FL1', 'FM1', 'FN1', 'FO1', 'FP1', 'FQ1', 'FR1', 'FS1', 'FT1', 'FU1', 'FV1', 'FW1', 'FX1', 'FY1', 'FZ1', 'GA1', 'GB1', 'GC1', 'GD1', 'GE1', 'GF1', 'GG1', 'GH1', 'GI1', 'GJ1', 'GK1', 'GL1', 'GM1', 'GN1', 'GO1', 'GP1', 'GQ1', 'GR1', 'GS1', 'GT1', 'GU1', 'GV1', 'GW1', 'GX1', 'GY1', 'GZ1', 'HA1', 'HB1', 'HC1', 'HD1', 'HE1', 'HF1', 'HG1', 'HH1', 'HI1', 'HJ1', 'HK1', 'HL1', 'HM1', 'HN1', 'HO1', 'HP1', 'HQ1', 'HR1', 'HS1', 'HT1', 'HU1', 'HV1', 'HW1', 'HX1', 'HY1', 'HZ1', 'IA1', 'IB1', 'IC1', 'ID1', 'IE1', 'IF1', 'IG1', 'IH1', 'II1', 'IJ1', 'IK1', 'IL1', 'IM1', 'IN1', 'IO1', 'IP1', 'IQ1', 'IR1', 'IS1', 'IT1', 'IU1', 'IV1')
  )
  
  mainDir <- gsub("/code/shiny_app", "",  getwd()) 
  mainDir <- gsub("/inst/shiny_app", "",  mainDir) 
  
  form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
  
  wb <- xlsx::createWorkbook(type = "xls") #create xls workbook
  
  if(is.null(survey)){
    survey <- tryCatch({
      as.data.frame(read_excel(form_tmp, sheet = "survey"),
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
    if(!is.null(xlsx::getSheets(wb)[[sheetname]]))
      xlsx::removeSheet(wb, sheetname)
    surveySheet <- xlsx::createSheet(wb, sheetname) #create survey sheet in wb
    xlsx::addDataFrame(survey, surveySheet, col.names=TRUE, row.names=FALSE) #add survey dataframe in the survey sheet
    #------------ add filters for each column in the survey sheet ------------#
    from <- "A1"
    to <- dfref[dfref$key==length(survey),"val"] #detect the upper bound of 'range' parameter
    xlsx::addAutoFilter(surveySheet, paste(from,":",to,sep = "")) #add filters for survey sheet
    #------------ Width of column in the survey sheet ------------#
    xlsx::autoSizeColumn(surveySheet, 1:length(survey))
    xlsx::setColumnWidth(surveySheet, 2:3, 30) #for name and label we set the width to 30, because those columns usually contain a long string
    #------------ create styles for header and cells ------------#
    headerSt <- xlsx::CellStyle(wb) +
      xlsx::Font(wb, isBold=TRUE, isItalic=FALSE, color="white", heightInPoints=13) + 
      xlsx::Fill(backgroundColor="GREY_50_PERCENT",foregroundColor="GREY_50_PERCENT",
                 pattern="SOLID_FOREGROUND")  + 
      xlsx::Border(color="GREY_80_PERCENT", position=c("TOP", "BOTTOM"), "BORDER_THIN")  
    cs1 <- CellStyle(wb) +
      xlsx::Font(wb, isBold=TRUE, isItalic=FALSE, color="black") + 
      xlsx::Fill(backgroundColor="SKY_BLUE", foregroundColor="SKY_BLUE",
                 pattern="SOLID_FOREGROUND")   + 
      xlsx::Border(color="SKY_BLUE", position=c("TOP", "BOTTOM"), "BORDER_THIN")  
    cs2 <- CellStyle(wb) +
      xlsx::Font(wb, isBold=TRUE, isItalic=FALSE, color="white") + 
      xlsx::Fill(backgroundColor="orange", foregroundColor="orange",
                 pattern="SOLID_FOREGROUND")    + 
      xlsx::Border(color="orange", position=c("TOP", "BOTTOM"), "BORDER_THIN")
    rows <- xlsx::getRows(surveySheet) # get rows of survey Sheet 
    cells <- xlsx::getCells(rows) # get cells of survey Sheet 
    
    values <- lapply(cells, xlsx::getCellValue) # get all values of cells
    
    #------------ apply cs1 style on cells with value equals to 'begin group' or 'end group' ------------#
    rowIndex <- c()
    for (i in names(values)) {
      if( is.na(values[[i]]) ){
        values[[i]] = " "
      }
      if(values[[i]]=="begin group" || values[[i]]=="end group" || values[[i]]=="end_group" || values[[i]]=="begin_group" ){
        temp <- as.numeric(names(values[i]))
        temp <- floor(temp)
        rowIndex <- c(rowIndex,temp )
      }
    }
    highlight <- NULL
    for (i in names(values)) {
      temp <- as.numeric(names(values[i]))
      temp <- floor(temp)
      if ( temp %in% rowIndex ) {
        highlight <- c(highlight, i)
      }    
    }
    lapply(names(cells[highlight]),
           function(ii) xlsx::setCellStyle(cells[[ii]], cs1))
    
    #------------ apply cs2 style on cells with value equals to 'begin repeat' or 'end repeat' ------------#
    rowIndex <- c()
    for (i in names(values)) {
      if( is.na(values[[i]]) ){
        values[[i]] = " "
      }
      if(values[[i]]=="begin repeat" || values[[i]]=="end repeat" || values[[i]]=="end_repeat" || values[[i]]=="begin_repeat"){
        temp <- as.numeric(names(values[i]))
        temp <- floor(temp)
        rowIndex <- c(rowIndex,temp )
      }
    }
    highlight <- NULL
    for (i in names(values)) {
      temp <- as.numeric(names(values[i]))
      temp <- floor(temp)
      if ( temp %in% rowIndex ) {
        highlight <- c(highlight, i)
      }    
    }
    lapply(names(cells[highlight]),
           function(ii) xlsx::setCellStyle(cells[[ii]], cs2))
    
    #------------ apply headerSt style on header cells ------------#
    highlight <- paste("1",c(1:length(survey)),sep = ".")
    lapply(names(cells[highlight]),
           function(ii) xlsx::setCellStyle(cells[[ii]], headerSt))
  }
  
  #################################### choices sheet ######################################
  if(is.null(choices)){
    choices <- tryCatch({
      as.data.frame(read_excel(form_tmp, sheet = "choices"),
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
    if(!is.null(xlsx::getSheets(wb)[[sheetname]]))
      xlsx::removeSheet(wb, sheetname)
    choicesSheet <- xlsx::createSheet(wb, sheetName=sheetname)
    xlsx::addDataFrame(choices, choicesSheet, col.names=TRUE, row.names=FALSE)
    from <- "A1"
    to <- dfref[dfref$key==length(choices),"val"]
    xlsx::addAutoFilter(choicesSheet, paste(from,":",to,sep = ""))
    rows <- xlsx::getRows(choicesSheet)     # get rows
    cells <- xlsx::getCells(rows)
    
    headerSt <- xlsx::CellStyle(wb) +
      xlsx::Font(wb, isBold=TRUE, isItalic=FALSE, color="white", heightInPoints=13) + 
      xlsx::Fill(backgroundColor="GREY_50_PERCENT",foregroundColor="GREY_50_PERCENT",
                 pattern="SOLID_FOREGROUND")  + 
      xlsx::Border(color="GREY_80_PERCENT", position=c("TOP", "BOTTOM"), "BORDER_THIN")  
    highlight <- paste("1",c(1:length(choices)),sep = ".")
    lapply(names(cells[highlight]),
           function(ii) xlsx::setCellStyle(cells[[ii]], headerSt))
    xlsx::autoSizeColumn(choicesSheet, 1:length(survey))
    xlsx::setColumnWidth(choicesSheet, 2:3, 30)
  }
  #################################### indicator sheet ######################################
  if(is.null(indicator)){
    indicator <- tryCatch({
      as.data.frame(read_excel(form_tmp, sheet = "indicator"),stringsAsFactors = FALSE)
    }, error = function(err) {
      data.frame(
        type = character(),
        fullname = character(),
        label = character(),
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
  }
  if(!is.null(indicator)){
    sheetname <- "indicator"
    if(!is.null(xlsx::getSheets(wb)[[sheetname]]))
      xlsx::removeSheet(wb, sheetname)
    indicatorSheet <- xlsx::createSheet(wb, sheetName=sheetname)
    xlsx::addDataFrame(indicator, indicatorSheet, col.names=TRUE, row.names=FALSE)
    from <- "A1"
    to <- dfref[dfref$key==length(indicator),"val"]
    xlsx::addAutoFilter(indicatorSheet, paste(from,":",to,sep = ""))
    rows <- xlsx::getRows(indicatorSheet)     # get rows
    cells <- xlsx::getCells(rows)
    headerSt <- xlsx::CellStyle(wb) +
      xlsx::Font(wb, isBold=TRUE, isItalic=FALSE, color="white", heightInPoints=13) + 
      xlsx::Fill(backgroundColor="GREY_50_PERCENT",foregroundColor="GREY_50_PERCENT",
                 pattern="SOLID_FOREGROUND")  + 
      xlsx::Border(color="GREY_80_PERCENT", position=c("TOP", "BOTTOM"), "BORDER_THIN")  
    highlight <- paste("1",c(1:length(indicator)),sep = ".")
    lapply(names(cells[highlight]),
           function(ii) xlsx::setCellStyle(cells[[ii]], headerSt))
    xlsx::autoSizeColumn(indicatorSheet, 1:length(survey))
  }
  
  #################################### settings sheet ######################################
  if(is.null(settings)){
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
  }
  if(!is.null(settings)){
    sheetname <- "settings"
    if(!is.null(xlsx::getSheets(wb)[[sheetname]]))
      xlsx::removeSheet(wb, sheetname)
    settingsSheet <- xlsx::createSheet(wb, sheetName=sheetname) #create sheet with settings name
    xlsx::addDataFrame(settings, settingsSheet, col.names=TRUE, row.names=FALSE) #add settings data frame to this sheet
    from <- "A1"
    to <- dfref[dfref$key==length(settings),"val"]
    xlsx::addAutoFilter(settingsSheet, paste(from,":",to,sep = ""))
    rows <- xlsx::getRows(settingsSheet)     # get rows
    cells <- xlsx::getCells(rows)
    headerSt <- xlsx::CellStyle(wb) +
      xlsx::Font(wb, isBold=TRUE, isItalic=FALSE, color="white", heightInPoints=13) + 
      xlsx::Fill(backgroundColor="GREY_50_PERCENT",foregroundColor="GREY_50_PERCENT",
                 pattern="SOLID_FOREGROUND")  + 
      xlsx::Border(color="GREY_80_PERCENT", position=c("TOP", "BOTTOM"), "BORDER_THIN")  
    highlight <- paste("1",c(1:length(settings)),sep = ".")
    lapply(names(cells[highlight]),
           function(ii) xlsx::setCellStyle(cells[[ii]], headerSt))
    xlsx::autoSizeColumn(settingsSheet, 1:length(survey))
  }
  
  if (file.exists(form_tmp)) file.remove(form_tmp)
  xlsx::saveWorkbook(wb, form_tmp)
  
  
}
NULL

