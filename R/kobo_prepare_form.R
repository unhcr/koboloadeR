#' @name kobo_prepare_form
#' @rdname kobo_prepare_form
#' @title  Prepare XLS form
#'
#' @description  Prepare XLSform by adding chapter, disaggregation, correlate, variable, anonymise, structuralequation,
#' clean, cluster, predict, mappoint, mappoly in case if those fields are not exist; the function will create dummy column for each one.
#' Also, coloring all rows that have type equal to "begin group", "end group", "begin repeat" or "end repeat".
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#'
#'
#' @return No return, this function edit the original XLSform directly
#'
#' @author Maher Daoud
#'
#'
#' @examples
#' \dontrun{
#' kobo_prepare_form("myform.xls")
#' }
#'
#' @export kobo_prepare_form
#'

kobo_prepare_form <- function(form = "form.xlsx") {
  tryCatch({
    #we need to define the below dataframe for Styling purposes.
    #So, If we add a filters for sheet we have to set the range for addAutoFilter() function
    #EX: addAutoFilter(sheet1, "A1:P1") --> add a filter on the 1rd row, columns A:P
    #this dataframe will help us to find 'P' based on number of columns in the sheet
    #if number of columns equal to 6 then the range will be A1:dfref[6,] -- A1:F1
    dfref <- data.frame(
      key = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256),
      val = c('A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1', 'H1', 'I1', 'J1', 'K1', 'L1', 'M1', 'N1', 'O1', 'P1', 'Q1', 'R1', 'S1', 'T1', 'U1', 'V1', 'W1', 'X1', 'Y1', 'Z1', 'AA1', 'AB1', 'AC1', 'AD1', 'AE1', 'AF1', 'AG1', 'AH1', 'AI1', 'AJ1', 'AK1', 'AL1', 'AM1', 'AN1', 'AO1', 'AP1', 'AQ1', 'AR1', 'AS1', 'AT1', 'AU1', 'AV1', 'AW1', 'AX1', 'AY1', 'AZ1', 'BA1', 'BB1', 'BC1', 'BD1', 'BE1', 'BF1', 'BG1', 'BH1', 'BI1', 'BJ1', 'BK1', 'BL1', 'BM1', 'BN1', 'BO1', 'BP1', 'BQ1', 'BR1', 'BS1', 'BT1', 'BU1', 'BV1', 'BW1', 'BX1', 'BY1', 'BZ1', 'CA1', 'CB1', 'CC1', 'CD1', 'CE1', 'CF1', 'CG1', 'CH1', 'CI1', 'CJ1', 'CK1', 'CL1', 'CM1', 'CN1', 'CO1', 'CP1', 'CQ1', 'CR1', 'CS1', 'CT1', 'CU1', 'CV1', 'CW1', 'CX1', 'CY1', 'CZ1', 'DA1', 'DB1', 'DC1', 'DD1', 'DE1', 'DF1', 'DG1', 'DH1', 'DI1', 'DJ1', 'DK1', 'DL1', 'DM1', 'DN1', 'DO1', 'DP1', 'DQ1', 'DR1', 'DS1', 'DT1', 'DU1', 'DV1', 'DW1', 'DX1', 'DY1', 'DZ1', 'EA1', 'EB1', 'EC1', 'ED1', 'EE1', 'EF1', 'EG1', 'EH1', 'EI1', 'EJ1', 'EK1', 'EL1', 'EM1', 'EN1', 'EO1', 'EP1', 'EQ1', 'ER1', 'ES1', 'ET1', 'EU1', 'EV1', 'EW1', 'EX1', 'EY1', 'EZ1', 'FA1', 'FB1', 'FC1', 'FD1', 'FE1', 'FF1', 'FG1', 'FH1', 'FI1', 'FJ1', 'FK1', 'FL1', 'FM1', 'FN1', 'FO1', 'FP1', 'FQ1', 'FR1', 'FS1', 'FT1', 'FU1', 'FV1', 'FW1', 'FX1', 'FY1', 'FZ1', 'GA1', 'GB1', 'GC1', 'GD1', 'GE1', 'GF1', 'GG1', 'GH1', 'GI1', 'GJ1', 'GK1', 'GL1', 'GM1', 'GN1', 'GO1', 'GP1', 'GQ1', 'GR1', 'GS1', 'GT1', 'GU1', 'GV1', 'GW1', 'GX1', 'GY1', 'GZ1', 'HA1', 'HB1', 'HC1', 'HD1', 'HE1', 'HF1', 'HG1', 'HH1', 'HI1', 'HJ1', 'HK1', 'HL1', 'HM1', 'HN1', 'HO1', 'HP1', 'HQ1', 'HR1', 'HS1', 'HT1', 'HU1', 'HV1', 'HW1', 'HX1', 'HY1', 'HZ1', 'IA1', 'IB1', 'IC1', 'ID1', 'IE1', 'IF1', 'IG1', 'IH1', 'II1', 'IJ1', 'IK1', 'IL1', 'IM1', 'IN1', 'IO1', 'IP1', 'IQ1', 'IR1', 'IS1', 'IT1', 'IU1', 'IV1')
    )

    wb <- openxlsx::createWorkbook()

    cat("\n Your form should be placed within the `data` folder. \n \n")

    mainDir <- kobo_getMainDirectory()

    form_tmp <- paste(mainDir, "data-raw", form, sep = "/", collapse = "/")

    # Survey sheet ######################################
    survey <- tryCatch({
      as.data.frame(readxl::read_excel(form_tmp, sheet = "survey"),
                    stringsAsFactors = FALSE) #read survey sheet from the form
    }, error = function(err) {
      data.frame( #if it doesn't exist, we need to create empty dataframe with those fields
        type = character(),
        name = character(),
        label = character(),
        labelReport = character(),
        hint = character(),
        hintReport = character(),
        relevant = character(),
        required = character(),
        constraint = character(),
        repeat_count = character(),
        calculation = character(),
        choice_filter = character(),
        read_only = character(),
        default = character(),

        ## Additionnal variable
        report = character(),
        chapter = character(),
        variable = character(),
        disaggregation = character(),
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
        stringsAsFactors = FALSE,
        check.names = F
      )
    })


    cat("################################# \n")
    cat("### Checking now survey sheet ## \n")
    cat("################################# \n")


    namesOfSur <- c("type", "name" , "label")

    ## Rename the variable label
    names(survey)[tolower(names(survey)) == "label::english"] <- "label"
    names(survey)[tolower(names(survey)) == "hint::english"] <- "hint"
    names(survey)[tolower(names(survey)) == "label::english (en)"] <- "label"
    names(survey)[tolower(names(survey)) == "hint::english (en)"] <- "hint"

    if (sum(namesOfSur %in% colnames(survey)) != length(namesOfSur)) {
      return(structure('Please make sure the survey sheet haas at minima the following columns: "type", "name" , "label" or "label::English" , "hint" or "hint::English"', class = "try-error"))
    }

    if ("hint" %in% colnames(survey)) {
      cat(" Good: You have a column `hint` in your survey worksheet.\n");
    } else {
      cat(" No column `hint` in your survey worksheet. Creating a dummy one for the moment ...\n");
      survey$hint <- ""
    }
    namesOfSur <- c(namesOfSur,"hint")

    if ("required" %in% colnames(survey)) {
      cat(" Good: You have a column `required` in your survey worksheet.\n");
    } else {
      cat(" No column `chapter` in your survey worksheet. Creating a dummy one for the moment ...\n");
      survey$required <- ""
    }
    namesOfSur <- c(namesOfSur,"required")

    if ("relevant" %in% colnames(survey)) {
      cat(" Good: You have a column `relevant` in your survey worksheet.\n");
    } else {
      cat(" No column `relevant` in your survey worksheet. Creating a dummy one for the moment ...\n");
      survey$relevant <- ""
    }
    namesOfSur <- c(namesOfSur,"relevant")

    if ("constraint" %in% colnames(survey)) {
      cat(" Good: You have a column `constraint` in your survey worksheet.\n");
    } else {
      cat(" No column `constraint` in your survey worksheet. Creating a dummy one for the moment ...\n");
      survey$constraint <- ""
    }
    namesOfSur <- c(namesOfSur,"constraint")

    if ("calculation" %in% colnames(survey)) {
      cat(" Good: You have a column `calculation` in your survey worksheet.\n");
    } else {
      cat(" No column `calculation` in your survey worksheet. Creating a dummy one for the moment ...\n");
      survey$calculation <- ""
    }
    namesOfSur <- c(namesOfSur,"calculation")



    if ( "repeat_count" %in% colnames(survey)) {
        cat(" Good: You have a column `repeat_count` in your survey worksheet.\n");
      } else {
        cat(" No column `repeat_count` in your survey worksheet. Creating a dummy one for the moment ...\n");
        survey$repeat_count <- ""
      }
    namesOfSur <- c(namesOfSur, "repeat_count")

    cat("Checking now for additional information within your xlsform.
        Note that you can insert them in the xls and re-run the function! \n \n ")
    ### Add column if not present
    if ("labelReport" %in% colnames(survey)) {
      cat(" Good: You have a column `labelReport` in your survey worksheet.\n");
    } else {
      cat(" No column `labelReport` in your survey worksheet. Creating a dummy one for the moment based on the initial one - trimmed to 80 characters (see readme file). ...\n");
      survey["labelReport"] <- substr(survey[,"label"],1,80)
    }
    namesOfSur <- c(namesOfSur,"labelReport")


    if ("hintReport" %in% colnames(survey)) {
      cat(" Good: You have a column `hintReport` in your survey worksheet.\n");
    } else {
      cat(" No column `hintReport` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
      survey["hintReport"] <- substr(survey[,"hint"],1,300)
    }
    namesOfSur <- c(namesOfSur,"hintReport")



    if ("report" %in% colnames(survey)) {
      cat(" Good: You have a column `report` in your survey worksheet. This will be used to breakdown the generated report\n");
    } else {
      cat(" No column `report` in your survey worksheet. Creating a dummy one for the moment ...\n");
      # survey$report <- character()
      survey$report <- ""
    }
    namesOfSur <- c(namesOfSur,"report")

    if ("chapter" %in% colnames(survey)) {
      cat(" Good: You have a column `chapter` in your survey worksheet. This will be used to breakdown the generated report\n");
    } else {
      cat(" No column `chapter` in your survey worksheet. Creating a dummy one for the moment ...\n");
     # survey$chapter <- character()
      survey$chapter <- ""
    }
    namesOfSur <- c(namesOfSur,"chapter")

    if ("anonymise" %in% colnames(survey)) {
      cat(" Good: You have a column `anonymise` in your survey worksheet. This will be used to anonymise the dataset.\n");
    } else {
      cat(" No column `anonymise` in your survey worksheet. Creating a dummy one for the moment filled as `non-anonymised`. Other options to record are `Remove`, `Reference`, `Mask`, `Generalise` (see readme file) ...\n");
      survey$anonymise <- "default-non-anonymised"
    }
    namesOfSur <- c(namesOfSur,"anonymise")

    if ("correlate" %in% colnames(survey)) {
      cat(" Good: You have a column `correlate` in your survey worksheet. This will be used to define the variables that should be checked for correlation between each others.\n");
    } else {
      cat(" No column `correlate` in your survey worksheet. Creating a dummy one for the moment...\n");
      #survey$correlate <- character()
      survey$correlate <- ""
    }
    namesOfSur <- c(namesOfSur,"correlate")

    if ("variable" %in% colnames(survey)) {
      cat(" Good: You have a column `variable` in your survey worksheet. This will be used to flag ordinal variable.\n");
    } else {
      cat(" No column `variable` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
      #survey$variable <- character()
      survey$variable <- ""
    }
    namesOfSur <- c(namesOfSur,"variable")

    if ("disaggregation" %in% colnames(survey)) {
      cat(" Good: You have a column `disaggregation` in your survey worksheet.\n");
    } else {
      cat(" No column `disaggregation` in your survey worksheet. Creating a dummy one for the moment...\n");
      #survey$disaggregation <- character()
      survey$disaggregation <- ""
    }
    namesOfSur <- c(namesOfSur,"disaggregation")

    ## Adding 	clean cluster	predict
    if ("clean" %in% colnames(survey)) {
      cat(" Good: You have a column `clean` in your survey worksheet. This will be used to flag variables that shoudl be clean with kobo_clean function.\n");
    } else {
      cat(" No column `clean` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
      survey$clean <- "no"
    }
    namesOfSur <- c(namesOfSur,"clean")

    if ("cluster" %in% colnames(survey)) {
      cat(" Good: You have a column `cluster` in your survey worksheet. This will be used to flag variables to be used for clustering exploration.\n");
    } else {
      cat(" No column `cluster` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
      #survey$cluster <- character()
      survey$cluster <- ""
    }
    namesOfSur <- c(namesOfSur,"cluster")

    if ("predict" %in% colnames(survey)) {
      cat(" Good: You have a column `predict` in your survey worksheet. This will be used to flag variables to be used for clustering exploration.\n");
    } else {
      cat(" No column `predict` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
      survey$predict <- ""
    }
    namesOfSur <- c(namesOfSur,"predict")

    if ("mappoint" %in% colnames(survey)) {
      cat(" Good: You have a column `mappoint` in your survey worksheet. This will be used to flag variables to be used for clustering exploration.\n");
    } else {
      cat(" No column `mappoint` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
      survey$mappoint <- ""
    }
    namesOfSur <- c(namesOfSur,"mappoint")

    if ("mappoly" %in% colnames(survey)) {
      cat(" Good: You have a column `mappoly` in your survey worksheet. This will be used to flag variables to be used for clustering exploration.\n");
    } else {
      cat(" No column `mappoly` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
      survey$mappoly <- ""
    }
    namesOfSur <- c(namesOfSur,"mappoly")

    ### order in user friendly    way

    namesOfSur <- c( "type",
     "name",
     "labelReport",
     "hintReport",
     "report",
     "chapter",
     "anonymise",
     "disaggregation",
     "correlate",
     "variable",
     "clean"     ,
     "cluster"  ,
     "predict" ,
     "mappoint",
     "mappoly",
     "label",
      "hint",
      "required",
      "relevant",
      "constraint",
      "calculation",
     "repeat_count" )

    ## Avoid columns without names
    survey <- survey[ ,namesOfSur]


    ## need to delete empty rows from the form
    survey <- as.data.frame(survey[!is.na(survey$type), ])
    survey[is.na(survey)] <-  ""

    #------------ create styles for header and cells ------------#
    headerSt <- 
      openxlsx::createStyle(
        textDecoration = "bold", fontColour = "white", fontSize = 13, 
        fgFill = "grey50",
        border = "TopBottom", borderColour = "grey80", borderStyle = "thin")
    cs1 <- 
      openxlsx::createStyle(
        textDecoration = "bold", fontColour = "black",
        fgFill = "orange",
        border = "TopBottom", borderColour = "orange", borderStyle = "thin")
    cs2 <- 
      openxlsx::createStyle(
        textDecoration = "bold", fontColour = "white",
        fgFill = "skyblue",
        border = "TopBottom", borderColour = "skyblue", borderStyle = "thin")
    
    #### Styling part for survey sheet
    sheetname <- "survey"
    
    openxlsx::addWorksheet(wb, sheetname)
    openxlsx::writeData(wb, sheetname, survey, withFilter = TRUE)
    
    openxlsx::setColWidths(wb, sheetname, cols = 1:ncol(survey), widths = "auto")
    openxlsx::setColWidths(wb, sheetname, cols = 2:3, widths = 30)
    
    all.cols <- 1:ncol(survey)
    hdr.rows <- 1
    group.rows <- which(stringr::str_detect(survey$type, "group"))+1
    repeat.rows <- which(stringr::str_detect(survey$type, "repeat"))+1
    
    openxlsx::addStyle(wb, sheetname, headerSt, hdr.rows, all.cols, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheetname, cs1, group.rows, all.cols, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheetname, cs2, repeat.rows, all.cols, gridExpand = TRUE)
    
    cat("\n********************Survey sheet, ready to be used*********************\n \n")


    # Choices sheet ######################################
    choices <- tryCatch({
      as.data.frame(readxl::read_excel(form_tmp, sheet = "choices"),
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
    cat("################################# \n")
    cat("### Checking now choices sheet ## \n")
    cat("################################# \n")
    ## Rename the variable label
    names(choices)[tolower(names(choices)) == "label::english"] <- "label"
    names(choices)[tolower(names(choices)) == "label::english (en)"] <- "label"

    namesOfCho <- c("list_name", "name", "label")

    if (sum(namesOfCho %in% colnames(choices)) != length(namesOfCho)) {
      return(structure('Please make sure the choices sheet has at minima the following columns:
                        "list_name", "name" , "label"', class = "try-error"))
    }

    ### add column if not present
    if ("order" %in% colnames(choices)) {
      cat(" Good: You have a column `order` in your choices worksheet.\n");
    } else {
      cat(" No column `order` in your choices worksheet. Creating a dummy one for the moment...\n");
      choices$order <- ""
    }
    if ("labelReport" %in% colnames(choices)) {
      cat(" Good: You have a column `labelReport` in your choices worksheet.\n");
    } else {
      cat(" No column `labelReport` in your choices worksheet. Creating a dummy one for the moment based on the initial one - trimmed to 50 characters...\n");
      choices["labelReport"] <- substr(choices[,"label"],1,50)
    }

    if ("weight" %in% colnames(choices))
    {
      cat("  Good: You have a column `weight` in your `choices` worksheet.\n");
    } else
    {cat(" No column `weight` in your `choices` worksheet. Creating a dummy one for the moment...\n");
      choices$weight <- ""}

    if ("recategorise" %in% colnames(choices))
    {
      cat("  Good: You have a column `recategorise` in your `choices` worksheet.\n");
    } else
    {cat("  No column `recategorise` in your `choices` worksheet. Creating a dummy one for the moment...\n");
      choices$recategorise <- ""}

    if ("score" %in% colnames(choices))
    {
      cat("  Good: You have a column `score` in your `choices` worksheet.\n");
    } else
    {cat("  No column `score` in your `choices` worksheet. Creating a dummy one for the moment...\n");
      choices$score <- ""}

    namesOfCho <- c("list_name", "name", "label", "labelReport", "order", "weight","score","recategorise")
    choices <- choices[ ,namesOfCho]

    sheetname <- "choices"
    
    openxlsx::addWorksheet(wb, sheetname)
    openxlsx::writeData(wb, sheetname, choices, withFilter = TRUE)
    
    openxlsx::setColWidths(wb, sheetname, cols = 1:ncol(choices), widths = "auto")
    openxlsx::setColWidths(wb, sheetname, cols = 2:3, widths = 30)
    
    openxlsx::addStyle(wb, sheetname, headerSt, hdr.rows, 1:ncol(choices), gridExpand = TRUE)
    
    cat("\n********************Choices sheet, ready to be used*********************\n \n")



    ### Settings sheet ######################################


      cat("\n\n################################### \n")
      cat("### Checking now settings sheet ## \n")
      cat("################################### \n\n")


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

      sheetname <- "settings"
      
      openxlsx::addWorksheet(wb, sheetname)
      openxlsx::writeData(wb, sheetname, settings, withFilter = TRUE)
      
      openxlsx::setColWidths(wb, sheetname, cols = 1:ncol(settings), widths = "auto")
      
      openxlsx::addStyle(wb, sheetname, headerSt, hdr.rows, 1:ncol(settings), gridExpand = TRUE)
      
      cat("\n******************** Settings sheet, ready to be used*********************\n \n")







    ### Analysis settings sheet ######################################


    cat("\n\n######################################### \n")
    cat("### Checking now analysis settings  sheet ## \n")
    cat("############################################ \n\n")

    analysisSettings <- tryCatch({
      as.data.frame(readxl::read_excel(form_tmp, sheet = "analysisSettings"),
                    stringsAsFactors = FALSE)
    }, error = function(err) {
      data.frame(
        name = character(),
        label = character(),
        options = character(),
        value = character(),
        path = character(),
        stringsAsFactors = FALSE
      )
    })

    ## DDI info


    if (!"titl" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "titl",
                                           label = "Title of the study",
                                           options = "Free Text",
                                           value = "Refugee Survey in Country x",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }


    if (!"abstract" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "abstract",
                                           label = "Abstract",
                                           options = "Free Text",
                                           value = "Blablablablablabla",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }


    if (!"disclaimer" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "disclaimer",
                                           label = "Rights & Disclaimer",
                                           options = "Free Text - adjust if necessary",
                                           value = "UNHCR does not warrant in any way the accuracy of the information and data contained in the datasets and shall not be held liable for any loss caused by reliance on the accuracy or reliability thereof.",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }


    if (!"Country" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "Country",
                                           label = "Country where the study took place",
                                           options = "Please use Country ISO code Alpha 3",
                                           value = "JOR",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    if (!"geogCover" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "geogCover",
                                           label = "Geographic Coverage for the study within the country",
                                           options = "Free Text",
                                           value = "Blablablablablabla",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    ##    Kind of Data
    if (!"dataKind" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "dataKind",
                                           label = "Kind of Data",
                                           options = "Sample survey data [ssd] or Census/enumeration data [cen]",
                                           value = "ssd",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }


    # Unit of Analysis
    if (!"AnalysisUnit" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "AnalysisUnit",
                                           label = "Describes the entity being analyzed in the study or in the variable.",
                                           options = "HousingUnit (household Survey) or GeographicUnit (Key informant Interview or Observation)",
                                           value = "HousingUnit",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }


    if (!"ModeOfCollection" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "ModeOfCollection",
                                           label = "The procedure, technique, or mode of inquiry used to attain the data.",
                                           options = "Interview.FaceToFace.CAPI or Interview.Telephone.CATI or SelfAdministeredQuestionnaire.FixedForm.WebBased or FocusGroup.FaceToFace or Observation",
                                           value = "Interview.FaceToFace.CAPI",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

   ## Universe

    if (!"universe" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "universe",
                                           label = "Description of the study Universe: The group of persons or other elements that are the object of research and to which any analytic results refer.",
                                           options = "Free Text",
                                           value = "Refugee Survey in Country x",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    if (!"universeyes" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "universeyes",
                                           label = "Do you have a file describing the universe that can be joined to the survey (for instance registration data)?",
                                           options = "yes or no",
                                           value = "no",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    if (!"universefile" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "universefile",
                                           label = "Name of the csv file with universe data",
                                           options = "",
                                           value = "universe.csv",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    if (!"universeid" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "universeid",
                                           label = "Name of the variable within universe to do the join with the survey",
                                           options = "",
                                           value = "progres.id",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    if (!"universesurveyid" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "universesurveyid",
                                           label = "Name of the variable within survey to do the join with the universe",
                                           options = "",
                                           value = "progres.id",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    ### Sampling

    if (!"sample_type" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "sample_type",
                                           label = "Sample type of the project",
                                           options = "1. No sampling (type 1) 2. Cluster sample (type 2) 3. Stratified sample (type 3)",
                                           value = "No sampling (type 1)",
                                           path = NA,
                                           stringsAsFactors = FALSE)
                                )
    }

    if (!"sampProc" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "sampProc",
                                           label = "Description of the Sampling Procedure in the context of the study",
                                           options = "Free Text",
                                           value = "Blablablablablabla",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }


    if (!"weight" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "weight",
                                           label = "Description of the generation of the final weight - for instance usage of post-stratification and other calibration",
                                           options = "Free Text",
                                           value = "Blablablablablabla",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    if (!"variable_name" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "variable_name",
                                           label = "If there is sampling, select the name of cluster variable that will be used to join the weight file with the main file, please make sure the name of this variable exists in both files",
                                           options = NA,
                                           value = NA,
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    if (!"weights_info" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "weights_info",
                                           label = "If there is sampling,  weights file that will be used in Stratified or cluster sample",
                                           options = NA,
                                           value = NA,
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    if (!"weightsVariable" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "weightsVariable",
                                           label = "If there is sampling, the variable that contains the weights in weights file",
                                           options = NA,
                                           value = NA,
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    if (!"numberOfClusters" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "numberOfClusters",
                                           label = "If the sample type is cluster sample, enter number of clusters",
                                           options = NA,
                                           value = NA,
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }

    ### Cleaning
    if (!"cleanOps" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "cleanOps",
                                           label = "Data Editing and Cleaning Operation: Description of the cleaning procedure",
                                           options = "Free Text",
                                           value = "Blablablablablabla",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }
    if (!"cleaning_log" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "cleaning_log",
                                           label = "cleaning log plan for the project",
                                           options = "1. Yes 2. No, 3. csv filename",
                                           value = "No",
                                           path = NA,
                                           stringsAsFactors = FALSE)
      )
    }


    ## Reference to data file

    if (!"MainDataFrame" %in% analysisSettings$name) {
      analysisSettings <- rbind(analysisSettings,
                                data.frame(name = "MainDataFrame",
                                           label = "Name and the path of MainDataFrame",
                                           options = NA,
                                           value = "MainDataFrame",
                                           path = paste0(mainDir,"/data/MainDataFrame.csv"),
                                           stringsAsFactors = FALSE)
      )
    }

    levelsOfDF <- kobo_get_dataframes_levels(form)
    levelsOfDF <- levelsOfDF[levelsOfDF$name != "MainDataFrame",]

    if (nrow(levelsOfDF) != 0) {
      for (dbr in levelsOfDF$name) {
        child <- levelsOfDF[levelsOfDF$name == dbr, "name"]
        parent <- levelsOfDF[levelsOfDF$name == dbr, "parent"]

        if (!dbr %in% analysisSettings$name) {
          analysisSettings <- rbind(analysisSettings,
                                    data.frame(name = dbr,
                                               label = paste("Name and the path of", dbr),
                                               options = NA,
                                               value = paste0( dbr,".csv"),
                                               path = paste0(mainDir,"/data/", dbr,".csv"),
                                               stringsAsFactors = FALSE)
          )
        }

        if (!paste0("instanceID_", child, "_", parent) %in% analysisSettings$name) {
          analysisSettings <- rbind(analysisSettings,
                                    data.frame(name = paste0("instanceID_", child, "_", parent),
                                               label = paste0("The instanceID between the child (", child, ") and the parent (", parent, ")" ),
                                               options = NA,
                                               value =  "instanceID",
                                               path = NA,
                                               stringsAsFactors = FALSE)
          )
        }

        if (!paste0("instanceID_", parent, "_", child) %in% analysisSettings$name) {
          analysisSettings <- rbind(analysisSettings,
                                    data.frame(name = paste0("instanceID_", parent, "_", child),
                                               label = paste0("The instanceID between the parent (", parent, ") and the child (", child, ")" ),
                                               options = NA,
                                               value = "instanceID",
                                               path = NA,
                                               stringsAsFactors = FALSE)
          )
        }

        ## Geographic file for maps
        
        
        if (!"geofile" %in% analysisSettings$name) {
          analysisSettings <- rbind(analysisSettings,
                                    data.frame(name = "universefile",
                                               label = "Name of the geojson file with geo data",
                                               options = "",
                                               value = "geofile.geojson",
                                               path = NA,
                                               stringsAsFactors = FALSE)
          )
        }
        
        if (!"geofileid" %in% analysisSettings$name) {
          analysisSettings <- rbind(analysisSettings,
                                    data.frame(name = "geofileid",
                                               label = "Name of the variable within geofile to do the join with the survey",
                                               options = "",
                                               value = "geoid",
                                               path = NA,
                                               stringsAsFactors = FALSE)
          )
        }
        
        if (!"geosurveyid" %in% analysisSettings$name) {
          analysisSettings <- rbind(analysisSettings,
                                    data.frame(name = "geosurveyid",
                                               label = "Name of the variable within survey to do the join with the geofile",
                                               options = "",
                                               value = "geoid",
                                               path = NA,
                                               stringsAsFactors = FALSE)
          )
        }        
        

      }
    }

    sheetname <- "analysisSettings"
    
    openxlsx::addWorksheet(wb, sheetname)
    openxlsx::writeData(wb, sheetname, analysisSettings, withFilter = TRUE)
    
    openxlsx::setColWidths(wb, sheetname, cols = 1:ncol(analysisSettings), widths = "auto")
    
    openxlsx::addStyle(wb, sheetname, headerSt, hdr.rows, 1:ncol(analysisSettings), gridExpand = TRUE)
    
    
    cat("\n******************** Project Analysis Settings sheet, ready to be used*********************\n \n")




    ## Indicator sheet ######################################


    cat("################################### \n")
    cat("### Checking now indicator sheet ## \n")
    cat("################################### \n")


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
        report = character(),
        chapter = character(),
        disaggregation = character(),
        correlate = character(),
        anonymise = character(),
        cluster = character(),
        predict = character(),
        variable = character(),
        mappoint = character(),
        mappoly = character(),
        stringsAsFactors = FALSE
      )
     }
    )
    if ("type" %in% colnames(indicator)) {
      cat(" Good: You have a column `type` in your indicator worksheet.\n");
    } else {
      cat(" No column `type` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$type <- ""
    }
    if ("fullname" %in% colnames(indicator)) {
      cat(" Good: You have a column `fullname` in your indicator worksheet.\n");
    } else {
      cat(" No column `fullname` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$fullname <- ""
    }
    if ("frame" %in% colnames(indicator)) {
      cat(" Good: You have a column `frame` in your indicator worksheet.\n");
    } else {
      cat(" No column `frame` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$frame <- ""
    }
    if ("labelReport" %in% colnames(indicator)) {
      cat(" Good: You have a column `labelReport` in your indicator worksheet.\n");
    } else {
      cat(" No column `labelReport` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$labelReport <- ""
    }
    if ("hintReport" %in% colnames(indicator)) {
      cat(" Good: You have a column `hintReport` in your indicator worksheet.\n");
    } else {
      cat(" No column `hintReport` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$hintReport <- ""
    }
    if ("listname" %in% colnames(indicator)) {
      cat(" Good: You have a column `listname` in your indicator worksheet.\n");
    } else {
      cat(" No column `listname` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$listname <- ""
    }
    if ("calculation" %in% colnames(indicator)) {
      cat(" Good: You have a column `calculation` in your indicator worksheet.\n");
    } else {
      cat(" No column `calculation` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$calculation <- ""
    }
    if ("report" %in% colnames(indicator)) {
      cat(" Good: You have a column `report` in your indicator worksheet.\n");
    } else {
      cat(" No column `report` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$report <- ""
    }
    if ("chapter" %in% colnames(indicator)) {
      cat(" Good: You have a column `chapter` in your indicator worksheet.\n");
    } else {
      cat(" No column `chapter` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$chapter <- ""
    }
    if ("disaggregation" %in% colnames(indicator)) {
      cat(" Good: You have a column `disaggregation` in your indicator worksheet.\n");
    } else {
      cat(" No column `disaggregation` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$disaggregation <- ""
    }
    if ("correlate" %in% colnames(indicator)) {
      cat(" Good: You have a column `correlate` in your indicator worksheet.\n");
    } else {
      cat(" No column `correlate` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$correlate <- ""
    }
    if ("anonymise" %in% colnames(indicator)) {
      cat(" Good: You have a column `anonymise` in your indicator worksheet.\n");
    } else {
      cat(" No column `anonymise` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$anonymise <- ""
    }
    if ("cluster" %in% colnames(indicator)) {
      cat(" Good: You have a column `cluster` in your indicator worksheet.\n");
    } else {
      cat(" No column `cluster` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$cluster <- ""
    }
    if ("predict" %in% colnames(indicator)) {
      cat(" Good: You have a column `predict` in your indicator worksheet.\n");
    } else {
      cat(" No column `predict` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$predict <- ""
    }
    if ("variable" %in% colnames(indicator)) {
      cat(" Good: You have a column `variable` in your indicator worksheet.\n");
    } else {
      cat(" No column `variable` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$variable <- ""
    }
    if ("mappoint" %in% colnames(indicator)) {
      cat(" Good: You have a column `mappoint` in your indicator worksheet.\n");
    } else {
      cat(" No column `mappoint` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$mappoint <- ""
    }
    if ("mappoly" %in% colnames(indicator)) {
      cat(" Good: You have a column `mappoly` in your indicator worksheet.\n");
    } else {
      cat(" No column `mappoly` in your indicator worksheet. Creating a dummy one for the moment...\n");
      indicator$mappoly <- ""
    }

    indicator <- indicator[ ,c("type","fullname","labelReport", "hintReport",
                               "frame", "listname","calculation",
                               "report","chapter", "disaggregation", "correlate",
                               "anonymise", "cluster", "predict", "variable", "mappoint", "mappoly")]

    sheetname <- "indicator"
    
    openxlsx::addWorksheet(wb, sheetname)
    openxlsx::writeData(wb, sheetname, indicator, withFilter = TRUE)
    
    openxlsx::setColWidths(wb, sheetname, cols = 1:ncol(indicator), widths = "auto")
    
    openxlsx::addStyle(wb, sheetname, headerSt, hdr.rows, 1:ncol(indicator), gridExpand = TRUE)
    
    
    cat("\n******************** Indicator sheet, ready to be used *********************\n \n")

    cat("############################### \n")
    cat("### Checking now RIDL sheets ## \n")
    cat("############################### \n")
  
    ridl_schema <- jsonlite::fromJSON("https://raw.githubusercontent.com/okfn/ckanext-unhcr/master/ckanext/unhcr/schemas/dataset.json")
    
    ridl_dataset_fields <- ridl_schema$dataset_fields # %>% tibble::as_tibble()
    ridl_resource_fields <- ridl_schema$resource_fields # %>% tibble::as_tibble()
    
    ridl_choices <- 
      ridl_dataset_fields %>% 
      dplyr::mutate(choices = purrr::map(choices, as.data.frame)) %>% 
      dplyr::select(field_name, choices) %>% 
      tidyr::unnest(choices) %>% 
      dplyr::select(list_name = field_name, name = value, label) %>% 
      as.data.frame()
    
    ridl_metadata <- 
      ridl_dataset_fields %>% 
      dplyr::transmute(
        type = 
          dplyr::case_when(
            preset == "multiple_select" ~ stringr::str_c("select_multiple", field_name, sep = " "),
            field_name %in% ridl_choices$list_name ~ stringr::str_c("select_one", field_name, sep = " "),
            TRUE ~ "text"),
        name = field_name,
        label,
        required,
        hint = dplyr::if_else(!is.na(help_text), help_text, form_placeholder),
        value = "")
    
    sheetname <- "ridl-metadata"
    if (sheetname %in% readxl::excel_sheets(form_tmp)) {
      ridl_metadata <- 
        ridl_metadata %>% 
        dplyr::select(-value) %>% 
        dplyr::left_join(readxl::read_excel(form_tmp, sheet = sheetname) %>% dplyr::select(name, value), by = "name")
    }
    
    sheetname <- "ridl-metadata"
    
    openxlsx::addWorksheet(wb, sheetname)
    openxlsx::writeData(wb, sheetname, ridl_metadata, withFilter = TRUE)
    
    openxlsx::setColWidths(wb, sheetname, cols = 1:ncol(ridl_metadata), widths = "auto")
    
    openxlsx::addStyle(wb, sheetname, headerSt, hdr.rows, 1:ncol(ridl_metadata), gridExpand = TRUE)
    
    sheetname <- "ridl-choices"
    
    openxlsx::addWorksheet(wb, sheetname)
    openxlsx::writeData(wb, sheetname, ridl_choices, withFilter = TRUE)
    
    openxlsx::setColWidths(wb, sheetname, cols = 1:ncol(ridl_choices), widths = "auto")
    
    openxlsx::addStyle(wb, sheetname, headerSt, hdr.rows, 1:ncol(ridl_choices), gridExpand = TRUE)
    
    cat("\n******************** RIDL sheets, ready to be used *********************\n \n")
    
    if (file.exists(form_tmp)) file.remove(form_tmp)
    openxlsx::saveWorkbook(wb, form_tmp)
    
    
    cat("\n******************** The XLSFORM has now been extended to include your analysis plan *********************\n \n")
    
  }, error = function(err) {
    print("There was an error in the xlsform preparation step!!! \n\n")
    return(structure(err, class = "try-error"))
  })

}
NULL

