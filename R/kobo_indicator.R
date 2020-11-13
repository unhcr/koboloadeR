#' @name kobo_indicator
#' @rdname kobo_indicator
#' @title  Import & perform the indicator calculation from the XLS form
#'
#' @description  Add additional variables based on the data analysis plan to the data frame
#' @param mainDir Path to the project's working directory: mainly for shiny app
#'
#' @return A file with all elements to get your data & form.
#'
#' @author Elliott Messeiller
#'
#' @export kobo_indicator
#'
#' @examples
#' \dontrun{
#' kobo_indicator()
#' }
#'


kobo_indicator <- function(mainDir = '') {
  if (mainDir == '') {
    mainDir <- getwd()
  }
  source(paste0(mainDir, "/code/0-config.R"), local = TRUE)
  data <- readxl::read_excel(path.to.data, sheet = sheet)


  if (analysis_plan == "y") {
    # Getting the analysis plan
    selectdf <- readxl::read_excel(paste0(mainDir, "/data/", form) , sheet = 'analysis_plan')
    selectdf <- data.frame(selectdf, stringsAsFactors = FALSE)

    calculation <- data.frame(selectdf["calculation"])

    # Creating columns to match dico format
    selectdf$type <- "decimal"
    selectdf$listname <- NA
    selectdf$qlevel <- NA
    selectdf$ordinal <- NA
    selectdf$formpart <- "questions"
    selectdf$fullname <- ""
    selectdf$labelchoice <- ""
    selectdf$weight <- NA



    # Renaming columns to match dictionnary format
    names(selectdf)[names(selectdf) == "group"] <- "qgroup"
    names(selectdf)[names(selectdf) == "name"] <- "name"
    names(selectdf)[names(selectdf) == "label"] <- "label"
    names(selectdf)[names(selectdf) == "disaggregation"] <- "disaggregation"
    names(selectdf)[names(selectdf) == "Ordinal"] <- "ordinal"
    names(selectdf)[names(selectdf) == "correlate"] <- "correlate"
    names(selectdf)[names(selectdf) == "weight"] <- "weight"


    #Columns in the same order
    selectdf <-
      selectdf[, c(
        "type",
        "name",
        "fullname",
        "label",
        "disaggregation",
        "correlate",
        "listname",
        "qlevel",
        "qgroup",
        "labelchoice",
        "ordinal",
        "formpart",
        "weight"
      )]



    for (i in 1:nrow(selectdf)) {
      # Filling "fullname" column"
      var_name <- paste0(selectdf[i, "qgroup"], '.', selectdf[i, "name"])
      selectdf[i, "fullname"] <- var_name

      # Getting calculation
      calc <- as.character(calculation[i, "calculation"])
      #splitting variables and operators
      calc_split <- data.frame(strsplit(calc, ",")[[1]], stringsAsFactors = FALSE)
      operators <- c("+", "-", "/", "*", "(", ")")

      #Matching variables with dico and renaming
      for (j in 1:nrow(calc_split)) {
        split_temp <- as.character(glue::trim(calc_split[j, ]))

        if (split_temp %in% operators) {
          calc_split[j, ] <- glue::trim(calc_split[j, ])

        } else{
          calc_split[j, ] <- as.character(dico[dico$name == split_temp, c("fullname"), ])
        }
      }
      #adding the questions to the dico
      dico <- rbind(dico, selectdf)

      # Calculating values
      ### data frame to keep all the results
      res_tab <- data.frame(c(1:nrow(calc_split)), stringsAsFactors = FALSE)

      #Going through all observations
      for (k in 1:nrow(data)) {
        #Looping calculation
        for (l in 1:nrow(calc_split)) {
          #Skipping operators
          split_col <- calc_split[l, ]
          if (split_col %in% operators) {
            res_tab[l, ] <- split_col
            # Fetching values
          } else{
            res_tab[l, ] <- data[k, split_col]
          }
        }
        #Resetting result
        result <- ""
        #Concatenate results
        for (m in 1:nrow(res_tab)) {
          result <- paste0(result, res_tab[m, ])
        }
        #Calculating
        result <- eval(parse(text = result))
        #Placing value in main data frame as new column
        data[k, var_name] <- result

      }
    }
    # Rewritting dico file
    write.csv(
      dico,
      paste0(mainDir, "/data/dico_", form, ".csv"),
      row.names = FALSE,
      na = ""
    )

    # Rewritting data file
    # Coherce data to a clean dataframe
    data <- data.frame(data)

    #Rewrite data with new variables
    wb <- openxlsx::loadWorkbook(path.to.data)
    sheets <- openxlsx::names(wb)
    openxlsx::removeWorksheet(wb, "cleaned_data")
    
    openxlsx::createSheet(wb, "cleaned_data")
    openxlsx::writeData(wb, "cleaned_data", data)
    openxlsx::saveWorkbook(wb, path.to.data)
  }


}
NULL
