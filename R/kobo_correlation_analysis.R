#' @name kobo_correlation_analysis
#' @rdname kobo_correlation_analysis
#' @title Correlation Analysis
#'
#' @description This function apllay all correlations test to discover if there is a relation between the targe variable and other variables
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#' @param frame  The dataframe that contains the target variable and the independent variable(s)
#' @param target The name of dependent variable, the variable being tested and measured
#' @param app The place where the function has been executed, the default is the console and the second option is the shiny app
#'
#' @return A list that includes all analysis and charts
#'
#' @author Maher Daoud
#'
#' @examples
#' \dontrun{
#' kobo_correlation_analysis()
#' }
#'
#' @export kobo_correlation_analysis
#'

kobo_correlation_analysis <- function(form = "form.xls", frame, target, app = "console") {
  tryCatch({
    if (nrow(frame) == 0) {
      return(structure("Error: the frame is empty", class = "try-error"))
    }
    if (!target %in% colnames(frame)) {
      return(structure("Error: the target doesn't exist in the frame", class = "try-error"))
    }
    mainDir <- kobo_getMainDirectory()

    dico <- utils::read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "", stringsAsFactors = F)

    dico <- dico[c("type","fullname","variable","listname","labelchoice","order")]
    names(dico) <- c("type","name","variable","listname","labelchoice","order")


    dico <- dico[tolower(dico$type) == "select_one" |
                         tolower(dico$type) == "select_multiple" |
                         tolower(dico$type) == "integer" |
                         tolower(dico$type) == "decimal" |
                         tolower(dico$type) == "numeric" |
                         tolower(dico$type) == "date"
                       ,]

    result <- list()

    dico <- dico[!is.na(dico$name),]
    targetType <- dico[dico$name == target, "variable"]
    if (is.null(targetType) | length(targetType) == 0 | is.na(targetType)) {
      return(structure(paste(target,", does not exist in the survey or indicator sheet!!"), class = "try-error"))
    }


    if (targetType == "factor") {
      frame[, target] <- factor(frame[, target])
    } else if (targetType == "ordinal") {
      ln <- dico[dico$name == target, "listname"]
      ordersNames <- dico[dico$listname == ln, c("labelchoice","order")]
      ordersNames <- ordersNames[stats::complete.cases(ordersNames),]
      ordersNames <- ordersNames[order(ordersNames$order),]
      frame[, target] <- factor(frame[, target],  levels = ordersNames$labelchoice)
    } else if (targetType == "numeric") {
      frame[, target] <- as.numeric(frame[, target])
    } else if (targetType == "integer") {
      frame[, target] <- as.integer(frame[, target])
    }


    for (ind in colnames(frame)) {
      if (ind == target) {
        next
      }
      if (sum(dico$name == ind) == 0) {
        cat(paste(ind,", does not exist!"))
        next
      }
      independent <- frame[,ind]
      df <- frame[c(target, ind)]
      countBefore <- nrow(df)
      df <- df[stats::complete.cases(df),]
      countAfter <- nrow(df)
      names(df) <- c("target", "independent")

      # percCount <- countAfter / countBefore
      #
      # if(percCount < 0.5){
      #   print(paste(ind,", perc less than 0.5"))
      #   next
      # }
      independentType <- dico[dico$name == ind, "variable"]
      if (is.null(independentType) | length(independentType) == 0 | is.na(independentType)) {
        cat(paste(ind,", does not exist in the survey or indicator sheet!!"))
        next
        #return(structure(paste(ind,", does not exist in the survey or indicator sheet!!"), class = "try-error"))
      }
      if (independentType == "factor") {
        frame[, ind] <- factor(frame[, ind])
      } else if (independentType == "ordinal") {
        ln <- dico[dico$name == ind, "listname"]
        ordersNames <- dico[dico$listname == ln, c("labelchoice","order")]
        ordersNames <- ordersNames[stats::complete.cases(ordersNames),]
        ordersNames <- ordersNames[order(ordersNames$order),]
        frame[, ind] <- factor(frame[, ind],  levels = ordersNames$labelchoice)
      } else if (independentType == "numeric") {
        frame[, ind] <- as.numeric(frame[, ind])
      } else if (independentType == "integer") {
        frame[, ind] <- as.integer(frame[, ind])
      }
    }

    return(result)
  }, error = function(err) {
    print("kobo_correlation_analysis_ERROR")
    return(structure(err, class = "try-error"))
  })
}
