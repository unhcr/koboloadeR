#' @name kobo_to_xlsform
#' @rdname kobo_to_xlsform
#' @title  Generate xlsfrom skeleton from a dataframe
#'
#' @description Creates and save a xlsform skeleton from a data.frames in your data folder
#' The form.xls will be saved in the data folder of your project.
#' The generated xlsfrom will need to be manually edited to configure your analysis
#'
#' Note that this function only works with \code{data.frames}. The function
#' will throw an error for any other object types.
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#' @param n number of levels for a factor to be considered as a text
#'
#'
#' @author Edouard Legoupil
#'
#' @examples
#' data(iris)
#' str(iris)
#' kobo_to_xlsform(iris)
#'
#' @export kobo_to_xlsform
#' 
#' 
#' 

kobo_to_xlsform <- function(df,form = "form.xls",
                            n=100) {
  
  stopifnot(is.data.frame(df))
  # df <- data.df
  ## str(df)
  # n = 10
  df[sapply(df, is.labelled)] <- lapply(df[sapply(df, is.labelled)], as.factor)
  ## build survey sheet
  survey <- data.frame( type = rep(as.character(NA), ncol(df)),
                        name = names(df),
                        label = names(df),
                        chapter = rep(as.character(NA), ncol(df)),
                        disaggregation = rep(as.character(NA), ncol(df)),
                        correlate = rep(as.character(NA), ncol(df)),
                        variable = rep(as.character(NA), ncol(df)),
                        sensitive = rep(as.character(NA), ncol(df)),
                        anonymise = rep(as.character(NA), ncol(df)),
                        stringsAsFactors = FALSE)
  
  ## Fill survey type
  for(i in seq_along(df)) {
    #i <-12
    #cat(i)
    if(is.factor(df[,i])) {
      survey[i,]$type <- paste0('select_one ', as.character(names(df[i])), '_choices')
    } else {
      survey[i,]$type <- class(df[,i])[1]
    }
  }
  ## build choices sheet
  choices <- data.frame(list_name = as.character(NA),
                        name = as.character(NA),
                        label = as.character(NA),
                        order = as.integer(NA),
                        stringsAsFactors = FALSE)
  
  ## Loop around variables to build choices based on factor levels
  for(i in seq_along(df)) {
    #i <-2
    if(is.factor(df[,i])) {
      
      cat(paste0("Factor: ",i,"\n"))
      frame <- as.data.frame((levels(df[,i])))
      if (nrow(frame)!=0 & nrow(frame)<100 ){
        for(j in 1:nrow(frame)) {
          # j <- 1
          choices1 <- data.frame(list_name = as.character(NA),
                                 name = as.character(NA),
                                 label = as.character(NA),
                                 order = as.integer(NA),
                                 stringsAsFactors = FALSE)
          
          cat(paste0("Inserting level: ",j,"\n"))
          choices1[j,]$list_name <- paste0( as.character(names(df[i])), '_choices')
          choices1[j,]$name <- as.character(frame[j, ])
          choices1[j,]$label <- as.character(frame[j,])
          choices1[j,]$order <- j
          choices <- rbind(choices, choices1)
        }
        rm(choices1)
      }   else {cat("Too many choices to consider it as a factor\n")}
      ###
    }   else {cat("This is not a factor \n")}
  }

  wb <- createWorkbook(type = "xls")
  sheetname <- "survey"
  surveySheet <- createSheet(wb, sheetname)
  addDataFrame(survey, surveySheet, col.names=TRUE, row.names=FALSE)
  
  sheetname <- "choices"
  choicesSheet <- createSheet(wb, sheetName=sheetname)
  addDataFrame(choices, choicesSheet, col.names=TRUE, row.names=FALSE)
  
  
  mainDir <- kobo_getMainDirectory()
  form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
  
  if (file.exists(form_tmp)) file.remove(form_tmp)
  saveWorkbook(wb, form_tmp)
  cat("XLS form has been successfully generated")
}
NULL