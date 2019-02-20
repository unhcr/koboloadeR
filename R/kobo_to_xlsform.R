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
#' @param df The dataframe object to be processed. For groups of questions to be processed,
#' they must have been exported from an ODK plateform with dots (".") as separator. 
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
  
  ## build choices sheet
  choices <- data.frame(list_name = as.character(NA),
                        name = as.character(NA),
                        label = as.character(NA),
                        order = as.integer(NA),
                        stringsAsFactors = FALSE)
  
  ## Fill survey type
  for(i in 1:ncol(df)) {
    classcol <- class(df[,i][[1]])[1]
    
    if(classcol == "character"){
      if(ncol(df) != i){
       if(grepl(paste0(names(df[,i]),"."), names(df[,i+1])) && (is.logical (df[,i+1][[1]]) ||
                                                    (is.numeric(df[i+1][[1]]) && sum(df[,i+1]<=nrow(df))) || 
                                                    (sum(as.numeric(df[,i+1][[1]]), na.rm = TRUE) <= nrow(df))
            )){
        survey[i,]$type <- paste0('select_multiple ', as.character(names(df[i])), '_choices')
        for (j in 1:length(grep(paste0(names(df[, i]),"."), names(df)))){
          labelChoice <- stringr::str_remove(names(df[,i +j]),paste0(names(df[,i]),"."))
          choice <- c(names(df[i]),
                      labelChoice,
                      labelChoice,
                      NA)
          choices <- rbind(choices, choice)
          }
       }else if(sum(is.na(as.numeric(as.character(df[,i][[1]])))) <= nrow(df)*0.999){
          df[,i] <- as.numeric(df[,i][[1]])
          survey[i,]$type <- "decimal"
       }else if(nlevels(as.factor(df[,i][[1]])[[1]]) < n){
          vect_fact <- lapply(df[,i], factor)
          df[,i] <- vect_fact
          survey[i,]$type <- paste0('select_one ', as.character(names(df[i])), '_choices')
          
          labelChoice <- levels(df[,i][[1]])
          for (j in 1:length(labelChoice)){
            choice <- c(names(df[i]),
                        labelChoice[j],
                        labelChoice[j],
                        NA)
            choices <- rbind(choices, choice)
          }
        }else{
        survey[i,]$type <- "text"
        }
      }else{
      
        if(sum(is.na(as.numeric(as.character(df[,i][[1]])))) <= nrow(df)*0.999){
          df[,i] <- as.numeric(df[,i][[1]])
          survey[i,]$type <- "decimal"
      }else if(nlevels(lapply(df[,i], factor)[[1]]) < n){
          vect_fact <- lapply(df[,i], factor)
          df[,i] <- vect_fact
          survey[i,]$type <- paste0('select_one ', as.character(names(df[i])), '_choices')
          
          labelChoice <- levels(df[,i][[1]])
          for (j in 1:length(labelChoice)){
            choice <- c(names(df[i]),
                        labelChoice[j],
                        labelChoice[j],
                        NA)
            choices <- rbind(choices, choice)
          }
        }else if(nlevels(lapply(df[,i], factor)[[1]]) < n){
          vect_fact <- lapply(df[,i], factor)
          df[,i] <- vect_fact
          survey[i,]$type <- paste0('select_one ', as.character(names(df[i])), '_choices')
          
          
          labelChoice <- levels(df[,i][[1]])
          for (j in 1:length(labelChoice)){
            choice <- c(names(df[i]),
                        labelChoice[j],
                        labelChoice[j],
                        NA)
            choices <- rbind(choices, choice)
          }
        }else{
          survey[i,]$type <- "text"
        }
      }
    }else if(classcol == "numeric"){
      survey[i,]$type <- "decimal"
    
    }else if(classcol == "POSIXct"){
      survey[i, ]$type  <- "date"
    }else if (classcol == "logical"){
        survey[i, ] <- NA
    }else if (classcol == "factor"){
      survey[i,]$type <- paste0('select_one ', as.character(names(df[i])), '_choices')
      
      labelChoice <- levels(df[,i][[1]])
      for (j in 1:length(labelChoice)){
        choice <- c(names(df[i]),
                    labelChoice[j],
                    labelChoice[j],
                    NA)
        choices <- rbind(choices, choice)
      }
    }
  }
  survey <- survey[complete.cases(survey[1:3]), ]
  
  #create begin_groups
  
  form_str <- data.frame(col = names(df))
  n_vars <- form_str$col %>% stringr::str_split("\\.") %>% lapply(function(z) length(z)) %>% unlist() %>% max()
  
  form_str <- form_str %>%
    separate(col, into = as.character(paste0("X",1:n_vars)), sep = "\\.", fill = "right", remove = FALSE)
  
  insertRow <- function(existingDF, newrow, r) {
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
    existingDF
  }
  
  uc <- list()
  for (i in 2:ncol(form_str)){
      uc[i] <- unique(form_str[i])
      for(j in 1:length(uc[[i]])){
        if(!is.na(sum(uc[[i]][j] == form_str[,i]))){
           if(sum(uc[[i]][j] == form_str[,i]) > 1){
             q_name <- as.character(uc[[i]][j])
             begin_g <- c("begin group",q_name,q_name,rep(NA, 6))
             w_begin <- match(survey$name[grep(q_name, survey$name)[1]],survey$name)
             survey <- insertRow(survey, begin_g, w_begin)
             w_end <- match(survey$name[tail(grep(q_name, survey$name), n =1)],survey$name)+1
             end_g <- c("end group",q_name,q_name,rep(NA, 6))
             survey <- insertRow(survey, end_g, w_end)
           }
        }
      }
  }
  
  wb <- createWorkbook(type = "xls")
  sheetname <- "survey"
  surveySheet <- createSheet(wb, sheetname)
  addDataFrame(survey, surveySheet, col.names=TRUE, row.names=FALSE)
  
  sheetname <- "choices"
  choicesSheet <- createSheet(wb, sheetName=sheetname)
  addDataFrame(choices, choicesSheet, col.names=TRUE, row.names=FALSE)
  
  
  mainDir <- gsub("/inst/shiny_app", "",  getwd())
  form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
  
  if (file.exists(form_tmp)) file.remove(form_tmp)
  saveWorkbook(wb, form_tmp)
}
NULL