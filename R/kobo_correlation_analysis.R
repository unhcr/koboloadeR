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
#' kobo_correlation_analysis()
#'
#' @export kobo_correlation_analysis
#'

kobo_correlation_analysis <- function(form = "form.xls", frame, target, app="console") {
  tryCatch({
    if(nrow(frame)==0){
      return(structure("Error: the frame is empty", class = "try-error"))
    }
    if(!target %in% colnames(frame)){
      return(structure("Error: the target doesn't exist in the frame", class = "try-error"))
    }
    mainDir <- kobo_getMainDirectory()
    form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
    survey <- tryCatch({
      as.data.frame(read_excel(form_tmp, sheet = "survey"),
                    stringsAsFactors = FALSE) #read survey sheet from the form
    }, 
    error = function(err) {
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
    indicator <- tryCatch({
      as.data.frame(read_excel(form_tmp, sheet = "indicator"),stringsAsFactors = FALSE)
    },
    error = function(err) {
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
    
    survey <- survey[c("type","name","variable")]
    indicator <- indicator[c("type","fullname","variable")]
    names(indicator) <- c("type","name","variable")
    var_ind <- rbind(survey, indicator)
    
    result <- list()
    
    targetVar <- frame[,target]
    var_ind <- var_ind[!is.na(var_ind$name),]
    targetType <- var_ind[var_ind$name == "calcu_USD_Housing", "variable"]
    targetType <- ifelse(targetType=="integer","numeric", ifelse(targetType=="ordinal","factor",targetType))
    
    for (ind in colnames(frame)) {
      if(ind == target){
        next
      }
      if(sum(var_ind$name == ind)){
        next
      }
      independent <- frame[,ind]
      independentType <- var_ind[var_ind$name == ind, "variable"]
      independentType <- ifelse(independentType=="integer","numeric", ifelse(independentType=="ordinal","factor",independentType))
      if(targetType == 'numeric'){
        if(independentType == 'numeric'){
          correlation <- cor(targetVar, independent)
          if(correlation > 0.29 | correlation < -0.29){
            print(paste(targetVar, "<-------->" ,independent))
          }
        }else{
          test  <- aov(targetVar~independent)
          p_value <- summary(test)[[1]][["Pr(>F)"]][1]
          if(p_value <= 0.01 ){
            print(paste(targetVar, "<-------->" ,independent))
          }
        }
      }
      
      
    }
    
    return(result())
  }, error = function(err) {
    print("kobo_correlation_analysis_ERROR")
    return(structure(err, class = "try-error"))
  })
}