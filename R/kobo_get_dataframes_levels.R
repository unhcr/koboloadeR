#' @name kobo_get_dataframes_levels
#' @rdname kobo_get_dataframes_levels
#' @title  Dataframes Levels
#'
#' @description  Produce a dataframe that represents levels and parents for the main dataframe and all sub datasets.
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#'
#'
#' @return A "data.frame" contains levels and parents for the main dataframe and all sub datasets.
#'
#' @author Maher Daoud
#'
#' @examples
#' kobo_get_dataframes_levels()
#'
#' @examples
#' \dontrun{
#' kobo_get_dataframes_levels("myform.xls")
#' }
#'
#' @export kobo_get_dataframes_levels
#'

kobo_get_dataframes_levels <- function(form="form.xls") {
  tryCatch({
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
    survey <- survey[c("name","type")]
    survey$type <- tolower(survey$type)
    survey$type <- str_replace(survey$type,"_"," ")
    survey$type <- str_replace(survey$type,"-"," ")
    survey <- survey[!is.na(survey$type),]
    survey <- survey[survey$type=="begin repeat" | survey$type=="end repeat", ]
    
    if(nrow(survey)==0){
      return(data.frame(
        name = "MainDataFrame",
        level = 1,
        parent = "root"
        ,stringsAsFactors = F
      ))
    }
    
    result <- data.frame(
      name = "MainDataFrame",
      level = 1,
      parent = "root"
      ,stringsAsFactors = F
    )
    opcl <- data.frame(
      name = character(),
      open = logical(),
      stringsAsFactors = F
    )
    
    for(i in 1:nrow(survey)){
      st <- survey$type[i]
      sn <- ifelse(st=="begin repeat",survey$name[i],NA)
      if(st=="begin repeat"){
        temp <- opcl[opcl$open==T,]
        if(nrow(temp)==0){
          result <- rbind(result,
                          c(sn, 2, "MainDataFrame" )
          )
        }else{
          tempName <- temp[nrow(temp),"name"]
          result <- rbind(result,
                          c(sn, as.integer(result[result$name==tempName,"level"]) + 1, tempName)
          )
        }
        opcl <- rbind(opcl, 
                      data.frame(
                        name = sn,
                        open = T,
                        stringsAsFactors = F
                      )
        )
      }else if(st=="end repeat"){
        temp <- opcl[opcl$open==T,]
        tempName <- temp[nrow(temp),"name"]
        opcl[opcl$name == tempName,"open"] = F
      }
    }
    result$level <- as.integer(result$level)
    result <- result[order(result$level, result$parent),]
    return(result)
  }, error = function(err) {
    print("kkobo_get_dataframes_levels_ERROR")
    return(structure(err, class = "try-error"))
  })
}
