#' @name kobo_arrange_variablename
#' @rdname kobo_arrange_variablename
#' @title  Replace / or : in variable name in order to use the dictionnary
#'
#' @description  The character to be replaced - coudl be a "/" or a ":"
#'
#' @param data dataframe with Variables to be renamed
#'
#' @author Edouard Legoupil
#'
#'
#' @export kobo_arrange_variablename
#'
#' @examples
#' \dontrun{
#' kobo_arrange_variablename(data)
#' }
#'
#'
kobo_arrange_variablename <- function(data) {
  ### Need to replace slash by point in the variable name
  ## get variable name from data
  #datalabel <- as.data.frame( names(data))
  #names(datalabel)[1] <- "nameor"
  #datalabel$nameor <- as.character(datalabel$nameor)
  ## new variables name without :
  #datalabel$namenew <- str_replace_all(datalabel$nameor, ":", ".")
  ## new variables name without /
  #datalabel$namenew <- str_replace_all(datalabel$namenew, "/", ".")
  ## new variables name without /
  #datalabel$namenew <- str_replace_all(datalabel$namenew, "-", ".")
  ## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
  #names(data) <- datalabel[, 2]
  #return(data)
}
NULL
