#' @name kobo_arrange_variablename
#' @rdname kobo_arrange_variablename
#' @title  Replace / or : in variable name in order to use the dictionnary
#'
#' @description  The character to be replaced - coudl be a "/" or a ":"
#'
#' @param datavariable
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_arrange_variablename()
#'
#' @export kobo_arrange_variablename
#' @examples
#' \dontrun{r
#' kobo_arrange_variablename(data)
#' }
#'
#'

kobo_arrange_variablename <- function(datavariable) {

  ### Need to replace slash by point in the variable name
  ## get variable name from data
  datalabel <- as.data.frame( names(datavariable))
  names(datalabel)[1] <- "nameor"
  datalabel$nameor <- as.character(datalabel$nameor)

  ## new variables name without :
  datalabel$namenew <- str_replace_all(datalabel$nameor, ":", ".")
  ## new variables name without /
  datalabel$namenew <- str_replace_all(datalabel$namenew, "/", ".")
  ## new variables name without /
  datalabel$namenew <- str_replace_all(datalabel$namenew, "-", ".")

  ## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
  names(data) <- datalabel[, 2]

}
NULL
