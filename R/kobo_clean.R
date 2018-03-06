#' @name kobo_clean
#' @rdname kobo_clean
#' @title  Add cleaned variables to the frame based on a reference table
#'
#' @description  The function works on a loop based on the dictionnary.
#' Add a column clean and insert in the cell the name of the csv file that will be used to generate the cleaned variable
#' The first column of that file will be used for the matching, the second column will be added to the dataframe.
#' The new cleaned variable will be inserted in the dictionnary. with a suffix '.clean'
#'
#'
#' @frame  kobo or odk dataset to use
#' @param  dico Generated from kobo_dico function
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_clean()
#'
#' @export kobo_clean
#'
#' @examples
#' \dontrun{
#' kobo_clean(frame, dico)
#' }
#'
#'

kobo_clean <- function(frame, dico) {

  # frame <- household
  # framename <- "household"
  framename <- deparse(substitute(frame))
  ## library(digest)
  ## Get the anonymisation type defined within the xlsform / dictionnary

  if (levels(dico$clean) == "not-to-be-cleaned") {
    cat(paste0("You have not defined variables to clean within your xlsform. \n"))
    cat(paste0(" Insert a column named clean and reference the csv file to use for cleaning. \n")) }
  else {
    dico.clean <- dico[ !(is.na(dico$clean)) & dico$qrepeatlabel == framename,  ]
      if (nrow(dico.clean) > 0) {
        cat(paste0(nrow(dico.clean), " variables to clean\n"))
          for (i in 1:nrow(dico.clean)) {
           # i <- 1
            cat(paste0(i, "- Remove, if exists, the value of: ", as.character(dico.clean[ i, c("label")]),"\n"))
            ## Build and run the formula to insert the indicator in the right frame  ###########################
            varia <- paste0(framename,"$",as.character(dico.clean[ i, c("fullname")]))
            formula <-
            if (file.exists("code/temp.R")) file.remove("code/temp.R")
            cat(paste0("if (\"", as.character(dico.clean[ i, c("fullname")]) , "\" %in% names(", framename, ")) {" ), file = "code/temp.R" , sep = "\n", append = TRUE)
            cat(paste0(framename,"$",as.character(dico.clean[ i, c("fullname")]),"<- \"removed\" } else" ), file = "code/temp.R" , sep = "\n", append = TRUE)
            cat("{}", file = "code/temp.R" , sep = "\n", append = TRUE)
            source("code/temp.R")
            if (file.exists("code/temp.R")) file.remove("code/temp.R")
          }
      }
  }
  return(frame)
}
NULL
