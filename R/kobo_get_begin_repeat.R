#' @name kobo_get_begin_repeat
#' @rdname kobo_get_begin_repeat
#' @title  Get all begin repeat from xlsform
#'
#' @description  Get the 'name' column for all rows that have 'begin repeat' value in 'type' column
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#'
#'
#' @return return a list that contains 1.vector of string represent the names 2.message about status
#'
#' @author Maher Daoud
#'
#'
#' @examples
#' \dontrun{
#' kobo_get_begin_repeat("myform.xlsx")
#' }
#'
#' @export kobo_get_begin_repeat
#'
#'
#'

kobo_get_begin_repeat <- function(form = "form.xlsx") {

  mainDir <- kobo_getMainDirectory()
  form_tmp <- paste(mainDir, "data-raw", form, sep = "/", collapse = "/")

  ### First review all questions from survey sheet #################################################
  survey <- tryCatch({
    as.data.frame(readxl::read_excel(form_tmp, sheet = "survey")) #read survey sheet from the form
  }, error = function(err) {
    return(
      list(
        names = character(),
        message = "There is no survey sheet, please make sure the xlsform file contains survey sheet."
      )
    )
  })

  survey$type <- tolower(survey$type)
  result <- survey[ which(survey$type == "begin repeat" | survey$type == "begin_repeat" | survey$type == "begin-repeat"), ]
  result <- result$name

  if (length(result) == 0) {
    return(
      list(
        names = result,
        message = "xlsform file doesn't contain begin repeat, you only need to upload the main data file"
      )
    )
  }else{
    return(
      list(
        names = result,
        message = paste("xlsform file contains", length(result), "begin repeat, you have to upload the main data file and all begin repeat files\n",
                        paste(result, collapse = '\n')
        )
      )
    )
  }
}
NULL
