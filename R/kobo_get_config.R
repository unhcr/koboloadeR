#' @name kobo_get_config
#' @rdname kobo_get_config
#' @title Get Configuration
#'
#' @description Return all configuration from Analysis Settings sheet of xlsform
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file). where settings sheet contains all configuration of the project
#'
#' @return Return a dataframe that contains configuration of the project
#'
#' @author Maher Daoud
#'
#' @examples
#' \dontrun{
#' kobo_get_config()
#'}
#'
#' @export kobo_get_config
#'

kobo_get_config <- function(form = "form.xlsx") {
  mainDir <- kobo_getMainDirectory()
  form_tmp <- paste(mainDir, "data-raw", form, sep = "/", collapse = "/")
  settings <- tryCatch({
    as.data.frame(readxl::read_excel(form_tmp, sheet = "analysisSettings"), stringsAsFactors = FALSE)
  }, error = function(err) {
    data.frame(
      name = character(),
      label = character(),
      value = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
  })
  return(settings)
}
