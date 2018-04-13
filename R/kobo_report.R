#' @name kobo_report
#' @rdname kobo_report
#' @title  Generate the analysis

#' @description  Automatically generate the report with all variable analysed
#'
#' @author Edouard Legoupil, Elliott Messeiller
#' @param mainDir Path to the project's working directory: mainly for proper shiny app path

#'
#' @examples
#' kobo_report()
#'
#' @export kobo_report
#' @examples
#' \dontrun{
#' kobo_report()
#' }
#'
#'

kobo_report <- function(mainDir="") {
  if (mainDir==''){
    mainDir <- getwd()
  }

  source(paste0(mainDir,"/code/0-config.R"), local=TRUE)
  data <- read_excel(path.to.data, sheet=sheet)






}
NULL
