#' @name kobo_getMainDirectory
#' @rdname kobo_getMainDirectory
#' @title Main Directory for KoboloadeR package
#'
#' @description the function return the Main Directory for KoboloadeR packag
#'
#'
#' @return A string for Main Directory path.
#'
#' @export kobo_getMainDirectory
#'
#' @author Maher Daoud
#'
#' @examples
#' kobo_projectinit()
#'


kobo_getMainDirectory <- function() {
  mainDir <- gsub("/code/shiny_app", "",  getwd())
  mainDir <- gsub("/inst/shiny_app", "",  mainDir) 
  return(mainDir)
}