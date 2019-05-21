#' @name kobo_getMainDirectory
#' @rdname kobo_getMainDirectory
#' @title get Main Directory for a KoboloadeR project
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
  tryCatch({
    mainDir <- gsub("/code/shiny_app", "",  getwd())
    mainDir <- gsub("/inst/shiny_app", "",  mainDir)
    return(mainDir)
  }, error = function(err) {
    print("kobo_getMainDirectory_ERROR")
    return(structure(err, class = "try-error"))
  })
}
