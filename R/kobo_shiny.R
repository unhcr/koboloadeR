#' @name kobo_shiny
#' @rdname kobo_shiny
#' @title  Shiny app launcher

#' @description  A function to launch shiny apps
#'
#'
#' @param app script where shyni app is located
#'
#' @author Elliott Messeiller
#'
#'
#' @export kobo_shiny
#' @examples
#' \dontrun{
#' kobo_shiny(appname)
#' }
#'
#'
kobo_shiny <- function(app = "") {
  mainDir <- kobo_getMainDirectory()

  validApps <- list.files(system.file("shiny_app", package = "koboloadeR"))

  validAppsMsg <-
    paste0(
      "Valid apps are: '",
      paste(validApps, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(app) || !nzchar(app) || !app %in% validApps) {
    stop(
      "Please run 'kobo_shiny()' with a valid example app as an argument.\n",
      validAppsMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- paste0(mainDir,"/code/shiny_app/",app)
  shiny::runApp(appDir, display.mode = "normal")
}
NULL
