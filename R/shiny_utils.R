#' Shiny Apps for Viewing Online KoBo Data
#'
#' A launcher for the Shiny apps available in the koboloadeR package.
#'
#' @param app The name of the app to be run. If empty, the function will display
#' the names of the available apps.
#' @return Launches RStudio's viewer to view the data. The dataset is also
#' downloaded to your Global Environment.
#' @author Ananda Mahto
#'
#' @section Available Apps:
#'
#' \itemize{
#'  \item \code{"data_viewer"}
#' }
#'
#' @examples
#' \dontrun{
#' kobo_apps()
#' kobo_apps("data_viewer")
#' }
#'
#' @export kobo_apps
#'

kobo_apps <- function(app) {
  validExamples <- list.files(system.file("shiny_examples", package = "koboloadeR"))
  validExamplesMsg <- paste0(
    "Valid examples are: '", paste(validExamples, collapse = "', '"), "'")
  if (missing(app) || !nzchar(app) || !app %in% validExamples) {
    stop('Please run `kobo_app()` with a valid app name as an argument.\n',
         validExamplesMsg, call. = FALSE)
  }
  appDir <- system.file("shiny_examples", app, package = "koboloadeR")
  shiny::runApp(appDir, display.mode = "normal")
}
NULL
