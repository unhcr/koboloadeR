#' @name kobo_data_downloader
#' @rdname kobo_data_downloader
#' @title Retrieve the Data from a Specified Dataset
#'
#' @description Retrieves the data submitted to a specified dataset.
#'
#' @param formid The ID of the form to be accessed (as a character string).
#' @param user Optional. A single string indicating the username and password
#' (in the form of \code{"username:password"}), or a character vector or list,
#' length 2, with the first value being the "username", and the second being
#' the "password".
#' @param api The URL at which the API can be accessed.
#' Defaults to "unhcr", which loads the UNHCR KoBo Toolbox API.
#' @param check Logical. Should the function first check to see whether the
#' data is available offline.
#' @return A "data.table" with the full dataset. If data is already found on
#' disk and the number of rows matches with the online datasets, the local copy
#' would be used. The dataset would be named in the form of "data_formid".
#'
#' @author Ananda Mahto
#' @examples
#' \dontrun{
#' kobo_data_downloader("15051")
#' kobo_data_downloader("31511", api = "unhcr")
#' }
#'
#' @export kobo_data_downloader
#'


kobo_data_downloader <- function(formid, user = NULL, api = "unhcr", check = TRUE) {
  locfile <- sprintf(fmt = "data_%s", formid)

  if (isTRUE(check)) {
    if (exists(locfile)) {
      rows <- nrow(get(locfile))
      check <- kobo_submission_count(formid, user, api)
      if (rows == check) {
        message("Number of rows in local and remote file match.")
        redownload = FALSE
      } else {
        message(sprintf(fmt = "Local file found with %d rows. Remote file contains %d rows.", rows, check))
        redownload = TRUE
      }
    } else {
      message("No local dataset found.")
      redownload = TRUE
    }
  } else {
    redownload = TRUE
  }

  if (isTRUE(redownload)) {
    message("Downloading remote file.")
    URL <- sprintf(fmt = '%sdata/%s.csv', kobo_host(api), formid)

    x <- get_me(user, URL)
    cat("\n\n")
    out <- f_csv(x)
    assign(locfile, out #, envir = .GlobalEnv
           )
    out
  } else {
    message("Using local file.")
    get(locfile)
  }
}
NULL
