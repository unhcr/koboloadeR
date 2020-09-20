#' @name kobo_submission_count
#' @rdname kobo_submission_count
#' @title Retrieve the Number of Submissions in a Specified Dataset
#'
#' @description Retrieves the number of submissions made to a specified dataset.
#'
#' @param formid The ID of the form to be accessed (as a character string).
#' @param user Optional. A single string indicating the username and password
#' (in the form of \code{"username:password"}), or a character vector or list,
#' length 2, with the first value being the "username", and the second being
#' the "password".
#' @param api The URL at which the API can be accessed.
#' Defaults to "kobo", which loads the KoBo Toolbox API.
#' @return A single number indicating the number of submissions received.
#' @author Ananda Mahto
#'
#' @examples
#' \dontrun{
#' kobo_submission_count("15051")
#' kobo_submission_count("31511", api = "kobohr")
#' }
#'
#' @export kobo_submission_count
#'

kobo_submission_count <- function(formid, user = NULL, api = "unhcr") {
  URL <- "%sstats/submissions/%s.csv?group=dummydatagroupingvar"
  URL <- sprintf(fmt = URL, kobo_host(api), formid)
  x <- get_me(user, URL)
  cat("\n\n")
  f_csv(x)$count
}
NULL
