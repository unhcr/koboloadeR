#' @name kobo_datasets
#' @rdname kobo_datasets
#' @title Lists the Datasets Available
#'
#' @description Lists the datasets available at the URL being accessed, possibly according
#' to account.
#'
#' @param user Optional. A single string indicating the username and password
#' (in the form of \code{"username:password"}), or a character vector or list,
#' length 2, with the first value being the "username", and the second being
#' the "password".
#' @param api The URL at which the API can be accessed. Defaults to "unhcr",
#' which loads the UNHCR server KoBo Toolbox API.
#'
#' @return A data.table containing details about the datasets available,
#' including items like the "title", "id", and "url" of the datasets.
#'
#' @author Ananda Mahto
#'
#' @examples
#' \dontrun{
#' kobo_datasets()
#' }
#'
#' @export kobo_datasets
#'

kobo_datasets <- function(user = NULL, api = "unhcr") {
  URL <- sprintf(fmt = "%sdata.csv", kobo_host(api))
  x <- get_me(user, URL)
  cat("\n\n")
  f_csv(x)
}
NULL
