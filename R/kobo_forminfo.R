#' @name kobo_forminfo
#' @rdname kobo_forminfo
#' @title  Get form attributes
#'
#'
#' @description  Obtain form info in order to correctly retrieve the form.
#'
#' @param formid The ID of the form to be accessed (as a character string).
#' @param user Optional. A single string indicating the username and password
#' (in the form of \code{"username:password"}), or a character vector or list,
#' length 2, with the first value being the "username", and the second being
#' the "password".
#' @param api The URL at which the API can be accessed.
#' Defaults to "unhcr", which loads the UNHCR KoBo Toolbox API.
#' @return A "data.table" with the full forminfo.
#' The forminfo would be named in the form of "data_formid".
#'
#' @return The URL of the form based on form id.
#'
#' @author Edouard Legoupil
#'
#' @examples
#' \dontrun{
#' kobo_forminfo("15051")
#' kobo_forminfo("31511", api = "unhcr")
#' }
#'
#'
#' @export kobo_forminfo
#'

kobo_forminfo <- function(formid, user = NULL, api = api) {

  locfile <- sprintf(fmt = "forminfo_%s", formid)

  URL <- sprintf(fmt = '%sforms/%s?format=csv', api, formid)

  x <- get_me(user, URL)
  out <- f_csv(x)
  assign(locfile, out #, envir = .GlobalEnv
         )
  out
  formauthor <- as.charater(out[, c("owner")])
  formdescr <- out[, c("id_string")]
}
NULL
