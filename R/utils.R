#' @name get_me
#' @rdname get_me
#' @title  Authentify in Kobo Server
#'
#' @description Helper Function for GET, Depending on Whether Authentication is Required
#'
#' Adds basic level authentication if provided.
#'
#' @param user string of length 1 or 2 with user details
#' @param URL The URL to be passed to curl
#' @note This function is not intended to be called directly.
#' It is used in other functions.
#' @author Ananda Mahto
#'
get_me <- function(user, URL) {
  if (is.null(user)) {
    get(URL, progress())
  } else {
    u <- pwd_parse(user)
    get(URL, httr::authenticate(u$username, u$password), progress())
  }
}
NULL

#' @name pwd_parse
#' @rdname pwd_parse
#' @title  Parse Kobo Password
#'
#' @description Helper Function to Parse a String to be Used as a Username/Password Combination
#'
#' Converts a string of length 1 or of length 2 into a list that can then be
#' passed on to the \code{authenticate} function from the "httr" package.
#'
#' @param \dots A single string, character vetor, or list containing the
#' username and password that should be used. If it is a single string, it
#' should be in the form of "username:password".
#' @note This function is not intended to be called directly.
#' It is used in other functions.
#'
#' @examples
#' \dontrun{
#' pwd_parse("username", "password")
#' pwd_parse("username:password")
#' pwd_parse(c("username", "password"))
#' }
#' @author Ananda Mahto
#'
pwd_parse <- function(...) {
  upw <- unlist(list(...))
  nam <- c("username", "password")
  auth <- {
    if (length(upw) == 1) {
      unlist(strsplit(upw, ":", TRUE))
    } else {
      if (length(upw) > 2) {
        message("More than two values supplied. Using only first two values.")
        upw[1:2]
      } else {
        upw
      }
    }
  }
  setNames(as.list(auth), nam)
}
NULL

#' @name kobo_time_parser_UTC
#' @rdname kobo_time_parser_UTC
#' @title Parses Dates from KoBo Into a More Usable Format
#'
#' @description The date/time values in KoBo usually get stored in a format
#' like the following: "2015-08-27T13:28:29.000+06:30". These functions
#' process these date/times into more usable formats.
#'
#' @param instring A date/time format coming from KoBo.
#'
#' @return The \code{kobo_time_parser_UTC} function returns a POSIXct object,
#' while the \code{kobo_time_parser} function returns a formatted character
#' string that can be easily parsed as a date/time object.
#'
#' @author Ananda Mahto
#' @examples
#' \dontrun{
#' TIME <- "2015-08-27T13:28:29.000+06:30"
#' kobo_time_parser_UTC(TIME)
#' }
#'
#' @export kobo_time_parser_UTC
#' @aliases kobo_time_parser_UTC
kobo_time_parser_UTC <- function(instring) {
  tmp <- gsub("\\.\\d{3}|:", "", instring)
  tmp <- chartr(" ", "0", format(tmp, justify = "left", width = 22))
  as.POSIXct(strptime(tmp, format = "%Y-%m-%dT%H%M%S%z", tz = "UTC"))
}
NULL

