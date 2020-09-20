#' @name kobo_host
#' @rdname kobo_host
#' @title  Select server
#'
#' @description A helper function to conveniently switch different APIs.
#'
#' Specifies the Host URL of the API to Use
#'
#'
#' @param api Either "kobo", "kobohr", "ona", or a custom (full) URL.
#' @return A single string with the URL to use.
#' @note API URLs are made available for KoBo Toolbox ("kobo",
#' \url{https://kc.kobotoolbox.org/api/v1/}), KoBo Humanitarian Response
#' ("kobohr", \url{https://kc.humanitarianresponse.info/api/v1/}), Ona
#' ("ona", \url{https://ona.io/api/v1/}) and Unhcr ("unhcr", \url{https://kobocat.unhcr.org/api/v1/}) . For your own installation, or other
#' installations using the same API but accessed at a different URL,
#' enter the full URL.
#' @author Ananda Mahto
#' @note This function is not intended to be called directly.
#' It is used in other functions.
#'
#' @examples#'
#' \dontrun{
#' kobo_host("unhcr")
#' kobo_host("ttps://kobocat.unhcr.org/api/v1/")
#' }
#'

kobo_host <- function(api) {
  if (api %in% c("kobo", "kobohr", "ona","unhcr")) {
    switch(api,
           kobo = "https://kc.kobotoolbox.org/api/v1/",
           kobohr = "https://kc.humanitarianresponse.info/api/v1/",
           ona = "https://ona.io/api/v1/",
           unhcr = "https://kobocat.unhcr.org/api/v1/")
  } else {
    api
  }
}
NULL
