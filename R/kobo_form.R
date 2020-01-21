#' @name kobo_form
#' @rdname kobo_form
#' @title  Download form from the platform
#'
#' @description Download form from the platform
#'
#' @param formid The ID of the form to be accessed (as a character string).
#' @param userpwd Optional. A single string indicating the username and password
#' (in the form of \code{"username:password"}), or a character vector or list,
#' length 2, with the first value being the "username", and the second being
#' the "password".
#' @param api The URL at which the API can be accessed.
#' Defaults to "unhcr", which loads the UNHCR KoBo Toolbox API.
#'
#' @return Downloaded form path.
#'
#' @author Edouard Legoupil
#'
#'
#' @examples
#' \dontrun{
#' kobo_form("15051")
#' kobo_form("31511", user = userpwd, api = "unhcr")
#' }
#'
#' @export kobo_form
#'


kobo_form <- function(formid, userpwd, api) {

  URL1 <- sprintf(fmt = '%sforms/%s/form.xls', api, formid)
  form_tmp <- file(paste0("data/form_",formid,".xls"), open = "wb")
  #rm(form_tmp)
  bin <- RCurl::getBinaryURL(URL1, userpwd , httpauth = 1L, ssl.verifypeer=FALSE  )
  writeBin(bin, form_tmp)
  close(form_tmp)

  ## test with xlsx
  URL2 <- sprintf(fmt = '%sforms/%s/form.xlsx', api, formid)
  form_tmp2 <- file(paste0("data/form_",formid,".xlsx"), open = "wb")
  bin <- RCurl::getBinaryURL(URL2, userpwd , httpauth = 1L, ssl.verifypeer=FALSE  )
  writeBin(bin, form_tmp2)
  close(form_tmp2)

  #locfileform <- sprintf(fmt = "form_%s", formid)
  #URL <- sprintf(fmt = '%sforms/%s/form.xls', koboloadeR::host(api), formid)
  #x <- koboloadeR::get_me(user, URL)
  #cat("\n\n")
  #form <- koboloadeR::f_csv(x)
  #assign(locfileform, form, envir = .GlobalEnv)
  #out

  return(form_tmp)
}
