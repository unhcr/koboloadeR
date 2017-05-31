#' @name kobo_surveyname
#' @rdname kobo_surveyname
#' @title  Extract Survey name from XlsForm
#'
#' @description  parse xlsfrom
#'
#' @param form
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_surveyname()
#'
#' @export kobo_surveyname
#' @examples
#' \dontrun{r
#' kobo_surveyname(form)
#' }
#'
#'

kobo_surveyname <- function(form) {

  # read the survey tab of ODK from
  form_tmp <- paste0("data/",form)

  ###############################################################################################
  ### First review all questions first
  settings <- read_excel(form_tmp, sheet = "settings")
  form_title <- as.chartecter(settings$form_title)

  return(form_title)
}
NULL
