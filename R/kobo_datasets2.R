#' @name kobo_datasets2
#' @rdname kobo_datasets2
#' @title  Dataset list
#'
#' @description   Lists the Datasets Available including count of submission.
#' - description    id  -    id_string - title   -   url

#'
#' @param user Optional. A single string indicating the username and password
#' (in the form of \code{"username:password"}), or a character vector or list,
#' length 2, with the first value being the "username", and the second being
#' the "password".
#' @param api The URL at which the API can be accessed. Defaults to "unhcr",
#' which loads the UNHCR server KoBo Toolbox API.
#'
#' @return A data.table containing details about the datasets available,
#' including items like the "title", "id", and "submission".
#'
#' @author Edouard Legoupil
#'
#' @examples
#' \dontrun{
#' kobo_datasets2()
#' }
#'
#' @export kobo_datasets2
#'

kobo_datasets2 <- function(user , api ) {

  formlist <- kobo_datasets(user , api)
  formlist <- formlist[ , c("id",  "id_string")]
  formlist$count <- ""
  formlist$count <- as.integer(formlist$count)
  formlist <- as.data.frame(formlist)
  for (i in 1:nrow(formlist))
  {
    formidi <- as.integer(formlist[ i, 1])
    count <- kobo_submission_count(formid = formidi, user, api)
    if (length(count) == 1) {formlist[i ,3] <- count} else {i <- i + 1}
  }
  return(formlist)
}
NULL
