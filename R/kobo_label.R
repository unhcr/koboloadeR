#' @name kobo_label
#' @rdname kobo_label
#' @title  Label Variable
#'
#' @description    Insert the full label in data frame based on dictionnary
#'
#'
#' @param data .
#' @param dico ( generated from kobo_dico)
#'
#'
#' @return A "data.table" with the full data.label. To be used for graphs generation.
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_label()
#'
#' @export kobo_label
#' @examples
#' \dontrun{
#' kobo_label(data, dico)
#' }
#'
#' @export kobo_label
#'

kobo_label <- function(datalabel, dico) {
  ### First we provide attribute label to variable name
  #datalabel <- individual_biodata3
  data.label <- as.data.frame(names(datalabel))
  names(data.label)[1] <- "fullname"
  data.label <- join (x=data.label, y=dico, by="fullname", type="left" )
  for (i in 1:nrow(data.label)) { attributes(datalabel)$variable.labels[ i] <- as.character(data.label[ i, c("label")]) }

  return(datalabel)
}
