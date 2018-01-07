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
  #datalabel <- data
  data.label <- as.data.frame(names(datalabel))
  names(data.label)[1] <- "fullname"
  data.label <- join (x=data.label, y=dico, by="fullname", type="left" )
  for (i in 1:nrow(data.label)) { attributes(datalabel)$variable.labels[ i] <- as.character(data.label[ i, c("label")]) }
  test <- data.label[ !(is.na(data.label$name)), ]
  if (nrow(data.label) > nrow(test)) {
    cat (paste0("you have ",nrow(data.label), " variables in you frame but only ",nrow(test) ," were relabeled.\n"))
    cat(" You may double check that the form and the data are matching \n")
    cat("Double check as well that you did download the data with the correct header (i.e. full path with point delimiters) \n")
  } else { cat ("All variables were mapped. great \n")}
  return(datalabel)
}
