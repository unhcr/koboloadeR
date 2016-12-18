#' @name kobo_label
#' @rdname kobo_label
#' @title  Insert the full label in data frame based on dictionnary
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
#' @export data 
kobo_label <- function(datalabel, dico) {
  ### First we provide attribute label to variable name
  #datalabel <- data.single
  #i <- 3
  data.label <- as.data.frame(names(datalabel))
  names(data.label)[1] <- "fullname"
  data.label <- join (x=data.label, y=dico, by="fullname", type="left" )
  for (i in 1:nrow(datalabel)) { attributes(datalabel)$variable.labels[ i] <- as.character(data.label[ i, c("label")]) }
  
  ## Now we can also re-encode the records themself
  return(datalabel) 
  
}