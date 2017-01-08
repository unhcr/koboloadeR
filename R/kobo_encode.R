#' @name kobo_encode
#' @rdname kobo_encode
#' @title  Encode variable
#'
#' @description  Insert the full label in data frame based on dictionnary
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
#' kobo_encode()
#'
#' @export kobo_encode
#' @examples
#' \dontrun{
#' kobo_encode(data, dico)
#' }
#'
#' @export data
kobo_encode <- function(datalabel, dico) {
  ### First we provide attribute label to variable name
  #datalabel <- data
  data.label <- as.data.frame(names(datalabel))
  names(data.label)[1] <- "fullname"
  data.label <- join (x=data.label, y=dico, by="fullname", type="left" )
  ## Now we can also re-encode the records themself

  ## List of select one and select multiple variable to re-encode ## "select_one",
  selectdf <- as.data.frame(dico[dico$type %in% c("select_multiple", "select_one_d"), c("fullname","name","listname","type")])
  #names(selectdf)[1] <- "selectvar"

  for (i in 1:nrow(selectdf)) {
    #i <-228
    #i <-1
    #i <-228
    fullname <- as.character(selectdf [ i,1])
    variablename <- as.character(selectdf [ i,2])
    variablelistname <- as.character(selectdf [ i,3])
    variablelevel <- as.data.frame(levels(as.factor(datalabel[ ,fullname])))
    names(variablelevel)[1] <- "namecoded"

    if (nrow(variablelevel)>0) {
    variablelevel <- cbind(variablelevel,fullname,variablename,variablelistname)
    variablelevel <- join (x=variablelevel, y=dico, by="fullname", type="left" )
    labelchoice <- as.character(dico[dico$fullname==fullname, c("labelchoice")])
    datalabel[, fullname][datalabel[, fullname]=="1"] <- labelchoice
    #View(datalabel[i])
    } else {
      cat(i) }
    rm(fullname, variablename, variablelistname,variablelevel)
  }
  #for (i in 1:nrow(datalabel) ) {
  # looping around variables within dataframe
  #  first obtain the current levels for that
  #  i <- 1
  #  levelvariable <- as.data.frame(levels(as.factor(datalabel[ , i]))
  #  names(levelvariable)[1] <- "coded"
  # Now get for each levels the corresponding label
  # levelvariable$recoded <- ""
  #  for (i in 1:nrow(levelvariable)) { levelvariable[ i, 2] <- as.character(dico[ dico$name==levelvariable[ i, 1], c("label")]) }
  #  datalabel[ , i] <- revalue(datalabel[ , i], c( "Strongly Disagree"= "-2", "Disagree"= "-1", "No Agreement / Undecided"= "0", "Agree" = "1", "Strongly Agree" = "2"))
  # datalabel[, i] <- factor(datalabel[, i], levels=c( "Strongly Disagree", "Disagree", "No Agreement / Undecided", "Agree", "Strongly Agree" ) )
    #}
  return(datalabel)
}
