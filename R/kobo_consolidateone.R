#' @name kobo_consolidateone
#' @rdname kobo_consolidateone
#' @title  Merge disagregated select_one variable
#'
#' @description  Merge disagregated select_one variable
#'
#' @param data original dataset
#' @param dico dictionnary
#'
#' @return A "data.table" with additional select_one variable.
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_consolidateone()
#'
#' @export kobo_consolidateone
#' @examples
#' \dontrun{
#' kobo_consolidateone("myform.xls")
#' }
#'
#' @export kobo_consolidateone
#'
#' @return data

kobo_consolidateone <- function(data,dico) {

  ### List of select_one questions
  listoned <- dico[dico$type == "select_one_d" , c("listname","label","name","fullname")]
  listoned$qname <- ""
  for (i in 1:nrow(listoned)) {
    listoned[i,5] <- substr(as.character(listoned[i,4]), 1 , nchar(as.character(listoned[i,4])) - 1 - nchar(as.character(listoned[i,3])))
  }

  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  listoned2 <- plyr::join(x = listoned, y = check, by = "fullname",  type = "left")
  listoned <- listoned2[!is.na(listoned2$id), ]


  ## List of sub select_one
  liston <- dico[dico$type == "select_one" & is.na(dico$qrepeat), c("listname","label","name","fullname")]
 # liston2 <- plyr::join(x=liston, y=check, by="fullname",  type="left")
 # liston3 <- liston2[!is.na(liston$id), ]
  names(liston)[4] <- "qname"

  # test <- plyr::join(y=liston, x=listoned, type="left", by="qname")

  for (i in 1:nrow(liston) ) {
    # i <- 1
    newlistone <- as.character(liston[i,4])
    ### select variable for a specific multiple questions
    selectlistoned <- as.data.frame(listoned[listoned$qname == newlistone , c("fullname")])
    names(selectlistoned)[1] <- "vartoconact"
    selectlistoned$vartoconact <- as.character(selectlistoned$vartoconact)
    #data <- data[ , c(1:745)]
    data$newvar <- ""

    for (j in 1:nrow(selectlistoned) ) {
      #j <- 7
      if (!is.na(selectlistoned[j , 1]) ) {
        data[ ,selectlistoned[j,1] ][is.na(data[ ,selectlistoned[j,1] ])] <- ""
        data$newvar <- paste0(data$newvar, data[ ,selectlistoned[j,1] ] )
        cat(paste("i=",i,"  -  j=",j,"\n"))
      } else {  }
    }
    #View(data$newvar)
    names(data)[names(data) == "newvar"] <- newlistone
  }

  #rm(liston,listoned,selectlistoned,i,newlistone)
return(data)
}
NULL
