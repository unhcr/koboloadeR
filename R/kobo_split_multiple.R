#' @name kobo_split_multiple
#' @rdname kobo_split_multiple
#' @title  Split variables resulting from select_multiple questions
#'
#' @description  To be used when extracting from ODK that does not offers splitting capacity
#'
#'
#' @param data .
#' @param dico ( generated from kobo_dico)
#'
#' @return data A "data.table" with the full splitted select_multiple.
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_split_multiple()
#'
#' @export kobo_split_multiple
#'
#' @examples
#' \dontrun{
#' kobo_split_multiple(data, dico)
#' }
#'
#'

kobo_split_multiple <- function(data, dico) {

  ## list fields that have select multiple in the dico - select_multiple_d
  selectdf <- dico[dico$type=="select_multiple_d", c("fullname","listname","label","name","variable","disaggregation")]
  #rm(datalabel)
  datalabeldf <- as.data.frame( names(data))
  names(datalabeldf )[1] <- "fullname"
  datalabeldf$fullname <- as.character(datalabeldf$fullname)
  datalabeldf$id <- row.names(datalabeldf)
  datalabeldf <- join(datalabeldf,selectdf,by="fullname",type="right")

  ## Check if those select_multiple_d have corresponding select_multiple

  ## Now create the unique select_multiple and append to the dataframe
  for (i in 1:nrow(datalabeldf) ) {
    # i <- 7
    fullname <- as.character(datalabeldf[i,1])
    cat(paste0(i, "Splitting variable ", fullname))
    id <-  as.integer(as.character(datalabeldf[i,2]))

    data[ , id] <- as.character(data[ , id])
    ## Account non answered
    data[data[ , id]=='', id] <- "zNotAnswered"
    #levels(as.factor(data[ , id]))
    #list <- as.data.frame(data[ , id])

    ## thanks to: https://stackoverflow.com/questions/44232180/list-to-dataframe
    tosplitlist <- strsplit(data[ , id], " ")
    tosplitlist <- setNames(tosplitlist, seq_along(tosplitlist))
    tosplitlist <- stack(tosplitlist)
    tosplitframe <- dcast(tosplitlist, ind ~ values, fun.aggregate = length)
    for (h in 2:ncol(tosplitframe) ) { tosplitframe[tosplitframe$zNotAnswered==1, h] <- "Not replied"}
    drops <- c("ind", "zNotAnswered")
    tosplitframe <- tosplitframe[ , !(names(tosplitframe) %in% drops)]

  #Rename the variable to match with dictionnary
    datalabelframe <- as.data.frame( names(tosplitframe))
    names(datalabelframe )[1] <- "nameor"
    datalabelframe $nameor <- as.character(datalabelframe $nameor)

    ## new variables name without /
    datalabelframe$namenew <- paste(fullname, datalabelframe$nameor, sep=".")
    ## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
    names(tosplitframe) <- datalabelframe[, 2]

    ## Bind to original data
    cat(paste0("Number of columns: ", ncol(data), "\n"))
    data <- cbind(data, tosplitframe )

    cat(paste0("After binding Number of columns: ", ncol(data), "\n"))
    rm(tosplitframe,tosplitlist,datalabelframe)
  }
rm(selectdf,datalabeldf)
return(data)

}
NULL


