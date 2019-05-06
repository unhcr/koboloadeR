#' @name kobo_split_multiple
#' @rdname kobo_split_multiple
#' @title  Split variables resulting from select_multiple questions
#'
#' @description  To be used when extracting from ODK that does not offers splitting capacity
#'
#'
#' @param data Dataframe with selectmultiple column to split
#' @param dico Data dictionnary generated from kobo_dico
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
kobo_split_multiple <- function(data, dico) {

  ## list fields that have select multiple in the dico - select_multiple_d
  selectdf <- dico[dico$type == "select_multiple_d", c("fullname","listname","label","name","variable","disaggregation")]
  #rm(datalabel)
  # data <- household
  # data <- data.or
  datalabeldf <- as.data.frame( names(data))
  data <- as.data.frame(data)
  names(datalabeldf )[1] <- "fullname"
  datalabeldf$fullname <- as.character(datalabeldf$fullname)
  datalabeldf$id <- row.names(datalabeldf)
  datalabeldf <- join(x = datalabeldf,y = selectdf,by = "fullname",type = "right")
  ## Eliminate record from the wrong frame -i.e. id is NULL -
  datalabeldf <- datalabeldf[ !(is.na(datalabeldf$id)), ]

  ## Stop here if no select_multiple to split
  if ( nrow(datalabeldf) == 0 ) {

    cat("No match for Select multiple variable in your dataset!  \n")

    cat("You may double check that the form and the data are matching \n")

    cat("Double check as well that you did download the data with the correct header (i.e. full path with point delimiters) \n")

    return(data)
  } else {

  ## Check if those select_multiple_d have corresponding select_multiple

  ## Now create the unique select_multiple and append to the dataframe
  for (i in 1:nrow(datalabeldf) ) {
    # i <- 6
    fullname <- as.character(datalabeldf[i,1])
    id <-  as.integer(as.character(datalabeldf[i,2]))
    cat(paste0(i, " - Splitting variable ", fullname, " in column: ", id, "\n"))

    data[ , id] <- as.character(data[ , id])

    ## Account non answered - could be recognised either as null or na...

   # nrow(data[data[ , id]=='', id])

    data[is.na(data[ , id]), id] <- "zNotAnswered"
    data[data[ , id] =='', id] <- "zNotAnswered"

    #levels(as.factor(data[ , id]))
    #levels(data[ , id])
    list <- as.data.frame(data[ , id])

    ## thanks to: https://stackoverflow.com/questions/44232180/list-to-dataframe
    tosplitlist <- strsplit(as.character(data[ , id]), " ")
    tosplitlist <- setNames(tosplitlist, seq_along(tosplitlist))
    tosplitlist2 <- utils::stack(tosplitlist)
    tosplitframe <- dcast(tosplitlist2, ind ~ values, value.var = "ind", fun.aggregate = length)

    if (ncol(tosplitframe) == 3 ) {
      cat(paste0("There was only one modality selected for this select_multiple question in the whole dataset. \n"))
    } else {
      cat(paste0("There was ", ncol(tosplitframe) - 2 , " different modalities for that question. \n"))    }

    for (h in 2:ncol(tosplitframe) ) { tosplitframe[tosplitframe$zNotAnswered == 1, h] <- "Not replied"}
    drops <- c("ind", "zNotAnswered")
    tosplitframe <- as.data.frame(tosplitframe[ , !(names(tosplitframe) %in% drops)])



    ## Rename the variable to match with dictionnary
    datalabelframe <- as.data.frame( names(tosplitframe))
    names(datalabelframe )[1] <- "nameor"
    datalabelframe$nameor <- as.character(datalabelframe$nameor)

    ## Handling the case where no one replied to that question;
    if (nrow(datalabelframe) == 0) {
      cat("There was no recorded reponses for this question...\n")
      } else {

      cat("Spliting now!\n")
      ## new variables name without /
      datalabelframe$namenew <- paste(fullname, datalabelframe$nameor, sep = ".")
      ## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
      names(tosplitframe) <- datalabelframe[, 2]

      ## Bind to original data
      cat(paste0("Number of columns: ", ncol(data), ", number of additional splitted variables:",ncol(tosplitframe), "\n"))
      data <- cbind(data, tosplitframe )

      cat(paste0("After binding Number of columns: ", ncol(data), "\n"))
      }
    rm(tosplitframe,tosplitlist,datalabelframe)
  }

  #rm(selectdf,datalabeldf)

  return(data)
  }
}
NULL


