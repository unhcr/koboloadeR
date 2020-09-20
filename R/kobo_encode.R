#' @name kobo_encode
#' @rdname kobo_encode
#' @title  Encode variable
#'
#' @description  Insert the full label in data frame based on dictionnary
#'
#'
#' @param data Dataframe to relabel
#' @param dico Data dictionnary generated from kobo_dico
#'
#'
#' @return A "data.table" with the full data.label. To be used for graphs generation.
#'
#' @author Edouard Legoupil
#'
#'
#' @export kobo_encode
#'
#' @examples
#' \dontrun{
#' kobo_encode(data, dico)
#' }
#'
kobo_encode <- function(data, dico) {
  ### First we provide attribute label to variable name
  #data1 <- data
  # MainDataFrame <- read.csv("data/MainDataFrame_edited.csv")
  # dico <- read.csv("data/dico_form.xls.csv", comment.char="#")
  # data <- MainDataFrame
  data.label <- as.data.frame(names(data))
  names(data.label)[1] <- "fullname"
  data.label <- plyr::join(x = data.label, y = dico, by = "fullname", type = "left" )
  ## Now we can also re-encode the records themself

 
  ###### Case 1: Re-encoding  when we have select_multiple ####
  ## List of select one and select multiple variable to re-encode ## "select_one",

  selectdf <- as.data.frame(dico[dico$type %in% c( "select_multiple"), c("fullname","name","listname","type")])
  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf2 <- plyr::join(y = check, x = selectdf, by = "fullname", type = "left")
  selectdf3 <- selectdf[!is.na(selectdf2$id), ]

  if (nrow(selectdf3) == 0) {
    cat("************************************************************\n \n")
    cat("There's no disagreggated select_multiple variables to re-encoded \n")
    cat("No match for Select multiple variable in your dataset!  \n")
    cat(" You may double check that the form and the data are matching \n")
    cat("Double check as well that you did download the data with the correct header (i.e. full path with point delimiters) \n")
    cat("************************************************************\n \n")
  } else{
      #names(selectdf)[1] <- "selectvar"
    
      cat(paste0("There's ",nrow(selectdf3)," select_multiple modalities to encode \n"))
      for (i in 1:nrow(selectdf3)) {
        # i <- 98
        fullname <- as.character(selectdf3[ i,1])
        variablename <- as.character(selectdf3[ i,2])
        variablelistname <- as.character(selectdf3[ i,3])

       # variablelevel <- as.data.frame(levels(as.factor(data[ ,fullname])))

        variablelevel <- as.data.frame(levels(as.factor(data[[fullname]])))

        names(variablelevel)[1] <- "namecoded"
        labelchoice <- as.character(dico[dico$fullname == fullname, c("labelchoice")])
        if (nrow(variablelevel) > 0) {
          if (nrow(variablelevel) > 1) {

          data[ , fullname][is.na(data[ , fullname])] <- "Not replied"
          data[ , fullname][data[ , fullname] == "Not replied"] <- "Not replied"
          data[ , fullname][data[ , fullname] == "0"] <- "Not selected"
          data[ , fullname][data[ , fullname] == "FALSE"] <- "Not selected"
          data[ , fullname][data[ , fullname] == "False"] <- "Not selected"
          data[ , fullname][data[ , fullname] == "1"] <- labelchoice
          data[ , fullname][data[ , fullname] == "TRUE"] <- labelchoice
          data[ , fullname][data[ , fullname] == "True"] <- labelchoice

          } else{
          data[ , fullname][is.na(data[ , fullname])] <- ""
          data[ , fullname][data[ , fullname] == "0"] <- ""
          data[ , fullname][data[ , fullname] == "FALSE"] <- ""
          data[ , fullname][data[ , fullname] == "False"] <- ""
          data[ , fullname][data[ , fullname] == "1"] <- labelchoice
          data[ , fullname][data[ , fullname] == "TRUE"] <- labelchoice
          data[ , fullname][data[ , fullname] == "True"] <- labelchoice
          }

        cat(paste0(i, "- Recode disagreggated select_multiple modality ", fullname," for: ",labelchoice, "\n"))

        } else{cat(paste0("The following variable has no answers to recode in the dataset: ",fullname, "\n")) }

        rm(fullname, variablename, variablelistname,variablelevel)
      }
   }

  ###### Case 2: Re-encoding  when we have disaggregated select_one - select_one_d  ####
  ## List of select one and select multiple variable to re-encode ## "select_one",
  selectdf <- as.data.frame(dico[dico$type %in% c( "select_one_d"), c("fullname","name","listname","type")])
  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf2 <- plyr::join(y = check, x = selectdf, by = "fullname", type = "left")
  selectdf3 <- selectdf[!is.na(selectdf2$id), ]

  if (nrow(selectdf3) == 0) {
    cat("************************************************************\n \n")
    cat("There's no disaggregated select_one variable to re-encoded \n")
    cat("i.e. No match for Select one variables in your dataset!  \n")
    cat(" You may double check that the form and the data are matching \n")
    cat("Double check as well that you did download the data with the correct header (i.e. full path with point delimiters) \n")
    cat("************************************************************\n \n")
  } else{
    #names(selectdf)[1] <- "selectvar"

    for (j in 1:nrow(selectdf3)) {
      #j <-228
      #j <-1
      #i <-195
      #cat(i)
      fullname <- as.character(selectdf3[ j,1])
      variablename <- as.character(selectdf3[ j,2])
      variablelistname <- as.character(selectdf3[ j,3])

      variablelevel <- as.data.frame(levels(as.factor(data[ ,fullname])))
      names(variablelevel)[1] <- "namecoded"
      variablecode <- as.character(levels(as.factor(variablelevel$namecoded)))

      if (nrow(variablelevel) > 0) {
        variablelevel <- cbind(variablelevel,fullname,variablename,variablelistname)
        variablelevel <- plyr::join(x = variablelevel, y = dico, by = "fullname", type = "left" )
        labelchoice <- as.character(dico[dico$fullname == fullname, c("labelchoice")])
        data[ , fullname][data[ , fullname] == variablecode] <- labelchoice

        cat(paste0(" - Recode disaggregated select_one variable: ", fullname," for: ",labelchoice, "\n"))

        #View(data[i])
      } else {cat(paste0("The following disaggregated select_one variable has no answers to recode in the dataset: ",fullname, "\n")) }

      rm(fullname, variablename, variablelistname,variablelevel)
    }
  }


  ###### Case 3: Re-encoding  when we have select_select_one  ####
  ## List of select one and select multiple variable to re-encode ## "select_one",
  selectdf <- as.data.frame(dico[dico$type %in% c( "select_one"), c("fullname","name","listname","type")])
  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf2 <- plyr::join(y = check, x = selectdf, by = "fullname", type = "left")
  selectdf3 <- selectdf[!is.na(selectdf2$id), ]
  #names(selectdf)[1] <- "selectvar"

  if (nrow(selectdf3) == 0) {
    cat("************************************************************\n \n")
    cat("There's no  select_one variables to re-encode \n")
    cat("i.e. No match for Select one variables in your dataset!  \n")
    cat(" You may double check that the form and the data are matching \n")
    cat("Double check as well that you did download the data with the correct header (i.e. full path with point delimiters) \n")
    cat("************************************************************\n \n")
  } else {
      cat(paste0("There's ",nrow(selectdf3)," select_one variables to encode \n"))

     for (i in 1:nrow(selectdf3)) {
      # i <- 50
      fullname <- as.character(selectdf3[ i,1])
      variablename <- as.character(selectdf3[ i,2])
      variablelistname <- as.character(selectdf3[ i,3])

      variablelevel <- dico[ dico$listname == variablelistname & dico$type == "select_one_d", c("name","labelchoice")]

      # Need to account for the case where allow_choice_duplicates =   yes


      variablelevel <- unique(variablelevel[ c("name","labelchoice")])

      if (nrow(variablelevel) > 0) {
        #rm(df)
        df <- as.data.frame(data[ , fullname])
        names(df)[1] <- "name"
        df$name <- as.character(df$name)
        df <- plyr::join(x = df, y = variablelevel, by = "name")
        data[ , fullname] <- as.character(data[ , fullname])
        data[ , fullname] <- df$labelchoice
        #data[ , fullname] <- as.factor(data[ , fullname])
        data[ , fullname] <- as.factor(data[[fullname]])
        #View(data[i])
        cat(paste0(i, "- Recode variable: ", fullname," \n"))

      } else {cat(paste0("The following variable has no answers to recode in the dataset: ",fullname, "\n")) }

     rm(fullname, variablename, variablelistname,variablelevel)
    }
  }
  ###### Case 4: Re-encoding  when we have select_select_multiple  ####
  ## List of select one and select multiple variable to re-encode ## "select_one",
  selectdf <- as.data.frame(dico[dico$type %in% c( "select_multiple_d"), c("fullname","name","listname","type")])
  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf2 <- plyr::join(y = check, x = selectdf, by = "fullname", type = "left")
  selectdf3 <- selectdf[!is.na(selectdf2$id), ]
  #names(selectdf)[1] <- "selectvar"

  if (nrow(selectdf3) >= 1) {
    cat("************************************************************\n \n")
    cat("Uhmm... you have concatenated select_multiple. If this not already done, you may need to use the kobo_split_multiple function to deconcatenate those lists. \n")
    cat("************************************************************\n \n")
  } else{

    cat("************************************************************\n \n")
    cat("No concatenated select_multiple.! \n")
    cat("i.e. No match for Select multiple variables in your dataset!  \n")
    cat(" You may double check that the form and the data are matching \n")
    cat("Double check as well that you did download the data with the correct header (i.e. full path with point delimiters) \n")
    cat("************************************************************\n \n")
    }

return(data)
}
NULL
