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
#' @examples
#' kobo_encode()
#'
#' @export kobo_encode
#'
#' @examples
#' \dontrun{
#' kobo_encode(data, dico)
#' }
#'
#'


kobo_encode <- function(data, dico) {
  ### First we provide attribute label to variable name
  #data1 <- data
  #data <- data
  data.label <- as.data.frame(names(data))
  names(data.label)[1] <- "fullname"
  data.label <- join (x=data.label, y=dico, by="fullname", type="left" )
  ## Now we can also re-encode the records themself

  
  ###### Case 1: Re-encoding  when we have select_multiple 
  ## List of select one and select multiple variable to re-encode ## "select_one",
  selectdf <- as.data.frame(dico[dico$type %in% c( "select_multiple"), c("fullname","name","listname","type")])
  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf2 <- join(y=check, x=selectdf, by="fullname", type="left")
  selectdf3 <- selectdf[!is.na(selectdf2$id), ]
  
  if(nrow(selectdf3)==0){
    cat("There's no disagreggated select_multiple variables to re-encoded \n") 
  } else{ 
      #names(selectdf)[1] <- "selectvar"
    
      for (i in 1:nrow(selectdf)) {
        #i <-228
        #i <-1
        #i <-195
        #cat(i)
        fullname <- as.character(selectdf [ i,1])
        variablename <- as.character(selectdf [ i,2])
        variablelistname <- as.character(selectdf [ i,3])
        
        variablelevel <- as.data.frame(levels(as.factor(data[ ,fullname])))
        names(variablelevel)[1] <- "namecoded"
        variablecode <- as.character(levels(as.factor(variablelevel$namecoded)))
    
        if (nrow(variablelevel)>0) {
        variablelevel <- cbind(variablelevel,fullname,variablename,variablelistname)
        variablelevel <- join (x=variablelevel, y=dico, by="fullname", type="left" )
        labelchoice <- as.character(dico[dico$fullname==fullname, c("labelchoice")])
        data[ , fullname][data[ , fullname]==variablecode] <- labelchoice
        #View(data[i])
        } else { cat(paste0("The following variable has no answers to recode in the dataset: ",fullname, "\n")) }
        
        rm(fullname, variablename, variablelistname,variablelevel)
      }
   }

  ###### Case 2: Re-encoding  when we have disaggregated select_one - select_one_d
  ## List of select one and select multiple variable to re-encode ## "select_one",
  selectdf <- as.data.frame(dico[dico$type %in% c( "select_one_d"), c("fullname","name","listname","type")])
  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf2 <- join(y=check, x=selectdf, by="fullname", type="left")
  selectdf3 <- selectdf[!is.na(selectdf2$id), ]
  
  if(nrow(selectdf3)==0) {
    cat("There's no disaggregated select_one variable to re-encoded \n") 
  } else{
    #names(selectdf)[1] <- "selectvar"
    
    for (i in 1:nrow(selectdf)) {
      #i <-228
      #i <-1
      #i <-195
      #cat(i)
      fullname <- as.character(selectdf [ i,1])
      variablename <- as.character(selectdf [ i,2])
      variablelistname <- as.character(selectdf [ i,3])
      
      variablelevel <- as.data.frame(levels(as.factor(data[ ,fullname])))
      names(variablelevel)[1] <- "namecoded"
      variablecode <- as.character(levels(as.factor(variablelevel$namecoded)))
      
      if (nrow(variablelevel)>0) {
        variablelevel <- cbind(variablelevel,fullname,variablename,variablelistname)
        variablelevel <- join (x=variablelevel, y=dico, by="fullname", type="left" )
        labelchoice <- as.character(dico[dico$fullname==fullname, c("labelchoice")])
        data[ , fullname][data[ , fullname]==variablecode] <- labelchoice
        #View(data[i])
      } else { cat(paste0("The following variable has no answers to recode in the dataset: ",fullname, "\n")) }
      
      rm(fullname, variablename, variablelistname,variablelevel)
    }
  }
  
  
  ###### Case 3: Re-encoding  when we have select_select_one
  ## List of select one and select multiple variable to re-encode ## "select_one",
  selectdf <- as.data.frame(dico[dico$type %in% c( "select_one"), c("fullname","name","listname","type")])
  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf2 <- join(y=check, x=selectdf, by="fullname", type="left")
  selectdf3 <- selectdf[!is.na(selectdf2$id), ]
  #names(selectdf)[1] <- "selectvar"
  
  if(nrow(selectdf3)==0) {
    cat("There's no  select_one variables to re-encoded \n") 
  } else {
    #names(selectdf)[1] <- "selectvar"
    
      cat("There's a few  select_one variables to re-encoded \n") 
  #  for (i in 1:nrow(selectdf)) {
  #    #i <-228
  #    #i <-1
  #    #i <-195
  #    #cat(i)
  #    fullname <- as.character(selectdf [ i,1])
  #    variablename <- as.character(selectdf [ i,2])
  #    variablelistname <- as.character(selectdf [ i,3])
  #    
  #    variablelevel <- as.data.frame(levels(as.factor(data[ ,fullname])))
  #    names(variablelevel)[1] <- "namecoded"
  #    #variablecode <- as.character(levels(as.factor(variablelevel$namecoded)))
  #    if (nrow(variablelevel)>0) {
  #      variablelevel <- cbind(variablelevel,fullname,variablename,variablelistname)
  #      variablelevel <- join (x=variablelevel, y=dico, by="fullname", type="left" )
  #      labelchoice <- as.character(dico[dico$fullname==fullname, c("labelchoice")])
  #      data[ , fullname][data[ , fullname]==variablecode] <- labelchoice
  #      #View(data[i])
  #    } else { cat(paste0("The following variable has no answers to recode in the dataset: ",fullname, "\n")) }
  #    
   #   rm(fullname, variablename, variablelistname,variablelevel)
  #  }
  } 
  ###### Case 4: Re-encoding  when we have select_select_multiple
  ## List of select one and select multiple variable to re-encode ## "select_one",
  selectdf <- as.data.frame(dico[dico$type %in% c( "select_multiple_d"), c("fullname","name","listname","type")])
  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf2 <- join(y=check, x=selectdf, by="fullname", type="left")
  selectdf3 <- selectdf[!is.na(selectdf2$id), ]
  #names(selectdf)[1] <- "selectvar"
  
  if(nrow(selectdf3)>=1) { 
    cat("Uhmm you have concatenated select_multiple. This case is not handled yet. \n") 
  } else{   cat("No concatenated select_multiple. Better like this! \n")    }
  
return(data)
}
NULL
