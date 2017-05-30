#' @name kobo_bar_one
#' @rdname kobo_bar_one
#' @title  Generate bar Chart - frequency - for select_one questions
#'
#' @description  Automatically generate bar chart for each of the select_one question in the dataset. ggplot2 is used.
#'
#'
#' @param data .
#' @param dico ( generated from kobo_dico)
#'
#'
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_bar_one()
#'
#' @export kobo_bar_one
#' @examples
#' \dontrun{
#' kobo_bar_one(data, dico)
#' }
#'
#'

kobo_bar_one <- function(data, dico) {
  
  mainDir <- "out"
  subDir <- "bar_one"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("bar_one directory exists in out directory and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("bar_one directory exists in your out directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("bar_one directory does not exist in your out directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }
  

    ## get list of all nominal variables
    selectone <- as.character(dico[dico$type=="select_one", c("fullname")])
    
    ## Check that those variable are in the dataset
    selectdf <- dico[dico$type=="select_one"  , c("fullname","listname","label","name","variable","disaggregation","qrepeat")]
    
    check <- as.data.frame(names(data))
    names(check)[1] <- "fullname"
    check$id <- row.names(check)
    selectdf2 <- join(x=selectdf, y=check, by="fullname",  type="left")
    selectdf3 <- selectdf2[!is.na(selectdf2$id), ]
    selectone <- as.character(selectdf3[, c("fullname")])
    
    
    selectonet <- as.data.frame(selectone)
    
    if ( nrow(selectonet)==0){cat("There's no select_one variable in your dataset.\n") 
    } else{

    data.single <- data [ selectone ]
    ## Remove variable where we get only NA
    #data.single <- data.single[,colSums(is.na(data.single))<nrow(data.single)]
    
    ## force to data frame 
    data.single <- as.data.frame(data.single)
    
    #str(data.single)
    
    data.single <- kobo_label(data.single, dico)

    

  ### Now let's create proportion graphs -- bar chart
  for (i in 1:nrow(selectonet) ) {
    # i <-1
    variablename <- names(data.single)[i]
    title <- attributes(data.single)$variable.labels[i]

    ##### Set up factor level order --

    ## variable ordinal or not

    ## if variable is not ordinal, Proportion table used to order the levels of the factor
    
    frequ <- table (data.single[ , i])
    
    
    #frequ <- as.data.frame(table(data.single[ , i]))
    #frequ <- as.data.frame(table(data.single[[i]]))
    
    data.single[ , i] <- factor(data.single[ , i], levels=names(frequ[order(frequ, decreasing = TRUE)]))
    
    totalanswer <- nrow(data.single)
    ## subsetting to those who replied
    
    data.single1 <- data.single[ !(is.na(data.single[ ,i])), ]
    
    percentreponse <- paste(round((nrow(data.single1)/totalanswer)*100,digits=1),"%",sep="")
    

    #levels(data.single[ , i])
    
    ## and now the graph
    plotfreq <- ggplot(data.single1, aes(data.single1[ , i])) +
      geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))),
               fill="#2a87c8",colour="#2a87c8") +
      #facet_wrap(~subgov, ncol=4) +
	  #    geom_text(aes(label = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))), hjust = 1.1, color = "#FFFFFF") +
      guides(fill=FALSE) +
      ylab("Frequency") +
      scale_y_continuous(labels=percent)+
      xlab("") +
      coord_flip() +
      # coord_fixed() + ##used to maintain the adspect ratio of the plot when it needs to be saved
      ggtitle(title,  
              subtitle = paste0("Select_one question: Response rate to this question is ",percentreponse," of the total."))+
      
      theme(plot.title=element_text(face="bold", size=9),
            plot.background = element_rect(fill = "transparent",colour = NA))
    ggsave(filename=paste("out/bar_one/bar_onefreq_",variablename,".png",sep=""), plot=plotfreq, width=10, height=10,units="in", dpi=300)

    cat(paste0("Generated bar chart for question: ", title , "\n"))

    rm(variablename, freq)
    }
  }

}
NULL
