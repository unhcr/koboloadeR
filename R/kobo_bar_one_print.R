#' @name kobo_bar_one_print
#' @rdname kobo_bar_one_print
#' @title  Generate bar Chart - frequency - for select_one questions and save output as svg for illustrator
#'
#' @description  Automatically generate bar chart for each of the select_one question in the dataset. Used in report
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
#' kobo_bar_one_print()
#'
#' @export kobo_bar_one_print
#' @examples
#' \dontrun{
#' kobo_bar_one_print(data, dico)
#' }
#'
#'

kobo_bar_one_print <- function(data, dico) {


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
    # i <-6
    variablename <- names(data.single)[i]
    title <- attributes(data.single)$variable.labels[i]

    ##### Set up factor level order --

    ## variable ordinal or not

    ## if variable is not ordinal, Proportion table used to order the levels of the factor

    frequ <- table (data.single[ , i])


    frequ1 <- as.data.frame(table(data.single[ , i]))
    #frequ <- as.data.frame(table(data.single[[i]]))

    data.single[ , i] <- factor(data.single[ , i], levels=names(frequ[order(frequ, decreasing = TRUE)]))

    totalanswer <- nrow(data.single)
    ## subsetting to those who replied

    data.single1 <- data.single[ !(is.na(data.single[ ,i])), ]

    percentreponse <- paste(round((nrow(data.single1)/totalanswer)*100,digits=1),"%",sep="")


    #levels(data.single[ , i])

    ## and now the graph
    plot <- plotfreq <- ggplot(data.single1, aes(data.single1[ , i])) +
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
  #  ggsave(filename=paste("out/bar_one/bar_onefreq_",variablename,".png",sep=""), plot=plotfreq, width=10, height=10,units="in", dpi=300)

   # cat(paste0("Generated bar chart for question: ", title , "\n"))
    print(plot)
    cat("\n")
    cat("\n")
    rm(variablename, freq)
    }
  }

}
NULL
