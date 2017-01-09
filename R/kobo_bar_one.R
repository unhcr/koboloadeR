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
#' @return Save in the out folder one bar plot per select_one variable
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
#' @export plots
#'

kobo_bar_one <- function(data, dico) {

    ## get list of all nominal variables
    selectone <- as.character(dico[dico$type=="select_one", c("fullname")])
    selectonet <- as.data.frame(selectone)


    data.single <- data [ selectone ]
    ## Remove variable where we get only NA
    data.single <- data.single[,colSums(is.na(data.single))<nrow(data.single)]
    data.single <- kobo_label(data.single, dico)


  ### Now let's create proportion graphs -- bar chart
  for (i in 1:nrow(selectonet) ) {
    # i <-2
    variablename <- names(data.single)[i]
    title <- attributes(data.single)$variable.labels[i]

    ## Proportion table used to order the levels of the factor
    frequ <- table (data.single[ , i])
    data.single[ , i] <- factor(data.single[ , i], levels=names(frequ[order(frequ, decreasing = TRUE)]))

    ## and now the graph
    plotfreq <- ggplot(data.single, aes(data.single[ , i])) +
      geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))),
               fill="#2a87c8",colour="#2a87c8") +
      #facet_wrap(~subgov, ncol=4) +
      guides(fill=FALSE) +
      ylab("Frequency") +
      xlab("") +
      coord_flip() +
      # coord_fixed() + ##used to maintain the adspect ratio of the plot when it needs to be saved
      ggtitle(title)+
      theme(plot.title=element_text(face="bold", size=9),
            plot.background = element_rect(fill = "transparent",colour = NA))
    ggsave(filename=paste("out/bar_onefreq_",variablename,".png",sep=""), plot=plotfreq, width=8, height=10,units="in", dpi=300)
    
    cat(paste0("Generated graph for question: ", title , "\n"))

    rm(variablename, freq)
  }

}
NULL
