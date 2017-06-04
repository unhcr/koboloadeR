#' @name kobo_histo_print
#' @rdname kobo_histo_print
#' @title  Generate histograme for all integer questions
#'
#' @description  Automatically generate histogrammes for each of the integer questions in the dataset. ggplot2 is used.
#'
#' @param data kobodatset to use
#' @param dico ( generated from kobo_dico)
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_histo_print()
#'
#' @export kobo_histo_print
#'
#' @examples
#' \dontrun{
#' kobo_histo_print(data, dico)
#' }
#'
#'

kobo_histo_print <- function(data, dico) {

  selectdf <- dico[dico$type=="integer", c("fullname","listname","label","name","qrepeat","type")]


  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf <- join(x=selectdf, y=check, by="fullname",  type="left")
  selectdf <- selectdf[!is.na(selectdf$id), ]

  if (nrow(selectdf)==0){
    cat("There's no integer variables. \n")
  } else{

  selectinteger <- as.character(selectdf[, c("fullname")])
  data.integer <- data [selectinteger  ]

  ## force to data frame
  data.integer <- as.data.frame(data.integer)
  data.integer  <- kobo_label(data.integer, dico)

        for (i in 1:nrow(selectdf) ) {
        #  for (i in 1:2 ) {
          # i <- 67
          variablename <- names(data.integer)[i]
          title <- attributes(data.integer)$variable.labels[i]

          ## Ensure that the variable is recognised as integer
          data.integer[ , i] <- as.integer(data.integer[ , i])
          #  regular histogram
         plot <-  ggplot(data=data.integer, aes(data.integer[ , i])) +
            geom_histogram(fill="#2a87c8",colour="white", binwidth = 1) +
            ggtitle(title)+
            labs(x="", y="Count")+
            #scale_x_discrete() +
            scale_x_continuous(breaks= pretty_breaks()) +
            theme(plot.title=element_text(face="bold", size=9),
                  plot.background = element_rect(fill = "transparent",colour = NA))
          print(plot)
          ##ggsave(filename=paste("out/histo/histo_",variablename,".png",sep=""), width=10, height=10,units="in", dpi=300)
          ##cat(paste0("Generated histogramme for question: ", title , "\n"))
          }


  }
}
NULL
