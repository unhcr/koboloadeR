#' @name kobo_histo
#' @rdname kobo_histo
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
#' kobo_histo()
#'
#' @export kobo_histo
#'
#' @examples
#' \dontrun{
#' kobo_histo(data, dico)
#' }
#'
#'

kobo_histo <- function(data, dico) {


  mainDir <- "out"
  subDir <- "histo"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("histo directory exists in out directory and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("histo directory exists in your out directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("histo directory does not exist in your out directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }


  selectdf <- dico[dico$type=="integer", c("fullname","listname","label","name")]

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
  data.integer  <- kobo_label(data.integer, dico)

  for (i in 1:nrow(selectdf) ) {
    # i <- 2
    variablename <- names(data.integer)[i]
    title <- attributes(data.integer)$variable.labels[i]

    ## Ensure that the variable is recognised as integer
    data.integer[ , i] <- as.integer(data.integer[ , i])
    #str(data.integer[ , i])

    #  regular histogram
    ggplot(data=data.integer, aes(data.integer[ , i])) +
      geom_histogram(fill="#2a87c8",colour="white", binwidth = 1) +
      ggtitle(title)+
      labs(x="", y="Count")+
      theme(plot.title=element_text(face="bold", size=9),
            plot.background = element_rect(fill = "transparent",colour = NA))
    ggsave(filename=paste("out/histo/histo_",variablename,".png",sep=""), width=10, height=10,units="in", dpi=300)

    # trendline on histogram by adding geom_density
      ggplot(data=data.integer, aes(data.integer[ , i])) +
      geom_histogram(aes(y =..density..), fill="#2a87c8",colour="white", alpha = .6, binwidth = 1) +
      geom_density(col=2) +
      ggtitle(title)+
      labs(x="", y="Frequency")+
        theme(plot.title=element_text(face="bold", size=9),
              plot.background = element_rect(fill = "transparent",colour = NA))
      ggsave(filename=paste("out/histo/histodensity_",variablename,".png",sep=""), width=10, height=10,units="in", dpi=300)

    cat(paste0("Generated histogramme for question: ", title , "\n"))
  }
  }
}
NULL
