#' @name kobo_correlation
#' @rdname kobo_correlation
#' @title  Generate histogramm plots based on dates
#'
#' @description  Automatically generate maps for all nominal & ordinal variables based on dates. ggplot2 is used.
#'
#'
#' @param data kobodatset to use
#' @param dico ( generated from kobo_dico)
#'
#'
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_correlation()
#'
#' @export kobo_correlation
#'
#' @examples
#' \dontrun{
#' kobo_correlation(S)
#' }
#'
#'

kobo_correlation <- function() {

  source("code/0-config.R")
  data <- read.csv(path.to.data,sep = ";")
  dico <- read.csv(path.to.dico,sep = ",")

  mainDir <- "out"
  subDir <- "correlation"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("correlation directory exists in out directory and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("correlation directory exists in your out directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("correlation directory does not exist in your out directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }


  ## Check that those variable are in the dataset
  selectdf <- dico[dico$type=="integer" , c("fullname","listname","label","name","variable","disaggregation","correlate")]
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf <- join(x=selectdf, y=check, by="fullname",  type="left")
  selectdf <- selectdf[!is.na(selectdf$id), ]

  ## now correct list of variables
  selectinteger <- as.character(selectdf[, c("fullname")])
  ## df of variable to loop around
  selectintegert <- as.data.frame(selectinteger)

  ## get list of variables used for faceting
  selectcorrel <- as.character(selectdf[selectdf$correlate!="" , c("fullname")])
  selectcorrel <- selectcorrel[!is.na(selectcorrel)]
  selectcorrelt <- as.data.frame(selectcorrel)

  if(length(selectcorrel)==0) {
    cat("There's no variable to be correlated in your data analysis plan.\n")
  } else {  cat(paste0( length(selectcorrel) , " variable(s) to correlate in your data analysis plan. Let's proceed! \n"))

      ## subset data with selectone
      data.integer <- data [ selectinteger ]
      ## force to data frame
      data.integer <- as.data.frame(data.integer)

      ## Remove variable where we get only NA
      data.integer <- data.integer[,colSums(is.na(data.integer))<nrow(data.integer)]
      data.integer <- kobo_label(data.integer, dico)


        ## loop around the list of variables to facet
        for (i in 1:nrow(selectcorrelt) ) {
          # i <-3
            correlname <- as.character(selectcorrelt[i ,1])
            correllabel <- as.character(dico[dico$fullname==correlname,c("label")])

              ### Now let's create dot plot
              for (j in 1:nrow(selectintegert) ) {
                # j <-7
                variablename <- as.character(selectintegert[j,1])
                variablelabel <- as.character(dico[dico$fullname==variablename,c("label")])

                ## print sutff
                cat(paste(i," correl ",correllabel," - ",correlname," \n"  ))
                cat(paste(j," variable ",variablelabel," - ",variablename," \n"))
                ### testing that the variable to map is not the same than the variable to correlate!
                   if(correlname==variablename){
                     cat("Next. \n")
                        } else {
                        ## and now the graph
                          data.integer[ , i] <- as.integer(data.integer[ , i])
                          data.integer[ , j] <- as.integer(data.integer[ , j])

                          theme_set(theme_gray(base_size = 20))

                            ggplot(data.integer, aes(x=data.integer[ , i], y=data.integer[ , j])) +
                              geom_count(aes(size = ..prop.., group = 1)) +
                              scale_size_area(max_size = 10)+
                              guides(fill=FALSE) +
                              xlab(correllabel) +
                              ylab(variablelabel) +
                              geom_smooth(method=lm) +  # Add a loess smoothed fit curve with confidence region
                              ggtitle("Correlation")+
                              theme(plot.title=element_text(face="bold", size=9),
                                    plot.background = element_rect(fill = "transparent",colour = NA))
                            ggsave(filename=paste("out/correlation/correlation_",variablename,"_correl_",correlname,".png",sep=""), width=10, height=10,units="in", dpi=300)

                            cat(paste0("Generated bar chart for question: ", correllabel ," - in correlation with - ",variablelabel,"\n"))

                           rm(variablename)
                        }
                }
          rm(correlname)
        }
    }
}
NULL

