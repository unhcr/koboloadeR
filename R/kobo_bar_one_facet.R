#' @name kobo_bar_one_facet
#' @rdname kobo_bar_one_facet
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
#' kobo_bar_one_facet()
#'
#' @export kobo_bar_one_facet
#'
#' @examples
#' \dontrun{
#' kobo_bar_one_facet(data,  dico)
#' }
#'
#'

kobo_bar_one_facet <- function(data,  dico) {

  mainDir <- "out"
  subDir <- "facet_one"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("facet_one directory exists in out directory and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("facet_one directory exists in your out directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("facet_one directory does not exist in your out directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }

  ## get list of all nominal variables
  selectone <- as.character(dico[dico$type=="select_one", c("fullname")])

  ## Check that those variable are in the dataset
  selectdf <- dico[dico$type=="select_one" , c("fullname","listname","label","name","variable","disaggregation")]
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf <- join(x=selectdf, y=check, by="fullname",  type="left")
  selectdf <- selectdf[!is.na(selectdf$id), ]

  ## now correct list of variables
  selectone <- as.character(selectdf[, c("fullname")])
  ## df of variable to loop around
  selectonet <- as.data.frame(selectone)

  ## get list of variables used for faceting
  selectfacet <- as.character(selectdf[selectdf$disaggregation=="facet" , c("fullname")])
  selectfacet <- selectfacet[!is.na(selectfacet)]

  if(length(selectfacet)==0) {
    cat("There's no variable to facet in your data analysis plan.\n")
  } else {  cat(paste0( length(selectfacet) , " variable(s) to facet in your data analysis plan. Let's proceed! \n"))

      selectfacett <- as.data.frame(selectfacet)

      ## subset data with selectone
      data.single <- data [ selectone ]
      ## Remove variable where we get only NA
      data.single <- data.single[,colSums(is.na(data.single))<nrow(data.single)]
      data.single <- kobo_label(data.single, dico)


        ## loop around the list of variables to facet
        for (i in 1:nrow(selectfacett) ) {
          # i <-1
            facetname <- as.character(selectfacett[i,1])
            facetlabel <- as.character(dico[dico$fullname==facetname,c("label")])

            #facettitle <- attributes(data.single)$variable.labels[i]

              ### Now let's create proportion graphs -- bar chart
              for (i in 1:nrow(selectonet) ) {
                # i <-2
                variablename <- names(data.single)[i]
                title <- attributes(data.single)$variable.labels[i]

                ### testing that the variable to map is not the same than the variable to facet!
                   if(facetname==variablename){
                        } else {


                        ##### Set up factor level order --

                        ## variable ordinal or not

                        ## if variable is not ordinal, Proportion table used to order the levels of the factor
                        frequ <- table (data.single[ , i])
                        data.single[ , i] <- factor(data.single[ , i], levels=names(frequ[order(frequ, decreasing = TRUE)]))

                                ## and now the graph
                            ggplot(data.single, aes(data.single[ , i])) +
                              geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))),
                                       fill="#2a87c8",colour="#2a87c8") +
                              #facet_wrap(~subgov, ncol=4) +
                              guides(fill=FALSE) +
                              ylab("Frequency") +
                              scale_y_continuous(labels=percent)+
                              xlab("") +
                              facet_wrap(as.formula(paste("~", facetname)), ncol=2) +
                              coord_flip() +
                              # coord_fixed() + ##used to maintain the adspect ratio of the plot when it needs to be saved
                              ggtitle(title, subtitle = paste("Facetted by question: ",facetlabel,sep=""))+
                              theme(plot.title=element_text(face="bold", size=9),
                                    plot.background = element_rect(fill = "transparent",colour = NA))
                            ggsave(filename=paste("out/facet_one/bar_onefreq_",variablename,"_facet_",facetname,".png",sep=""), width=10, height=10,units="in", dpi=300)

                            cat(paste0("Generated bar chart for question: ", title ," - with facet on - ",facetlabel,"\n"))

                           rm(variablename, freq)
                        }
                }
          rm(facetname)
        }
    }
}
NULL

