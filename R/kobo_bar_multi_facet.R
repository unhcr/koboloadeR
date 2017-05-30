#' @name kobo_bar_multi_facet
#' @rdname kobo_bar_multi_facet
#' @title  Generate frequency bar chart for select_multiple variable
#'
#' @description  Automatically generate faceted chart for select multiple variables. ggplot2 is used.
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
#' kobo_bar_multi_facet()
#'
#' @export kobo_bar_multi_facet
#'
#' @examples
#' \dontrun{
#' kobo_bar_multi_facet(data,  dico)
#' }
#'
#'

kobo_bar_multi_facet <- function(data,  dico) {

  mainDir <- "out"
  subDir <- "facet_multi"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("facet_multi directory exists in out directory and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("facet_multi directory exists in your out directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("facet_multi directory does not exist in your out directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }

  selectdf <- dico[dico$type=="select_multiple", c("fullname","listname","label","name","variable","disaggregation")]


  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf <- join(x=selectdf, y=check, by="fullname",  type="left")
  selectdf <- selectdf[!is.na(selectdf$id), ]

  ## Check if disagreggated select_multiple
  if (nrow(selectdf)==0){
    cat("There's no disagreggated select_multiple variables. \n")
  } else{
    ## get list of variables used for faceting
    selectfacet <- as.character(selectdf[selectdf$disaggregation=="facet" , c("fullname")])
    selectfacet <- selectfacet[!is.na(selectfacet)]

    ## Check if variables to facet
    if(length(selectfacet)==0) {
      cat("There's no variable to facet in your data analysis plan.\n")
    } else {  cat(paste0( length(selectfacet) , " variable(s) to facet in your data analysis plan. Let's proceed! \n"))

        selectfacett <- as.data.frame(selectfacet)


        selectmulti <- as.character(selectdf[, c("fullname")])
        data.selectmulti <- data [selectfacet, selectmulti ]
        data.selectmulti  <- kobo_label(data.selectmulti, dico)


        listmulti <- dico[dico$type=="select_multiple_d", c("listname","label","name","fullname","variable","disaggregation","qrepeat")]
        selectdf1 <- as.data.frame(unique(selectdf$listname))
        names(selectdf1)[1] <- "listname"
        listmulti <- join(x=listmulti, y=selectdf1, by="listname", type="left")

        ## loop around listname
        for (i in 1:nrow(listmulti) ) {
          # i <- 6
          listloop <- as.character(listmulti[i,1])
          listlabel <-  as.character(listmulti[i,2])


          ### select variable for a specific multiple questions
          selectmultilist <- as.character(dico[dico$type=="select_multiple" & dico$listname==listloop , c("fullname")])

          ## Check that those variable are in the dataset
          selectdf <- dico[dico$type=="select_multiple" & dico$listname==listloop , c("fullname","listname","label","name","variable","disaggregation")]
          selectdf2 <- join(x=selectdf, y=check, by="fullname",  type="left")
          selectdf2 <- selectdf2[!is.na(selectdf2$id), ]

          ### Check that list are in dataset
          if (nrow(selectdf2)==0){ cat("passing \n")
          } else {

            selectmultilist <- as.character(selectdf2[, c("fullname")])

            ## loop around the list of variables to facet
            for (j in 1:nrow(selectfacett) ) {
              # j <- 2
              facetname <- as.character(selectfacett[j,1])
              facetlabel <- as.character(dico[dico$fullname==facetname,c("label")])

                    ## Reshape answers
                    data.selectmultilist <- data.selectmulti[facetname , selectmultilist ]
                    data.selectmultilist$id <- rownames(data.selectmultilist)

                    totalanswer <- nrow(data.selectmultilist)
                    ## subsetting to those who replied

                    data.selectmultilist <- data.selectmultilist[ data.selectmultilist[ ,1]!="Not replied", ]

                    percentreponse <- paste(round((nrow(data.selectmultilist)/totalanswer)*100,digits=1),"%",sep="")


                    meltdata <- melt(data.selectmultilist,id="id")

                    castdata <- as.data.frame(table(meltdata[c("value")])) #,  useNA = "ifany"
                    castdata$freqper <- castdata$Freq/nrow(data.selectmultilist)

                    castdata <- castdata[castdata$Var1!="Not selected", ]
                    #castdata <- dcast(meltdata, value~variable, fun.aggregate = length)
                    castdata$Var1 <-factor(castdata$Var1, levels=castdata[order(castdata$freqper), "Var1"])

                    #levels(castdata$Var1)
                    castdata <- castdata[castdata$Var1!="", ]

                    ggplot(castdata, aes(x=Var1, y=freqper)) +
                      geom_bar(fill="#2a87c8",colour="#2a87c8",stat = "identity") +
                      xlab("") + ylab("")+
                      scale_y_continuous(labels=percent)+
                      coord_flip()+
                      facet_wrap(as.formula(paste("~", facetname)), ncol=2) +
                      ggtitle(listlabel, subtitle = paste0("Facetted by question: ",facetlabel,"\n",
                                                           "select_multiple question: Response rate to this question is ",percentreponse," of the total.")) +
                      theme(plot.title=element_text(face="bold", size=9),
                            plot.background = element_rect(fill = "transparent",colour = NA))

                      ggsave(filename=paste("out/facet_multi/bar_multifreq_",variablename,"_facet_",facetname,".png",sep=""), width=10, height=10,units="in", dpi=300)
                      cat(paste0("Generated bar chart for question: ",i, " ", listlabel ," - with facet on - ",j, " ",facetlabel,"\n"))
                      rm(variablename, castdata)

                  }
                  ### End loop around facet
                }
                ### Check that list are in dataset
              }
             ### End loop around listname
        }
        ### Test if facet in dico
  }
  ### test if disagreggated select_multiple
}
NULL

