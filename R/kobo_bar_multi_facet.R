#' @name kobo_bar_multi_facet
#' @rdname kobo_bar_multi_facet
#' @title  Generate frequency bar chart for select_multiple variable and save output as svg for illustrator
#'
#' @description  Automatically generate faceted chart for select multiple variables. ggplot2 is used.
#'
#'
#' @param mainDir Path to the project's working directory: mainly for proper shiny app path
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
#' kobo_bar_multi_facet()
#' }
#'
#'

kobo_bar_multi_facet <- function(mainDir='') {
  # Making your life easier by finding the dico and data from 0-config.R (created during kobo_project_config())
  if (mainDir==''){
    mainDir <- getwd()
  }

  source(paste0(mainDir,"/code/0-config.R"), local=TRUE)


  # List of select_multiple questions and choices
  selectdf <- dico[dico$type == "select_multiple", c("fullname","listname","label","name","disaggregation"), ]

  ### Verify that those variables are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf <- join(x=selectdf, y=check, by="fullname",  type="left")
  selectdf <- selectdf[!is.na(selectdf$id), ]

  allvar<-dico[, c("fullname","listname","label","name","disaggregation"), ]

  ## now correct list of variables
  selectone <- as.character(selectdf[selectdf$disaggregation!=""& selectdf$disaggregation!="weight", c("fullname")])
  ## df of variable to loop around
  selectonet <- as.data.frame(selectone)


  if (nrow(selectdf)==0){
    cat("There's no select_multiple questions \n")
  } else{

    ## get list of variables used for faceting
    selectfacet <- as.character(selectdf[selectdf$disaggregation!="" & selectdf$disaggregation!="weight", c("fullname")])
    selectfacet <- selectfacet[!is.na(selectfacet)]

    if(length(selectfacet)==0) {
      cat("There's no variable to disaggregate in your data analysis plan.\n")

    } else {  cat(paste0( length(selectfacet) , " variable(s) to disaggregate in your data analysis plan. Let's proceed! \n"))

      selectmulti <- as.character(selectdf[, c("fullname")])
      data.selectmulti <- data [selectmulti]
      data.selectmulti <- data [selectfacet]
      data.selectmulti  <- kobo_label(data.selectmulti, dico)

      selectfacett <- selectdf[selectdf$disaggregation!=""& selectdf$disaggregation!="weight", c("fullname","disaggregation")]
      single.facet <- as.data.frame(table(selectfacett[,2]))
      single.facet <- as.data.frame(single.facet[single.facet$Var1!="",c("Var1")])
      names(single.facet) <- "Var1"


      listmulti <- dico[dico$type=="select_multiple_d", c("listname","label","name","fullname","disaggregation")]
      selectdf1 <- as.data.frame(unique(selectdf$listname))
      names(selectdf1)[1] <- "listname"
      listmulti <- join(x=listmulti, y=selectdf1, by="listname", type="left")

      listmultichoice <- dico[dico$type=="select_multiple_d", c("listname","label","name","fullname","disaggregation","labelchoice")]

      for (i in 1:nrow(listmulti) ) {
        # i <- 7
        variablename <- as.character(listmulti[i,"fullname"])
        listloop <- as.character(listmulti[i,1])
        listlabel <-  as.character(listmulti[i,2])


        ### select variable for a specific multiple questions
        selectmultilist <- as.character(dico[dico$type=="select_multiple" & dico$listname==listloop & dico$label==listlabel, c("fullname")])

        ## Check that those variable are in the dataset
        selectdf <- dico[dico$type=="select_multiple" & dico$listname==listloop & dico$qlevel==variablename , c("fullname","listname","label","name","disaggregation","labelchoice")]
        selectdf2 <- join(x=selectdf, y=check, by="fullname",  type="left")
        selectdf2 <- selectdf2[!is.na(selectdf2$id), ]

        # If no answers to this question, passing to the next select_multiple
        if (nrow(selectdf2)==0){ cat("Only empty values, passing. \n")
        } else {

          listlabelchoice <- as.character(selectdf2[,"labelchoice"])
          selectmultilist <- as.character(selectdf2[, c("fullname")])
          data.selectmultilist <- data.selectmulti[selectmultilist]
          names(data.selectmultilist) <- listlabelchoice

          # Listing the choices to the question
          selectmultilist <- as.character(selectdf2[, c("fullname")])

          ## Reshape answers
          # Selecting only the answers to this question

          data.selectmultilist <- data.selectmultilist[, colSums(!is.na(data.selectmultilist)) != 0]
          if (ncol(data.selectmultilist)==0){ cat("Only empty values, passing. \n")
          }else{

          #Selecting only the answers selected at least once
          data.selectmultilist <- sapply(data.selectmultilist, as.numeric)

          data.selectmultilist <- data.frame(data.selectmultilist[, colSums(data.selectmultilist,na.rm=TRUE) != 0, drop=FALSE],check.names=FALSE)

            for (j in 1:nrow(single.facet) ) {


              if(listmultichoice[i,"disaggregation"]!=single.facet[j,1]){
              } else{

                facetname1 <- as.character(single.facet[j,1])
                facetname <- as.character(allvar[allvar$name==facetname1,c("fullname")])

                facetlabel <- as.character(dico[dico$fullname==facetname,c("label")])
                facetchoices <- dico[dico$name==facetname1, c("name","labelchoice","listname")]
                facetchoices <-dico[dico$listname==facetchoices[,3], c("name","labelchoice","listname")]
                facetchoices <- facetchoices[facetchoices$name!=facetname1, c("name","labelchoice","listname")]

                # Put ID to each row

                data.selectmultilist$id <- rownames(data.selectmultilist)

                if(usedweight=="sampling_frame"){
                  data.selectmultilist$weight <- data$weight
                  names(data.selectmultilist$weight) <- "weight"
                }

                data.selectmultilist[facetname] <- data[facetname]
                names(data.selectmultilist)[length(names(data.selectmultilist))] <- "facet"



                #Count total answer (for the survey) and answered to this question

                totalanswer <- nrow(data.selectmultilist)
                count_replied <- as.numeric(sum(!is.na(data.selectmultilist[,1 ])))

                percentresponse <- paste(round((count_replied/totalanswer)*100,digits=2),"%",sep="")

                if(usedweight=="sampling_frame"){

                  meltdata <- melt(data.selectmultilist,id=c("weight","id","facet"))

                  castdata <- as.data.frame(table(meltdata[,c("value","variable","facet","weight")]))
                  castdata$Freq <- as.numeric(as.character(castdata$Freq))
                  castdata$weight <- as.numeric(as.character(castdata$weight))
                  castdata$freqper <- round((castdata$Freq*castdata$weight)/count_replied,digits=2)
                }

                else{
                  meltdata <- melt(data.selectmultilist,id=c("id","facet"))

                  castdata <- as.data.frame(table(meltdata[,c("value","variable","facet")]))
                  castdata$Freq <- as.numeric(as.character(castdata$Freq))
                  castdata$freqper <- round((castdata$Freq)/count_replied,digits=2)
                }

                castdata <- castdata[castdata$value!=0, ]

                #combining values
                castdata<- ddply(castdata, c("variable","facet"),numcolwise(sum))

                castdata$variable = str_wrap(castdata$variable,width=15)

                background_rect <- data.frame(unique(castdata[,c("variable")]))
                names(background_rect) <- c("variable")
                background_rect$freqper <-1

                theme_set(theme_gray(base_size = 20)
                          )

                  ggplot(castdata,aes(x=variable, y=freqper)) +
                    geom_bar(data=background_rect,aes(x=variable),stat = "identity", alpha=0.2)+
                    geom_bar(stat = "identity", position="dodge",aes(fill=facet))+
                    geom_text(aes(label=paste(round(freqper*100),"%",sep=""), fill=facet, hjust = -0.5), position=position_dodge(width=0.8))+
                    xlab("") + ylab("")+
                    scale_y_continuous(labels=percent, limits = c(0,1))+
                    scale_fill_brewer(name=paste0(facetlabel),palette="PuBu")+
                    coord_flip()+
                    ggtitle(str_wrap(listlabel,width=50))+
                    theme(plot.title=element_text(face="bold", size=25))

                  ggsave(filename=paste(mainDir, "/out/disagg_multi/",variablename,"_bar_multi_disagg_",facetname,".png",sep=""), width=12, height=10,units="in", dpi=300)

                cat(paste0("Generated bar chart for question: ", listlabel , "\n"))
              }
            }
           }
        }
      }
    }
  }

  cat(" \n")
  cat(" \n")
  cat(" ###################################################################\n")
  cat(" # The bar charts for select_mutliple questions were generated!    #\n")
  cat(" # You can find them in the folder 'out/disagg_multi'!             #\n")
  cat(" ###################################################################\n")

}
NULL

