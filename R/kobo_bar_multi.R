#' @name kobo_bar_multi
#' @rdname kobo_bar_multi
#' @title  Generate bar Chart - frequency - for select_multiple questions
#'
#' @description  Automatically generate bar chart for each of the select_multiple question in the dataset. ggplot2 is used.
#'
#'
#' @param mainDir Path to the project's working directory: mainly for proper shiny app path
#'
#'
#' @author Edouard Legoupil, Elliott Messeiller
#'
#' @examples
#' kobo_bar_multi()
#'
#' @export kobo_bar_multi
#'
#' @examples
#' \dontrun{
#' kobo_bar_multi()
#' }
#'
#'

kobo_bar_multi <- function(mainDir='') {

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

  if (nrow(selectdf)==0){
    cat("There's no select_multiple questions \n")
  } else{


    selectmulti <- as.character(selectdf[, c("fullname")])
    data.selectmulti <- data [selectmulti ]
    data.selectmulti  <- kobo_label(data.selectmulti, dico)

    listmulti <- dico[dico$type=="select_multiple_d", c("listname","label","name","fullname","disaggregation", "qlevel")]
    selectdf1 <- as.data.frame(unique(selectdf$listname))
    names(selectdf1)[1] <- "listname"
    listmulti <- join(x=listmulti, y=selectdf1, by="listname", type="left")

    listmultichoice <- dico[dico$type=="select_multiple", c("listname","label","name","fullname","disaggregation","labelchoice")]


    for (i in 1:nrow(listmulti) ) {
      listloop <- as.character(listmulti[i,1])
      listlabel <-  as.character(listmulti[i,2])
      listfullname <- as.character(listmulti[i,"fullname"])

      ### select variable for a specific multiple questions


      ## Check that those variable are in the dataset
      selectdf <- dico[dico$type=="select_multiple" & dico$listname==listloop & dico$qlevel==listfullname , c("fullname","listname","label","name","disaggregation","labelchoice")]
      selectdf2 <- join(x=selectdf, y=check, by="fullname",  type="left")
      selectdf2 <- selectdf2[!is.na(selectdf2$id), ]

      listlabelchoice <- as.character(selectdf2[,"labelchoice"])
      ordinal <- as.character(dico[dico$type=="select_multiple_d" & dico$listname==listloop,c("ordinal")])



      # If no answers to this question, passing to the next select_multiple
      if (nrow(selectdf2)==0){ cat("passing \n")
      }
      else {

        # Listing the choices to the question
        selectmultilist <- as.character(selectdf2[, c("fullname")])

        ## Reshape answers
        # Selecting only the answers to this question
        data.selectmultilist <- data.selectmulti[selectmultilist]
        #Getting ride of unselected choices
        data.selectmultilist <- data.selectmultilist[, colSums(!is.na(data.selectmultilist)) != 0]


        if (ncol(data.selectmultilist)==0){ cat("passing \n")
        }
        else{


        names(data.selectmultilist) <- listlabelchoice




        #Count total answer (for the survey) and answered to this question

        totalanswer <- nrow(data.selectmulti)
        count_replied <- as.numeric(sum(!is.na(data.selectmultilist[,1 ])))


        ## subsetting those who replied

        percentresponse <- paste(round((count_replied/totalanswer)*100,digits=2),"%",sep="")

        if (usedweight=="sampling_frame"){
          data.selectmultilist$weight <- data$weight
          meltdata <- melt(data.selectmultilist,id="weight")
          meltdata$value <- as.numeric(meltdata$value)

          castdata <- as.data.frame(table(meltdata[,c("value","variable","weight")]))
          castdata$Freq <- as.numeric(as.character(castdata$Freq))
          castdata$weight <- as.numeric(as.character(castdata$weight))
          castdata$freqper <- round((castdata$Freq*castdata$weight)/count_replied,digits=2)
        }
        else{
          data.selectmultilist$id <- rownames(data.selectmultilist)
          meltdata <- melt(data.selectmultilist,id="id")
          meltdata$value <- as.numeric(meltdata$value)

          castdata <- as.data.frame(table(meltdata[,c("value","variable")]))
          castdata$Freq <- as.numeric(as.character(castdata$Freq))
          castdata$freqper <- round((castdata$Freq)/count_replied,digits=2)

        }
        castdata <- castdata[castdata$Freq!=0, ]
        #castdata <- dcast(meltdata, value~variable, fun.aggregate = length)

        #levels(castdata$Var1)
        castdata <- castdata[castdata$value!=0, ]
        castdata<- ddply(castdata, "variable",numcolwise(sum))



        #castdata$variable<-factor(castdata$variable, levels = castdata$variable[order(castdata$freqper)])

        castdata <- arrange(castdata,freqper)

        castdata$variable = str_wrap(castdata$variable,width=15)
        castdata$variable <- factor(castdata$variable, levels=castdata$variable)


        theme_set(theme_gray(base_size = 20))


        ggplot(castdata, aes(x=variable, y=freqper)) +
          geom_bar(fill="#2a87c8",colour="#2a87c8",stat = "identity") +
          geom_text(aes(label=paste(round(freqper*100),"%",sep=""), hjust = -0.2))+
          xlab("") + ylab("")+
          scale_y_continuous(labels=percent, limits=c(0,1))+
          scale_fill_brewer("PuBu")+
          coord_flip()+
          ggtitle(str_wrap(listlabel,width=50))+
          theme(plot.title=element_text(face="bold", size=22),
                plot.background = element_rect(fill = "transparent",colour = NA))
        ggsave(filename=paste(mainDir, "/out/bar_multi/bar_multi_",listfullname,".png",sep=""), width=10, height=10,units="in", dpi=300)

        cat(paste0("Generated bar chart for question: ", listlabel , "\n"))
        }
      }



  }
  }

  cat(" \n")
  cat(" \n")
  cat(" ################################################################\n")
  cat(" # The bar charts for select_mutliple questions were generated! #\n")
  cat(" # You can find them in the folder 'out/bar_multi'!             #\n")
  cat(" ################################################################\n")




}
NULL
