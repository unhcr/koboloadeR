#' @name kobo_bar_one
#' @rdname kobo_bar_one
#' @title  Generate bar Chart - frequency - for select_one questions

#' @description  Automatically generate bar chart for each of the select_one question in the dataset. ggplot2 is used.
#'
#'
#' @param mainDir Path to the project's working directory: mainly for shiny app
#'
#'
#' @author Edouard Legoupil, Elliott Messeiller
#'
#' @examples
#' kobo_bar_one()
#'
#' @export kobo_bar_one
#' @examples
#' \dontrun{
#' kobo_bar_one()
#' }
#'
#'

kobo_bar_one <- function(mainDir='') {
  if (mainDir==''){
    mainDir <- getwd()
  }

  source(paste0(mainDir,"/code/0-config.R"), local=TRUE)


  ## Check that variable are in the dataset
  selectdf <- dico[dico$type=="select_one"  , c("fullname","listname","label","name","disaggregation","labelchoice")]

  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf2 <- join(x=selectdf, y=check, by="fullname",  type="left")
  selectdf3 <- selectdf2[!is.na(selectdf2$id), ]
  selectone <- as.character(selectdf3[, c("fullname")])

  selectonet <- as.data.frame(selectone)

  selectfacet <- as.character(selectdf[selectdf$disaggregation!="" , c("fullname")])
  selectfacet <- selectfacet[!is.na(selectfacet)]

  # Replacing names by labels
  selectchoices_questions <- dico[dico$type=="select_one_d"  , c("listname","name","labelchoice")]
  selectchoices <- unique(dico[dico$type=="select_one_d"  , c("listname","name","labelchoice")])

  selectoneans <-(dico[dico$type=="select_one_d", c("fullname","name","listname")])
  short_ans <- paste(sapply(strsplit(as.character(selectoneans$fullname),".",fixed = TRUE),"[[",1), sapply(strsplit(as.character(selectoneans$fullname),".",fixed = TRUE),"[[",2), sep = ".")
  selectchoices_questions$qname <- short_ans


    data.single <- data.frame(data [selectone])

      for (j in 1:ncol(data.single)){
        data.single[,j] <- data.frame(selectchoices[,3][match(data.single[,j],selectchoices[,2])], stringsAsFactors = FALSE)
        data.single[,j] <- factor(data.single[,j])
      }


    ## Remove variable where we get only NA
    data.single <- data.single[,colSums(is.na(data.single))<nrow(data.single)]

    ## force to data frame
    data.single <- as.data.frame(data.single)

    #str(data.single)

    data.single <- kobo_label(data.single, dico)

    data.single[data.single==""]<-NA



    ### Now let's create proportion graphs -- bar chart
    for (i in 1:ncol(data.single) ) {
      variablename <- names(data.single)[i]
      title <- attributes(data.single)$variable.labels[i]
      ordinal <- as.character(dico[dico$fullname==variablename,c("ordinal")])



      if(sum(is.na(data.single[,i]))==length(data.single[,i])){
        cat("There's no select_one variable in your dataset.\n")
        print(i)
      } else{

      if (usedweight=="sampling_frame"){
        frequ <- data.frame(svytable(~data.single[[i]], surveydesign))
      }
      else{
        frequ<-data.frame(table(data.single[,i]))
      }
      names(frequ)<- c("Var1","Freq")

      frequ$freqper <- as.numeric(frequ$Freq/sum(frequ$Freq))
      frequ$Var1 = str_wrap(frequ$Var1,width=15)


      totalanswer <- nrow(data.single)

      count_replied <- (sum(!is.na(data.single[,i ])))

      percentresponse <- paste(round((count_replied/totalanswer*100),digits=2),"%",sep="")

      if (is.na(ordinal)==T | ordinal==""){
        frequ$Var1<-factor(frequ$Var1, levels = frequ$Var1[order(frequ$freqper)])
      }else{
        ordinal_choices <- as.character(selectchoices_questions[selectchoices_questions$qname==variablename,c("labelchoice")])
        frequ$Var1 <- reorder.factor(frequ$Var1, new.order=ordinal_choices)
        frequ %>% arrange(Var1)
      }

      theme_set(theme_gray(base_size = 20))
      color<-"#2a87c8"


      ## and now the graph
      plotfreq <- ggplot(frequ, aes(x= Var1, y=freqper)) +
        geom_bar(fill=color,colour=color,stat = "identity") +
        geom_text(aes(label=paste(round(frequ$freqper*100),"%",sep=""), hjust = -0.5))+
        #facet_wrap(~subgov, ncol=4) +
        ylab("Frequency") +
        scale_y_continuous(labels=percent, limits = c(0,1))+
        scale_fill_brewer("PuBu")+
        xlab("") +
        coord_flip() +
        ggtitle(str_wrap(title,width=50))+
        theme( plot.title=element_text(face="bold", size=20),
              plot.background = element_rect(fill = "transparent",colour = NA))
      ggsave(filename=paste(mainDir,"/out/bar_one/",variablename,"_bar_one.png",sep=""), width=10, height=10,units="in", dpi=300)

      cat(paste0("Generated bar chart for question: ", title , "\n"))

      #Writting file

      #rm(variablename,frequ)
      }
    }

    cat(" \n")
    cat(" \n")
    cat(" ###########################################################\n")
    cat(" # The bar charts for select_one questions were generated! #\n")
    cat(" # You can find them in the folder 'out/bar_one'!          #\n")
    cat(" ###########################################################\n")
    if(length(selectfacet)!=0) {
      cat(" \n")
      cat(" ###########################################################\n")
      cat(" # Variable(s) to disaggregate in your data analysis plan. # \n")
      cat(" # Run kobo_bar_one_facet() !                              # \n")
      cat(" ###########################################################\n")

    }


}
NULL
