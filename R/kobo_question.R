#' @name kobo_question
#' @rdname kobo_question
#' @title  Generates graphics and basic information based on the type of question and if there's a disaggregation

#' @description  Automatically generates bar charts, histograms, and basic information of the question based on the question type to generate the report.
#'
#'
#' @param mainDir Path to the project's working directory: mainly for shiny app
#' @param question Name of the question to be treated, as in "fullname" column in dico
#'
#'
#' @author Elliott Messeiller
#'
#'
#' @export kobo_question
#' @examples
#' \dontrun{
#' kobo_question("s2.beneficiary_code")
#' }
#'
#'

kobo_question <- function(question,mainDir='') {
  # Source project config parameters
  if (mainDir==''){
    mainDir <- getwd()
  }
  source(paste0(mainDir,"/code/0-config.R"), local=TRUE)
  select_question <- dico[dico$fullname==question & dico$formpart=="questions", c("type","fullname","listname","label","name","disaggregation","labelchoice","ordinal")]
  select_question[select_question==""] <- NA

  variablename <- as.character(select_question$fullname)

  colour_palette <- RColorBrewer::brewer.pal(n=9,"PuBu")[9:3]
  #   select_one, no disaggregation

  if(select_question$type=="select_one" && is.na(select_question$disaggregation) ){

            ## Check that variable is in the dataset

            check <- as.data.frame(names(data))
            names(check)[1] <- "fullname"
            check$id <- row.names(check)
            selectdf2 <- plyr::join(x=select_question, y=check, by="fullname",  type="left")
            selectdf3 <- selectdf2[!is.na(selectdf2$id), ]
            selectone <- as.character(selectdf3[, c("fullname")])

            selectonet <- as.data.frame(selectone)

            # Replacing names by labels
            selectchoices_questions <- dico[dico$type=="select_one_d"  , c("listname","name","labelchoice")]
            selectchoices <- unique(dico[dico$type=="select_one_d"  , c("listname","name","labelchoice")])

            selectoneans <-(dico[dico$type=="select_one_d", c("fullname","name","listname")])
            short_ans <- paste(sapply(strsplit(as.character(selectoneans$fullname),".",fixed = TRUE),"[[",1), sapply(strsplit(as.character(selectoneans$fullname),".",fixed = TRUE),"[[",2), sep = ".")
            selectchoices_questions$qname <- short_ans


              data.single <- data.frame(data [selectone])
              names(data.single)[1] <- variablename

                for (j in 1:ncol(data.single)){
                  data.single[,j] <- data.frame(selectchoices[,3][match(data.single[,j],selectchoices[,2])], stringsAsFactors = FALSE)
                  data.single[,j] <- factor(data.single[,j])
                }


              ## Remove variable where we get only NA
              data.single <- data.single[,colSums(is.na(data.single))<nrow(data.single)]

              ## force to data frame
              data.single <- as.data.frame(data.single)

              #str(data.single)

              #data.single <- kobo_label(data.single, dico)

              data.single[data.single==""]<-NA



              ### Now let's create proportion graphs -- bar chart
              title <- as.character(select_question$label)
              ordinal <- as.character(select_question$ordinal)


                if (usedweight=="sampling_frame" ){
                    frequ <- as.data.frame(svytable(stats::as.formula(paste0("~",colnames(data[variablename]))),design))
                    frequ[,1] <- selectchoices_questions$labelchoice[match(frequ[,1], selectchoices_questions$name)]
                    frequ[,1] <- factor(frequ[,1])

                }
                else{
                  frequ<-data.frame(table(data.single[1]))
                }
                names(frequ)<- c("Var1","Freq")

                frequ$freqper <- as.numeric(frequ$Freq/sum(frequ$Freq))
                frequ_print <- frequ
                frequ$Var1 = str_wrap(frequ$Var1,width=15)


                totalanswer <- nrow(data.single)

                count_replied <- (sum(!is.na(data.single[,1 ])))

                percentresponse <- paste(round((count_replied/totalanswer*100),digits=2),"%",sep="")

                if (is.na(ordinal)==T | ordinal==""){
                  frequ$Var1<-factor(frequ$Var1, levels = frequ$Var1[order(frequ$freqper)])
                }else{
                  ordinal_choices <- as.character(selectchoices_questions[selectchoices_questions$qname==variablename,c("labelchoice")])
                  frequ$Var1 <- as.character(frequ$Var1)
                  frequ$Var1 <- reorder.factor(frequ$Var1, new.order=ordinal_choices)
                  frequ %>% arrange(Var1)
                }

                theme_set(theme_gray(base_size = 20))
                color<-"#2a87c8"


                ## and now the graph
                plotfreq <- ggplot(frequ, aes(x= Var1, y=freqper)) +
                  geom_bar(fill=color,colour=color,stat = "identity") +
                  geom_text(aes(label=paste(round(frequ$freqper*100),"%",sep=""), hjust = -0.3))+
                  #facet_wrap(~subgov, ncol=4) +
                  ylab("Frequency") +
                  scale_y_continuous(labels=percent, limits = c(0,1),expand = c(0,0))+
                  scale_fill_manual(values=colour_palette)+
                  xlab("") +
                  coord_flip() +
                  theme( plot.background = element_rect(fill = "transparent",colour = NA))
                cat("\n")
                print(plotfreq)
                cat("\n")
                frequ_print$freqper <- round(frequ$freqper*100,2)
                names(frequ_print) <- c("Choices","# answered", "% answered")
                print(kable(frequ_print))
                cat("\n")
                cat(paste0("Out of ", totalanswer," respondents, ", count_replied," (",percentresponse,")"," answered to this question."))
                cat("\n")

  }

  # Select_one question with disaggregation
  if(select_question$type=="select_one" & is.na(select_question$disaggregation)==F){
                  check <- as.data.frame(names(data))
                  names(check)[1] <- "fullname"
                  check$id <- row.names(check)
                  selectdf2 <- plyr::join(x=select_question, y=check, by="fullname",  type="left")
                  selectdf3 <- selectdf2[!is.na(selectdf2$id), ]
                  selectone <- as.character(selectdf3[, c("fullname")])

                  selectonet <- as.data.frame(selectone)


                  # Replacing names by labels
                  selectoneans <-(dico[dico$type=="select_one_d", c("fullname","name","listname","labelchoice")])
                  selectoneans$qname <- paste(sapply(strsplit(as.character(selectoneans$fullname),".",fixed = TRUE),"[[",1), sapply(strsplit(as.character(selectoneans$fullname),".",fixed = TRUE),"[[",2), sep = ".")
                  selectchoices_questions <- selectoneans[selectoneans$qname==variablename, ]

                  selectchoices <- selectchoices_questions[,c("listname","name","labelchoice")]

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
                  names(data.single)[1] <- variablename

                  #data.single <- kobo_label(data.single, dico)

                  data.single[data.single==""]<-NA


                    selectfacett <- select_question[, c("fullname","disaggregation")]

                    single.facet <- as.data.frame(table(selectfacett[,2]))
                    single.facet <- as.data.frame(single.facet[single.facet$Var1!="",c("Var1")])
                    names(single.facet) <- "Vsar1"


                    ## loop around the list of variables to facet
                    for (j in 1:nrow(single.facet) ) {
                      # j <- 1

                      facetname1 <- as.character(single.facet[j,1])
                      facetname <- as.character(dico[dico$name==facetname1,c("fullname")])
                      facetlist <- as.character(dico[dico$name==facetname1,c("listname")])


                      facetlabel <- as.character(dico[dico$fullname==facetname,c("label")])
                      facetchoices <- dico[dico$listname==facetlist, c("name","labelchoice","listname")]
                      facetchoices[,2] <- factor(facetchoices[,2])


                      selectonefacet <- as.character(select_question[select_question$disaggregation==facetname1, c("fullname")])


                      selectonefacett <- as.data.frame(selectonefacet)
                      ### Now let's create proportion graphs -- bar chart
                      for (i in 1:nrow(selectonefacett) ) {
                        # i <-23
                        if(sum(is.na(data.single[,i])==nrow(data.single[,i]))){ cat("passing \n")
                        } else {

                          ordinal <- as.character(dico[dico$fullname==facetname,c("ordinal")])

                          title <- attributes(data.single)$variable.labels[i]
                          ### testing that the variable to map is not the same than the variable to facet!
                          if(facetname==variablename){
                            cat("Oups, impossible to disaggregate if the disaggregation is the variable itself!")
                          } else {

                            #  str(data.single)
                            #  str(data.single)
                            data.single[facetname]<- data[facetname]

                            data.singlefacet <- data.frame(data.single[,c(facetname)], stringsAsFactors=F)
                            names(data.singlefacet)[1] <- facetname
                            data.singlefacet$data <- data.single[,i]

                            count_replied <- as.numeric(sum(!is.na(data.single[,i ])))
                            totalanswer <- as.numeric(nrow(data.single))


                            data.singlefacet[,1] <- data.frame(facetchoices[,2][match(data.singlefacet[,1],facetchoices[,1])], stringsAsFactors = FALSE)

                            if (usedweight=="sampling_frame"){
                              frequ <- as.data.frame(svytable(stats::as.formula(paste0("~",colnames(data[variablename]),"+",colnames(data[facetname]))),design))
                              frequ[,1] <- selectchoices_questions$labelchoice[match(frequ[,1], selectchoices_questions$name)]
                              frequ[,1] <- factor(frequ[,1])
                              frequ[,2] <- factor(frequ[,2])

                              names(frequ)[1] <- "data"
                              names(frequ)[2] <- "facet"


                            }
                            else {
                              frequ <- data.frame(table(data.singlefacet))
                              names(frequ)[1] <- "facet"
                              names(frequ)[2] <- "data"


                            }
                            freqperfacet <- ddply(frequ,"facet", function(frequ) c("freqfacet"=sum(frequ$Freq)))
                            frequ<- merge(frequ, freqperfacet, id="facet")


                            frequ$freqper <- frequ$Freq/frequ$freqfacet
                            frequ <- frequ[frequ$facet!=facetlabel,]


                            percentresponse <- paste(round(sum(!is.na(data.single[,i ]))/nrow(data.single)*100,digits=2 ),"%")

                            ordinal <- as.character(dico[dico$fullname==variablename,c("ordinal")])

                            if (is.na(ordinal)==T | ordinal==""){
                              frequ<-frequ[with(frequ,order(freqper)),]
                              frequ_print <- frequ
                              frequ$data = str_wrap(frequ$data,width=15)
                              frequ<-frequ[with(frequ,order(freqper)),]
                            }else{
                              ordinal_choices <- as.character(selectchoices_questions[selectchoices_questions$qname==variablename,c("labelchoice")])
                              frequ$data <- reorder.factor(frequ$data, new.order=ordinal_choices)
                              frequ %>% arrange(data)
                              frequ_print <- frequ
                              frequ$data = str_wrap(frequ$data,width=15)

                            }


                            ## and now the graph

                            background_rect <- data.frame(unique(frequ[,c("data")]))
                            names(background_rect) <- c("data")
                            background_rect$freqper <-1

                            theme_set(theme_gray(base_size = 10))


                            bar_one_facet_plot <- ggplot(frequ,aes(x=data, y=freqper)) +
                              geom_bar(data=background_rect,aes(x=data),stat = "identity", alpha=0.2)+
                              geom_bar(stat = "identity", position="dodge",aes(fill=facet))+
                              geom_text(aes(label=paste(round(freqper*100),"%",sep=""), fill=facet, hjust = -0.3), position=position_dodge(width=0.8))+
                              xlab("") + ylab("")+
                              scale_y_continuous(labels=percent, limits = c(0,1), expand = c(0,0))+
                              scale_fill_manual(name=paste0(facetlabel),values=colour_palette)+
                              coord_flip()
                            # Printing graphs
                            cat("\n")
                            print(bar_one_facet_plot)
                            cat("\n")
                            frequ_print$freqper <- round(frequ$freqper*100,2)
                            names(frequ_print) <- c("Choices","Disaggregation", "# answered", "% answered")
                            print(kable(frequ_print))
                            cat("\n")
                            print(paste0("Out of ", totalanswer," respondents, ", count_replied," (",percentresponse,")"," answered to this question."))
                            cat("\n")

                          }
                        }

                    }
                  }
    }

  # Select_multiple question without disaggregation
  if(select_question$type=="select_multiple_d" & is.na(select_question$disaggregation)){

    selectdf <- dico[dico$type == "select_multiple" & dico$listname==select_question$listname, c("fullname","listname","label","name","disaggregation"), ]


    ### Verify that those variables are actually in the original dataframe
    check <- as.data.frame(names(data))
    names(check)[1] <- "fullname"
    check$id <- row.names(check)
    selectdf <- plyr::join(x=selectdf, y=check, by="fullname",  type="left")
    selectdf <- selectdf[!is.na(selectdf$id), ]

    if (nrow(select_question)==0){
      cat("There's no select_multiple questions \n")
    } else{


      selectmulti <- as.character(selectdf[, c("fullname")])
      data.selectmulti <- data [selectmulti ]
      #data.selectmulti  <- kobo_label(data.selectmulti, dico)

      listmulti <- as.data.frame(select_question$listname)
      names(listmulti)[1] <- "listname"
      selectdf1 <- as.data.frame(unique(select_question$listname))
      names(selectdf1)[1] <- "listname"
      listmulti <- plyr::join(x=listmulti, y=selectdf1, by="listname", type="left")

      listmultichoice <- dico[dico$type=="select_multiple", c("listname","label","name","fullname","disaggregation","labelchoice")]



        listloop <- as.character(select_question$listname)
        listlabel <-  as.character(select_question$label)
        listfullname <- as.character(select_question$fullname)

        ### select variable for a specific multiple questions


        ## Check that those variable are in the dataset
        selectdf <- dico[dico$type=="select_multiple" & dico$listname==listloop & dico$qlevel==listfullname , c("fullname","listname","label","name","disaggregation","labelchoice")]
        selectdf2 <- plyr::join(x=selectdf, y=check, by="fullname",  type="left")
        selectdf2 <- selectdf2[!is.na(selectdf2$id), ]

        listlabelchoice <- as.character(selectdf2[,"labelchoice"])



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
              meltdata <- reshape2::melt(data.selectmultilist,id="weight")
              meltdata$value <- as.numeric(meltdata$value)

              castdata <- as.data.frame(table(meltdata[,c("value","variable","weight")]))
              castdata$Freq <- as.numeric(as.character(castdata$Freq))
              castdata$weight <- as.numeric(as.character(castdata$weight))
              castdata$freqper <- round((castdata$Freq*castdata$weight)/count_replied,digits=2)
            }
            else{
              data.selectmultilist$id <- rownames(data.selectmultilist)
              meltdata <- reshape2::melt(data.selectmultilist,id="id")

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
            castdata_print <- castdata[,c("variable","Freq","freqper")]
            castdata$variable = str_wrap(castdata$variable,width=15)
            castdata$variable <- factor(castdata$variable, levels=castdata$variable)


            theme_set(theme_gray(base_size = 10))


            bar_multi <- ggplot(castdata, aes(x=variable, y=freqper)) +
              geom_bar(fill="#2a87c8",colour="#2a87c8",stat = "identity") +
              geom_text(aes(label=paste(round(freqper*100),"%",sep=""), hjust = -0.2))+
              xlab("") + ylab("")+
              scale_y_continuous(labels=percent, limits=c(0,1), expand = c(0,0))+
              scale_fill_manual(values=colour_palette)+
              coord_flip()+
              theme(plot.background = element_rect(fill = "transparent",colour = NA))
            cat("\n")
            print(bar_multi)
            cat("\n")
            castdata_print$freqper <- round(castdata$freqper*100,2)
            names(castdata_print) <- c("Choices","# answered", "% answered")
            print(kable(castdata_print))
            cat("\n")
            cat(paste0("Out of ", totalanswer," respondents, ", count_replied," (",percentresponse,")"," answered to this question."))


          }
        }




    }


  }
  # Select_multiple question without disaggregation
  if(select_question$type=="select_multiple_d" & is.na(select_question$disaggregation)==F){

    ### Verify that those variables are actually in the original dataframe
    check <- as.data.frame(names(data))
    names(check)[1] <- "fullname"
    check$id <- row.names(check)
    selectdf <- plyr::join(x=select_question, y=check, by="fullname",  type="left")
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
      selectfacet <- as.character(selectdf[selectdf$disaggregation!="" & selectdf$fullname==variablename & selectdf$disaggregation!="weight", c("fullname")])
      selectfacet <- selectfacet[!is.na(selectfacet)]

      if(length(selectfacet)==0) {
        cat("There's no variable to disaggregate in your data analysis plan.\n")

      } else {
        selectmulti <- as.character(selectdf[, c("fullname")])
        listname <- as.character(selectdf$listname)
        selectmultichoices <- as.character(dico[dico$type=="select_multiple" & dico$listname==listname,c("fullname")])
        data.selectmulti <- data [selectmultichoices]
        #data.selectmulti  <- kobo_label(data.selectmulti, dico)

        selectfacett <- selectdf[selectdf$disaggregation!=""& selectdf$disaggregation!="weight", c("fullname","disaggregation")]
        single.facet <- as.data.frame(table(selectfacett[,2]))
        single.facet <- as.data.frame(single.facet[single.facet$Var1!="",c("Var1")])
        names(single.facet) <- "Var1"


        listmulti <- dico[dico$type=="select_multiple_d", c("listname","label","name","fullname","disaggregation")]
        selectdf1 <- as.data.frame(unique(selectdf$listname))
        names(selectdf1)[1] <- "listname"
        listmulti <- plyr::join(x=listmulti, y=selectdf1, by="listname", type="left")

        listmultichoice <- dico[dico$type=="select_multiple_d", c("listname","label","name","fullname","disaggregation","labelchoice")]

          listloop <- as.character(select_question$listname)
          listlabel <-  as.character(select_question$label)


          ### select variable for a specific multiple questions
          selectmultilist <- as.character(dico[dico$type=="select_multiple" & dico$listname==listloop, c("fullname")])

          ## Check that those variable are in the dataset
          selectdf <- dico[dico$type=="select_multiple" & dico$listname==listloop & dico$qlevel==variablename , c("fullname","listname","label","name","disaggregation","labelchoice")]
          selectdf2 <- plyr::join(x=selectdf, y=check, by="fullname",  type="left")
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

                    meltdata <- reshape2::melt(data.selectmultilist,id=c("weight","id","facet"))

                    castdata <- as.data.frame(table(meltdata[,c("value","variable","facet","weight")]))
                    castdata$Freq <- as.numeric(as.character(castdata$Freq))
                    castdata$weight <- as.numeric(as.character(castdata$weight))
                    castdata <- castdata[castdata$value!=0, ]
                    freqperfacet <- as.data.frame(table(data.selectmultilist[is.na(data.selectmultilist[,1])==F, c("facet")]))
                    names(freqperfacet) <- c("facet","freqfacet")
                    castdata<- merge(castdata, freqperfacet, id="facet")
                    castdata$freqper <- round((castdata$Freq*castdata$weight)/(castdata$freqfacet*castdata$weight),digits=2)
                  }

                  else{
                    meltdata <- reshape2::melt(data.selectmultilist,id=c("id","facet"))

                    castdata <- as.data.frame(table(meltdata[,c("value","variable","facet")]))
                    freqperfacet <- as.data.frame(table(data.selectmultilist[is.na(data.selectmultilist[,1])==F, c("facet")]))
                    names(freqperfacet) <- c("facet","freqfacet")
                    castdata<- merge(castdata, freqperfacet, id="facet")
                    castdata$freqper <- round((castdata$Freq)/(castdata$freqfacet),digits=2)

                  }

                  castdata <- castdata[castdata$value!=0, ]

                  #combining values
                  castdata<- ddply(castdata, c("variable","facet"),numcolwise(sum))
                  castdata_print <- castdata[,c("variable","facet","Freq","freqper")]

                  castdata$variable = str_wrap(castdata$variable,width=15)

                  background_rect <- data.frame(unique(castdata[,c("variable")]))
                  names(background_rect) <- c("variable")
                  background_rect$freqper <-1

                  theme_set(theme_gray(base_size = 10)
                  )

                  bar_multi_disagg <- ggplot(castdata,aes(x=variable, y=freqper)) +
                    geom_bar(data=background_rect,aes(x=variable),stat = "identity", alpha=0.2)+
                    geom_bar(stat = "identity", position="dodge",aes(fill=facet))+
                    geom_text(aes(label=paste(round(freqper*100),"%",sep=""), fill=facet, hjust = -0.3), position=position_dodge(width=0.8))+
                    xlab("") + ylab("")+
                    scale_y_continuous(labels=percent, limits = c(0,1), expand = c(0,0))+
                    scale_fill_manual(name=paste0(facetlabel),values=colour_palette)+
                    coord_flip()
                  cat("\n")
                  print(bar_multi_disagg)
                  cat("\n")
                  castdata_print$freqper <- round(castdata$freqper*100,2)
                  names(castdata_print) <- c("Choices","Disaggregation", "# answered", "% answered")
                  print(kable(castdata_print))
                  cat("\n")
                  print(paste0("Out of ", totalanswer," respondents, ", count_replied," (",percentresponse,")"," answered to this question."))




            }
          }
        }
      }
    }


  }
  # Integer, decimal or calculat question without disaggregation
  if(select_question$type=="integer" | select_question$type=="decimal" | select_question$type=="calculate"){
    if (select_question$name=="__version__" | select_question$name=="_version_"){
      cat("Passing")}

  else{
    check <- as.data.frame(names(data))
    names(check)[1] <- "fullname"
    check$id <- row.names(check)
    select_question <- plyr::join(x=select_question, y=check, by="fullname",  type="left")
    select_question <- select_question[!is.na(select_question$id), ]

    if (nrow(select_question)==0){
      cat("There's no integer variables. \n")
    } else{

      selectinteger <- as.character(select_question[, c("fullname")])
      data.integer <- data [selectinteger  ]

      selectfacet <- as.character(select_question[select_question$disaggregation!="" , c("fullname")])
      selectfacet <- selectfacet[!is.na(selectfacet)]


      ## force to data frame
      data.integer <- as.data.frame(data.integer)
      #data.integer  <- kobo_label(data.integer, dico)
        # for (i in 1:2 ) {
        # i <- 67
        title <- select_question$label

        ## Ensure that the variable is recognised as numeric
        select.data.integer <- data.frame(as.numeric(na.omit(data.integer[ ,1])))
        #str(data.integer[ , i])

        totalanswer <- nrow(data.integer)

        count_replied <- (sum(!is.na(data.integer[,1 ])))

        percentresponse <- paste(round((count_replied/totalanswer*100),digits=2),"%",sep="")

        theme_set(theme_gray(base_size = 10))


        # trendline on histogram by adding geom_density
        histograms <- ggplot(data=select.data.integer, aes(select.data.integer)) +
          geom_histogram(aes(y =..density..), fill="#2a87c8", alpha = .6, binwidth=0.5) +
          geom_density(adjust=2) +
          scale_x_continuous(expand = c(0,0)) +
          labs(x="", y="Frequency")+
          theme(plot.background = element_rect(fill = "transparent",colour = NA))
        cat("\n")
        print(histograms)
        cat("\n")
        cat("\n")
        names(select.data.integer) <- select_question$label
        cat("\n")
        print(summary(select.data.integer))
        cat("\n")
        cat(paste0("Out of ", totalanswer," respondents, ", count_replied," (",percentresponse,")"," answered to this question."))
        cat("\n")


    }
  }
  }



}
NULL
