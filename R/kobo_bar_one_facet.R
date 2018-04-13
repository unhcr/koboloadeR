#' @name kobo_bar_one_facet
#' @rdname kobo_bar_one_facet
#' @title  Generate faceted frequency bar chart
#'
#' @description  Automatically generate faceted chart for select one variable.. ggplot2 is used.
#'
#'
#' @param mainDir Path to the project's working directory: mainly for proper shiny app path
#'
#'
#'
#' @author Edouard Legoupil, Elliott Messeiller
#'
#' @examples
#' kobo_bar_one_facet()
#'
#' @export kobo_bar_one_facet
#'
#' @examples
#' \dontrun{
#' kobo_bar_one_facet()
#' }
#'
#'

kobo_bar_one_facet <- function(mainDir='') {
  if (mainDir==''){
    mainDir <- getwd()
  }

  source(paste0(mainDir,"/code/0-config.R"), local=TRUE)


  mainDirectory <- paste0(mainDir,"/out")
  subDir <- "/disagg_one"
  if (file.exists(paste(mainDirectory, subDir, "/", sep = "/", collapse = "/"))) {
    cat("disagg_one directory exists in out directory and is a directory.\n")
  } else if (file.exists(paste(mainDirectory, subDir, sep = "/", collapse = "/"))) {
    cat("disagg_one directory exists in your out directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("disagg_one directory does not exist in your out directory - creating now!\n ")
    dir.create(file.path(mainDirectory, subDir))
  }


  ## get list of all nominal variables


  ## Check that those variable are in the dataset
  selectdf <- dico[dico$type=="select_one"  , c("fullname","listname","label","name","disaggregation","labelchoice")]

  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf2 <- join(x=selectdf, y=check, by="fullname",  type="left")
  selectdf3 <- selectdf2[!is.na(selectdf2$id), ]
  selectone <- as.character(selectdf3[, c("fullname")])

  selectonet <- as.data.frame(selectone)

  selectfacet <- as.character(selectdf[selectdf$disaggregation!="" & selectdf$disaggregation!="weight" , c("fullname")])
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

    if(length(selectfacet)==0) {
      cat("There's no variable to disaggregate in your data analysis plan.\n")
    } else {  cat(paste0( length(selectfacet) , " variable(s) to disaggregate in your data analysis plan. Let's proceed! \n"))

      selectfacett <- selectdf[selectdf$disaggregation!="" , c("fullname","disaggregation")]

      single.facet <- as.data.frame(table(selectfacett[,2]))
      single.facet <- as.data.frame(single.facet[single.facet$Var1!="",c("Var1")])
      names(single.facet) <- "Var1"

      data.single <- as.data.frame(data.single)
      ## Remove variable where we get only NA
      data.single <- kobo_label(data.single, dico)

      data.single[data.single==""]<-NA


      ## loop around the list of variables to facet
      for (j in 1:nrow(single.facet) ) {
        # j <- 1

        facetname1 <- as.character(single.facet[j,1])
        facetname <- as.character(dico[dico$name==facetname1,c("fullname")])
        facetlist <- as.character(dico[dico$name==facetname1,c("listname")])


        facetlabel <- as.character(dico[dico$fullname==facetname,c("label")])
        facetchoices <- dico[dico$listname==facetlist, c("name","labelchoice","listname")]
        facetchoices[,2] <- factor(facetchoices[,2])


        selectonefacet <- as.character(selectdf[selectdf$disaggregation==facetname1, c("fullname")])


        selectonefacett <- as.data.frame(selectonefacet)
        ### Now let's create proportion graphs -- bar chart
        for (i in 1:nrow(selectonefacett) ) {
            # i <-23
          if(sum(is.na(data.single[,i])==nrow(data.single[,i]))){ cat("passing \n")
          } else {

            variablename <- names(data.single)[i]
            ordinal <- as.character(dico[dico$fullname==facetname,c("ordinal")])

            title <- attributes(data.single)$variable.labels[i]
               ### testing that the variable to map is not the same than the variable to facet!
               if(facetname==variablename){
                       cat("")
                        } else {

                        #  str(data.single)
                          #  str(data.single)
                          data.single[facetname]<- data[facetname]

                          data.singlefacet <- as.data.frame(data.single[,c(facetname)])
                          names(data.singlefacet)[1] <- facetname
                          data.singlefacet$data <- data.single[,i]

                          count_replied <- as.numeric(sum(!is.na(data.single[,i ])))


                          data.singlefacet[,1] <- data.frame(facetchoices[,2][match(data.singlefacet[,1],facetchoices[,1])], stringsAsFactors = FALSE)

                          if (usedweight=="sampling_frame"){
                            frequ <- data.frame(svytable(~data.singlefacet[["data"]]+data.singlefacet[[facetname]], surveydesign))
                            names(frequ)[1] <- "data"
                            names(frequ)[2] <- "facet"


                          }
                          else {
                            frequ <- data.frame(table(data.singlefacet))
                            names(frequ)[1] <- "facet"
                            names(frequ)[2] <- "data"


                          }

                          frequ$freqper <- frequ$Freq/count_replied
                          frequ$data = str_wrap(frequ$data,width=15)
                          frequ <- frequ[frequ$facet!=facetlabel,c("data","facet", "Freq","freqper")]


                          percentresponse <- paste(round(sum(!is.na(data.single[,i ]))/nrow(data.single)*100,digits=2 ),"%")

                          ordinal <- as.character(dico[dico$fullname==variablename,c("ordinal")])

                          if (is.na(ordinal)==T | ordinal==""){
                              frequ<-frequ[with(frequ,order(freqper)),]
                          }else{
                            ordinal_choices <- as.character(selectchoices_questions[selectchoices_questions$qname==variablename,c("labelchoice")])
                            frequ$data <- reorder.factor(frequ$Var1, new.order=ordinal_choices)
                            frequ %>% arrange(data)
                          }



                          ## and now the graph

                          background_rect <- data.frame(unique(frequ[,c("data")]))
                          names(background_rect) <- c("data")
                          background_rect$freqper <-1

                          theme_set(theme_gray(base_size = 20))


                           ggplot(frequ,aes(x=data, y=freqper)) +
                            geom_bar(data=background_rect,aes(x=data),stat = "identity", alpha=0.2)+
                            geom_bar(stat = "identity", position="dodge",aes(fill=facet))+
                            geom_text(aes(label=paste(round(freqper*100),"%",sep=""), fill=facet, hjust = -0.5), position=position_dodge(width=0.8))+
                            xlab("") + ylab("")+
                            scale_y_continuous(labels=percent, limits = c(0,1))+
                            scale_fill_brewer(name=paste0(facetlabel),palette="PuBu")+
                            coord_flip()+
                            ggtitle(str_wrap(title,width=50))+
                            theme(plot.title=element_text(face="bold", size=25))
                          # Saving graphs
                          ggsave(filename=paste(mainDir,"/out/disagg_one/",variablename,"_disagg_",facetname,"bar_one.png",sep=""), width=10, height=10,units="in", dpi=300)
                          cat(paste0("Generated bar chart for question: ",i, " ", title ," - with disaggregation on - ",j, " ",facetlabel, "  saved as image:   ", variablename,"_disagg_",facetname,"\n"))
                        }
                        ### End testing
          }
              }
             ### End loop around variable
        }
        ### End loop around facet

  }
  ### Test if facet in dico
  cat(" ########################################################################\n")
  cat(" # The bar charts for select_one questions dissagrated  were generated! #\n")
  cat(" # You can find them in the folder 'out/disagg_one'!                    #\n")
  cat(" ########################################################################\n")

}
NULL

