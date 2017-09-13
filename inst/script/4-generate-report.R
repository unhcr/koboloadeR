#### Generate Rmd files for each chapter ------
rm(list = ls())
## Load the form

mainDir <- getwd()
## Load all required packages
source(paste0(mainDir,"/code/0-config.R"))
source(paste0(mainDir,"/code/0-packages.R"))
library(koboloadeR)


### Load the data
cat("\n\n Loading data. It is assumed that the cleaning, weighting & re-encoding has been done previously \n")
household <- read.csv("data/data2.csv", encoding = "UTF-8", na.strings = "NA")

###Form##########################################
## Load form
cat("\n\n Building dictionnary from the xlsform \n")
#rm(form)
#form <- "form.xls"
## Generate & Load dictionnary
kobo_dico(form)
dico <- read.csv(paste("data/dico_",form,".csv",sep = ""), encoding = "UTF-8", na.strings = "")
#rm(form)


## label Variables
cat("\n\n Labelling variables \n")
household <- kobo_label(household , dico)


## Get a list of variables to be used for disaggregation #######
disaggregation <- dico[which(dico$disaggregation %in% c("facet","correlate") & dico$formpart == "questions"),
                       c("chapter", "name", "label", "type", "qrepeatlabel", "fullname","disaggregation") ]

## Get a list of variables to be used for ddisaggregation - analyisis of association - chisquarred #######
correlation <- dico[which(dico$type %in% c("select_multiple_d","select_one") & !(is.na(dico$correlate)) & dico$formpart == "questions"),
                    c("chapter", "name", "label", "type", "qrepeatlabel", "fullname","disaggregation") ]




## To do: insert reference to formpart when there's an indicator reference
# & dico$formpart=="questions"


### Get the dico with list of chapter

cat("\n\n Building now the chapters of the reports in Rmd  format \n")

chapters <- as.data.frame(unique(dico$chapter))
names(chapters)[1] <- "Chapter"

## Default behavior if no chapter was defined in xlsform
if( (nrow(chapters) == 1) & is.na(chapters$Chapter) ) {
  cat("Defaulting questions allocation to chapter")
  dico$chapter[ dico$type %in% c("select_one","select_multiple_d")] <- "report"
  chapters <- as.data.frame(unique(dico$chapter))
  names(chapters)[1] <- "Chapter"
} else {}

chapters <- as.data.frame(chapters[!is.na(chapters$Chapter), ])

names(chapters)[1] <- "Chapter"




## for each chapter: create a Rmd file -------

##Loop.chapter ------------

for( i in 1:nrow(chapters) )
{
  # i <-1
  chaptersname <- as.character(chapters[ i , 1])
  cat(paste(i, " - Render chapter for ",as.character(chapters[ i , 1]),"\n" ))
  chapter.name <- paste("code/",i,"-", chaptersname, "-chapter.Rmd", sep="")

  ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
  if (file.exists(chapter.name)) file.remove(chapter.name)

  ## TO DO : put in configuration file name of report, author, organisation & location
  ## TO DO : put in configuration wethere report should be portrait or landscape
  cat("---", file = chapter.name , sep="\n", append=TRUE)
  cat(paste("title: \"Preliminary exploration of results: ",chaptersname , "- Draft not for distribution. \"", sep=""), file = chapter.name ,sep="\n", append=TRUE)
  cat("author: \"Prepared by UNHCR\"", file = chapter.name ,sep="\n", append=TRUE)
  cat("date: \"Amman, prepared on the `r format(Sys.Date(),  '%d %B %Y')`\"", file = chapter.name ,sep="\n", append=TRUE)
  cat("output:",file = chapter.name ,sep="\n", append=TRUE)
  cat("  word_document:", file = chapter.name , sep="\n", append=TRUE)
  cat("    fig_caption: yes", file = chapter.name , sep="\n", append=TRUE)
  cat("    fig_height: 5", file = chapter.name , sep="\n", append=TRUE)
  cat("    fig_width: 8", file = chapter.name , sep="\n", append=TRUE)
  cat("    toc: yes", file = chapter.name , sep="\n", append=TRUE)
  cat("    toc_depth: 2", file = chapter.name , sep="\n", append=TRUE)
  cat("    reference_docx: style-unhcr-portrait.docx", file = chapter.name , sep="\n", append=TRUE)
  cat("---", file = chapter.name , sep="\n", append=TRUE)


  ## First chunk to get the data in the report

  cat("```{r setup, include=FALSE, echo=FALSE, warning=FALSE, message=FALSE}", file = chapter.name , sep="\n", append=TRUE)
  cat("mainDir <- getwd()", file = chapter.name , sep="\n", append=TRUE)
  cat("mainDirroot <- substring(mainDir, 0 , nchar(mainDir)- 5)", file = chapter.name , sep="\n", append=TRUE)
  cat("## Load all required packages", file = chapter.name , sep="\n", append=TRUE)
  cat("source(paste0(mainDirroot,\"/code/0-packages.R\"))", file = chapter.name , sep="\n", append=TRUE)
  cat("source(paste0(mainDirroot,\"/code/0-theme.R\"))", file = chapter.name , sep="\n", append=TRUE)
  cat("library(koboloadeR)", file = chapter.name , sep="\n", append=TRUE)
  cat("## Provide below the name of the form in xsl form - format should be xls not xlsx", file = chapter.name , sep="\n", append=TRUE)
  cat(paste0("form <- \"",form,"\""), file = chapter.name , sep="\n", append=TRUE)
  cat("dico <- read.csv(paste0(mainDirroot,\"/data/dico_\",form,\".csv\"), encoding=\"UTF-8\", na.strings=\"\")", file = chapter.name , sep="\n", append=TRUE)


  ## TO DO: Use config file to load the different frame
  cat("household <- read.csv(paste0(mainDirroot,\"/data/data2.csv\"), encoding=\"UTF-8\", na.strings=\"NA\")", file = chapter.name , sep="\n", append=TRUE)

  cat("## label Variables", file = chapter.name , sep="\n", append=TRUE)
  cat("household <- kobo_label(household , dico)", file = chapter.name , sep="\n", append=TRUE)


  ## To do use configuration file to weight the data #######
  cat("## Create weighted survey object", file = chapter.name , sep="\n", append=TRUE)


  #cat("data.survey <- svydesign(ids = ~ section1.location.district ,  data = data,  weights = ~Normalized.Weight ,  fpc = ~fpc )", file = chapter.name , sep="\n", append=TRUE)
  ## If no weight, the weighted object is unweigthted
  cat("household.survey <- svydesign(ids = ~ 1 ,  data = household )", file = chapter.name , sep="\n", append=TRUE)

  cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append=TRUE)


  ### To DO : Offer option to insert in the report skeleton interpretation questions
  ###selectone.quali####################################################################
  cat(paste("# Introduction\n"),file = chapter.name ,sep="\n",append=TRUE)
  cat(paste("This data crunching report allows to quickly explore the results of the survey that can be regenerated as needed."),file = chapter.name ,sep="\n",append=TRUE)
  cat(paste("You can:  \n "),file = chapter.name ,sep="\n",append=TRUE)
  cat(paste("  *  adjust the configuration in the xlsform to break it into chapter,   \n"),file = chapter.name ,sep="\n",append=TRUE)
  cat(paste("  *  configure disaggregation & correlation for each questions,   \n"),file = chapter.name ,sep="\n",append=TRUE)
  cat(paste("  *  revise the data cleansing based on the cleaning log,    \n "),file = chapter.name ,sep="\n",append=TRUE)
  cat(paste("  *  add weight to each observation in order to account for a specific sampling plan,  \n"),file = chapter.name ,sep="\n",append=TRUE)
  cat(paste("  *  append calculated indicators to your data frame  \n\n"),file = chapter.name ,sep="\n",append=TRUE)

  cat(paste("When analysisng those representation, you may:  \n "),file = chapter.name ,sep="\n",append=TRUE)
  cat(paste("  *  __Reflect__: Data quality and or suggestions to change questions,   \n"),file = chapter.name ,sep="\n",append=TRUE)
  cat(paste("  *  __Interpret__: Qualitative interpretations of data patterns,   \n"),file = chapter.name ,sep="\n",append=TRUE)
  cat(paste("  *  __Recommend__: Recommendation in terms of programmatic adjustment,   \n"),file = chapter.name ,sep="\n",append=TRUE)
  cat(paste("  *  __Classify__: Level of sensitivity for the information,   \n"),file = chapter.name ,sep="\n",append=TRUE)


  cat(paste("# Compilation of questions results"),file = chapter.name ,sep="\n", append=TRUE)

  ## Getting chapter questions #######
  chapterquestions <- dico[which(dico$chapter== chaptersname ), c("chapter", "name", "label", "type", "qrepeatlabel", "fullname","listname") ]

  #levels(as.factor(as.character(dico[which(!(is.na(dico$chapter)) & dico$formpart=="questions"), c("type") ])))
  ##Loop.questions####################################################################################################

  for(j in 1:nrow(chapterquestions))
  {
    # j <-1
    ## Now getting level for each questions
    questions.name <- as.character(chapterquestions[ j , c("fullname")])
    questions.shortname <- as.character(chapterquestions[ j , c("name")])
    questions.type <- as.character(chapterquestions[ j , c("type")])
    questions.frame <- as.character(chapterquestions[ j , c("qrepeatlabel")])
    questions.label <- as.character(chapterquestions[ j , c("label")])
    questions.listname <- as.character(chapterquestions[ j , c("listname")])
    questions.variable <- paste0(questions.frame,"$",questions.name)
    cat(paste("\n", j, " - Render question: ", questions.variable,"\n" ))

    ## write question name-------
    cat("\n ",file = chapter.name , sep="\n",append=TRUE)
    cat(paste("## ", questions.label ,sep=""),file = chapter.name , sep="\n", append=TRUE)

    ## Now create para based on question type-------




    ###selectone###################################################################################################
    if (questions.type =="select_one" ) {

      cat(paste("Single choice question ","\n\n",sep=""),file = chapter.name ,sep="\n",append=TRUE)

      ###selectone.tabulation######################################################################
      ## compute frequency to see if it's not empty
      frequ <- as.data.frame(table( get(paste0(questions.frame))[[questions.name]]))


      figheight <- as.integer(nrow(frequ))
      if ( figheight==0){ figheight<-1} else {figheight<-figheight/1.2}

      cat(paste("### Tabulation" ,sep=""),file = chapter.name ,sep="\n",append=TRUE)
      ## Open chunk
      cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=",figheight,", size=\"small\"}\n"), file = chapter.name, append=TRUE)
      cat(paste("### Tabulation" ,sep=""),file = chapter.name ,sep="\n",append=TRUE)
      cat(paste0("##Compute contengency table"),file = chapter.name ,sep="\n",append=TRUE)
      cat(paste0("frequ <- as.data.frame(table(",questions.variable,"))"),file = chapter.name ,sep="\n",append=TRUE)
      #cat(paste0("if (nrow(frequ)==0){ cat(\"No response for this question\") } else{"),file = chapter.name ,sep="\n",append=TRUE)

      if (nrow(frequ) %in% c("0","1")){
        cat(paste0("cat(\"No responses recorded for this question...\")"),file = chapter.name , sep="\n", append=TRUE)
        cat("No responses recorded for this question...\n")
      } else{
        cat(paste0("## display table"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("## Reorder factor"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ[ ,1] = factor(frequ[ ,1],levels(frequ[ ,1])[order(frequ$Freq, decreasing = TRUE)])"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ <- frequ[ order(frequ[ , 1]) ,  ]"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("names(frequ)[1] <- \"", questions.shortname,"\""),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\") %>% kable_styling ( position = \"center\")"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("## Frequency table with NA in order to get non response rate"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ1 <- as.data.frame(prop.table(table(", questions.variable,", useNA=\"ifany\")))"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ1 <- frequ1[!(is.na(frequ1$Var1)), ]"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ1 <- frequ1[!(frequ1$Var1==\"NA\"), ]"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("percentreponse <- paste0(round(sum(frequ1$Freq)*100,digits=1),\"%\")"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("## Frequency table without NA"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ2 <- as.data.frame(prop.table(table(", questions.variable,",useNA = \"no\")))"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("## Frequency table with weight"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ.weight <- as.data.frame(svymean(~ ",questions.name,", design = ",questions.frame,".survey, na.rm=TRUE))"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("## Binding the two"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ3 <- cbind(frequ2,frequ.weight)"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("## Reorder factor"),file = chapter.name ,sep="\n",append=TRUE)
        #    cat(paste0("frequ2[ ,1] = factor(frequ2[ ,1],levels(frequ2[ ,1])[order(frequ2$Freq, decreasing = TRUE)])"),file = chapter.name ,sep="\n",append=TRUE)
        #    cat(paste0("frequ2 <- frequ2[ order(frequ2[ , 1]) ,  ]"),file = chapter.name ,sep="\n",append=TRUE)
        #    cat(paste0("frequ2[ ,3] <- paste0(round(frequ2[ ,2]*100,digits=1),\"%\")"),file = chapter.name ,sep="\n",append=TRUE)
        #    cat(paste0("names(frequ2)[3] <- \"freqper2\""),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ3[ ,1] = factor(frequ3[ ,1],levels(frequ3[ ,1])[order(frequ3$mean, decreasing = TRUE)])"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ3 <- frequ3[ order(frequ3[ , 1]) ,  ]"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ3[ ,5] <- paste0(round(frequ3[ ,3]*100,digits=1),\"%\")"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("names(frequ3)[5] <- \"freqper2\""),file = chapter.name ,sep="\n",append=TRUE)

        cat(paste0("\n"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("## and now the graph"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("ggplot(frequ3, aes(x=frequ3$Var1, y=frequ3$mean)) +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("geom_bar(fill=\"#2a87c8\",colour=\"#2a87c8\", stat =\"identity\", width=.8) +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("guides(fill=FALSE) +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("geom_label_repel(aes(y = mean, label = freqper2), fill = \"#2a87c8\", color = 'white') +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("ylab(\"Frequency\") +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("scale_y_continuous(labels=percent)+"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("xlab(\"\") +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("coord_flip() +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("ggtitle(\"",questions.label,"\","),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("subtitle = paste0(\"Weighted results. Question response rate: \",percentreponse,\" .\")) +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("theme(plot.title=element_text(face=\"bold\", size=9),"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("plot.background = element_rect(fill = \"transparent\",colour = NA))"),file = chapter.name ,sep="\n",append=TRUE)
      }


      #cat(paste0("}"),file = chapter.name ,sep="\n",append=TRUE)
      ## Close chunk
      cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append=TRUE)
      cat(paste0("\n\n\n\n", sep = '\n'), file = chapter.name, append=TRUE)

      ##selectone.crosstabulation #######################################################################
      if( nrow(disaggregation)==0 ) {
        cat("No disaggregation requested for this question...\n",file = chapter.name , sep="\n", append=TRUE)
        cat("No disaggregation requested for this question...\n")
        cat("\n", file = chapter.name, append=TRUE)
      } else if (nrow(frequ) %in% c("0","1")){
        # cat("No responses recorded for this question. No disaggregation...\n",file = chapter.name , sep="\n", append=TRUE)
        cat("No responses recorded for this question. No disaggregation...\n")
        cat("\n", file = chapter.name, append=TRUE)
      } else {

        cat(paste("### Cross-tabulations" ,sep=""),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("\n", sep = '\n'), file = chapter.name, append=TRUE)

        for(h in 1:nrow(disaggregation))
        {
          #h <-1
          ## Now getting level for each questions
          disag.name <- as.character(disaggregation[ h , c("fullname")])
          disag.shortname <- as.character(disaggregation[ h , c("name")])
          disag.type <- as.character(disaggregation[ h , c("type")])
          disag.frame <- as.character(disaggregation[ h , c("qrepeatlabel")])
          disag.label <- as.character(disaggregation[ h , c("label")])
          disag.listname <- as.character(disaggregation[ h , c("listname")])
          disag.variable <- paste0(questions.frame,"$",disag.name)

          if (disag.variable==questions.variable){
            cat(paste0("cat(\"\\n\")"),file = chapter.name , sep="\n", append=TRUE)
          } else {

            # Get number levels to set up chart height
            figheight <- nlevels( get(paste0(questions.frame))[[disag.name]])
            if ( figheight==0){ figheight<-1
            } else if ( figheight==1) {figheight<-"2"} else if ( figheight==2) {figheight<-"3"} else if ( figheight==3) {figheight<-"3"} else if ( figheight==4) {figheight<-"4"
            } else if ( figheight==5) {figheight<-"4"} else if ( figheight==6) {figheight<-"5"} else if ( figheight==7) {figheight<-"6"} else if ( figheight==8) {figheight<-"7"
            } else if ( figheight==9) {figheight<-"8"} else if ( figheight==10) {figheight<-"9"} else if ( figheight>=11) {figheight<-"10"}

            ## Open chunk
            cat(paste0("\n```{r ", questions.name,h, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=",figheight,", size=\"small\"}\n"), file = chapter.name, append=TRUE)
            cat(paste("\n", h, " - Render disaggregation: ", disag.label, "for question: ", questions.label,"\n" ))
            cat(paste0("crosssfrequ.weight <-as.data.frame(prop.table(svytable(~", questions.name ," + ", disag.name,", design =",questions.frame ,".survey  ), margin = 2))"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("names(crosssfrequ.weight)[1] <- \"quest\""),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("names(crosssfrequ.weight)[2] <- \"disag\""),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("crosssfrequ.weight$Freq2 <- paste0(round(crosssfrequ.weight$Freq*100,digits=1),\"%\")"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("## Reorder factor"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("cross <- dcast(crosssfrequ.weight, disag  ~ quest, value.var = \"Freq\")"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("cross <- cross[ order(cross[ ,2], decreasing = TRUE) ,  ]"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("crosssfrequ.weight$disag <- factor(crosssfrequ.weight$disag, levels = as.character(cross[ ,1]))"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("\n"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("## and now the graph"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("ggplot(crosssfrequ.weight, aes(fill=crosssfrequ.weight$quest, y=crosssfrequ.weight$Freq, x=crosssfrequ.weight$disag)) +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("geom_bar(colour=\"#2a87c8\", stat =\"identity\", width=.8, aes(fill = quest), position = position_stack(reverse = TRUE)) +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("geom_label_repel(aes(label = Freq2), fill = \"#2a87c8\", color = 'white') +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("ylab(\"Frequency\") +"),file = chapter.name ,sep="\n",append=TRUE)
            #cat(paste0("facet_wrap(~disag, ncol=3) +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("scale_y_continuous(labels=percent)+"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("xlab(\"\") +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("coord_flip() +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("ggtitle(\"",questions.label,"\","),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("subtitle = paste0(\"Weighted results. Question response rate: \",percentreponse,\" by question: \",disag.label,\" .\")) +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("theme(plot.title=element_text(face=\"bold\", size=9),"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("plot.background = element_rect(fill = \"transparent\",colour = NA))+"),file = chapter.name ,sep="\n",append=TRUE)
            ## setting up the legend
            #cat(paste0("guides(fill=FALSE) +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("theme(legend.direction = \"horizontal\", legend.position = \"bottom\", legend.box = \"horizontal\",legend.title=element_blank()  )"),file = chapter.name ,sep="\n",append=TRUE)
            ## Close chunk
            cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append=TRUE)
          }
        }
      }





      ## Selectone.correlations   #######################################################################
      ### We can test all correlation before and keep in the report only the multiple plots
      ## First check that variables are in the frame
      correlation1 <- correlation[correlation$qrepeatlabel %in% questions.frame, ]
      check <- as.data.frame(names(get(paste0(questions.frame))))
      names(check)[1] <- "fullname"
      check$id <- row.names(check)
      correlationdf <- join(x=correlation1, y=check, by="fullname",  type="left")
      correlationdf <- correlationdf[!is.na(correlationdf$id), ]

      if( nrow(correlationdf) == 0 ) {
        cat("No correlation requested for this question...\n",file = chapter.name , sep = "\n", append=TRUE)
        cat("No correlation requested for this question...\n")
        cat("\n", file = chapter.name, append=TRUE)
      } else if (nrow(frequ) %in% c("0","1")){
        #cat("No responses recorded for this question. No analysis of correlation...\n",file = chapter.name , sep="\n", append=TRUE)
        cat("No responses recorded for this question. No analysis of correlation...\n")
        cat("\n", file = chapter.name, append=TRUE)
      } else {

        cat(paste("### Analysis of Correlations" ,sep=""),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("\n", sep = '\n'), file = chapter.name, append=TRUE)

        rm(chiquare.resultall)
        chiquare.resultall <- data.frame(c(1))
        names(chiquare.resultall)[1] <- "id"
        chiquare.resultall$target <- "target"
        chiquare.resultall$tested <- "result"
        chiquare.resultall$frame <- "frame"
        chiquare.resultall$target.n <- 1
        chiquare.resultall$tested.n <- 2
        chiquare.resultall$target.label <- "target.label"
        chiquare.resultall$tested.label <- "tested.label"
        chiquare.resultall$p.value <- 0.999


        for (l in 1:nrow(correlationdf)) {
          #l <- 1
          chiquare.result <- data.frame(c(1))
          names(chiquare.result)[1] <- "id"
          chiquare.result$id <- h
          chiquare.result[1, c("target")] <- questions.name
          chiquare.result[1, c("tested")] <-  as.character(correlationdf[l, c("fullname")])
          chiquare.result[1, c("frame")]  <- questions.frame
          ## getting labels
          chiquare.result[1, c("target.n")] <-   which(colnames(get(paste0(chiquare.result$frame))) == chiquare.result[1, c("target")])
          chiquare.result[1, c("tested.n")] <-   which(colnames(get(paste0(chiquare.result$frame))) == chiquare.result[1, c("tested")])
          chiquare.result[1, c("target.label")]  <- attributes(get(paste0(chiquare.result$frame)))$variable.labels[chiquare.result[1, c("target.n")]]
          chiquare.result[1, c("tested.label")]  <- attributes(get(paste0(chiquare.result$frame)))$variable.labels[chiquare.result[1, c("tested.n")]]

          cat(paste0(h," correlation between --",chiquare.result[1, c("target.label")],"-- and --",chiquare.result[1, c("tested.label")],"--.\n"))
          formula <- cbind(as.data.frame(get(paste0(chiquare.result$frame))[[chiquare.result$target]]), as.data.frame(get(paste0(chiquare.result$frame))[[chiquare.result$tested]]))
          names(formula)[1] <- "target"
          names(formula)[2] <- "tested"
          formula <- formula[!(is.na(formula$target)),]
          formula <- formula[!(is.na(formula$tested)),]
          ### Testing number of levels for the 2 variables as 'x' and 'y' must have at least 2 levels
          if ( (chiquare.result[1, c("target")] != chiquare.result[1, c("tested")] ) &
               (nlevels(as.factor(as.character(formula$target))) > 1 ) &
               (nlevels(as.factor(as.character(formula$tested))) > 1 ) &
               (nlevels(as.factor(as.character(formula$target))) < 8 ) &
               (nlevels(as.factor(as.character(formula$tested))) < 8 ) )
          { chiquare.result[1, c("p.value")]  <- round(chisq.test(formula$target,formula$tested)$p.value,4)
          } else {chiquare.result[1, c("p.value")] <- 1  }
          chiquare.resultall <- rbind(chiquare.resultall, chiquare.result)
          rm(chiquare.result)
        }

        ## Subsetting results on test where pvvalue is below 0.05
        chiquare.true <- chiquare.resultall[ chiquare.resultall$p.value <= 0.05, ]

        ### Case there not any positive test
        if (nrow(chiquare.true) == 0 ) {
          cat("No significative association found for this question...\n",file = chapter.name , sep = "\n", append=TRUE)
          cat("No significative association found for this question...\n")

        } else {
          ## now generating correlation plot for each of the dependent.
          for (m in 1:nrow(chiquare.true)) {
            frame<- as.character(chiquare.true[m,4 ])
            target <- as.character(chiquare.true[m,2 ])
            tested <- as.character(chiquare.true[m,3 ])
            target.label <- as.character(chiquare.true[m,7])
            tested.label <- as.character(chiquare.true[m,8 ])

            formula.target    <-get(paste0(frame))[[target]]
            formula.tested    <-get(paste0(frame))[[tested]]

            formula.target1    <-paste0(frame, "$", target)
            formula.tested1    <-paste0(frame, "$", tested)

            ## Open chunk
            cat(paste0("\n```{r ", questions.name,"ccc",m, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=6, size=\"small\"}\n"), file = chapter.name, append=TRUE)

            cat(paste0("corrplot(chisq.test(",formula.target1,",", formula.tested1,")$residuals, is.cor = FALSE,"), file = chapter.name , sep="\n", append=TRUE)
            cat(paste0("cl.pos=\"n\", ## Do not display the color legend"), file = chapter.name , sep="\n", append=TRUE)
            cat(paste0("tl.cex = 0.7, ## Size of axis label"), file = chapter.name , sep="\n", append=TRUE)
            cat(paste0("mar=c(1,1,4,1), ## margin of plots"), file = chapter.name , sep="\n", append=TRUE)
            cat(paste0("title= paste0(\"Correlation between ", "\n",target.label,"\n", " & ",tested.label,"\")) "), file = chapter.name , sep="\n", append=TRUE)
            ## Close chunk
            cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append=TRUE)
          }
        }
      }








      ##Decimal####################################################################################################
    } else if (questions.type =="decimal" | questions.type =="integer" ) {
      cat(paste("Numeric question  " ,"\n\n",sep=""),file = chapter.name ,sep="\n",append=TRUE)

      ## Check the lenght of the table to see if we can display it or not...
      frequ <- as.data.frame(table( get(paste0(questions.frame))[[questions.name]]))

      ####Decimal.tabulation########################################################################
      cat(paste("### Tabulation\n" ,sep=""),file = chapter.name ,sep="\n",append=TRUE)

      ## Open chunk
      cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file = chapter.name, append=TRUE)
      cat(paste0("frequ <- as.data.frame(table(",questions.variable,"))"),file = chapter.name ,sep="\n",append=TRUE)

      ### Check if we have records or if we have too many records
      if (nrow(frequ) %in% c("0","1")){
        cat(paste0("cat(\"No responses recorded for this question...\")"),file = chapter.name , sep="\n", append=TRUE)
        cat("No responses recorded for this question...\n")
      } else if (nrow(frequ) > 10){
        cat(paste0("cat(\"There's too many potential values to display. We will only show the histogram. \n \")"),file = chapter.name ,sep="\n", append=TRUE)
      } else{
        cat(paste0("## display table"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\") %>% kable_styling ( position = \"center\")"),file = chapter.name ,sep="\n",append=TRUE)
      }

      ## To do implement FD number of bin: https://www.r-bloggers.com/friday-function-nclass/
      cat(paste0("average <- as.data.frame(svymean(~ ",questions.name,", design = ",questions.frame,".survey, na.rm=TRUE))"),file = chapter.name ,sep="\n",append=TRUE)
      cat(paste0("cat(paste0(\"Based on the sample design, the average weighted mean response for this question is \", as.numeric(round(average$mean, digits=2))))"),file = chapter.name ,sep="\n",append=TRUE)
      cat(paste0("#  regular histogram"),file = chapter.name ,sep="\n",append=TRUE)
      cat(paste0("ggplot(data=frequ, aes(x=frequ$Var1, y=frequ$Freq)) +"),file = chapter.name ,sep="\n",append=TRUE)
      cat(paste0("geom_bar(fill=\"#2a87c8\",colour=\"white\", stat =\"identity\", width=.8)+"),file = chapter.name ,sep="\n",append=TRUE)
      cat(paste0("labs(x=\"\", y=\"Count\")+"),file = chapter.name ,sep="\n",append=TRUE)
      cat(paste0("theme(plot.title=element_text(face=\"bold\", size=9), plot.background = element_rect(fill = \"transparent\",colour = NA))"),file = chapter.name ,sep="\n",append=TRUE)
      ## Close chunk
      cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append=TRUE)

      ###Decimal.relation###########################################################################
      cat(paste("### Analysis of relationship" ,sep=""),file = chapter.name ,sep="\n",append=TRUE)

      if (nrow(frequ) %in% c("0","1")){
        cat(paste0("cat(\"No responses recorded for this question...\")"),file = chapter.name , sep="\n", append=TRUE)
        cat("No responses recorded for this question...\n")
      } else {
        for(h in 1:nrow(disaggregation))
        {
          #h <-1
          ## Now getting level for each questions
          disag.name <- as.character(disaggregation[ h , c("fullname")])
          disag.shortname <- as.character(disaggregation[ h , c("name")])
          disag.type <- as.character(disaggregation[ h , c("type")])
          disag.frame <- as.character(disaggregation[ h , c("qrepeatlabel")])
          disag.label <- as.character(disaggregation[ h , c("label")])
          disag.listname <- as.character(disaggregation[ h , c("listname")])
          disag.variable <- paste0(questions.frame,"$",disag.name)

          if (disag.variable==questions.variable){
            cat(paste0("cat(\"\\n\")"),file = chapter.name , sep="\n", append=TRUE)
          } else {

            # Get number levels to set up chart height
            figheight <- nlevels( get(paste0(questions.frame))[[disag.name]])
            if ( figheight==0){ figheight<-1}
            else if ( figheight==1) {figheight<-"2"}
            else if ( figheight==2) {figheight<-"3"}
            else if ( figheight==3) {figheight<-"3"}
            else if ( figheight==4) {figheight<-"4"}
            else if ( figheight==5) {figheight<-"4"}
            else if ( figheight==6) {figheight<-"5"}
            else if ( figheight==7) {figheight<-"6"}
            else if ( figheight==8) {figheight<-"7"}
            else if ( figheight==9) {figheight<-"8"}
            else if ( figheight==10) {figheight<-"9"}
            else if ( figheight>=11) {figheight<-"10"}

            ## Open chunk
            cat(paste0("\n```{r ", questions.name,"x",h, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=",figheight,", size=\"small\"}\n"), file = chapter.name, append=TRUE)

            cat(paste("\n", h, " - Render disaggregation: ", disag.label, "for question: ", questions.label,"\n" ))

            cat(paste("\n", h, " - Render disaggregation: ", disag.label, "for question: ", questions.label,"\n" ))
            cat(paste0("crosssfrequ.weight <-as.data.frame(prop.table(svytable(~", questions.name ," + ", disag.name,", design =",questions.frame ,".survey  ), margin = 2))"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("names(crosssfrequ.weight)[1] <- \"quest\""),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("names(crosssfrequ.weight)[2] <- \"disag\""),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("crosssfrequ.weight$Freq2 <- paste0(round(crosssfrequ.weight$Freq*100,digits=1),\"%\")"),file = chapter.name ,sep="\n",append=TRUE)


            cat(paste0("## Reorder factor"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("cross <- dcast(crosssfrequ.weight, disag  ~ quest, value.var = \"Freq\")"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("cross <- cross[ order(cross[ ,2], decreasing = TRUE) ,  ]"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("crosssfrequ.weight$disag <- factor(crosssfrequ.weight$disag, levels = as.character(cross[ ,1]))"),file = chapter.name ,sep="\n",append=TRUE)

            cat(paste0("\n"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("## and now the graph"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("ggplot(crosssfrequ.weight, aes(fill=crosssfrequ.weight$quest, y=crosssfrequ.weight$Freq, x=crosssfrequ.weight$disag)) +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("geom_bar(colour=\"#2a87c8\", stat =\"identity\", width=.8, aes(fill = quest), position = position_stack(reverse = TRUE)) +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("guides(fill=FALSE) +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("geom_label_repel(aes(label = Freq2), fill = \"#2a87c8\", color = 'white') +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("ylab(\"Frequency\") +"),file = chapter.name ,sep="\n",append=TRUE)
            #cat(paste0("facet_wrap(~disag, ncol=3) +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("scale_y_continuous(labels=percent)+"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("xlab(\"\") +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("coord_flip() +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("ggtitle(\"",questions.label,"\","),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("subtitle = paste0(\"Weighted results.\")) +"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("theme(plot.title=element_text(face=\"bold\", size=9),"),file = chapter.name ,sep="\n",append=TRUE)
            cat(paste0("plot.background = element_rect(fill = \"transparent\",colour = NA))"),file = chapter.name ,sep="\n",append=TRUE)

            ## Close chunk
            cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append=TRUE)
          }
        }
      }


      ##select.multi####################################################################################################
    } else if ( questions.type =="select_multiple_d" ) {
      cat(paste("Multiple choice question  " ,"\n\n",sep=""),file = chapter.name ,sep="\n",append=TRUE)


      ###select.multi.tab######################################################################

      cat(paste("### Tabulation" ,sep=""),file = chapter.name ,sep="\n",append=TRUE)

      ##Compute contengency table
      selectmultilist1 <- as.data.frame(dico[dico$type=="select_multiple" & dico$listname == as.character(questions.listname) &
                                               grepl(as.character(questions.shortname),dico$fullname)==TRUE , c("fullname")])
      names(selectmultilist1)[1] <- "check"

      check <- as.data.frame(names(get(paste0(questions.frame))))
      names(check)[1] <- "check"
      check$id <- row.names(check)
      check <- merge(x=check, y=selectmultilist1,by="check")
      selectmultilist <- as.character(check[ ,1])
      ## Reshape answers
      data.selectmultilist <- get(paste0(questions.frame))[ ,selectmultilist ]
      data.selectmultilist$id <- rownames(data.selectmultilist)
      totalanswer <- nrow(data.selectmultilist)
      data.selectmultilist <- data.selectmultilist[ data.selectmultilist[ ,1]!="Not replied", ]
      percentreponse <- paste0(round((nrow(data.selectmultilist)/totalanswer)*100,digits=1),"%")
      meltdata <- melt(data.selectmultilist,id="id")
      castdata <- as.data.frame(table(meltdata[c("value")]))
      castdata$freqper <- castdata$Freq/nrow(data.selectmultilist)
      castdata <- castdata[castdata$Var1!="Not selected", ]
      castdata$Var1 <-factor(castdata$Var1, levels=castdata[order(castdata$freqper), "Var1"])
      frequ <- castdata[castdata$Var1!="", ]

      if (nrow(frequ) %in% c("0","1")){
        cat("No responses recorded for this question...\n",file = chapter.name , sep="\n", append=TRUE)
        cat("No responses recorded for this question...\n")
      } else{

        ## Open chunk
        cat(paste0("\n```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=8, size=\"small\"}\n", sep = '\n'), file = chapter.name, append=TRUE)
        cat(paste0("### Tabulation"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("##Compute contengency table"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("selectmultilist1 <- as.data.frame(dico[dico$type==\"select_multiple\" & dico$listname==\"",questions.listname, "\" & grepl(\"", questions.shortname,"\",dico$fullname)==TRUE , c(\"fullname\")])"),file = chapter.name ,sep="\n",append=TRUE)

        cat(paste0("names(selectmultilist1)[1] <- \"check\""),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("check <- as.data.frame(names(",questions.frame ,"))"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("names(check)[1] <- \"check\""),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("check$id <- row.names(check)"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("check <- merge(x=check, y=selectmultilist1,by=\"check\")"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("selectmultilist <- as.character(check[ ,1])"),file = chapter.name ,sep="\n",append=TRUE)

        cat(paste0("## Reshape answers"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("data.selectmultilist <- ",questions.frame ,"[ selectmultilist ]"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("data.selectmultilist$id <- rownames(data.selectmultilist)"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("totalanswer <- nrow(data.selectmultilist)"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("data.selectmultilist <- data.selectmultilist[ data.selectmultilist[ ,1]!=\"Not replied\", ]"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("percentreponse <- paste0(round((nrow(data.selectmultilist)/totalanswer)*100,digits=1),\"%\")"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("meltdata <- melt(data.selectmultilist,id=\"id\")"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("castdata <- as.data.frame(table(meltdata[c(\"value\")]))"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("castdata$freqper <- castdata$Freq/nrow(data.selectmultilist)"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("castdata <- castdata[castdata$Var1!=\"Not selected\", ]"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("castdata$Var1 <-factor(castdata$Var1, levels=castdata[order(castdata$freqper), \"Var1\"])"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ <- castdata[castdata$Var1!=\"\", ]"),file = chapter.name ,sep="\n",append=TRUE)

        cat(paste0("## display table"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("names(frequ)[1] <- \"", questions.shortname,"\""),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ[ ,3] <- paste0(round(frequ[ ,3]*100,digits=1),\"%\")"),file = chapter.name ,sep="\n",append=TRUE)

        cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\") %>% kable_styling ( position = \"center\")"),file = chapter.name ,sep="\n",append=TRUE)

        cat(paste0("frequ1 <- castdata[castdata$Var1!=\"\", ]"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("frequ1[ ,4] <- paste0(round(frequ1[ ,3]*100,digits=1),\"%\")"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("names(frequ1)[4] <- \"freqper2\""),file = chapter.name ,sep="\n",append=TRUE)

        cat(paste0("\n"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("## and now the graph"),file = chapter.name ,sep="\n",append=TRUE)

        cat(paste0("ggplot(frequ1, aes(x=Var1, y=freqper)) +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("geom_bar(fill=\"#2a87c8\",colour=\"#2a87c8\", stat =\"identity\", width=.8) +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("guides(fill=FALSE) +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("geom_label_repel(aes(y = freqper, label = freqper2), fill = \"#2a87c8\", color = 'white') +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("ylab(\"Frequency\") +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("scale_y_continuous(labels=percent)+"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("xlab(\"\") +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("coord_flip() +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("ggtitle(\"",questions.label,"\","),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("subtitle = paste0(\"Question response rate: \",percentreponse,\" .\")) +"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("theme(plot.title=element_text(face=\"bold\", size=9),"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("plot.background = element_rect(fill = \"transparent\",colour = NA))"),file = chapter.name ,sep="\n",append=TRUE)
        cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append=TRUE)
        ###select.multi.rel######################################################################


        cat(paste("### Analysis of relationship" ,sep=""),file = chapter.name ,sep="\n",append=TRUE)
        ## Open chunk
        cat(paste0("\n```{r ", questions.name, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file = chapter.name, append=TRUE)
        ## Close chunk
        cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append=TRUE)

      }

      ####date###############################################################################################
    } else if (questions.type =="date") {
      cat(paste("Date question  in data frame: ",questions.frame,"\n\n",sep=""),file = chapter.name ,sep="\n",append=TRUE)

      ####text#############################################################################################
    } else if ( questions.type =="text" ) {
      cat(paste("Open ended question  in data frame: ",questions.frame,"\n\n", sep=""),file = chapter.name ,sep="\n",append=TRUE)


      cat(paste("List of given answers \n" ,sep=""),file = chapter.name ,sep="\n",append=TRUE)
      ## Open chunk
      cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file = chapter.name, append=TRUE)
      cat(paste0("textresponse <- as.data.frame(table(",questions.frame,"[!(is.na(",questions.variable,")), c(\"",questions.name,"\")]))"),file = chapter.name ,sep="\n",append=TRUE)

      cat(paste0("names(textresponse)[1] <- \"", questions.shortname,"\""),file = chapter.name ,sep="\n",append=TRUE)
      cat(paste0("kable(textresponse, caption=\"__Table__:", questions.label,"\") %>% kable_styling ( position = \"center\")"),file = chapter.name ,sep="\n",append=TRUE)

      ## Close chunk
      cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append=TRUE)
      # End test on question on type
    }

    ## End loop on questions
  }
  cat(paste("##### Page Break"),file = chapter.name ,sep="\n", append=TRUE)
  #cat(paste("# Indicators from data analysis plan"),file = chapter.name ,sep="\n", append=TRUE)

  # Write the reference to the chapter in the main report file
  #cat(paste0("\n```{r child = '",i,"-", as.character(chapters[ i , 1]), "-chapter.Rmd", "'}\n```\n"), sep = '\n',file="code/report-tabulation.Rmd",append=TRUE)
  # End chapter
}

#rmd <- list.files(pattern = '*-chapter.Rmd', recursive = T, include.dirs = T)
#chunks <- paste0("\n```{r child = '", rmd, "'}\n```\n")
#cat(chunks, sep = '\n')
## Inser chapter child Rmd in the report-tabulation.Rmd


#```{r child = 'chapter1.Rmd'}
#```

### Render now all reports
cat(" Render now reports... \n")

for(i in 1:nrow(chapters)) {
  chaptersname <- as.character(chapters[ i , 1])
  cat(paste(i, " - Render word output report for ",chaptersname))
  render(paste0("code/",i,"-", chaptersname, "-chapter.Rmd", sep=""))
  ## Put the report in the out folder
  file.rename(paste0("code/",i,"-", chaptersname, "-chapter.docx", sep=""), paste0("out/",i,"-", chaptersname,Sys.Date(), "-chapter.docx"))

}

#rmarkdown::render('report-tabulation.Rmd')

cat(" Done!! Reports are in the folder OUT - Review the report- Adjust your configuration files and you will be very soon ready to start the qualitative analysis and the analysis workshops...")



