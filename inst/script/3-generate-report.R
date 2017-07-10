#### Generate Rmd files for each chapter


## Load the form

mainDir <- getwd()
## Load all required packages
source(paste0(mainDir,"/code/0-packages.R"))
library(koboloadeR)

##############################################
## Load form


cat("\n\n Building dictionnary from the xlsform \n")

rm(form)
form <- "form.xls"
## Generate & Load dictionnary
kobo_dico(form)
dico <- read.csv(paste("data/dico_",form,".csv",sep=""), encoding="UTF-8", na.strings="")
rm(form)


### Get the dico with list of chapter

cat("\n\n Building now the chapters of the reports in Rmd  format \n")

chapters <- as.data.frame(unique(dico$chapter))
names(chapters)[1] <- "Chapter"
chapters <- as.data.frame(chapters[!is.na(chapters$Chapter), ])

names(chapters)[1] <- "Chapter"

disaggregation <- dico[which(dico$disaggregation %in% c("facet","correlate")& dico$formpart=="questions"),
                       c("chapter", "name", "label", "type", "qrepeatlabel", "fullname","disaggregation") ]

## for each chapter: create a Rmd file
for(i in 1:nrow(chapters))
{
  # i <-3
  chaptersname <- as.character(chapters[ i , 1])
  cat(paste(i, " - Render chapter for ",as.character(chapters[ i , 1]),"\n" ))
  chapter.name <- paste("code/",i,"-", chaptersname, "-chapter.Rmd", sep="")

  ## Get the

  cat("---", file=chapter.name , sep="\n", append=TRUE)
  cat(paste("title: \"Preliminary exploration of results for Chapter: ",chaptersname , "- Draft not for distribution. \"", sep=""), file=chapter.name ,sep="\n", append=TRUE)
  cat("author: \"Prepared by UNHCR\"", file=chapter.name ,sep="\n", append=TRUE)
  cat("date: \"Amman, prepared on the `r format(Sys.Date(),  '%d %B %Y')`\"", file=chapter.name ,sep="\n", append=TRUE)
  cat("output:",file=chapter.name ,sep="\n", append=TRUE)
  cat("  word_document:", file=chapter.name , sep="\n", append=TRUE)
  cat("    fig_caption: yes", file=chapter.name , sep="\n", append=TRUE)
  cat("    fig_height: 5", file=chapter.name , sep="\n", append=TRUE)
  cat("    fig_width: 8", file=chapter.name , sep="\n", append=TRUE)
  cat("    toc: yes", file=chapter.name , sep="\n", append=TRUE)
  cat("    toc_depth: 2", file=chapter.name , sep="\n", append=TRUE)
  cat("    reference_docx: style-unhcr-portrait.docx", file=chapter.name , sep="\n", append=TRUE)
  cat("---", file=chapter.name , sep="\n", append=TRUE)

  cat(paste("# Compilation of questions results"),file=chapter.name ,sep="\n", append=TRUE)

  ## First chunk to get the data in the report

  cat("```{r setup, include=FALSE, echo=FALSE, warning=FALSE, message=FALSE}", file=chapter.name , sep="\n", append=TRUE)
  cat("mainDir <- getwd()", file=chapter.name , sep="\n", append=TRUE)
  cat("mainDirroot <- substring(mainDir, 0 , nchar(mainDir)- 5)", file=chapter.name , sep="\n", append=TRUE)
  cat("## Load all required packages", file=chapter.name , sep="\n", append=TRUE)
  cat("source(paste0(mainDirroot,\"/code/0-packages.R\"))", file=chapter.name , sep="\n", append=TRUE)
  cat("library(koboloadeR)", file=chapter.name , sep="\n", append=TRUE)
  cat("## Provide below the name of the form in xsl form - format should be xls not xlsx", file=chapter.name , sep="\n", append=TRUE)
  cat("form <- \"form.xls\"", file=chapter.name , sep="\n", append=TRUE)
  cat("dico <- read.csv(paste0(mainDirroot,\"/data/dico_\",form,\".csv\"), encoding=\"UTF-8\", na.strings=\"\")", file=chapter.name , sep="\n", append=TRUE)

  cat("household <- read.csv(paste0(mainDirroot,\"/data/household.csv\"), encoding=\"UTF-8\", na.strings=\"NA\")", file=chapter.name , sep="\n", append=TRUE)
  #cat("case_number_details <- read.csv(paste0(mainDirroot,\"/data/case_number_details.csv\"), encoding=\"UTF-8\", na.strings=\"NA\")", file=chapter.name , sep="\n", append=TRUE)
  #cat("individual_biodata <- read.csv(paste0(mainDirroot,\"/data/individual_biodata.csv\"), encoding=\"UTF-8\", na.strings=\"NA\")", file=chapter.name , sep="\n", append=TRUE)

  cat("## label Variables", file=chapter.name , sep="\n", append=TRUE)
  cat("household <- kobo_label(household , dico)", file=chapter.name , sep="\n", append=TRUE)
  #cat("case_number_details <- kobo_label(case_number_details , dico)", file=chapter.name , sep="\n", append=TRUE)
  #cat("individual_biodata <- kobo_label(individual_biodata , dico)", file=chapter.name , sep="\n", append=TRUE)

  cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)


  chapterquestions <- dico[which(dico$chapter== chaptersname & dico$formpart=="questions"),
                           c("chapter", "name", "label", "type", "qrepeatlabel", "fullname","listname") ]

  #levels(as.factor(as.character(dico[which(!(is.na(dico$chapter)) & dico$formpart=="questions"), c("type") ])))

  for(j in 1:nrow(chapterquestions))
  {
   #j <-3
  ## Now getting level for each questions
  questions.name <- as.character(chapterquestions[ j , c("fullname")])
  questions.shortname <- as.character(chapterquestions[ j , c("name")])
  questions.type <- as.character(chapterquestions[ j , c("type")])
  questions.frame <- as.character(chapterquestions[ j , c("qrepeatlabel")])
  questions.label <- as.character(chapterquestions[ j , c("label")])
  questions.listname <- as.character(chapterquestions[ j , c("listname")])
  questions.variable <- paste0(questions.frame,"$",questions.name)
  ## write question name
  cat("\n ",file=chapter.name , sep="\n",append=TRUE)
  cat(paste("## ", questions.label ,sep=""),file=chapter.name , sep="\n", append=TRUE)

  ## Now create para based on question type

######################################################################################################
######################################################################################################
  if (questions.type =="select_one" ) {
    cat(paste("Single choice question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ##############################################################################

    cat(paste("### Tabulation" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n"), file=chapter.name, append=TRUE)

    cat(paste("### Tabulation" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("##Compute contengency table"),file=chapter.name ,sep="\n",append=TRUE)

    frequ <- as.data.frame(table( get(paste0(questions.frame))[[questions.name]]))

    cat(paste0("frequ <- as.data.frame(table(",questions.variable,"))"),file=chapter.name ,sep="\n",append=TRUE)

    #cat(paste0("if (nrow(frequ)==0){ cat(\"No response for this question\") } else{"),file=chapter.name ,sep="\n",append=TRUE)

    if (nrow(frequ) %in% c("0","1")){
    cat(paste0("cat(\"No response recorded for this question...\")"),file=chapter.name ,sep="\n", append=TRUE)


        } else{

    cat(paste0("## display table"),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("## Reorder factor"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ[ ,1] = factor(frequ[ ,1],levels(frequ[ ,1])[order(frequ$Freq, decreasing = TRUE)])"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ <- frequ[ order(frequ[ , 1]) ,  ]"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("names(frequ)[1] <- \"", questions.shortname,"\""),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\") %>% kable_styling ( position = \"center\")"),file=chapter.name ,sep="\n",append=TRUE)


    cat(paste0("## Frequency table with NA in order to get non response rate"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ1 <- as.data.frame(prop.table(table(", questions.variable,", useNA=\"ifany\")))"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ1 <- frequ1[!(is.na(frequ1$Var1)), ]"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ1 <- frequ1[!(frequ1$Var1==\"NA\"), ]"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("percentreponse <- paste0(round(sum(frequ1$Freq)*100,digits=1),\"%\")"),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("## Frequency table without NA"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ2 <- as.data.frame(prop.table(table(", questions.variable,",useNA = \"no\")))"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("## Reorder factor"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ2[ ,1] = factor(frequ2[ ,1],levels(frequ2[ ,1])[order(frequ2$Freq, decreasing = TRUE)])"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ2 <- frequ2[ order(frequ2[ , 1]) ,  ]"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ2[ ,3] <- paste0(round(frequ2[ ,2]*100,digits=1),\"%\")"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("names(frequ2)[3] <- \"freqper2\""),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("## and now the graph"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("ggplot(frequ2, aes(x=frequ2$Var1, y=frequ2$Freq)) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("geom_bar(fill=\"#2a87c8\",colour=\"#2a87c8\", stat =\"identity\", width=.5) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("guides(fill=FALSE) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("geom_label_repel(aes(y = Freq, label = freqper2), fill = \"#2a87c8\", color = 'white') +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("ylab(\"Frequency\") +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("scale_y_continuous(labels=percent)+"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("xlab(\"\") +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("coord_flip() +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("ggtitle(\"",questions.label,"\","),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("subtitle = paste0(\"Question response rate: \",percentreponse,\" .\")) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("theme(plot.title=element_text(face=\"bold\", size=9),"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("plot.background = element_rect(fill = \"transparent\",colour = NA))"),file=chapter.name ,sep="\n",append=TRUE)
    }
    #cat(paste0("}"),file=chapter.name ,sep="\n",append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################


    cat(paste("### Analysis of relationship" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)

    cat(paste("### Qualitative elements\n"),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("#### __Reflect__: Data quality and or suggestions to change questions  \n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("#### __Interpret__: Qualitative interpretations of data patterns  \n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("#### __Recommend__: Recommendation in terms of programmatic adjustment\n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("#### __Classify__: Level of sensitivity for the information  \n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)

######################################################################################################
######################################################################################################
  } else if (questions.type =="decimal" | questions.type =="integer" ) {
    cat(paste("Numeric question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ##############################################################################
    cat(paste("### Tabulation\n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ## Open chunk
    cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file=chapter.name, append=TRUE)


    cat(paste0("frequ <- as.data.frame(table(",questions.variable,"))"),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("## display table"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\") %>% kable_styling ( position = \"center\")"),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("#  regular histogram"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("ggplot(data=frequ, aes(x=frequ$Var1, y=frequ$Freq)) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("geom_bar(fill=\"#2a87c8\",colour=\"white\", stat =\"identity\", width=.5)+"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("labs(x=\"\", y=\"Count\")+"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("theme(plot.title=element_text(face=\"bold\", size=9), plot.background = element_rect(fill = \"transparent\",colour = NA))"),file=chapter.name ,sep="\n",append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################

    cat(paste("### Analysis of relationship" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)


    cat(paste("### Qualitative elements  \n"),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("#### __Reflect__: Data quality and or suggestions to change questions  \n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("#### __Interpret__: Qualitative interpretations of data patterns   \n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("#### __Recommend__: Recommendation in terms of programmatic adjustment  \n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("#### __Classify__: Level of sensitivity for the information  \n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)

######################################################################################################
######################################################################################################
  } else if ( questions.type =="select_multiple_d" ) {
    cat(paste("Multiple choice question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

    ##############################################################################

    cat(paste("### Tabulation" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file=chapter.name, append=TRUE)
    cat(paste0("### Tabulation"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("##Compute contengency table"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("selectmultilist1 <- as.data.frame(dico[dico$type==\"select_multiple\" & dico$listname==\"",questions.listname, "\" & grepl(\"", questions.shortname,"\",dico$fullname)==TRUE , c(\"fullname\")])"),file=chapter.name ,sep="\n",append=TRUE)


    cat(paste0("names(selectmultilist1)[1] <- \"check\""),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("check <- as.data.frame(names(",questions.frame ,"))"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("names(check)[1] <- \"check\""),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("check$id <- row.names(check)"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("check <- merge(x=check, y=selectmultilist1,by=\"check\")"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("selectmultilist <- as.character(check[ ,1])"),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("## Reshape answers"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("data.selectmultilist <- ",questions.frame ,"[ selectmultilist ]"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("data.selectmultilist$id <- rownames(data.selectmultilist)"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("totalanswer <- nrow(data.selectmultilist)"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("data.selectmultilist <- data.selectmultilist[ data.selectmultilist[ ,1]!=\"Not replied\", ]"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("percentreponse <- paste0(round((nrow(data.selectmultilist)/totalanswer)*100,digits=1),\"%\")"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("meltdata <- melt(data.selectmultilist,id=\"id\")"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("castdata <- as.data.frame(table(meltdata[c(\"value\")]))"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("castdata$freqper <- castdata$Freq/nrow(data.selectmultilist)"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("castdata <- castdata[castdata$Var1!=\"Not selected\", ]"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("castdata$Var1 <-factor(castdata$Var1, levels=castdata[order(castdata$freqper), \"Var1\"])"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ <- castdata[castdata$Var1!=\"\", ]"),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("## display table"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("names(frequ)[1] <- \"", questions.shortname,"\""),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ[ ,3] <- paste0(round(frequ[ ,3]*100,digits=1),\"%\")"),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\") %>% kable_styling ( position = \"center\")"),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("frequ1 <- castdata[castdata$Var1!=\"\", ]"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("frequ1[ ,4] <- paste0(round(frequ1[ ,3]*100,digits=1),\"%\")"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("names(frequ1)[4] <- \"freqper2\""),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("## and now the graph"),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("ggplot(frequ1, aes(x=Var1, y=freqper)) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("geom_bar(fill=\"#2a87c8\",colour=\"#2a87c8\", stat =\"identity\", width=.5) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("guides(fill=FALSE) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("geom_label_repel(aes(y = freqper, label = freqper2), fill = \"#2a87c8\", color = 'white') +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("ylab(\"Frequency\") +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("scale_y_continuous(labels=percent)+"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("xlab(\"\") +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("coord_flip() +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("ggtitle(\"",questions.label,"\","),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("subtitle = paste0(\"Question response rate: \",percentreponse,\" .\")) +"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("theme(plot.title=element_text(face=\"bold\", size=9),"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("plot.background = element_rect(fill = \"transparent\",colour = NA))"),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
    ##############################################################################


    cat(paste("### Analysis of relationship" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("\n```{r ", questions.name, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file=chapter.name, append=TRUE)
    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)


    cat(paste("### Qualitative elements  \n"),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("#### __Reflect__: Data quality and or suggestions to change questions  \n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("#### __Interpret__: Qualitative interpretations of data patterns  \n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("#### __Recommend__: Recommendation in terms of programmatic adjustment  \n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("#### __Classify__: Level of sensitivity for the information  \n"),file=chapter.name ,sep="\n",append=TRUE)
    cat("_Insert your notes here!_  \n",file=chapter.name ,sep="\n",append=TRUE)



######################################################################################################
######################################################################################################
  } else if (questions.type =="date") {
    cat(paste("Date question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)

######################################################################################################
######################################################################################################
  } else if ( questions.type =="text" ) {
    cat(paste("Open ended question \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)


    cat(paste("List of given answers \n" ,sep=""),file=chapter.name ,sep="\n",append=TRUE)
    ## Open chunk
    cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file=chapter.name, append=TRUE)
    cat(paste0("textresponse <- as.data.frame(table(",questions.frame,"[!(is.na(",questions.variable,")), c(\"",questions.name,"\")]))"),file=chapter.name ,sep="\n",append=TRUE)

    cat(paste0("names(textresponse)[1] <- \"", questions.shortname,"\""),file=chapter.name ,sep="\n",append=TRUE)
    cat(paste0("kable(textresponse, caption=\"__Table__:", questions.label,"\") %>% kable_styling ( position = \"center\")"),file=chapter.name ,sep="\n",append=TRUE)

    ## Close chunk
    cat(paste0("\n```\n", sep = '\n'), file=chapter.name, append=TRUE)
  # End test on question on type
  }

  ## End loop on questions
  }
  cat(paste("##### Page Break"),file=chapter.name ,sep="\n", append=TRUE)
  cat(paste("# Indicators from data analysis plan"),file=chapter.name ,sep="\n", append=TRUE)

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
  render(paste0("code/",i,"-", chaptersname, "-chapter.Rmd", sep="")) }

#rmarkdown::render('report-tabulation.Rmd')

cat(" Done!! Reports are in the folder CODE > REPORT - You are now ready to start the qualitative analysis and the analysis workshops...")



