#' @name kobo_crunching_report
#' @rdname kobo_crunching_report
#' @title Generate Data Crunching Report
#'
#' @description Generate crunching Report that contains all descriptive statistics, correlation analysis, tabulation and data visualization for variables and indicators.
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#' @param app The place where the function has been executed, the default is the console and the second option is the shiny app
#'
#' @return No return, All results will be saved on RMD files and Word files
#'
#' @author Edouard Legoupil, Maher Daoud
#'
#'
#' @examples
#' \dontrun{
#' kobo_crunching_report("myform.xls")
#' }
#'
#' @export kobo_crunching_report
#'

kobo_crunching_report <- function(form = "form.xls", app = "console") {
  tryCatch({
    if (app == "shiny") {
      progress <- shiny::Progress$new()
      progress$set(message = "Generating crunching report in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 100
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()
    }
    ## Load all required packages
    kobo_load_packages()
    configInfo <- kobo_get_config()
    configInfo <- configInfo[!is.na(configInfo$name),]
    mainDir <- kobo_getMainDirectory()
    form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
    #library(koboloadeR)

    ### Load the data
    cat("\n\n Loading data. It is assumed that the cleaning, weighting & re-encoding has been done previously \n")

    MainDataFrame <- utils::read.csv(paste(mainDir,"/data/MainDataFrame_encoded.csv",sep = ""), encoding = "UTF-8", na.strings = "")


    ###Form##########################################
    ## Load form
    cat("\n\n Building dictionnary from the xlsform \n")

    #form <- "form.xls"
    ## Generate & Load dictionnary
    #kobo_dico(form)
    dico <- utils::read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")
    #rm(form)


    ## label Variables
    cat("\n\n Labelling variables \n")
    if (app == "shiny") {
      progress$set(message = "Labelling variables in the Main Data File in progress...")
      updateProgress()
    }

    MainDataFrame <- kobo_label(MainDataFrame , dico)


    cat("\n\nload all required data files..\n")
    dataBeginRepeat <- kobo_get_begin_repeat()
    dataBeginRepeat <- dataBeginRepeat$names
    for (dbr in dataBeginRepeat) {

      dataFrame <- utils::read.csv(paste(mainDir,"/data/",dbr,"_encoded.csv",sep = ""),stringsAsFactors = F)

      assign(dbr, kobo_label(dataFrame, dico))
      if (app == "shiny") {
        progress$set(message = paste("Labelling variables in",dbr,"File in progress..."))
        updateProgress()
      }
    }


    ## Get a list of variables to be used for disaggregation #######
    disaggregation <- dico[ which(dico$disaggregation %in% c("facet", "stak", "fill", "dodge") & dico$formpart == "questions"),
                            c("chapter", "name", "label","labelReport", "type", "qrepeatlabel", "fullname", "disaggregation", "correlate", "listname", "variable") ]

    ## Get a list of variables to be used for analyisis of association - chisquarred #######
    correlation <- dico[which(dico$type %in% c("select_multiple_d","select_one") & !(is.na(dico$correlate)) & dico$formpart == "questions"),
                        c("chapter", "name", "label","labelReport", "type", "qrepeatlabel", "fullname", "disaggregation", "correlate", "listname", "variable") ]

    ## Get a list of variables to be used for analyisis of association - chisquarred #######
    ordinal <- dico[which(dico$type %in% c("select_multiple_d","select_one") & dico$variable == "ordinal"),
                    c( "qrepeatlabel", "fullname",  "listname", "variable") ]



    ## To do: insert reference to formpart when there's an indicator reference
    # & dico$formpart=="questions"


    ### Get the dico with list of chapter

    cat("\n\n Building now the chapters of the reports in Rmd  format \n")
    if (app == "shiny") {
      progress$set(message = "Building now the chapters of the reports in Rmd format in progress...")
      updateProgress()
    }
    chapters <- as.data.frame(unique(dico$chapter))
    names(chapters)[1] <- "Chapter"

    ## Default behavior if no chapter was defined in xlsform
    if ((nrow(chapters) == 1) & is.na(chapters$Chapter)) {
      cat("Defaulting questions allocation to chapter")
      dico$chapter[ dico$type %in% c("select_one","select_multiple_d")] <- "report"
      chapters <- as.data.frame(unique(dico$chapter))
      names(chapters)[1] <- "Chapter"
    } else {}

    chapters <- as.data.frame(chapters[!is.na(chapters$Chapter), ])

    names(chapters)[1] <- "Chapter"

    utils::write.csv(chapters, paste(mainDir,"/data/chapters.csv",sep = ""), row.names = FALSE, na = "")


    ## for each chapter: create a Rmd file -------

    ##Loop.chapter ------------

    for (i in 1:nrow(chapters) )
    {
      # i <-1
      chaptersname <- as.character(chapters[ i , 1])
      if (app == "shiny") {
        progress$set(message = paste(i, " - Render chapter for ",as.character(chapters[ i , 1])))
        updateProgress()
      }
      cat(paste(i, " - Render chapter for ",as.character(chapters[ i , 1]),"\n" ))
      chapter.name <- paste(mainDir, "/code/",i,"-", chaptersname, "-chapter.Rmd", sep = "")

      ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
      if (file.exists(chapter.name)) file.remove(chapter.name)

      ## TO DO : put in configuration file name of report, author, organisation & location
      ## TO DO : put in configuration wethere report should be portrait or landscape
      cat("---", file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("title: \"Data Crunching Report: ",chaptersname , "- Draft not for distribution. \"", sep = ""), file = chapter.name , sep = "\n", append = TRUE)
      cat("author: \"Generated with [Koboloader](https://unhcr.github.io/koboloadeR/docs) \"", file = chapter.name , sep = "\n", append = TRUE)
      cat("date: \" `r format(Sys.Date(),  '%d %B %Y')`\"", file = chapter.name , sep = "\n", append = TRUE)
      cat("always_allow_html: yes", file = chapter.name , sep = "\n", append = TRUE)
      cat("output:",file = chapter.name , sep = "\n", append = TRUE)
      cat("  word_document:", file = chapter.name , sep = "\n", append = TRUE)
      cat("    fig_caption: yes", file = chapter.name , sep = "\n", append = TRUE)
      cat("    fig_height: 5", file = chapter.name , sep = "\n", append = TRUE)
      cat("    fig_width: 8", file = chapter.name , sep = "\n", append = TRUE)
      cat("    toc: yes", file = chapter.name , sep = "\n", append = TRUE)
      cat("    toc_depth: 2", file = chapter.name , sep = "\n", append = TRUE)
      cat("    reference_docx: style-unhcr-portrait.docx", file = chapter.name , sep = "\n", append = TRUE)
      cat("---", file = chapter.name , sep = "\n", append = TRUE)


      ## First chunk to get the data in the report

      cat("```{r setup, include = FALSE, echo = FALSE, warning = FALSE, message = FALSE}", file = chapter.name , sep = "\n", append = TRUE)
      cat("mainDir <- getwd()", file = chapter.name , sep = "\n", append = TRUE)
      cat("mainDirroot <- substring(mainDir, 0 , nchar(mainDir) - 5)", file = chapter.name , sep = "\n", append = TRUE)


      cat("## Load all required packages", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(tidyverse)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(ggthemes)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(plyr)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(ggrepel)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(viridis)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(RColorBrewer)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(extrafont)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(corrplot)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(reshape2)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(scales)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(survey)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(knitr)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(rmarkdown)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(ggpubr)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(grid)", file = chapter.name , sep = "\n", append = TRUE)
      cat("library(koboloadeR)", file = chapter.name , sep = "\n", append = TRUE)
      cat("options(scipen = 999) # turn-off scientific notation like 1e+48", file = chapter.name , sep = "\n", append = TRUE)

      cat("## Provide below the name of the form in xsl form - format should be xls not xlsx", file = chapter.name , sep = "\n", append = TRUE)
      cat(paste0("form <- \"",form,"\""), file = chapter.name , sep = "\n", append = TRUE)
      cat("dico <- utils::read.csv(paste0(mainDirroot,\"/data/dico_\",form,\".csv\"), encoding = \"UTF-8\", na.strings = \"\")", file = chapter.name , sep = "\n", append = TRUE)


      ## TO DO: Use config file to load the different frame


      cat("MainDataFrame <- utils::read.csv(paste0(mainDirroot,\"/data/MainDataFrame_encoded.csv\"), encoding = \"UTF-8\", na.strings = \"\")", file = chapter.name , sep = "\n", append = TRUE)


      for (dbr in dataBeginRepeat) {
        cat(paste(dbr, " <- utils::read.csv(paste0(mainDirroot,\"/data/",dbr,"_encoded.csv\"), encoding = \"UTF-8\", na.strings = \"\")", sep = ""), file = chapter.name , sep = "\n", append = TRUE)

      }


      cat("\n", file = chapter.name , sep = "\n", append = TRUE)
      cat("## label Variables", file = chapter.name , sep = "\n", append = TRUE)

      cat("MainDataFrame <- kobo_label(MainDataFrame , dico)", file = chapter.name , sep = "\n", append = TRUE)

      for (dbr in dataBeginRepeat) {
        cat(paste(dbr, " <- kobo_label(",dbr ," , dico)", sep = ""), file = chapter.name , sep = "\n", append = TRUE)
      }

      #### Convert to ordinal variable

      cat("\n", file = chapter.name , sep = "\n", append = TRUE)
      cat("## Set up ordinal Variables", file = chapter.name , sep = "\n", append = TRUE)
      if (nrow(ordinal) > 0) {

        for (o in 1:nrow(ordinal)) {
          # o <- 1
          ordinal.listname <- as.character(ordinal[ o, c("listname")])
          ordinal.name <- as.character(ordinal[ o, c("fullname")])
          ordinal.frame <- as.character(ordinal[ o, c("qrepeatlabel")])
          if ( exists(paste0(ordinal.frame)) == TRUE) {
            cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", ordinal.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0(ordinal.frame,"$",ordinal.name," <- factor(",ordinal.frame,"$",ordinal.name,", levels = list.ordinal)"),file = chapter.name ,sep = "\n", append = TRUE)
          } else {}
        }
      } else {}

      ## To do use configuration file to weight the data #######
      cat("\n", file = chapter.name , sep = "\n", append = TRUE)
      cat("## Create weighted survey object", file = chapter.name , sep = "\n", append = TRUE)

        ## If no weight, the weighted object is unweigthted

      if (configInfo[configInfo$name == "sample_type","value"] == "No sampling (type 1)") {
        ## If no weight, the weighted object is unweigthted
        cat("MainDataFrame.survey <- survey::svydesign(ids = ~ 1 ,  data = MainDataFrame )", file = chapter.name , sep = "\n", append = TRUE)

        for (dbr in dataBeginRepeat) {
          cat(paste(dbr,".survey <- survey::svydesign(ids = ~ 1 ,  data = ",dbr," )", sep = ""), file = chapter.name , sep = "\n", append = TRUE)
        }
        ## with clusters

      }else if (configInfo[configInfo$name == "sample_type","value"] == "Cluster sample (type 2)") {
        ## with clusters
        cat(paste("MainDataFrame.survey <- survey::svydesign(ids = ~ ", configInfo[configInfo$name == "variable_name","value"],",  data = MainDataFrame,  weights = ~ ", configInfo[configInfo$name == "weightsVariable","value"]," ,  fpc = ~ fpc )", sep = ""), file = chapter.name , sep = "\n", append = TRUE)

        for (dbr in dataBeginRepeat) {
          cat(paste(dbr,".survey <- survey::svydesign(ids = ~ ", configInfo[configInfo$name == "variable_name","value"],",  data = ",dbr,",  weights = ~ ", configInfo[configInfo$name == "weightsVariable","value"]," ,  fpc = ~ fpc )", sep = ""), file = chapter.name , sep = "\n", append = TRUE)
        }
        ## with strata

      }else if (configInfo[configInfo$name == "sample_type","value"] == "Stratified sample (type 3)") {
        ## with strata
        cat(paste("MainDataFrame.survey <- survey::svydesign(id=~1, strata= ~ ", configInfo[configInfo$name == "variable_name","value"]," ,check.strata = TRUE,  data = MainDataFrame,  weights = ~ ", configInfo[configInfo$name == "weightsVariable","value"],"  )", sep = ""), file = chapter.name , sep = "\n", append = TRUE)

        for (dbr in dataBeginRepeat) {
          cat(paste(dbr,".survey <- survey::svydesign(id=~1, strata= ~ ", configInfo[configInfo$name == "variable_name","value"]," ,check.strata = TRUE,  data = ",dbr,",  weights = ~ ", configInfo[configInfo$name == "weightsVariable","value"],"  )", sep = ""), file = chapter.name , sep = "\n", append = TRUE)
        }
      }



      ## with strata
      #cat("MainDataFrame_edited.survey <- survey::svydesign(id=~1, strata= ~ RecordCategory ,check.strata = TRUE,  data = MainDataFrame_edited,  weights = ~ WeightingCoefficient  )", file = chapter.name , sep = "\n", append = TRUE)

      ## with clusters
      #cat("MainDataFrame_edited.survey <- survey::svydesign(ids = ~ Camp.Province ,  data = MainDataFrame_edited,  weights = ~ weight ,  fpc = ~ fpc )", file = chapter.name , sep = "\n", append = TRUE)
      #cat("br1.survey <- survey::svydesign(ids = ~ Camp.Province ,  data = br1,  weights = ~ weight ,  fpc = ~ fpc )", file = chapter.name , sep = "\n", append = TRUE)
      #cat("br2.survey <- survey::svydesign(ids = ~ Camp.Province ,  data = br2,  weights = ~ weight ,  fpc = ~ fpc )", file = chapter.name , sep = "\n", append = TRUE)

      # ## If no weight, the weighted object is unweigthted
      # cat("MainDataFrame_edited.survey <- survey::svydesign(ids = ~ 1 ,  data = MainDataFrame_edited )", file = chapter.name , sep = "\n", append = TRUE)
      # cat("br1.survey <- survey::svydesign(ids = ~ 1 ,  data = br1 )", file = chapter.name , sep = "\n", append = TRUE)
      # cat("br2.survey <- (ids = ~ 1 ,  data = br2 )", file = chapter.name , sep = "\n", append = TRUE)


      cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)


      ### To DO : Offer option to insert in the report skeleton interpretation questions
      ### Intro text####################################################################
      cat(paste("# Introduction\n"),file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("This data crunching report allows to quickly explore the results of the survey that can be regenerated as needed."),file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("You can:  \n "),file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("  *  adjust the configuration in the xlsform to break it into chapter;   \n"),file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("  *  configure disaggregation & correlation for each questions;   \n"),file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("  *  revise the data cleansing based on the cleaning log;   \n "),file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("  *  add weight to each observation in order to account for a specific sampling plan;  \n"),file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("  *  append calculated indicators to your data frame. \n\n"),file = chapter.name , sep = "\n", append = TRUE)

      cat(paste("The objective of this report is to allow to quickly identify potential patterns in your dataset.
                A quick screening of this initial report should allow to select the most meaningful graphs.
                A data digest from this initial report can be then reviewed through a __data analysis workshop__
                where subject matter experts can contribute with qualitative components.
                When analyzing those representations in a collective setting, you may:  \n "),file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("  *  __Reflect__: question data quality and/or make suggestions to change questions;   \n"),file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("  *  __Interpret__: develop qualitative interpretations of data patterns;   \n"),file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("  *  __Recommend__: develop recommendation in terms of programmatic adjustment;   \n"),file = chapter.name , sep = "\n", append = TRUE)
      cat(paste("  *  __Classify__: order the level of sensitivity for the information;   \n"),file = chapter.name , sep = "\n", append = TRUE)


      cat(paste("# Compilation of questions results"),file = chapter.name , sep = "\n", append = TRUE)
      if (app == "shiny") {
        progress$set(message = "Compilation of questions results in progress...")
        updateProgress()
      }
      ## Getting chapter questions #######
      #chapterquestions <- dico[which(dico$chapter== chaptersname ), c("chapter", "name", "label", "type", "qrepeatlabel", "fullname","listname") ]
      chapterquestions <- dico[which(dico$chapter == chaptersname & dico$type %in% c("select_one","integer","select_multiple_d", "text","date", "numeric")),
                               c("chapter", "name", "label", "labelReport","hintReport", "type", "qrepeatlabel", "fullname","listname","variable") ]
      #levels(as.factor(as.character(dico[which(!(is.na(dico$chapter)) & dico$formpart=="questions"), c("type") ])))
      ##Loop.questions####################################################################################################
      if (app == "shiny") {
        progress$set(message = "Getting level for each questions in progress...")
        updateProgress()
      }
      for (j in 1:nrow(chapterquestions))
      {
        # j <-1
        ## Now getting level for each questions
        if (app == "shiny") {
          progress$set(message = paste("Render question: ",as.character(chapterquestions[ j , c("labelReport")])))
          updateProgress()
        }
        questions.name <- as.character(chapterquestions[ j , c("fullname")])
        questions.shortname <- as.character(chapterquestions[ j , c("name")])
        questions.type <- as.character(chapterquestions[ j , c("type")])
        questions.frame <- as.character(chapterquestions[ j , c("qrepeatlabel")])
        questions.label <- as.character(chapterquestions[ j , c("labelReport")])
        questions.hint <- as.character(chapterquestions[ j , c("hintReport")])
        questions.listname <- as.character(chapterquestions[ j , c("listname")])
        questions.ordinal <- as.character(chapterquestions[ j , c("variable")])
        if (is.na(questions.ordinal) ) {questions.ordinal <- "not.defined"} else {questions.ordinal <- questions.ordinal }
        questions.variable <- paste0(questions.frame,"$",questions.name)
        cat(paste("\n", i, "-", j, " - Render question: ", questions.variable, " -",questions.type, "\n" ))

        ## write question name-------
        cat("\n ",file = chapter.name , sep = "\n", append = TRUE)
        cat(paste("## ", questions.label ,sep = ""),file = chapter.name , sep = "\n", append = TRUE)

        ## Now create para based on question type-------


        cat(paste(questions.hint,"\n\n",sep = ""),file = chapter.name ,sep = "\n", append = TRUE)


        ###select one###################################################################################################
        if (questions.type == "select_one" ) {

          cat(paste("Single choice question ","\n\n",sep = ""),file = chapter.name ,sep = "\n", append = TRUE)

          ###selectone.tabulation######################################################################
          ## compute frequency to see if it's not empty
          frequ <- as.data.frame(table( get(paste0(questions.frame))[[questions.name]]))

          figheight <- as.integer(nrow(frequ))
          if (figheight == 0) { figheight <- 1} else {figheight <- figheight/1.2}

          ## Check that there are responses to be displayed ####
          if (nrow(frequ) %in% c("0","1") ) {
            cat(paste0("cat(\"No responses or only one modality recorded for this question...\")"),file = chapter.name , sep = "\n", append = TRUE)
            cat("No responses recorded for this question...\n")

            #  names(frequ)[2] <- "ccheck"
            #  try <- frequ$ccheck
            #  } else if (sum(try) == 0) {
            #   cat(paste0("cat(\"No responses recorded for this question...\")"),file = chapter.name , sep = "\n", append = TRUE)
            #    cat("No responses recorded for this question...\n")
          }      else {

          cat(paste("### Tabulation" ,sep = ""),file = chapter.name ,sep = "\n", append = TRUE)
          ## Open chunk
          cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=",figheight,", size=\"small\"}\n"), file = chapter.name, append = TRUE)
          cat(paste("### Tabulation" ,sep = ""),file = chapter.name ,sep = "\n", append = TRUE)
          cat(paste0("##Compute contengency table"),file = chapter.name ,sep = "\n", append = TRUE)
          cat(paste0("frequ <- as.data.frame(table(",questions.variable,"))"),file = chapter.name ,sep = "\n", append = TRUE)
          #cat(paste0("if (nrow(frequ)==0){ cat(\"No response for this question\") } else{"),file = chapter.name ,sep = "\n", append = TRUE)


         #   cat(paste0("## display table"),file = chapter.name ,sep = "\n", append = TRUE)
         #   cat(paste0("## Reorder factor"),file = chapter.name ,sep = "\n", append = TRUE)


            ## Check variable type to order the factor ####
            ## - if not ordinal order according to frequency - if ordinal order according to order in the dico
            if (questions.ordinal == "ordinal" ) {
              ### get the list of options in the right order
              cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", questions.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("levels(frequ$Var1) <- list.ordinal"),file = chapter.name ,sep = "\n", append = TRUE)
            } else {
              cat(paste0("frequ[ ,1] = factor(frequ[ ,1],levels(frequ[ ,1])[order(frequ$Freq, decreasing = FALSE)])"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ <- frequ[ order(frequ[ , 1]) ,  ]"),file = chapter.name ,sep = "\n", append = TRUE)
            }


            cat(paste0("names(frequ)[1] <- \"", questions.shortname,"\""),file = chapter.name ,sep = "\n", append = TRUE)
           # cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\")"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("## Frequency table with NA in order to get non response rate"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("frequ1 <- as.data.frame(prop.table(table(", questions.variable,", useNA = \"ifany\")))"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("frequ1 <- frequ1[!(is.na(frequ1$Var1)), ]"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("frequ1 <- frequ1[!(frequ1$Var1 == \"NA\"), ]"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("percentreponse <- paste0(round(sum(frequ1$Freq)*100,digits = 1),\"%\")"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("## Frequency table without NA"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("frequ2 <- as.data.frame(prop.table(table(", questions.variable,",useNA = \"no\")))"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("## Frequency table with weight"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("frequ.weight <- as.data.frame(svymean(~ ",questions.name,", design = ",questions.frame,".survey, na.rm = TRUE))"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("## Binding the two"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("frequ3 <- cbind(frequ2,frequ.weight)"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("## Reorder factor"),file = chapter.name ,sep = "\n", append = TRUE)
            #    cat(paste0("frequ2[ ,1] = factor(frequ2[ ,1],levels(frequ2[ ,1])[order(frequ2$Freq, decreasing = FALSE)])"),file = chapter.name ,sep = "\n", append = TRUE)
            #    cat(paste0("frequ2 <- frequ2[ order(frequ2[ , 1]) ,  ]"),file = chapter.name ,sep = "\n", append = TRUE)
            #    cat(paste0("frequ2[ ,3] <- paste0(round(frequ2[ ,2]*100,digits = 1),\"%\")"),file = chapter.name ,sep = "\n", append = TRUE)
            #    cat(paste0("names(frequ2)[3] <- \"freqper2\""),file = chapter.name ,sep = "\n", append = TRUE)


            if (questions.ordinal == "ordinal" ) {
              cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", questions.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("levels(frequ3$Var1) <- list.ordinal"),file = chapter.name ,sep = "\n", append = TRUE)
            } else {
              cat(paste0("frequ3[ ,1] = factor(frequ3[ ,1],levels(frequ3[ ,1])[order(frequ3$mean, decreasing = FALSE)])"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ3 <- frequ3[ order(frequ3[ , 1]) ,  ]"),file = chapter.name ,sep = "\n", append = TRUE)
            }

            cat(paste0("frequ3[ ,5] <- paste0(round(frequ3[ ,3]*100,digits = 1),\"%\")"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("names(frequ3)[5] <- \"freqper2\""),file = chapter.name ,sep = "\n", append = TRUE)

            cat(paste0("\n"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("## and now the graph"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("plot1 <- ggplot(frequ3, aes(x = frequ3$Var1, y = frequ3$mean)) +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("geom_bar(fill = \"#2a87c8\", colour = \"#2a87c8\", stat = \"identity\", width=.8) +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("guides(fill = FALSE) +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("geom_label_repel(aes(y = mean, label = freqper2), fill = \"#2a87c8\", color = 'white') +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("ylab(\"Frequency\") +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("scale_y_continuous(labels = percent) +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("xlab(\"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("coord_flip() +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("ggtitle(\"",questions.label,"\","),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("subtitle = paste0(\" Question response rate: \",percentreponse,\" .\")) +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("kobo_unhcr_style_bar()"),file = chapter.name ,sep = "\n", append = TRUE)

            cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = chapter.name ,sep = "\n", append = TRUE)
          }


          #cat(paste0("}"),file = chapter.name ,sep = "\n", append = TRUE)
          ## Close chunk
          cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)
          cat(paste0("\n\n\n\n", sep = '\n'), file = chapter.name, append = TRUE)

          ##selectone.crosstabulation #######################################################################
          if (nrow(disaggregation) == 0) {
            cat("No disaggregation requested for this question...\n",file = chapter.name , sep = "\n", append = TRUE)
            cat("No disaggregation requested for this question...\n")
            cat("\n", file = chapter.name, append = TRUE)
          } else if (nrow(frequ) %in% c("0","1")) {
            # cat("No responses recorded for this question. No disaggregation...\n",file = chapter.name , sep = "\n", append = TRUE)
            cat("No responses or only one modality recorded for this question. No disaggregation...\n")
            cat("\n", file = chapter.name, append = TRUE)
          } else {

            cat(paste("### Cross-tabulations" ,sep = ""),file = chapter.name ,sep = "\n", append = TRUE)
            cat("\n", file = chapter.name, append = TRUE)

            for (h in 1:nrow(disaggregation))
            {
              #h <-3
              ## Now getting level for each questions
              disag.name <- as.character(disaggregation[ h , c("fullname")])
              disag.shortname <- as.character(disaggregation[ h , c("name")])
              disag.type <- as.character(disaggregation[ h , c("type")])
              disag.frame <- as.character(disaggregation[ h , c("qrepeatlabel")])
              disag.label <- as.character(disaggregation[ h , c("labelReport")])
              disag.listname <- as.character(disaggregation[ h , c("listname")])
              disag.ordinal <- as.character(disaggregation[ h  , c("variable")])
              if (is.na(disag.ordinal) ) {disag.ordinal <- "not.defined"} else {disag.ordinal <- disag.ordinal }
              disag.variable <- paste0(questions.frame,"$",disag.name)



              if (disag.variable == questions.variable) {
                cat(paste0("\n"),file = chapter.name , sep = "\n", append = TRUE)
              } else if (nrow(as.data.frame(table( get(paste0(questions.frame))[[disag.name]]))) == 0) {
                cat(paste0("\n"),file = chapter.name , sep = "\n", append = TRUE)
              } else {


                ## Case #1 - categoric*numeric -> box plot
                if (disag.type == "integer") {
                  # Get number levels to set up chart height
                  figheight <- nlevels( get(paste0(questions.frame))[[disag.name]])
                  if ( figheight == 0) { figheight <- 1}
                  else if ( figheight == 1) {figheight <- "2"}
                  else if ( figheight == 2) {figheight <- "3"}
                  else if ( figheight == 3) {figheight <- "3"}
                  else if ( figheight == 4) {figheight <- "4"}
                  else if ( figheight == 5) {figheight <- "4"}
                  else if ( figheight == 6) {figheight <- "5"}
                  else if ( figheight == 7) {figheight <- "6"}
                  else if ( figheight == 8) {figheight <- "7"}
                  else if ( figheight == 9) {figheight <- "8"}
                  else if ( figheight == 10) {figheight <- "9"}
                  else if ( figheight >= 11) {figheight <- "10"}

                  ## Open chunk
                  cat(paste0("\n```{r ", questions.name,"x",h, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=",figheight*3,", size=\"small\"}\n"), file = chapter.name, append = TRUE)

                  cat(paste("\n",i,"-", j,"-" , h, " - Render disaggregation : ", disag.label, "for question: ", questions.label,"\n" ))

                  ### Just making sure that the variable is actually a numeric one... in case it was not parsed correctly ####
                  cat(paste0(questions.frame,"$",disag.name," <- as.numeric(",questions.frame,"$",disag.name,")"),file = chapter.name ,sep = "\n", append = TRUE)

                  ## Boxplot
                  ## To do test if there's outliers... ###
                  #if( quantile(questions.frame$disag.name, probs=c(.25, .75), na.rm = T))

                  cat(paste0("data.outlier1 <- ",questions.frame,"$",disag.name),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("data.nooutlier1 <- as.data.frame(",questions.frame,"$",disag.name,")"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("qnt1 <- quantile(data.outlier1, probs=c(.25, .75), na.rm = T)"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("caps.df1 <- as.data.frame(quantile(data.outlier1, probs=c(.05, .95), na.rm = T))"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("H1  <- 1.5 * IQR(data.outlier1, na.rm = T)"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("data.nooutlier1[(data.nooutlier1 < (qnt1[1] - H1)) & !(is.na( data.nooutlier1))  ] <- caps.df1[1,1]"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("data.nooutlier1[ (data.nooutlier1 > (qnt1[2] + H1)) & !(is.na(data.nooutlier1)) ] <- caps.df1[2,1]"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("names(data.nooutlier1)[1] <- \"variable\""),file = chapter.name ,sep = "\n", append = TRUE)

                  if (disag.ordinal == "ordinal" ) {
                    cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", disag.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("levels(",questions.frame,"$",disag.name,") <- list.ordinal"),file = chapter.name ,sep = "\n", append = TRUE)
                  } else {}

                  cat(paste0("plot1 <- ggplot(",questions.frame,", aes(x=",questions.frame,"$",questions.name," , y=",questions.frame,"$",disag.name,")) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("geom_boxplot(fill=\"#2a87c8\",colour=\"black\" ) + "),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_size_area(max_size = 10) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("guides(fill = FALSE) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("xlab(\"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ylab(\"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("coord_flip() +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_y_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggtitle(\"",questions.label,"\","),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("subtitle = \"Before data capping treatement. By question: ",disag.label,".\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("kobo_unhcr_style_histo()"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = chapter.name ,sep = "\n", append = TRUE)

                  ## Boxplot with capping treatment
                  cat(paste0("## Boxplot"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("plot1 <- ggplot(",questions.frame,", aes(y=data.nooutlier1$variable, x= ",questions.frame,"$",questions.name,")) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("geom_boxplot(fill=\"#2a87c8\",colour=\"black\") +  #notch=TRUE"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_size_area(max_size = 10) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("guides(fill = FALSE) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("xlab(\"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ylab(\"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("coord_flip() +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_y_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggtitle(\"",questions.label,"\","),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("subtitle = \"After data capping treatement. By question: ",disag.label,".\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("kobo_unhcr_style_histo()"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = chapter.name ,sep = "\n", append = TRUE)

                  ## Close chunk
                  cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

                  ## Case #2 - categoric*categoric -> stacked bar plot
                } else if (disag.type == "select_one") {

                  # Get number levels to set up chart height
                  figheight <- nlevels( get(paste0(questions.frame))[[disag.name]])
                  if ( figheight == 0) { figheight <- 1}
                  else if ( figheight == 1) {figheight <- "2"}
                  else if ( figheight == 2) {figheight <- "3"}
                  else if ( figheight == 3) {figheight <- "3"}
                  else if ( figheight == 4) {figheight <- "4"}
                  else if ( figheight == 5) {figheight <- "4"}
                  else if ( figheight == 6) {figheight <- "5"}
                  else if ( figheight == 7) {figheight <- "6"}
                  else if ( figheight == 8) {figheight <- "7"}
                  else if ( figheight == 9) {figheight <- "8"}
                  else if ( figheight == 10) {figheight <- "9"}
                  else if ( figheight >= 11) {figheight <- "10"}

                  ## Open chunk
                  cat(paste0("\n```{r ", questions.name,h, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=",figheight,", size=\"small\"}\n"), file = chapter.name, append = TRUE)
                  cat(paste("\n",i,"-", j,"-" , h, " - Render disaggregation: ", disag.label, "for question: ", questions.label,"\n" ))

                  if (disag.ordinal == "ordinal" & questions.ordinal == "ordinal" ) {
                    cat(paste0("## Reorder factor"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("crosssfrequ.weight <- as.data.frame(prop.table(survey::svytable(~", questions.name ," + ", disag.name,", design =",questions.frame ,".survey  ), margin = 2))"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("names(crosssfrequ.weight)[1] <- \"quest\""),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("names(crosssfrequ.weight)[2] <- \"disag\""),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("crosssfrequ.weight$Freq2 <- paste0(round(crosssfrequ.weight$Freq*100,digits = 1),\"%\")"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", disag.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("levels(crosssfrequ.weight$disag) <- list.ordinal"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", questions.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("levels(crosssfrequ.weight$quest) <- list.ordinal"),file = chapter.name ,sep = "\n", append = TRUE)

                  } else if (disag.ordinal == "ordinal" ) {
                    cat(paste0("## Reorder factor"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("crosssfrequ.weight <-as.data.frame(prop.table(survey::svytable(~", questions.name ," + ", disag.name,", design =",questions.frame ,".survey  ), margin = 2))"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("names(crosssfrequ.weight)[1] <- \"quest\""),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("names(crosssfrequ.weight)[2] <- \"disag\""),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("crosssfrequ.weight$Freq2 <- paste0(round(crosssfrequ.weight$Freq*100,digits = 1),\"%\")"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", disag.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("levels(crosssfrequ.weight$disag) <- list.ordinal"),file = chapter.name ,sep = "\n", append = TRUE)

                  } else {
                    cat(paste0("crosssfrequ.weight <-as.data.frame(prop.table(survey::svytable(~", questions.name ," + ", disag.name,", design =",questions.frame ,".survey  ), margin = 2))"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("names(crosssfrequ.weight)[1] <- \"quest\""),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("names(crosssfrequ.weight)[2] <- \"disag\""),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("crosssfrequ.weight$Freq2 <- paste0(round(crosssfrequ.weight$Freq*100,digits = 1),\"%\")"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("## Reorder factor"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("cross <- dcast(crosssfrequ.weight, disag  ~ quest, value.var = \"Freq\")"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("cross <- cross[ order(cross[ ,2], decreasing = FALSE) ,  ]"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("crosssfrequ.weight$disag <- factor(crosssfrequ.weight$disag, levels = as.character(cross[ ,1]))"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("\n"),file = chapter.name ,sep = "\n", append = TRUE)
                  }


                  cat(paste0("## and now the graph"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("plot1 <- ggplot(crosssfrequ.weight, aes(fill=crosssfrequ.weight$quest, y=crosssfrequ.weight$Freq, x = crosssfrequ.weight$disag)) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("geom_bar(colour=\"white\", stat =\"identity\", width=.8, aes(fill = quest), position = position_stack(reverse = TRUE)) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  #cat(paste0("geom_label_repel(aes(label = Freq2), fill = \"#2a87c8\", color = 'white') +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ylab(\"Frequency\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  #cat(paste0("facet_wrap(~disag, ncol=3) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_y_continuous(labels = percent) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_fill_viridis(discrete=TRUE) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("xlab(\"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("coord_flip() +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggtitle(\"",questions.label," (color)\","),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("subtitle = \" By question: ",disag.label," (bar)\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("kobo_unhcr_style_bar() +"),file = chapter.name ,sep = "\n", append = TRUE)
                  ## setting up the legend
                  #cat(paste0("guides(fill = FALSE) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("theme(legend.direction = \"horizontal\", legend.position = \"bottom\", legend.box = \"horizontal\",legend.title=element_blank()  )"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = chapter.name ,sep = "\n", append = TRUE)

                  ## Close chunk
                  cat(paste0("\n```\n", sep = ""), file = chapter.name, append = TRUE)
                  cat("\n", file = chapter.name, append = TRUE)

                }
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
          correlationdf <- plyr::join(x = correlation1, y = check, by = "fullname", type = "left")
          correlationdf <- correlationdf[!is.na(correlationdf$id), ]


          if (nrow(correlationdf) == 0 ) {
            cat("No correlation requested for this question...\n",file = chapter.name , sep = "\n", append = TRUE)
            cat("No correlation requested for this question...\n")
            cat("\n", file = chapter.name, append = TRUE)
          } else if (nrow(frequ) %in% c("0","1")) {
            #cat("No responses recorded for this question. No analysis of correlation...\n",file = chapter.name , sep = "\n", append = TRUE)
            cat("No responses recorded for this question. No analysis of correlation...\n")
            cat("\n", file = chapter.name, append = TRUE)
          } else {

            cat("\n", file = chapter.name, append = TRUE)
            cat(paste("### Significant Associations (chi-square with p value < 5%)" ,sep = ""),file = chapter.name ,sep = "\n", append = TRUE)
            cat("\n", file = chapter.name, append = TRUE)

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
              chiquare.result$id <- l
              chiquare.result[1, c("target")] <- questions.name
              chiquare.result[1, c("tested")] <-  as.character(correlationdf[l, c("fullname")])
              chiquare.result[1, c("frame")]  <- questions.frame
              ## getting labels
              chiquare.result[1, c("target.n")] <-   which(colnames(get(paste0(chiquare.result$frame))) == chiquare.result[1, c("target")])
              chiquare.result[1, c("tested.n")] <-   which(colnames(get(paste0(chiquare.result$frame))) == chiquare.result[1, c("tested")])
              chiquare.result[1, c("target.label")]  <- attributes(get(paste0(chiquare.result$frame)))$variable.labels[chiquare.result[1, c("target.n")]]
              chiquare.result[1, c("tested.label")]  <- attributes(get(paste0(chiquare.result$frame)))$variable.labels[chiquare.result[1, c("tested.n")]]

              cat(paste0(i,"-", j,"-" , "-" ,l," correlation between -- ",chiquare.result[1, c("target.label")]," -- and -- ",chiquare.result[1, c("tested.label")]," --.\n"))
              formula <- cbind(as.data.frame(get(paste0(chiquare.result$frame))[[chiquare.result$target]]), as.data.frame(get(paste0(chiquare.result$frame))[[chiquare.result$tested]]))
              names(formula)[1] <- "target"
              names(formula)[2] <- "tested"
              formula <- formula[!(is.na(formula$target)),]
              formula <- formula[!(is.na(formula$tested)),]

              ## Check that each class is represented
              check.class <- as.data.frame(table(formula$target,formula$tested))
              n.class <- nrow(check.class)
              n.class.notnull <- nrow(check.class[check.class$Freq > 0, ])

              ### Testing number of levels for the 2 variables as 'x' and 'y' must have at least 2 levels
              if ( (chiquare.result[1, c("target")] != chiquare.result[1, c("tested")] ) &
                   (nlevels(as.factor(as.character(formula$target))) > 1 ) &
                   (nlevels(as.factor(as.character(formula$tested))) > 1 ) &

                   ## If too many levels, the corrogram is not legible...
                   (nlevels(as.factor(as.character(formula$target))) < 8 ) &
                   (nlevels(as.factor(as.character(formula$tested))) < 8 ) &
                   ## May have class with zero value...
                   n.class == n.class.notnull

              )
              { chiquare.result[1, c("p.value")]  <- round(stats::chisq.test(formula$target,formula$tested)$p.value,4)
              } else {chiquare.result[1, c("p.value")] <- 1  }
              chiquare.resultall <- rbind(chiquare.resultall, chiquare.result)
              rm(chiquare.result)
            }

            ## Subsetting results on test where p-value is below 0.05
            chiquare.true <- chiquare.resultall[ chiquare.resultall$p.value <= 0.05, ]

            ### Case there not any positive test
            if (nrow(chiquare.true) == 0 ) {
              cat("No significant association found for this question...\n",file = chapter.name , sep = "\n", append = TRUE)
              cat("No significant association found for this question...\n")

            } else {
              ## now generating correlation plot for each of the dependent.
              for (m in 1:nrow(chiquare.true)) {
                frame <- as.character(chiquare.true[m,4 ])
                target <- as.character(chiquare.true[m,2 ])
                target.label <- as.character(chiquare.true[m,7])

                tested <- as.character(chiquare.true[m,3 ])
                tested.label <- as.character(chiquare.true[m,8 ])

                #formula.target  <- get(paste0(frame))[[target]]
                #formula.tested  <- get(paste0(frame))[[tested]]

                formula.target1 <- paste0(frame, "$", target)
                formula.tested1 <- paste0(frame, "$", tested)

                ## Open chunk
                cat(paste0("\n```{r ", questions.name,"ccc",m, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=6, size=\"small\"}\n"), file = chapter.name, append = TRUE)

                cat(paste0("corrplot(stats::chisq.test(",formula.target1,",", formula.tested1,")$residuals,"), file = chapter.name , sep = "\n", append = TRUE)
                cat(paste0("is.cor = FALSE, # use for general matrix to convert to Sq form"), file = chapter.name , sep = "\n", append = TRUE)
                cat(paste0("cl.pos = \"n\", ## Do not display the color legend"), file = chapter.name , sep = "\n", append = TRUE)
                cat(paste0("cl.cex = 0.7, # Size of all label"), file = chapter.name , sep = "\n", append = TRUE)
                cat(paste0("tl.cex = 0.7, # Size of axis label"), file = chapter.name , sep = "\n", append = TRUE)
                cat(paste0("tl.srt = 45, # string rotation in degrees"), file = chapter.name , sep = "\n", append = TRUE)
                cat(paste0("tl.col = \"black\", # color of text label."), file = chapter.name , sep = "\n", append = TRUE)
                cat(paste0("addCoef.col = \"grey\", # add coeff in the chart"), file = chapter.name , sep = "\n", append = TRUE)
                cat(paste0("number.cex= 3/ncol(stats::chisq.test(",formula.target1,",", formula.tested1,")), # size of coeff"), file = chapter.name , sep = "\n", append = TRUE)
                cat(paste0("mar = c(0.5,0.5,4, 0.5), ## margin of plots"), file = chapter.name , sep = "\n", append = TRUE)
                cat(paste0("title= paste0(\"Correlation between", "\n",target.label," (row)\n", " & ",tested.label," (col)\")) "), file = chapter.name , sep = "\n", append = TRUE)
                ## Close chunk
                cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)
              }
            }
          }




          ##Decimal####################################################################################################
        } else if (questions.type == "decimal" | questions.type == "integer" | questions.type == "numeric") {
          cat(paste("Numeric question  " ,"\n\n",sep = ""),file = chapter.name ,sep = "\n", append = TRUE)

          ## Check the lenght of the table to see if we can display it or not...
          frequ <- as.data.frame(table( get(paste0(questions.frame))[[questions.name]]))

          ####Decimal.tabulation########################################################################
          cat(paste("### Tabulation\n" ,sep = ""),file = chapter.name ,sep = "\n", append = TRUE)

          ## Open chunk
          cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)
          ### Just making sure that the variable is actually a numeric one... in case it was not parsed correctly ####
          cat(paste0(questions.frame,"$",questions.name," <- as.numeric(",questions.frame,"$",questions.name,")"),file = chapter.name ,sep = "\n", append = TRUE)
          cat(paste0("frequ <- as.data.frame(table(",questions.variable,"))"),file = chapter.name ,sep = "\n", append = TRUE)

          ### Check if we have records or if we have too many records
          if (nrow(frequ) %in% c("0","1")) {
            cat(paste0("cat(\"No responses recorded for this question...\")"),file = chapter.name , sep = "\n", append = TRUE)
            cat("No responses recorded for this question...\n")
          } else if (nrow(frequ) > 10) {
          #   cat(paste0("cat(\"There's too many potential values to display. We will only show the histogram. \n \")"),file = chapter.name ,sep = "\n", append = TRUE)
          } else{
         #   cat(paste0("## display table"),file = chapter.name ,sep = "\n", append = TRUE)
         #   cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\")"),file = chapter.name ,sep = "\n", append = TRUE)
          }

          ## To do implement FD number of bin: https://www.r-bloggers.com/friday-function-nclass/
          if (nrow(frequ) %in% c("0","1")) {
            cat(paste0("cat(\"No responses recorded for this question...\")"),file = chapter.name , sep = "\n", append = TRUE)
            cat("\n")
          } else {
            cat(paste0("average <- as.data.frame(svymean(~ ",questions.name,", design = ",questions.frame,".survey, na.rm = TRUE))"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("cat(paste0(\"Based on the sample design, the average weighted mean response for this question is \", as.numeric(round(average$mean, digits = 2))))"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("#  regular histogram"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("plot1 <- ggplot(data = frequ, aes(x = frequ$Var1, y = frequ$Freq)) +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("geom_bar(fill = \"#2a87c8\",colour = \"white\", stat = \"identity\", width = .8) +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("labs(x = \"\", y = \"Count\") +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("ggtitle(\"",questions.label,"\",subtitle = \"Before data capping treatement.\") +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("kobo_unhcr_style_histo()"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = chapter.name ,sep = "\n", append = TRUE)


            ### Detect outliers and adjust bien numbers #####
            ### To -- check there's outlier or not
            ## Double check that we have a continuous value -- not a factor --
            cat(paste0("data.outlier <- ",questions.frame,"$",questions.name),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("data.nooutlier <- as.data.frame(",questions.frame,"$",questions.name,")"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("qnt <- quantile(data.outlier, probs = c(.25, .75), na.rm = T)"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("caps.df <- as.data.frame(quantile(data.outlier, probs = c(.05, .95), na.rm = T))"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("H  <- 1.5 * IQR(data.outlier, na.rm = T)"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("data.nooutlier[(data.nooutlier < (qnt[1] - H)) & !(is.na(data.nooutlier))  ] <- caps.df[1,1]"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("data.nooutlier[ (data.nooutlier > (qnt[2] + H)) & !(is.na(data.nooutlier)) ] <- caps.df[2,1]"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("names(data.nooutlier)[1] <- \"variable\""),file = chapter.name ,sep = "\n", append = TRUE)


            ### Now graphs with treated variable #####
            cat(paste0("plot1 <- ggplot(data = data.nooutlier, aes(x = data.nooutlier$variable)) +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("geom_histogram(color = \"white\",fill = \"#2a87c8\", breaks = pretty(data.nooutlier$variable, n = nclass.Sturges(data.nooutlier$variable),min.n = 1)) +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("labs(x = \"\", y = \"Count\") +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("ggtitle(\"",questions.label,"\","),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("subtitle = \"After data capping treatement.\") +"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("kobo_unhcr_style_histo()"),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = chapter.name ,sep = "\n", append = TRUE)
          }
          ## Close chunk
          cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

          ###Decimal.crosstabulation###########################################################################
          if (nrow(disaggregation) == 0) {
            cat("No disaggregation requested for this question...\n",file = chapter.name , sep = "\n", append = TRUE)
            cat("No disaggregation requested for this question...\n")
            cat("\n", file = chapter.name, append = TRUE)
          } else if (nrow(frequ) %in% c("0","1")) {
            # cat("No responses recorded for this question. No disaggregation...\n",file = chapter.name , sep = "\n", append = TRUE)
            cat("No responses recorded for this question. No disaggregation...\n")
            cat("\n", file = chapter.name, append = TRUE)
          } else {


            cat(paste("### Analysis of relationship" ,sep = ""),file = chapter.name ,sep = "\n", append = TRUE)
            for (h in 1:nrow(disaggregation)) {
              #h <-1
              ## Now getting level for each questions
              disag.name <- as.character(disaggregation[ h , c("fullname")])
              disag.shortname <- as.character(disaggregation[ h , c("name")])
              disag.type <- as.character(disaggregation[ h , c("type")])
              disag.frame <- as.character(disaggregation[ h , c("qrepeatlabel")])
              disag.label <- as.character(disaggregation[ h , c("label")])
              disag.listname <- as.character(disaggregation[ h , c("listname")])
              disag.ordinal <- as.character(disaggregation[ h  , c("variable")])
              if (is.na(disag.ordinal) ) {disag.ordinal <- "not.defined"} else {disag.ordinal <- disag.ordinal }
              disag.variable <- paste0(questions.frame,"$",disag.name)

              if (disag.variable == questions.variable) {
                cat(paste0("\n"),file = chapter.name , sep = "\n", append = TRUE)
              }  else if (nrow(as.data.frame(table( get(paste0(questions.frame))[[disag.name]]))) == 0) {
                cat(paste0("\n"),file = chapter.name , sep = "\n", append = TRUE)
              } else {


                ## Case #1 - numeric * categoric -> box plot
                if (disag.type == "select_one") {


                  # Get number levels to set up chart height
                  figheight <- nlevels( get(paste0(questions.frame))[[disag.name]])
                  if ( figheight == 0) { figheight <- 1}
                  else if ( figheight == 1) {figheight <- "2"}
                  else if ( figheight == 2) {figheight <- "3"}
                  else if ( figheight == 3) {figheight <- "3"}
                  else if ( figheight == 4) {figheight <- "4"}
                  else if ( figheight == 5) {figheight <- "4"}
                  else if ( figheight == 6) {figheight <- "5"}
                  else if ( figheight == 7) {figheight <- "6"}
                  else if ( figheight == 8) {figheight <- "7"}
                  else if ( figheight == 9) {figheight <- "8"}
                  else if ( figheight == 10) {figheight <- "9"}
                  else if ( figheight >= 11) {figheight <- "10"}

                  ## Open chunk
                  cat(paste0("\n```{r ", questions.name,"x",h, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=",figheight,", size=\"small\"}\n"), file = chapter.name, append = TRUE)

                  cat(paste("\n", i,"-", j,"-" , h, " - Render disaggregation : ", disag.label, "for question: ", questions.label,"\n" ))

                  ## account of Ordinal variable
                  if (disag.ordinal == "ordinal") {
                    cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", disag.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0(questions.frame,"$",disag.name," <- as.factor(",questions.frame,"$",disag.name,")"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("levels(",questions.frame,"$",disag.name,") <- list.ordinal"),file = chapter.name ,sep = "\n", append = TRUE)
                    cat(paste0("\n"),file = chapter.name ,sep = "\n", append = TRUE)
                  } else {
                    cat(paste0("\n"),file = chapter.name ,sep = "\n", append = TRUE)
                  }


                  ## Boxplot

                  cat(paste0("plot1 <- ggplot(",questions.frame,", aes(y=",questions.frame,"$",questions.name," , x=",questions.frame,"$",disag.name,")) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("geom_boxplot(fill=\"#2a87c8\",colour=\"black\") + "),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_size_area(max_size = 10) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("guides(fill = FALSE) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("xlab(\"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ylab(\"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("coord_flip() +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_y_continuous(breaks= pretty_breaks()) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggtitle(\"",questions.label,"\","),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("subtitle = \"Before data capping treatement, by question: ",disag.label,"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("kobo_unhcr_style_bar()"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = chapter.name ,sep = "\n", append = TRUE)

                  ## Boxplot with capping treatment
                  cat(paste0("## Boxplot"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("plot1 <- ggplot(",questions.frame,", aes(y=data.nooutlier$variable, x= ",questions.frame,"$",disag.name,")) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("geom_boxplot(fill=\"#2a87c8\",colour=\"black\") +  #notch=TRUE"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_size_area(max_size = 10) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("guides(fill = FALSE) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("xlab(\"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ylab(\"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("coord_flip() +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_y_continuous(breaks= pretty_breaks()) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggtitle(\"",questions.label,"\","),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("subtitle = \"After data capping treatement. By question: ",disag.label,"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("kobo_unhcr_style_bar()"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = chapter.name ,sep = "\n", append = TRUE)

                  ## Close chunk
                  cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

                  ## Case #2 - numeric*numeric -> scatter plot
                } else if (disag.type == "integer") {

                  ## Open chunk
                  cat(paste0("\n```{r ", questions.name,"x",h, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=8, size=\"small\"}\n"), file = chapter.name, append = TRUE)

                  cat(paste("\n", i,"-", j,"-" , h, " - Render disaggregation : ", disag.label, "for question: ", questions.label,"\n" ))


                  cat(paste0("data.outlier1 <- ",questions.frame,"$",disag.name),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("data.nooutlier1 <- as.data.frame(",questions.frame,"$",disag.name,")"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("qnt1 <- quantile(data.outlier1, probs=c(.25, .75), na.rm = T)"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("caps.df1 <- as.data.frame(quantile(data.outlier1, probs=c(.05, .95), na.rm = T))"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("H1  <- 1.5 * IQR(data.outlier1, na.rm = T)"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("data.nooutlier1[(data.nooutlier1 < (qnt1[1] - H)) & !(is.na( data.nooutlier1))  ] <- caps.df1[1,1]"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("data.nooutlier1[ (data.nooutlier1 > (qnt1[2] + H)) & !(is.na(data.nooutlier1)) ] <- caps.df1[2,1]"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("names(data.nooutlier1)[1] <- \"variable\""),file = chapter.name ,sep = "\n", append = TRUE)

                  cat(paste0("## Scatter plot"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("plot1 <- ggplot(",questions.frame,", aes(x= ",questions.frame,"$",disag.name, ", y=",questions.frame,"$",questions.name,")) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("geom_count(aes(size = ..prop.., group = 1)) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_size_area(max_size = 10) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("guides(fill = FALSE) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_y_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_x_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("# xlab(correllabel) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("# ylab(variablelabel) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("geom_smooth(method=lm) +  # Add a loess smoothed fit curve with confidence region"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggtitle(\"Scatterplot before data capping treatment\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("kobo_unhcr_style_scatter()"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = chapter.name ,sep = "\n", append = TRUE)



                  cat(paste0("## Scatter plot rev "),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("plot1 <- ggplot(",questions.frame,", aes(x= data.nooutlier$variable, y=data.nooutlier1$variable )) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("geom_count(aes(size = ..prop.., group = 1)) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_size_area(max_size = 10) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("guides(fill = FALSE) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_y_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("scale_x_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("# xlab(correllabel) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("#ylab(variablelabel) +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("geom_smooth(method=lm) +  # Add a loess smoothed fit curve with confidence region"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggtitle(\"Scatterplot after data capping treatment\") +"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("kobo_unhcr_style_scatter()"),file = chapter.name ,sep = "\n", append = TRUE)
                  cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = chapter.name ,sep = "\n", append = TRUE)

                  ## Close chunk
                  cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)
                }
              }
            }
          }


          ##select.multi####################################################################################################
        } else if ( questions.type == "select_multiple_d" ) {
          cat(paste("Multiple choice question  " ,"\n\n",sep = ""),file = chapter.name ,sep = "\n", append = TRUE)


          ###select.multi.tab######################################################################

          cat(paste("### Tabulation" ,sep = ""),file = chapter.name ,sep = "\n", append = TRUE)

          ##Compute contengency table
          selectmultilist1 <- as.data.frame(dico[dico$type == "select_multiple" & dico$listname == as.character(questions.listname) &
                                                   grepl(as.character(questions.shortname),dico$fullname) == TRUE , c("fullname")])
          names(selectmultilist1)[1] <- "check"

          check <- as.data.frame(names(get(paste0(questions.frame))))
          names(check)[1] <- "check"
          check$id <- row.names(check)
          check <- merge(x = check, y = selectmultilist1,by = "check")
          selectmultilist <- as.character(check[ ,1])
          ## Reshape answers
          data.selectmultilist <- get(paste0(questions.frame))[ ,selectmultilist ]
          data.selectmultilist <- get(paste0(questions.frame))[ ,selectmultilist ]
          data.selectmultilist$id <- rownames(data.selectmultilist)

          ### Account for the case where there no answer to given the questions
          if (ncol(data.selectmultilist) %in% c("0","1")) {
            cat("No responses recorded for this question...\n",file = chapter.name , sep = "\n", append = TRUE)
            cat("No responses recorded for this question...\n")
          } else{


            totalanswer <- nrow(data.selectmultilist)
            data.selectmultilist <- data.selectmultilist[ data.selectmultilist[ ,1] != "Not replied", ]
            percentreponse <- paste0(round((nrow(data.selectmultilist)/totalanswer)*100,digits = 1),"%")
            meltdata <- reshape2::melt(data.selectmultilist,id = "id")
            castdata <- as.data.frame(table(meltdata[c("value")]))


            castdata$freqper <- castdata$Freq/nrow(data.selectmultilist)
            castdata <- castdata[castdata$Var1 != "Not selected", ]
            castdata$Var1 <- factor(castdata$Var1, levels = castdata[order(castdata$freqper), "Var1"])
            frequ <- castdata[castdata$Var1 != "", ]

            if (nrow(frequ) %in% c("0","1")) {
              cat("No responses recorded for this question...\n",file = chapter.name , sep = "\n", append = TRUE)
              cat("No responses recorded for this question...\n")
            } else{

              ## Open chunk
              cat(paste0("\n```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=8, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)
              cat(paste0("### Tabulation"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("##Compute contengency table"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("selectmultilist1 <- as.data.frame(dico[dico$type == \"select_multiple\" & dico$listname==\"",questions.listname, "\" & grepl(\"", questions.shortname,"\",dico$fullname)==TRUE , c(\"fullname\")])"),file = chapter.name ,sep = "\n", append = TRUE)

              cat(paste0("names(selectmultilist1)[1] <- \"check\""),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("check <- as.data.frame(names(",questions.frame ,"))"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("names(check)[1] <- \"check\""),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("check$id <- row.names(check)"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("check <- merge(x = check, y = selectmultilist1, by = \"check\")"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("selectmultilist <- as.character(check[ ,1])"),file = chapter.name ,sep = "\n", append = TRUE)

              cat(paste0("## Reshape answers"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("data.selectmultilist <- ",questions.frame ,"[ selectmultilist ]"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("data.selectmultilist$id <- rownames(data.selectmultilist)"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("totalanswer <- nrow(data.selectmultilist)"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("data.selectmultilist <- data.selectmultilist[ data.selectmultilist[ ,1]!=\"Not replied\", ]"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("percentreponse <- paste0(round((nrow(data.selectmultilist)/totalanswer)*100,digits = 1),\"%\")"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("meltdata <- reshape2::melt(data.selectmultilist,id=\"id\")"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("castdata <- as.data.frame(table(meltdata[c(\"value\")]))"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("castdata$freqper <- castdata$Freq/nrow(data.selectmultilist)"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("castdata <- castdata[castdata$Var1!=\"Not selected\", ]"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("castdata$Var1 <- factor(castdata$Var1, levels=castdata[order(castdata$freqper), \"Var1\"])"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ <- castdata[castdata$Var1!=\"\", ]"),file = chapter.name ,sep = "\n", append = TRUE)

              cat(paste0("## display table"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("names(frequ)[1] <- \"", questions.shortname,"\""),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ[ ,3] <- paste0(round(frequ[ ,3]*100,digits = 1),\"%\")"),file = chapter.name ,sep = "\n", append = TRUE)

          #    cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\")"),file = chapter.name ,sep = "\n", append = TRUE)

              cat(paste0("frequ1 <- castdata[castdata$Var1!=\"\", ]"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ1[ ,4] <- paste0(round(frequ1[ ,3]*100,digits = 1),\"%\")"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("names(frequ1)[4] <- \"freqper2\""),file = chapter.name ,sep = "\n", append = TRUE)

              cat(paste0("\n"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("## and now the graph"),file = chapter.name ,sep = "\n", append = TRUE)

              cat(paste0("plot1 <- ggplot(frequ1, aes(x=Var1, y=freqper)) +"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("geom_bar(fill = \"#2a87c8\", colour = \"#2a87c8\", stat = \"identity\", width=.8) +"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("guides(fill = FALSE) +"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("geom_label_repel(aes(y = freqper, label = freqper2), fill = \"#2a87c8\", color = 'white') +"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("ylab(\"Frequency\") +"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("scale_y_continuous(labels = percent) +"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("xlab(\"\") +"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("coord_flip() +"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("ggtitle(\"",questions.label,"\","),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("subtitle = paste0(\"Question response rate: \",percentreponse,\" .\")) +"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("kobo_unhcr_style_bar()"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = chapter.name ,sep = "\n", append = TRUE)
              cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)
              ###select.multi.rel######################################################################


              cat(paste("### Analysis of relationship" ,sep = ""),file = chapter.name ,sep = "\n", append = TRUE)
              ## Open chunk
              cat(paste0("\n```{r ", questions.name, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)
              ## Close chunk
              cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)

            }
          }

          ####date###############################################################################################
        } else if (questions.type == "date") {
          cat(paste("Date question  in data frame: ",questions.frame,"\n\n",sep = ""),file = chapter.name ,sep = "\n", append = TRUE)

          ####text#############################################################################################
        } else if ( questions.type == "text" ) {
          cat(paste("Open ended question  in data frame: ",questions.frame,"\n\n", sep = ""),file = chapter.name ,sep = "\n", append = TRUE)

          ## Check if the there are answeers to that questions...
          frequ <- as.data.frame(table( get(paste0(questions.frame))[[questions.name]]))

          if (nrow(frequ) %in% c("0","1")) {
            cat(paste0("cat(\"No responses recorded for this question...\")"),file = chapter.name , sep = "\n", append = TRUE)
            cat("No responses recorded for this question...\n")
          } else{

            cat(paste("List of given answers \n" ,sep = ""),file = chapter.name ,sep = "\n", append = TRUE)
            ## Open chunk
            cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file = chapter.name, append = TRUE)
            cat(paste0("textresponse <- as.data.frame(table(",questions.frame,"[!(is.na(",questions.variable,")), c(\"",questions.name,"\")]))"),file = chapter.name ,sep = "\n", append = TRUE)

            cat(paste0("names(textresponse)[1] <- \"", questions.shortname,"\""),file = chapter.name ,sep = "\n", append = TRUE)
            cat(paste0("kable(textresponse, caption=\"__Table__:", questions.label,"\")"),file = chapter.name ,sep = "\n", append = TRUE)

            ## Close chunk
            cat(paste0("\n```\n", sep = '\n'), file = chapter.name, append = TRUE)
          }
          # End test on question on type
        }

        ## End loop on questions
      }


      cat(paste("##### Page Break"),file = chapter.name ,sep = "\n", append = TRUE)
    }
    if (app == "shiny") {
      progress$set(message = "Render now all reports...")
      updateProgress()
    }
    cat(" Clean memory... \n")
    gc()
    #rm(list = ls())
    kobo_load_packages()
    mainDir <- kobo_getMainDirectory()
    chapters <- utils::read.csv(paste(mainDir,"/data/chapters.csv",sep = ""), encoding = "UTF-8", na.strings = "")
    ### Render now all reports
    cat(" Render now reports... \n")
    for (i in 1:nrow(chapters)) {
      chaptersname <- as.character(chapters[ i , 1])
      if (app == "shiny") {
        progress$set(message = paste("Rendering word output report for ",chaptersname, " chapter in progress..."))
        updateProgress()
      }
      cat(paste(i, " - Render word output report for ",chaptersname))
      mainDir <- kobo_getMainDirectory()
      rmarkdown::render(paste(mainDir,"/code/",i,"-", chaptersname, "-chapter.Rmd", sep = ""))
      ## Put the report in the out folder
      mainDir <- kobo_getMainDirectory()
      file.rename(paste(mainDir,"/code/",i,"-", chaptersname, "-chapter.docx", sep = ""), paste0(mainDir,"/out/crunching_reports/Crunching-report-",i,"-", chaptersname,"-",Sys.Date(), "-chapter.docx"))
      ## Clean  memory
      gc()
    }
    if (app == "shiny") {
      updateProgress()
    }
    cat(" Done!! Reports are in the folder OUT - Review the report- Adjust your configuration files and you will be very soon ready to start the qualitative analysis and the analysis workshops...")
  }, error = function(err) {
    print("kobo_crunching_report_ERROR")
    return(structure(err, class = "try-error"))
  })
}
