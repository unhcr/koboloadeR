#' @name kobo_crunching_report
#' @rdname kobo_crunching_report
#' @title Generate Data Crunching Report
#'
#' @description Generate crunching Report that contains all descriptive statistics, correlation analysis, tabulation and data visualization for variables and indicators.
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#' @param output The output format html or aspx if you need to upload on sharepoint), docx (to quickly cut non interesting vz and take note during data interpretation session), pptx (to quickly cut non interesting vz and persent during data interpretation session)
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

kobo_crunching_report <- function(form = "form.xls", app = "console", output ="html") {
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
    ## Generate dico to test here - in normal process - it has been done just before in kobo_load_data()
    #kobo_dico(form)

    ## Load dictionnary
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
      progress$set(message = "Building now the configures reports in Rmd format in progress...")
      updateProgress()
    }

    ##get list of report
    reports <- as.data.frame(unique(dico$report))
    names(reports)[1] <- "Report"

    ## Default behavior if no report was defined in xlsform
    if ((nrow(reports) == 1) & is.na(reports$Report)) {
      cat("Defaulting questions allocation to chapter")
      dico$report[ dico$type %in% c("select_one","select_multiple_d")] <- "report"
      reports <- as.data.frame(unique(dico$chapter))
      names(reports)[1] <- "Report"
    } else {}

    reports <- as.data.frame(reports[!is.na(reports$Report), ])

    names(reports)[1] <- "Report"

    utils::write.csv(reports, paste(mainDir,"/data/reports.csv",sep = ""), row.names = FALSE, na = "")


    ## for each chapter: create a Rmd file -------

    ##Loop.chapter ------------

    for (i in 1:nrow(reports) )
    {
      # i <-1
      reportsname <- as.character(reports[ i , 1])
      if (app == "shiny") {
        progress$set(message = paste(i, " - Write chapter for ",as.character(reports[ i , 1])))
        updateProgress()
      }
      cat(paste(i, " - Write chapter for ",as.character(reports[ i , 1]),"\n" ))
      report.name <- paste(mainDir, "/code/",i,"-", reportsname, "-report.Rmd", sep = "")

      ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
      if (file.exists(report.name)) file.remove(report.name)

      ## TO DO : put in configuration file name of report, author, organisation & location
      ## TO DO : put in configuration wethere report should be portrait or landscape
      cat("---", file = report.name , sep = "\n", append = TRUE)
      cat(paste("title: \"Data Crunching Report: ",reportsname , "- Draft not for distribution. \"", sep = ""), file = report.name , sep = "\n", append = TRUE)
      cat("author: \"Generated with [Koboloader](https://unhcr.github.io/koboloadeR/docs) \"", file = report.name , sep = "\n", append = TRUE)
      cat("date: \" `r format(Sys.Date(),  '%d %B %Y')`\"", file = report.name , sep = "\n", append = TRUE)

      if (output == "docx") {

        cat("always_allow_html: yes", file = report.name , sep = "\n", append = TRUE)
        cat("output:",file = report.name , sep = "\n", append = TRUE)
        cat("  word_document:", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_caption: yes", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_height: 5", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_width: 8", file = report.name , sep = "\n", append = TRUE)
        cat("    toc: yes", file = report.name , sep = "\n", append = TRUE)
        cat("    toc_depth: 2", file = report.name , sep = "\n", append = TRUE)
        cat("    reference_docx: style-unhcr-portrait.docx", file = report.name , sep = "\n", append = TRUE)
        cat("---", file = report.name , sep = "\n", append = TRUE)

      } else if (output == "html") {

        cat("always_allow_html: yes", file = report.name , sep = "\n", append = TRUE)
        cat("output:",file = report.name , sep = "\n", append = TRUE)
        cat("  html_document:", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_caption: yes", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_height: 5", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_width: 8", file = report.name , sep = "\n", append = TRUE)
        cat("    toc: yes", file = report.name , sep = "\n", append = TRUE)
        cat("    toc_depth: 2", file = report.name , sep = "\n", append = TRUE)
        cat("    toc_float: yes", file = report.name , sep = "\n", append = TRUE)
        cat("    includes:", file = report.name , sep = "\n", append = TRUE)
        cat("       in_header: css/header.html", file = report.name , sep = "\n", append = TRUE)
        cat("---", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/unhcr-bootstrap.css\">", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/style.css\">", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/unhcr-header.css\">", file = report.name , sep = "\n", append = TRUE)

      }else if (output == "aspx") {

        cat("always_allow_html: yes", file = report.name , sep = "\n", append = TRUE)
        cat("output:",file = report.name , sep = "\n", append = TRUE)
        cat("  html_document:", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_caption: yes", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_height: 5", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_width: 8", file = report.name , sep = "\n", append = TRUE)
        cat("    toc: yes", file = report.name , sep = "\n", append = TRUE)
        cat("    toc_depth: 2", file = report.name , sep = "\n", append = TRUE)
        cat("    toc_float: yes", file = report.name , sep = "\n", append = TRUE)
        cat("    includes:", file = report.name , sep = "\n", append = TRUE)
        cat("       in_header: css/header.html", file = report.name , sep = "\n", append = TRUE)
        cat("---", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/unhcr-bootstrap.css\">", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/style.css\">", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/unhcr-header.css\">", file = report.name , sep = "\n", append = TRUE)

      } else if (output == "pptx") {

        cat("always_allow_html: yes", file = report.name , sep = "\n", append = TRUE)
        cat("output:",file = report.name , sep = "\n", append = TRUE)
        cat("  powerpoint_presentation:", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_caption: yes", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_height: 9", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_width: 18", file = report.name , sep = "\n", append = TRUE)
        cat("    dpi = 300", file = report.name , sep = "\n", append = TRUE)
        cat("    reference_docx: templateUNHCR.pptx", file = report.name , sep = "\n", append = TRUE)
        cat("    slide_level: 2", file = report.name , sep = "\n", append = TRUE)
        cat("---", file = report.name , sep = "\n", append = TRUE)
      }



      cat("---", file = report.name , sep = "\n", append = TRUE)


      ## First chunk to get the data in the report

      cat("```{r setup, include = FALSE, echo = FALSE, warning = FALSE, message = FALSE}", file = report.name , sep = "\n", append = TRUE)
      cat("mainDir <- getwd()", file = report.name , sep = "\n", append = TRUE)
      cat("mainDirroot <- substring(mainDir, 0 , nchar(mainDir) - 5)", file = report.name , sep = "\n", append = TRUE)


      cat("## Load all required packages", file = report.name , sep = "\n", append = TRUE)
      cat("library(tidyverse)", file = report.name , sep = "\n", append = TRUE)
      cat("library(ggthemes)", file = report.name , sep = "\n", append = TRUE)
      cat("library(plyr)", file = report.name , sep = "\n", append = TRUE)
      cat("library(ggrepel)", file = report.name , sep = "\n", append = TRUE)
      cat("library(viridis)", file = report.name , sep = "\n", append = TRUE)
      cat("library(RColorBrewer)", file = report.name , sep = "\n", append = TRUE)
      cat("library(extrafont)", file = report.name , sep = "\n", append = TRUE)
      cat("library(corrplot)", file = report.name , sep = "\n", append = TRUE)
      cat("library(reshape2)", file = report.name , sep = "\n", append = TRUE)
      cat("library(scales)", file = report.name , sep = "\n", append = TRUE)
      cat("library(survey)", file = report.name , sep = "\n", append = TRUE)
      cat("library(knitr)", file = report.name , sep = "\n", append = TRUE)
      cat("library(rmarkdown)", file = report.name , sep = "\n", append = TRUE)
      cat("library(ggpubr)", file = report.name , sep = "\n", append = TRUE)
      cat("library(grid)", file = report.name , sep = "\n", append = TRUE)
      cat("library(jtools)", file = report.name , sep = "\n", append = TRUE)
      cat("library(moments)", file = report.name , sep = "\n", append = TRUE)
      cat("library(koboloadeR)", file = report.name , sep = "\n", append = TRUE)
      cat("options(scipen = 999) # turn-off scientific notation like 1e+48", file = report.name , sep = "\n", append = TRUE)

      cat("## Provide below the name of the form in xsl form - format should be xls not xlsx", file = report.name , sep = "\n", append = TRUE)
      cat(paste0("form <- \"",form,"\""), file = report.name , sep = "\n", append = TRUE)
      cat("dico <- utils::read.csv(paste0(mainDirroot,\"/data/dico_\",form,\".csv\"), encoding = \"UTF-8\", na.strings = \"\")", file = report.name , sep = "\n", append = TRUE)


      ## TO DO: Use config file to load the different frame


      cat("MainDataFrame <- utils::read.csv(paste0(mainDirroot,\"/data/MainDataFrame_encoded.csv\"), encoding = \"UTF-8\", na.strings = \"\")", file = report.name , sep = "\n", append = TRUE)


      for (dbr in dataBeginRepeat) {
        cat(paste(dbr, " <- utils::read.csv(paste0(mainDirroot,\"/data/",dbr,"_encoded.csv\"), encoding = \"UTF-8\", na.strings = \"\")", sep = ""), file = report.name , sep = "\n", append = TRUE)

      }


      cat("\n", file = report.name , sep = "\n", append = TRUE)
      cat("## label Variables", file = report.name , sep = "\n", append = TRUE)

      cat("MainDataFrame <- kobo_label(MainDataFrame , dico)", file = report.name , sep = "\n", append = TRUE)

      for (dbr in dataBeginRepeat) {
        cat(paste(dbr, " <- kobo_label(",dbr ," , dico)", sep = ""), file = report.name , sep = "\n", append = TRUE)
      }

      #### Convert to ordinal variable

      cat("\n", file = report.name , sep = "\n", append = TRUE)
      cat("## Set up ordinal Variables", file = report.name , sep = "\n", append = TRUE)
      if (nrow(ordinal) > 0) {

        for (o in 1:nrow(ordinal)) {
          # o <- 1
          ordinal.listname <- as.character(ordinal[ o, c("listname")])
          ordinal.name <- as.character(ordinal[ o, c("fullname")])
          ordinal.frame <- as.character(ordinal[ o, c("qrepeatlabel")])
          if ( exists(paste0(ordinal.frame)) == TRUE) {
            cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", ordinal.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = report.name ,sep = "\n", append = TRUE)
            cat(paste0(ordinal.frame,"$",ordinal.name," <- factor(",ordinal.frame,"$",ordinal.name,", levels = list.ordinal)"),file = report.name ,sep = "\n", append = TRUE)
          } else {}
        }
      } else {}

      ## To do use configuration file to weight the data #######
      cat("\n", file = report.name , sep = "\n", append = TRUE)
      cat("## Create weighted survey object", file = report.name , sep = "\n", append = TRUE)

        ## If no weight, the weighted object is unweigthted

      if (configInfo[configInfo$name == "sample_type","value"] == "No sampling (type 1)") {
        ## If no weight, the weighted object is unweigthted
        cat("MainDataFrame.survey <- survey::svydesign(ids = ~ 1 ,  data = MainDataFrame )", file = report.name , sep = "\n", append = TRUE)

        for (dbr in dataBeginRepeat) {
          cat(paste(dbr,".survey <- survey::svydesign(ids = ~ 1 ,  data = ",dbr," )", sep = ""), file = report.name , sep = "\n", append = TRUE)
        }
        ## with clusters

      }else if (configInfo[configInfo$name == "sample_type","value"] == "Cluster sample (type 2)") {
        ## with clusters
        cat(paste("MainDataFrame.survey <- survey::svydesign(ids = ~ ", configInfo[configInfo$name == "variable_name","value"],",  data = MainDataFrame,  weights = ~ ", configInfo[configInfo$name == "weightsVariable","value"]," ,  fpc = ~ fpc )", sep = ""), file = report.name , sep = "\n", append = TRUE)

        for (dbr in dataBeginRepeat) {
          cat(paste(dbr,".survey <- survey::svydesign(ids = ~ ", configInfo[configInfo$name == "variable_name","value"],",  data = ",dbr,",  weights = ~ ", configInfo[configInfo$name == "weightsVariable","value"]," ,  fpc = ~ fpc )", sep = ""), file = report.name , sep = "\n", append = TRUE)
        }
        ## with strata

      }else if (configInfo[configInfo$name == "sample_type","value"] == "Stratified sample (type 3)") {
        ## with strata
        cat(paste("MainDataFrame.survey <- survey::svydesign(id=~1, strata= ~ ", configInfo[configInfo$name == "variable_name","value"]," ,check.strata = TRUE,  data = MainDataFrame,  weights = ~ ", configInfo[configInfo$name == "weightsVariable","value"],"  )", sep = ""), file = report.name , sep = "\n", append = TRUE)

        for (dbr in dataBeginRepeat) {
          cat(paste(dbr,".survey <- survey::svydesign(id=~1, strata= ~ ", configInfo[configInfo$name == "variable_name","value"]," ,check.strata = TRUE,  data = ",dbr,",  weights = ~ ", configInfo[configInfo$name == "weightsVariable","value"],"  )", sep = ""), file = report.name , sep = "\n", append = TRUE)
        }
      }



      ## with strata
      #cat("MainDataFrame_edited.survey <- survey::svydesign(id=~1, strata= ~ RecordCategory ,check.strata = TRUE,  data = MainDataFrame_edited,  weights = ~ WeightingCoefficient  )", file = report.name , sep = "\n", append = TRUE)

      ## with clusters
      #cat("MainDataFrame_edited.survey <- survey::svydesign(ids = ~ Camp.Province ,  data = MainDataFrame_edited,  weights = ~ weight ,  fpc = ~ fpc )", file = report.name , sep = "\n", append = TRUE)
      #cat("br1.survey <- survey::svydesign(ids = ~ Camp.Province ,  data = br1,  weights = ~ weight ,  fpc = ~ fpc )", file = report.name , sep = "\n", append = TRUE)
      #cat("br2.survey <- survey::svydesign(ids = ~ Camp.Province ,  data = br2,  weights = ~ weight ,  fpc = ~ fpc )", file = report.name , sep = "\n", append = TRUE)

      # ## If no weight, the weighted object is unweigthted
      # cat("MainDataFrame_edited.survey <- survey::svydesign(ids = ~ 1 ,  data = MainDataFrame_edited )", file = report.name , sep = "\n", append = TRUE)
      # cat("br1.survey <- survey::svydesign(ids = ~ 1 ,  data = br1 )", file = report.name , sep = "\n", append = TRUE)
      # cat("br2.survey <- (ids = ~ 1 ,  data = br2 )", file = report.name , sep = "\n", append = TRUE)


        cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)


        ### To DO : Offer option to insert in the report skeleton interpretation questions
        ### Intro text####################################################################
        cat(paste("# Crunching step\n"),file = report.name , sep = "\n", append = TRUE)
        cat(paste("This data crunching report allows to quickly explore the results of the survey that can be regenerated as needed."),file = report.name , sep = "\n", append = TRUE)

        cat(paste("The objective of this report is to allow to quickly identify potential patterns in your dataset.
                  A quick screening of this initial report should allow to select the most meaningful graphs.
                  The crunching process produces a lot of visuals. Therefore it is key to carefully select the
                  most relevant visual that will be presented for potential interpretation in the next step.
                  A typical data interpretation session shall not include more than 60 visuals (60 slides…)
                  to look at in order to keep participants with a good focus level.\n "),file = report.name , sep = "\n", append = TRUE)
        cat(paste("In order to guide this selection phase, the data crunching expert and report designer,
                  in collaboration with the data analysis group, can use the following elements:\n "),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  For numeric value, check the frequency distributions of each variable to average, deviation, including outliers and oddities\n "),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  For categorical variables, check for unexpected values: any weird results based on common sense expectations\n "),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  Use correlation analysis to check for potential contradictions in respondents’ answers to different questions for identified associations (chi-square)\n "),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  Always, Check for missing data (NA) or \"%of respondent who answered\" that you cannot confidently explain\n "),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  Check unanswered questions, that corresponds to unused skip logic in the questionnaire: For instance, did a person who was never displaced answer displacement-related questions? Were employment-related answers provided for a toddler?\n "),file = report.name , sep = "\n", append = TRUE)


        cat(paste("when analyzing those representations in a collective setting during data interpretation sessions, you may:  \n "),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  __Reflect__: question data quality and/or make suggestions to adjust questions, identify additional cleaning steps;   \n"),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  __Interpret__: develop qualitative interpretations of data patterns;     \n"),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  __Recommend__: suggest recommendations in terms of programmatic adjustment;    \n"),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  __Classify__: define level of sensitivity for certain topics if required;     \n"),file = report.name , sep = "\n", append = TRUE)

        cat(paste("The report can be regenerated as needed by:  "),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  adjusting the report configuration in the xlsform to break it into report and chapter;   \n"),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  configuring disaggregation & correlation for each questions;   \n"),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  revising the data cleansing based on the cleaning log;   \n "),file = report.name , sep = "\n", append = TRUE)
        cat(paste("  *  appending calculated indicators to your data frame to reshape variable - also called feature engineering. \n\n"),file = report.name , sep = "\n", append = TRUE)



      cat(paste("# Dataset description\n"),file = report.name , sep = "\n", append = TRUE)
      cat(paste("__Title of the study:__ ",configInfo[configInfo$name == "titl", c("value")],", ,\n\n"),file = report.name , sep = "\n", append = TRUE)
      cat(paste("__Abstract:__ ",configInfo[configInfo$name == "abstract", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
      cat(paste("__Rights & Disclaimer:__ ",configInfo[configInfo$name == "disclaimer", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
      cat(paste("__Country where the study took place:__ ",configInfo[configInfo$name == "Country", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
      cat(paste("__Geographic Coverage for the study within the country:__ ",configInfo[configInfo$name == "geogCover", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
      cat(paste("__Kind of Data:__ ",configInfo[configInfo$name == "dataKind", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
      cat(paste("__Number of records in the main data frame__: `r nrow(MainDataFrame)`\n"),file = report.name , sep = "\n\n", append = TRUE)
      cat(paste("__Period of data collection__: between `r min(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))` and `r max(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))`\n"),file = report.name , sep = "\n\n", append = TRUE)
      cat(paste("__Documented cleaning__: ",configInfo[configInfo$name == "cleanOps", c("value")],"\n\n"),file = report.name , sep = "\n\n", append = TRUE)
      cat(paste("__Entity being analyzed in the study:__ ",configInfo[configInfo$name == "AnalysisUnit", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
      cat(paste("__Procedure, technique, or mode of inquiry used to attain the data:__ ",configInfo[configInfo$name == "ModeOfCollection", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
      cat(paste("__Study Universe:__  (i.e. group of persons or other elements that are the object of research and to which any analytic results refer:",configInfo[configInfo$name == "universe", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)

      ## get list of chapters
      chapters <- as.data.frame(unique(dico[ , c("chapter","report")]))
      names(chapters)[2] <- "Report"
      names(chapters)[1] <- "Chapter"

      ## Default behavior if no chapter was defined in xlsform
      if ((nrow(chapters) == 1) & is.na(chapters$Chapter)) {
        cat("Defaulting questions allocation to chapter")
        dico$chapter[ dico$type %in% c("select_one","select_multiple_d")] <- "report"
        chapters <- as.data.frame(unique(dico$chapter))
        names(chapters)[1] <- "Chapter"
      } else {}

      chapters <- as.data.frame(chapters[!is.na(chapters$Chapter) & chapters$Report == reportsname, ])

      names(chapters)[1] <- "Chapter"

      for (v in 1:nrow(chapters) )
      {
        # i <-v
        chaptersname <- as.character(chapters[ v , 1])


        cat(paste("# ", chaptersname),file = report.name , sep = "\n", append = TRUE)
        if (app == "shiny") {
          progress$set(message = "Compilation of questions results in progress...")
          updateProgress()
        }
        ## Getting chapter questions #######
        #chapterquestions <- dico[which(dico$chapter== chaptersname ), c("chapter", "name", "label", "type", "qrepeatlabel", "fullname","listname") ]
        chapterquestions <- dico[which(dico$chapter == chaptersname & dico$type %in% c("select_one","integer","select_multiple_d", "text","date", "numeric", "calculate")),
                                 c("chapter", "name", "label", "labelReport","hintReport", "type", "qrepeatlabel", "fullname","listname","variable") ]
        #levels(as.factor(as.character(dico[which(!(is.na(dico$chapter)) & dico$formpart=="questions"), c("type") ])))
        ##Loop.questions####################################################################################################
        if (app == "shiny") {
          progress$set(message = "Getting level for each questions in progress...")
          updateProgress()
        }
        for (j in 1:nrow(chapterquestions))
        {
          # j <- 20
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
          cat("\n ",file = report.name , sep = "\n", append = TRUE)
          cat(paste("## ", questions.label ,sep = ""),file = report.name , sep = "\n", append = TRUE)

          ## Now create para based on question type-------


          cat(paste(if (is.na(questions.hint)){paste0("")} else {paste0(questions.hint)},"\n\n",sep = ""),file = report.name ,sep = "\n", append = TRUE)


          ###select one###################################################################################################
          if (questions.type == "select_one" ) {

            cat(paste("Single choice question ","\n\n",sep = ""),file = report.name ,sep = "\n", append = TRUE)

            ## selectone.tabulation######################################################################
            ## compute frequency to see if it's not empty
            frequ <- as.data.frame(table( get(paste0(questions.frame))[[questions.name]]))

            figheight <- as.integer(nrow(frequ))
            if (figheight == 0) { figheight <- "3"} else {figheight <- figheight/1.2}

            ## Check that there are responses to be displayed ####
            if (nrow(frequ) %in% c("0","1") ) {
              cat(paste0("cat(\"No responses or only one modality recorded for this question...\")"),file = report.name , sep = "\n", append = TRUE)
              cat("No responses recorded for this question...\n")

              #  names(frequ)[2] <- "ccheck"
              #  try <- frequ$ccheck
              #  } else if (sum(try) == 0) {
              #   cat(paste0("cat(\"No responses recorded for this question...\")"),file = report.name , sep = "\n", append = TRUE)
              #    cat("No responses recorded for this question...\n")
            }      else {

              #cat(paste("### Tabulation" ,sep = ""),file = report.name ,sep = "\n", append = TRUE)
              ## Open chunk
              cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=",figheight,", size=\"small\"}\n"), file = report.name, append = TRUE)
              #  cat(paste("### Tabulation" ,sep = ""),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("##Compute contengency table"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ <- as.data.frame(table(",questions.variable,"))"),file = report.name ,sep = "\n", append = TRUE)
              #cat(paste0("if (nrow(frequ)==0){ cat(\"No response for this question\") } else{"),file = report.name ,sep = "\n", append = TRUE)


              # cat(paste0("## display table"),file = report.name ,sep = "\n", append = TRUE)
              # cat(paste0("## Reorder factor"),file = report.name ,sep = "\n", append = TRUE)


              ## Check variable type to order the factor ####
              ## - if not ordinal order according to frequency - if ordinal order according to order in the dico
              if (questions.ordinal == "ordinal" ) {
                ### get the list of options in the right order
                cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", questions.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("levels(frequ$Var1) <- list.ordinal"),file = report.name ,sep = "\n", append = TRUE)
              } else {
                cat(paste0("frequ[ ,1] = factor(frequ[ ,1],levels(frequ[ ,1])[order(frequ$Freq, decreasing = FALSE)])"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("frequ <- frequ[ order(frequ[ , 1]) ,  ]"),file = report.name ,sep = "\n", append = TRUE)
              }


              cat(paste0("names(frequ)[1] <- \"", questions.shortname,"\""),file = report.name ,sep = "\n", append = TRUE)
             # cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\")"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("## Frequency table with NA in order to get non response rate"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ1 <- as.data.frame(prop.table(table(", questions.variable,", useNA = \"ifany\")))"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ1 <- frequ1[!(is.na(frequ1$Var1)), ]"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ1 <- frequ1[!(frequ1$Var1 == \"NA\"), ]"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("percentreponse <- paste0(round(sum(frequ1$Freq)*100,digits = 1),\"%\")"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("## Frequency table without NA"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ2 <- as.data.frame(prop.table(table(", questions.variable,",useNA = \"no\")))"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("## Frequency table with weight"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ.weight <- as.data.frame(svymean(~ ",questions.name,", design = ",questions.frame,".survey, na.rm = TRUE))"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("## Binding the two"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("frequ3 <- cbind(frequ2,frequ.weight)"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("## Reorder factor"),file = report.name ,sep = "\n", append = TRUE)
              #    cat(paste0("frequ2[ ,1] = factor(frequ2[ ,1],levels(frequ2[ ,1])[order(frequ2$Freq, decreasing = FALSE)])"),file = report.name ,sep = "\n", append = TRUE)
              #    cat(paste0("frequ2 <- frequ2[ order(frequ2[ , 1]) ,  ]"),file = report.name ,sep = "\n", append = TRUE)
              #    cat(paste0("frequ2[ ,3] <- paste0(round(frequ2[ ,2]*100,digits = 1),\"%\")"),file = report.name ,sep = "\n", append = TRUE)
              #    cat(paste0("names(frequ2)[3] <- \"freqper2\""),file = report.name ,sep = "\n", append = TRUE)


              if (questions.ordinal == "ordinal" ) {
                cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", questions.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("levels(frequ3$Var1) <- list.ordinal"),file = report.name ,sep = "\n", append = TRUE)
              } else {
                cat(paste0("frequ3[ ,1] = factor(frequ3[ ,1],levels(frequ3[ ,1])[order(frequ3$mean, decreasing = FALSE)])"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("frequ3 <- frequ3[ order(frequ3[ , 1]) ,  ]"),file = report.name ,sep = "\n", append = TRUE)
              }

              cat(paste0("frequ3[ ,5] <- paste0(round(frequ3[ ,3]*100,digits = 1),\"%\")"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("names(frequ3)[5] <- \"freqper2\""),file = report.name ,sep = "\n", append = TRUE)

              cat(paste0("\n"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("## and now the graph"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("plot1 <- ggplot(frequ3, aes(x = frequ3$Var1, y = frequ3$mean)) +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("geom_bar(fill = \"#2a87c8\", colour = \"#2a87c8\", stat = \"identity\", width = .8) +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("guides(fill = FALSE) +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("geom_label_repel(aes(y = mean, label = freqper2), fill = \"#2a87c8\", color = 'white') +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("ylab(\"Frequency\") +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("scale_y_continuous(labels = percent) +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("xlab(\"\") +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("coord_flip() +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("ggtitle(\"",questions.label,"\","),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("subtitle = paste0(\" Question response rate: \",percentreponse,\" .\")) +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("kobo_unhcr_style_bar()"),file = report.name ,sep = "\n", append = TRUE)

              cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)
              #cat(paste0("}"),file = report.name ,sep = "\n", append = TRUE)
              ## Close chunk
              cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)
              cat(paste0("\n\n\n\n", sep = '\n'), file = report.name, append = TRUE)


              ##selectone.crosstabulation #######################################################################
              if (nrow(disaggregation) == 0) {
                cat("No disaggregation requested for this question...\n",file = report.name , sep = "\n", append = TRUE)
                cat("No disaggregation requested for this question...\n")
                cat("\n", file = report.name, append = TRUE)
              } else if (nrow(frequ) %in% c("0","1")) {
                # cat("No responses recorded for this question. No disaggregation...\n",file = report.name , sep = "\n", append = TRUE)
                cat("No responses or only one modality recorded for this question. No disaggregation...\n")
                cat("\n", file = report.name, append = TRUE)
              } else {

                cat(paste("### Cross-tabulations" ,sep = ""),file = report.name ,sep = "\n", append = TRUE)
                cat("\n", file = report.name, append = TRUE)

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
                    cat(paste0("\n"),file = report.name , sep = "\n", append = TRUE)
                  } else if (nrow(as.data.frame(table( get(paste0(questions.frame))[[disag.name]]))) == 0) {
                    cat(paste0("\n"),file = report.name , sep = "\n", append = TRUE)
                  } else {


                    ## Case #1 - categoric*numeric -> box plot
                    if (disag.type == "integer") {
                      # Get number levels to set up chart height
                      figheight <- nlevels( as.factor(get(paste0(questions.frame))[[disag.name]]))
                      if ( figheight == 0) { figheight <- "3"}
                      else if ( figheight == 1) {figheight <- "3"}
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
                      cat(paste0("\n```{r ", questions.name,"x",h, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=",figheight*3,", size=\"small\"}\n"), file = report.name, append = TRUE)

                      cat(paste("\n",i,"-", j,"-" , h, " - Render disaggregation : ", disag.label, "for question: ", questions.label,"\n" ))

                      ### Just making sure that the variable is actually a numeric one... in case it was not parsed correctly ####
                      cat(paste0(questions.frame,"$",disag.name," <- as.numeric(",questions.frame,"$",disag.name,")"),file = report.name ,sep = "\n", append = TRUE)

                      ## Boxplot
                      ## To do test if there's outliers... ###
                      #if( quantile(questions.frame$disag.name, probs=c(.25, .75), na.rm = T))



                      if (disag.ordinal == "ordinal" ) {
                        cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", disag.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("levels(",questions.frame,"$",disag.name,") <- list.ordinal"),file = report.name ,sep = "\n", append = TRUE)
                      } else {}

                      cat(paste0("plot1 <- ggplot(",questions.frame,", aes(x=",questions.frame,"$",questions.name," , y=",questions.frame,"$",disag.name,")) +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("geom_boxplot(fill=\"#2a87c8\",colour = \"black\" ) + "),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("scale_size_area(max_size = 10) +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("guides(fill = FALSE) +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("xlab(\"\") +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("ylab(\"\") +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("coord_flip() +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("scale_y_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("ggtitle(\"",questions.label,"\","),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("subtitle = \". By question: ",disag.label,".\") +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("kobo_unhcr_style_histo()"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)


                      data.outlier <- get(paste0(questions.frame))[[disag.name]]
                      data.nooutlier <- as.data.frame(get(paste0(questions.frame))[[disag.name]])
                      qnt <- quantile(data.outlier, probs = c(.25, .75), na.rm = T)
                      caps.df <- as.data.frame(quantile(data.outlier, probs = c(.05, .95), na.rm = T))
                      H  <- stats::IQR(data.outlier, na.rm = T)
                      if(H >= 1.349 ) {
                        cat(paste0("cat(\"No outliers detectected...\")"),file = report.name , sep = "\n", append = TRUE)
                        cat("\n")
                      } else {
                              cat(paste0("data.outlier1 <- ",questions.frame,"$",disag.name),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("data.nooutlier1 <- as.data.frame(",questions.frame,"$",disag.name,")"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("qnt1 <- quantile(data.outlier1, probs=c(.25, .75), na.rm = T)"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("caps.df1 <- as.data.frame(quantile(data.outlier1, probs=c(.05, .95), na.rm = T))"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("H1  <- 1.5 * IQR(data.outlier1, na.rm = T)"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("data.nooutlier1[(data.nooutlier1 < (qnt1[1] - H1)) & !(is.na( data.nooutlier1))  ] <- caps.df1[1,1]"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("data.nooutlier1[ (data.nooutlier1 > (qnt1[2] + H1)) & !(is.na(data.nooutlier1)) ] <- caps.df1[2,1]"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("names(data.nooutlier1)[1] <- \"variable\""),file = report.name ,sep = "\n", append = TRUE)
                              ## Boxplot with capping treatment
                              cat(paste0("## Boxplot"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("plot1 <- ggplot(",questions.frame,", aes(y=data.nooutlier1$variable, x= ",questions.frame,"$",questions.name,")) +"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("geom_boxplot(fill=\"#2a87c8\",colour = \"black\") +  #notch=TRUE"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("scale_size_area(max_size = 10) +"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("guides(fill = FALSE) +"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("xlab(\"\") +"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("ylab(\"\") +"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("coord_flip() +"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("scale_y_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("ggtitle(\"",questions.label,"\","),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("subtitle = \"After data capping treatement. By question: ",disag.label,".\") +"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("kobo_unhcr_style_histo()"),file = report.name ,sep = "\n", append = TRUE)
                              cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)
                      }
                      ## Close chunk
                      cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)

                      ## Case #2 - categoric*categoric -> stacked bar plot
                    } else if (disag.type == "select_one") {

                      # Get number levels to set up chart height
                      figheight <- nlevels(as.factor( get(paste0(questions.frame))[[disag.name]]))
                      if ( figheight == 0) { figheight <- "3"}
                      else if ( figheight == 1) {figheight <- "3"}
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
                      cat(paste0("\n```{r ", questions.name,h, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=",figheight,", size=\"small\"}\n"), file = report.name, append = TRUE)
                      cat(paste("\n",i,"-", j,"-" , h, " - Render disaggregation: ", disag.label, "for question: ", questions.label,"\n" ))

                      if (disag.ordinal == "ordinal" & questions.ordinal == "ordinal" ) {
                        cat(paste0("## Reorder factor"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("crosssfrequ.weight <- as.data.frame(prop.table(survey::svytable(~", questions.name ," + ", disag.name,", design = ",questions.frame ,".survey  ), margin = 2))"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("names(crosssfrequ.weight)[1] <- \"quest\""),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("names(crosssfrequ.weight)[2] <- \"disag\""),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("crosssfrequ.weight$Freq2 <- paste0(round(crosssfrequ.weight$Freq*100,digits = 1),\"%\")"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", disag.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("levels(crosssfrequ.weight$disag) <- list.ordinal"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", questions.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("levels(crosssfrequ.weight$quest) <- list.ordinal"),file = report.name ,sep = "\n", append = TRUE)

                      } else if (disag.ordinal == "ordinal" ) {
                        cat(paste0("## Reorder factor"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("crosssfrequ.weight <-as.data.frame(prop.table(survey::svytable(~", questions.name ," + ", disag.name,", design = ",questions.frame ,".survey  ), margin = 2))"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("names(crosssfrequ.weight)[1] <- \"quest\""),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("names(crosssfrequ.weight)[2] <- \"disag\""),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("crosssfrequ.weight$Freq2 <- paste0(round(crosssfrequ.weight$Freq*100,digits = 1),\"%\")"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", disag.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("levels(crosssfrequ.weight$disag) <- list.ordinal"),file = report.name ,sep = "\n", append = TRUE)

                      } else {
                        cat(paste0("crosssfrequ.weight <-as.data.frame(prop.table(survey::svytable(~", questions.name ," + ", disag.name,", design = ",questions.frame ,".survey  ), margin = 2))"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("names(crosssfrequ.weight)[1] <- \"quest\""),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("names(crosssfrequ.weight)[2] <- \"disag\""),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("crosssfrequ.weight$Freq2 <- paste0(round(crosssfrequ.weight$Freq*100,digits = 1),\"%\")"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("## Reorder factor"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("cross <- dcast(crosssfrequ.weight, disag  ~ quest, value.var = \"Freq\")"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("cross <- cross[ order(cross[ ,2], decreasing = FALSE) ,  ]"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("crosssfrequ.weight$disag <- factor(crosssfrequ.weight$disag, levels = as.character(cross[ ,1]))"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("\n"),file = report.name ,sep = "\n", append = TRUE)
                      }


                      cat(paste0("## and now the graph"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("plot1 <- ggplot(crosssfrequ.weight, aes(fill=crosssfrequ.weight$quest, y=crosssfrequ.weight$Freq, x = crosssfrequ.weight$disag)) +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("geom_bar(colour = \"white\", stat = \"identity\", width = .8, aes(fill = quest), position = position_stack(reverse = TRUE)) +"),file = report.name ,sep = "\n", append = TRUE)
                      #cat(paste0("geom_label_repel(aes(label = Freq2), fill = \"#2a87c8\", color = 'white') +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("ylab(\"Frequency\") +"),file = report.name ,sep = "\n", append = TRUE)
                      #cat(paste0("facet_wrap(~disag, ncol=3) +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("scale_y_continuous(labels = percent) +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("scale_fill_viridis(discrete = TRUE) +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("xlab(\"\") +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("coord_flip() +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("ggtitle(\"",questions.label," (color)\","),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("subtitle = \" By question: ",disag.label," (bar)\") +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("kobo_unhcr_style_bar() +"),file = report.name ,sep = "\n", append = TRUE)
                      ## setting up the legend
                      #cat(paste0("guides(fill = FALSE) +"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("theme(legend.direction = \"horizontal\", legend.position = \"bottom\", legend.box = \"horizontal\",legend.title = element_blank()  )"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)

                      ## Close chunk
                      cat(paste0("\n```\n", sep = ""), file = report.name, append = TRUE)
                      cat("\n", file = report.name, append = TRUE)

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
              cat("No correlation requested for this question...\n",file = report.name , sep = "\n", append = TRUE)
              cat("No correlation requested for this question...\n")
              cat("\n", file = report.name, append = TRUE)
            } else if (nrow(frequ) %in% c("0","1")) {
              #cat("No responses recorded for this question. No analysis of correlation...\n",file = report.name , sep = "\n", append = TRUE)
              cat("No responses recorded for this question. No analysis of correlation...\n")
              cat("\n", file = report.name, append = TRUE)
            } else {

              cat("\n", file = report.name, append = TRUE)
              cat(paste("### Significant Associations (chi-square with p value < 5%)" ,sep = ""),file = report.name ,sep = "\n", append = TRUE)
              cat("\n", file = report.name, append = TRUE)

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
                cat("No significant association found for this question...\n",file = report.name , sep = "\n", append = TRUE)
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
                  cat(paste0("\n```{r ", questions.name,"ccc",m, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=6, size=\"small\"}\n"), file = report.name, append = TRUE)

                  cat(paste0("corrplot(stats::chisq.test(",formula.target1,",", formula.tested1,")$residuals,"), file = report.name , sep = "\n", append = TRUE)
                  cat(paste0("is.cor = FALSE, # use for general matrix to convert to Sq form"), file = report.name , sep = "\n", append = TRUE)
                  cat(paste0("cl.pos = \"n\", ## Do not display the color legend"), file = report.name , sep = "\n", append = TRUE)
                  cat(paste0("cl.cex = 0.7, # Size of all label"), file = report.name , sep = "\n", append = TRUE)
                  cat(paste0("tl.cex = 0.7, # Size of axis label"), file = report.name , sep = "\n", append = TRUE)
                  cat(paste0("tl.srt = 45, # string rotation in degrees"), file = report.name , sep = "\n", append = TRUE)
                  cat(paste0("tl.col = \"black\", # color of text label."), file = report.name , sep = "\n", append = TRUE)
                  cat(paste0("addCoef.col = \"grey\", # add coeff in the chart"), file = report.name , sep = "\n", append = TRUE)
                  cat(paste0("number.cex= 3/ncol(stats::chisq.test(",formula.target1,",", formula.tested1,")), # size of coeff"), file = report.name , sep = "\n", append = TRUE)
                  cat(paste0("mar = c(0.5,0.5,4, 0.5), ## margin of plots"), file = report.name , sep = "\n", append = TRUE)
                  cat(paste0("title= paste0(\"Correlation between", "\n",target.label," (row)\n", " & ",tested.label," (col)\")) "), file = report.name , sep = "\n", append = TRUE)
                  ## Close chunk
                  cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)
                }
              }
            }
            }


            ##Decimal####################################################################################################
          } else if (questions.type == "decimal" | questions.type == "integer" | questions.type == "numeric" | questions.type == "calculate") {
            cat(paste("Numeric question  " ,"\n\n",sep = ""),file = report.name ,sep = "\n", append = TRUE)

            ## Check the lenght of the table to see if we can display it or not...
            frequ <- as.data.frame(table( get(paste0(questions.frame))[[questions.name]]))

            ####Decimal.tabulation########################################################################
          #  cat(paste("### Tabulation\n" ,sep = ""),file = report.name ,sep = "\n", append = TRUE)

            ## Open chunk
            cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file = report.name, append = TRUE)
            ### Just making sure that the variable is actually a numeric one... in case it was not parsed correctly ####
            cat(paste0(questions.frame,"$",questions.name," <- as.numeric(",questions.frame,"$",questions.name,")"),file = report.name ,sep = "\n", append = TRUE)
            cat(paste0("frequ <- as.data.frame(table(",questions.variable,"))"),file = report.name ,sep = "\n", append = TRUE)

            ### Check if we have records or if we have too many records
            if (nrow(frequ) %in% c("0","1")) {
              cat(paste0("cat(\"No responses recorded for this question...\")"),file = report.name , sep = "\n", append = TRUE)
              cat("No responses recorded for this question...\n")
            } else if (nrow(frequ) > 10) {
            #   cat(paste0("cat(\"There's too many potential values to display. We will only show the histogram. \n \")"),file = report.name ,sep = "\n", append = TRUE)
            } else{
           #   cat(paste0("## display table"),file = report.name ,sep = "\n", append = TRUE)
           #   cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\")"),file = report.name ,sep = "\n", append = TRUE)
            }

            ## To do implement FD number of bin: https://www.r-bloggers.com/friday-function-nclass/
            if (nrow(frequ) %in% c("0","1")) {
              cat(paste0("cat(\"No responses recorded for this question...\")"),file = report.name , sep = "\n", append = TRUE)
              cat("\n")
            } else {

              cat(paste0("average <- as.data.frame(survey::svymean(~ ",questions.name,", design = ",questions.frame,".survey, na.rm = TRUE))"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("cat(paste0(\"Based on the sample design, the average weighted mean response for this question is \", as.numeric(round(average$mean, digits = 2))))"),file = report.name ,sep = "\n", append = TRUE)
            #  cat(paste0("sd <- as.data.frame(jtools::svysd(~ ",questions.name,", design = ",questions.frame,".survey, na.rm = TRUE))"),file = report.name ,sep = "\n", append = TRUE)
             # cat(paste0("cat(paste0(\"Based on the sample design, the average weighted standard deviation for this question is \", as.numeric(round(sd, digits = 2))))"),file = report.name ,sep = "\n", append = TRUE)


              ### Detect outliers and adjust bien numbers #####
              ### To -- check there's outlier or not
              ## Double check that we have a continuous value -- not a factor --

              data.outlier <- get(paste0(questions.frame))[[questions.name]]
              data.nooutlier <- as.data.frame(get(paste0(questions.frame))[[questions.name]])
              qnt <- quantile(data.outlier, probs = c(.25, .75), na.rm = T)
              caps.df <- as.data.frame(quantile(data.outlier, probs = c(.05, .95), na.rm = T))
              H  <- stats::IQR(data.outlier, na.rm = T)



              cat(paste0("#  regular histogram"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("plot1 <- ggplot(data = frequ, aes(x = frequ$Var1, y = frequ$Freq)) +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("geom_bar(fill = \"#2a87c8\",colour = \"white\", stat = \"identity\", width = .8) +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("labs(x = \"\", y = \"Count\") +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("ggtitle(\"",questions.label,"\","),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("subtitle = \":\n\""),file = report.name ,sep = "\n", append = TRUE)
              #cat(paste0("\"Mean: \",round(mean(frequ$Var1),2) ,\n\""),file = report.name ,sep = "\n", append = TRUE)
             # cat(paste0("\"Standard Deviation: \",round(sd(frequ$Var1),2) ,\n\""),file = report.name ,sep = "\n", append = TRUE)
              #cat(paste0("\"Coefficient of Variation: \",round(cv(frequ$Var1),2) ,\n\""),file = report.name ,sep = "\n", append = TRUE)
              #cat(paste0("\"Skewness: \",round(skewness(frequ$Var1),2) ,\n\""),file = report.name ,sep = "\n", append = TRUE)
              #cat(paste0("\"and Kurtosis: \",round(kurtosis(frequ$Var1),2) ,\n\""), file = report.name ,sep = "\n", append = TRUE)
              cat(paste0(") +"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("kobo_unhcr_style_histo()"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("\n\n"),file = report.name ,sep = "\n", append = TRUE)

              if (H >= 1.349 ) {
                  cat(paste0("cat(\"No outliers detectected...\")"),file = report.name , sep = "\n", append = TRUE)
                  cat("\n")
                } else {
                        cat(paste0("data.outlier <- ",questions.frame,"$",questions.name),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("data.nooutlier <- as.data.frame(",questions.frame,"$",questions.name,")"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("qnt <- quantile(data.outlier, probs = c(.25, .75), na.rm = T)"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("caps.df <- as.data.frame(quantile(data.outlier, probs = c(.05, .95), na.rm = T))"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("H  <- 1.5 * stats::IQR(data.outlier, na.rm = T)"),file = report.name ,sep = "\n", append = TRUE)

                        cat(paste0("data.nooutlier[(data.nooutlier < (qnt[1] - H)) & !(is.na(data.nooutlier))  ] <- caps.df[1,1]"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("data.nooutlier[ (data.nooutlier > (qnt[2] + H)) & !(is.na(data.nooutlier)) ] <- caps.df[2,1]"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("names(data.nooutlier)[1] <- \"variable\""),file = report.name ,sep = "\n", append = TRUE)


                        ### Now graphs with treated variable #####
                        cat(paste0("plot1 <- ggplot(data = data.nooutlier, aes(x = data.nooutlier$variable)) +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("geom_histogram(color = \"white\",fill = \"#2a87c8\", breaks = pretty(data.nooutlier$variable, n = nclass.Sturges(data.nooutlier$variable),min.n = 1)) +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("labs(x = \"\", y = \"Count\") +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("ggtitle(\"",questions.label,"\","),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("subtitle = \"After data capping treatement:\n\""),file = report.name ,sep = "\n", append = TRUE)
                        #cat(paste0("\"Mean: \",round(mean(data.nooutlier$variable),2) ,\n\""),file = report.name ,sep = "\n", append = TRUE)
                        #cat(paste0("\"Standard Deviation: \",round(sd(data.nooutlier$variable),2) ,\n\""),file = report.name ,sep = "\n", append = TRUE)
                        #cat(paste0("\"Coefficient of Variation: \",round(cv(data.nooutlier$variable),2) ,\n\""),file = report.name ,sep = "\n", append = TRUE)
                        #cat(paste0("\"Skewness: \",round(skewness(data.nooutlier$variable),2) ,\n\""),file = report.name ,sep = "\n", append = TRUE)
                        #cat(paste0("\"and Kurtosis: \",round(kurtosis(data.nooutlier$variable),2) ,\n\""), file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0(") +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("kobo_unhcr_style_histo()"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)
                      }
            }
            ## Close chunk
            cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)

            ###Decimal.crosstabulation###########################################################################
            if (nrow(disaggregation) == 0) {
              cat("No disaggregation requested for this question...\n",file = report.name , sep = "\n", append = TRUE)
              cat("No disaggregation requested for this question...\n")
              cat("\n", file = report.name, append = TRUE)
            } else if (nrow(frequ) %in% c("0","1")) {
              # cat("No responses recorded for this question. No disaggregation...\n",file = report.name , sep = "\n", append = TRUE)
              cat("No responses recorded for this question. No disaggregation...\n")
              cat("\n", file = report.name, append = TRUE)
            } else {


              cat(paste("### Analysis of relationship" ,sep = ""),file = report.name ,sep = "\n", append = TRUE)
              for (h in 1:nrow(disaggregation)) {
                #h <-1
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
                  cat(paste0("\n"),file = report.name , sep = "\n", append = TRUE)
                }  else if (nrow(as.data.frame(table( get(paste0(questions.frame))[[disag.name]]))) == 0) {
                  cat(paste0("\n"),file = report.name , sep = "\n", append = TRUE)
                } else {


                  ## Case #1 - numeric * categoric -> box plot
                  if (disag.type == "select_one") {


                    # Get number levels to set up chart height
                    figheight <- nlevels(as.factor( get(paste0(questions.frame))[[disag.name]]))
                    if ( figheight == 0) { figheight <- "3"}
                    else if ( figheight == 1) {figheight <- "3"}
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
                    cat(paste0("\n```{r ", questions.name,"x",h, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=",figheight,", size=\"small\"}\n"), file = report.name, append = TRUE)

                    cat(paste("\n", i,"-", j,"-" , h, " - Render disaggregation : ", disag.label, "for question: ", questions.label,"\n" ))

                    ## account of Ordinal variable
                    if (disag.ordinal == "ordinal") {
                      cat(paste0("list.ordinal <- as.character(unique(dico[ dico$listname == \"", disag.listname,"\" & dico$type == \"select_one_d\", c(\"labelchoice\") ]))"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0(questions.frame,"$",disag.name," <- as.factor(",questions.frame,"$",disag.name,")"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("levels(",questions.frame,"$",disag.name,") <- list.ordinal"),file = report.name ,sep = "\n", append = TRUE)
                      cat(paste0("\n"),file = report.name ,sep = "\n", append = TRUE)
                    } else {
                      cat(paste0("\n"),file = report.name ,sep = "\n", append = TRUE)
                    }


                    ## Boxplot

                    cat(paste0("plot1 <- ggplot(",questions.frame,", aes(y=",questions.frame,"$",questions.name," , x=",questions.frame,"$",disag.name,")) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("geom_boxplot(fill=\"#2a87c8\",colour = \"black\") + "),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("scale_size_area(max_size = 10) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("guides(fill = FALSE) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("xlab(\"\") +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("ylab(\"\") +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("coord_flip() +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("scale_y_continuous(breaks= pretty_breaks()) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("ggtitle(\"",questions.label,"\","),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("subtitle = \"by question: ",disag.label,"\") +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("kobo_unhcr_style_bar()"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)

                    if(H >= 1.349 ) {
                      cat(paste0("cat(\"No outliers detectected...\")"),file = report.name , sep = "\n", append = TRUE)
                      cat("\n")
                    } else {
                        ## Boxplot with capping treatment
                        cat(paste0("## Boxplot"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("plot1 <- ggplot(",questions.frame,", aes(y=data.nooutlier$variable, x= ",questions.frame,"$",disag.name,")) +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("geom_boxplot(fill=\"#2a87c8\",colour = \"black\") +  #notch=TRUE"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("scale_size_area(max_size = 10) +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("guides(fill = FALSE) +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("xlab(\"\") +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("ylab(\"\") +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("coord_flip() +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("scale_y_continuous(breaks= pretty_breaks()) +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("ggtitle(\"",questions.label,"\","),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("subtitle = \"After data capping treatement. By question: ",disag.label,"\") +"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("kobo_unhcr_style_bar()"),file = report.name ,sep = "\n", append = TRUE)
                        cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)
                        }
                    ## Close chunk
                    cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)

                    ## Case #2 - numeric*numeric -> scatter plot
                  } else if (disag.type == "integer") {

                    ## Open chunk
                    cat(paste0("\n```{r ", questions.name,"x",h, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=8, size=\"small\"}\n"), file = report.name, append = TRUE)

                    cat(paste("\n", i,"-", j,"-" , h, " - Render disaggregation : ", disag.label, "for question: ", questions.label,"\n" ))


                    cat(paste0("data.outlier1 <- ",questions.frame,"$",disag.name),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("data.nooutlier1 <- as.data.frame(",questions.frame,"$",disag.name,")"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("qnt1 <- quantile(data.outlier1, probs=c(.25, .75), na.rm = T)"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("caps.df1 <- as.data.frame(quantile(data.outlier1, probs=c(.05, .95), na.rm = T))"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("H1  <- 1.5 * IQR(data.outlier1, na.rm = T)"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("data.nooutlier1[(data.nooutlier1 < (qnt1[1] - H)) & !(is.na( data.nooutlier1))  ] <- caps.df1[1,1]"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("data.nooutlier1[ (data.nooutlier1 > (qnt1[2] + H)) & !(is.na(data.nooutlier1)) ] <- caps.df1[2,1]"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("names(data.nooutlier1)[1] <- \"variable\""),file = report.name ,sep = "\n", append = TRUE)

                    cat(paste0("## Scatter plot"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("plot1 <- ggplot(",questions.frame,", aes(x= ",questions.frame,"$",disag.name, ", y=",questions.frame,"$",questions.name,")) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("geom_count(aes(size = ..prop.., group = 1)) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("scale_size_area(max_size = 10) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("guides(fill = FALSE) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("scale_y_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("scale_x_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("# xlab(correllabel) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("# ylab(variablelabel) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("geom_smooth(method=lm) +  # Add a loess smoothed fit curve with confidence region"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("ggtitle(\"Scatterplot before data capping treatment\") +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("kobo_unhcr_style_scatter()"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)



                    cat(paste0("## Scatter plot rev "),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("plot1 <- ggplot(",questions.frame,", aes(x= data.nooutlier$variable, y=data.nooutlier1$variable )) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("geom_count(aes(size = ..prop.., group = 1)) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("scale_size_area(max_size = 10) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("guides(fill = FALSE) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("scale_y_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("scale_x_continuous(breaks = pretty_breaks(), label = format_si()) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("# xlab(correllabel) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("#ylab(variablelabel) +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("geom_smooth(method=lm) +  # Add a loess smoothed fit curve with confidence region"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("ggtitle(\"Scatterplot after data capping treatment\") +"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("kobo_unhcr_style_scatter()"),file = report.name ,sep = "\n", append = TRUE)
                    cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)

                    ## Close chunk
                    cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)
                  }
                }
              }
            }


            ##select.multi####################################################################################################
          } else if ( questions.type == "select_multiple_d" ) {
            cat(paste("Multiple choice question  " ,"\n\n",sep = ""),file = report.name ,sep = "\n", append = TRUE)


            ###select.multi.tab######################################################################

          #  cat(paste("### Tabulation" ,sep = ""),file = report.name ,sep = "\n", append = TRUE)

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
              cat("No responses recorded for this question...\n",file = report.name , sep = "\n", append = TRUE)
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
                cat("No responses recorded for this question...\n",file = report.name , sep = "\n", append = TRUE)
                cat("No responses recorded for this question...\n")
              } else{

                ## Open chunk
                cat(paste0("\n```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=8, size=\"small\"}\n", sep = '\n'), file = report.name, append = TRUE)
        #         cat(paste0("### Tabulation"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("##Compute contengency table"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("selectmultilist1 <- as.data.frame(dico[dico$type == \"select_multiple\" & dico$listname==\"",questions.listname, "\" & grepl(\"", questions.shortname,"\",dico$fullname)==TRUE , c(\"fullname\")])"),file = report.name ,sep = "\n", append = TRUE)

                cat(paste0("names(selectmultilist1)[1] <- \"check\""),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("check <- as.data.frame(names(",questions.frame ,"))"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("names(check)[1] <- \"check\""),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("check$id <- row.names(check)"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("check <- merge(x = check, y = selectmultilist1, by = \"check\")"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("selectmultilist <- as.character(check[ ,1])"),file = report.name ,sep = "\n", append = TRUE)

                cat(paste0("## Reshape answers"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("data.selectmultilist <- ",questions.frame ,"[ selectmultilist ]"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("data.selectmultilist$id <- rownames(data.selectmultilist)"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("totalanswer <- nrow(data.selectmultilist)"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("data.selectmultilist <- data.selectmultilist[ data.selectmultilist[ ,1]!=\"Not replied\", ]"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("percentreponse <- paste0(round((nrow(data.selectmultilist)/totalanswer)*100,digits = 1),\"%\")"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("meltdata <- reshape2::melt(data.selectmultilist,id=\"id\")"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("castdata <- as.data.frame(table(meltdata[c(\"value\")]))"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("castdata$freqper <- castdata$Freq/nrow(data.selectmultilist)"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("castdata <- castdata[castdata$Var1!=\"Not selected\", ]"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("castdata$Var1 <- factor(castdata$Var1, levels=castdata[order(castdata$freqper), \"Var1\"])"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("frequ <- castdata[castdata$Var1!=\"\", ]"),file = report.name ,sep = "\n", append = TRUE)

                cat(paste0("## display table"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("names(frequ)[1] <- \"", questions.shortname,"\""),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("frequ[ ,3] <- paste0(round(frequ[ ,3]*100,digits = 1),\"%\")"),file = report.name ,sep = "\n", append = TRUE)

            #    cat(paste0("kable(frequ, caption=\"__Table__:", questions.label,"\")"),file = report.name ,sep = "\n", append = TRUE)

                cat(paste0("frequ1 <- castdata[castdata$Var1!=\"\", ]"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("frequ1[ ,4] <- paste0(round(frequ1[ ,3]*100,digits = 1),\"%\")"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("names(frequ1)[4] <- \"freqper2\""),file = report.name ,sep = "\n", append = TRUE)

                cat(paste0("\n"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("## and now the graph"),file = report.name ,sep = "\n", append = TRUE)

                cat(paste0("plot1 <- ggplot(frequ1, aes(x=Var1, y=freqper)) +"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("geom_bar(fill = \"#2a87c8\", colour = \"#2a87c8\", stat = \"identity\", width = .8) +"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("guides(fill = FALSE) +"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("geom_label_repel(aes(y = freqper, label = freqper2), fill = \"#2a87c8\", color = 'white') +"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("ylab(\"Frequency\") +"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("scale_y_continuous(labels = percent) +"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("xlab(\"\") +"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("coord_flip() +"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("ggtitle(\"",questions.label,"\","),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("subtitle = paste0(\"Question response rate: \",percentreponse,\" .\")) +"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("kobo_unhcr_style_bar()"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)
                cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)
                ###select.multi.rel######################################################################


                cat(paste("### Analysis of relationship" ,sep = ""),file = report.name ,sep = "\n", append = TRUE)
                ## Open chunk
                cat(paste0("\n```{r ", questions.name, ".rel, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file = report.name, append = TRUE)
                ## Close chunk
                cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)

              }
            }

            ####date###############################################################################################
          } else if (questions.type == "date") {
            cat(paste("Date question  in data frame: ",questions.frame,"\n\n",sep = ""),file = report.name ,sep = "\n", append = TRUE)

            ####text#############################################################################################
          } else if ( questions.type == "text" ) {
            cat(paste("Open ended question  in data frame: ",questions.frame,"\n\n", sep = ""),file = report.name ,sep = "\n", append = TRUE)

            ## Check if the there are answeers to that questions...
            frequ <- as.data.frame(table( get(paste0(questions.frame))[[questions.name]]))

            if (nrow(frequ) %in% c("0","1")) {
              cat(paste0("cat(\"No responses recorded for this question...\")"),file = report.name , sep = "\n", append = TRUE)
              cat("No responses recorded for this question...\n")
            } else{

              cat(paste("List of given answers \n" ,sep = ""),file = report.name ,sep = "\n", append = TRUE)
              ## Open chunk
              cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=4, size=\"small\"}\n", sep = '\n'), file = report.name, append = TRUE)
              cat(paste0("textresponse <- as.data.frame(table(",questions.frame,"[!(is.na(",questions.variable,")), c(\"",questions.name,"\")]))"),file = report.name ,sep = "\n", append = TRUE)

              cat(paste0("names(textresponse)[1] <- \"", questions.shortname,"\""),file = report.name ,sep = "\n", append = TRUE)
              cat(paste0("kable(textresponse, caption=\"__Table__:", questions.label,"\")"),file = report.name ,sep = "\n", append = TRUE)

              ## Close chunk
              cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)
            }
            # End test on question on type
          }

          ## End loop on questions
        }


        cat(paste("##### Page Break"),file = report.name ,sep = "\n", append = TRUE)
      }
    }


    #### Render things... ###################
    if (app == "shiny") {
      progress$set(message = "Render now all reports...")
      updateProgress()
    }
    cat(" Clean memory... \n")
    gc()
    #rm(list = ls())
    kobo_load_packages()
    mainDir <- kobo_getMainDirectory()
    reports <- utils::read.csv(paste(mainDir,"/data/reports.csv",sep = ""), encoding = "UTF-8", na.strings = "")
    ### Render now all reports
    cat(" Render now reports... \n")
    for (i in 1:nrow(reports)) {
      reportsname <- as.character(reports[ i , 1])
      if (app == "shiny") {
        progress$set(message = paste("Rendering word output report for ",reportsname, " chapter in progress..."))
        updateProgress()
      }



      if (output == "docx") {

        cat(paste(i, " - Render word output report for ",reportsname))
        mainDir <- kobo_getMainDirectory()
        rmarkdown::render(paste(mainDir,"/code/",i,"-", reportsname, "-report.Rmd", sep = ""))
        ## Put the report in the out folder
        mainDir <- kobo_getMainDirectory()
        file.rename(paste(mainDir,"/code/",i,"-", reportsname, "-report.docx", sep = ""), paste0(mainDir,"/out/crunching_reports/Crunching-report-",i,"-", reportsname,"-",Sys.Date(), "-report.docx"))
        ## Clean  memory
        gc()

      } else if (output == "html") {

        cat(paste(i, " - Render html output report for ",reportsname))
        mainDir <- kobo_getMainDirectory()
        rmarkdown::render(paste(mainDir,"/code/",i,"-", reportsname, "-report.Rmd", sep = ""))
        ## Put the report in the out folder
        mainDir <- kobo_getMainDirectory()
        file.rename(paste(mainDir,"/code/",i,"-", reportsname, "-report.html", sep = ""), paste0(mainDir,"/out/crunching_reports/Crunching-report-",i,"-", reportsname,"-",Sys.Date(), "-report.html"))
        ## Clean  memory
        gc()

      } else if (output == "aspx") {

        cat(paste(i, " - Render aspx output - for sharepoint hosting - report for ",reportsname))
        mainDir <- kobo_getMainDirectory()
        rmarkdown::render(paste(mainDir,"/code/",i,"-", reportsname, "-report.Rmd", sep = ""))
        ## Put the report in the out folder
        mainDir <- kobo_getMainDirectory()
        file.rename(paste(mainDir,"/code/",i,"-", reportsname, "-report.html", sep = ""), paste0(mainDir,"/out/crunching_reports/Crunching-report-",i,"-", reportsname,"-",Sys.Date(), "-report.aspx"))
        ## Clean  memory
        gc()

      } else if (output == "pptx") {

        cat(paste(i, " - Render PowerPoint output report for ",reportsname))
        mainDir <- kobo_getMainDirectory()
        rmarkdown::render(paste(mainDir,"/code/",i,"-", reportsname, "-report.Rmd", sep = ""))
        ## Put the report in the out folder
        mainDir <- kobo_getMainDirectory()
        file.rename(paste(mainDir,"/code/",i,"-", reportsname, "-report.pptx", sep = ""), paste0(mainDir,"/out/crunching_reports/Crunching-report-",i,"-", reportsname,"-",Sys.Date(), "-report.pptx"))
        ## Clean  memory
        gc()
      }



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
