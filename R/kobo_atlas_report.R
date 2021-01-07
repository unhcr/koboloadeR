#' @name kobo_atlas_report
#' @rdname kobo_atlas_report
#' @title  Generate an atlas out of the dataset
#'
#' @description  Generate report with data aggregated by location & spatial visualisation / cartography
#'
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#' @param output The output format html or aspx if you need to upload on sharepoint), docx (to quickly cut non interesting vz and take note during data interpretation session), pptx (to quickly cut non interesting vz and persent during data interpretation session), Default is html
#' @param app The place where the function has been executed, the default is the console and the second option is the shiny app
#' @param render TRUE or FALSE - Tells wheter to only produce Rmd or to also knit it in the required output format. Default is TRUE. Usefull for testing as rending takes time.
#' @param lang eng, fre or esp - Change the langauge of the intro to the report - default is english
#'
#' @return No return, All results will be saved on RMD files and Word files
#'
#' @author Edouard Legoupil#'
#'
#' @export kobo_atlas_report
#'
#' @examples
#' \dontrun{
#' kobo_atlas_report(form, mapfile)
#' }
#'

kobo_atlas_report <- function(form = "form.xlsx", 
                              app = "console", 
                              output ="html", 
                              render = "TRUE", 
                              lang = "eng") {

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
    configInfo <- kobo_get_config(form)
    configInfo <- configInfo[!is.na(configInfo$name),]
    mainDir <- kobo_getMainDirectory()
    form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
    #library(koboloadeR)
    
    ### Load the data
    cat("\n\n Loading data. It is assumed that the cleaning, weighting & re-encoding has been done previously \n")
    
    MainDataFrame <- utils::read.csv(paste(mainDir,"/data/MainDataFrame_encoded.csv",sep = ""), encoding = "UTF-8", na.strings = "")
    
    
    # Form ##########################################
    ## Load form
   # cat("\n\n Building dictionnary from the xlsform \n")
    
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
    
    mapref <- configInfo[configInfo$name == "geosurveyid", c("value")]
    
    kobo_aggregate(form,
                   aggregVar = mapref)
    
    MainDataFrameMap <- utils::read.csv(paste(mainDir,"/data/MainDataFrame_edited_map.csv",sep = ""), encoding = "UTF-8", na.strings = "")
    
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
    
    utils::write.csv(reports, paste(mainDir,"/data/mapreports.csv",sep = ""), row.names = FALSE, na = "")
    
    
    ## For each Report: create a Rmd file -------
    
    ##Loop through defined reports ------------
    
    for (i in 1:nrow(reports) )
    {
      # i <-1
      reportsname <- as.character(reports[ i , 1])
      if (app == "shiny") {
        progress$set(message = paste(i, " - Write chapter for ",as.character(reports[ i , 1])))
        updateProgress()
      }
      cat(paste(i, " - Write chapter for ",as.character(reports[ i , 1]),"\n" ))
      report.name <- paste(mainDir, "/code/",i,"-", reportsname, "-atlas.Rmd", sep = "")
      
      ## TO DO : CHECK IF FILE EXIST - AND REQUEST USER TO DELETE BEFORE REGENERATING - SUGGESTING TO SAVE PREVIOUS UNDER NEW NAME
      if (file.exists(report.name)) file.remove(report.name)
      
      ## TO DO : put in configuration file name of report, author, organisation & location
      ## TO DO : put in configuration wethere report should be portrait or landscape
      cat("---", file = report.name , sep = "\n", append = TRUE)
      cat(paste("title: \"Atlas Report: ",reportsname , "- Draft not for distribution. \"", sep = ""), file = report.name , sep = "\n", append = TRUE)
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
        cat("\n\n", file = report.name , sep = "\n", append = TRUE)
        
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
        cat("\n\n", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/unhcr-bootstrap.css\">", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/style.css\">", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/unhcr-header.css\">", file = report.name , sep = "\n", append = TRUE)
        cat("\n\n", file = report.name , sep = "\n", append = TRUE)
        
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
        cat("\n\n", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/unhcr-bootstrap.css\">", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/style.css\">", file = report.name , sep = "\n", append = TRUE)
        cat("<link rel=\"stylesheet\" href=\"css/unhcr-header.css\">", file = report.name , sep = "\n", append = TRUE)
        cat("\n\n", file = report.name , sep = "\n", append = TRUE)
        
      } else if (output == "pptx") {
        
        cat("always_allow_html: yes", file = report.name , sep = "\n", append = TRUE)
        cat("output:",file = report.name , sep = "\n", append = TRUE)
        cat("  powerpoint_presentation:", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_caption: yes", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_height: 9", file = report.name , sep = "\n", append = TRUE)
        cat("    fig_width: 18", file = report.name , sep = "\n", append = TRUE)
        cat("    reference_doc: templateUNHCR.pptx", file = report.name , sep = "\n", append = TRUE)
        cat("    slide_level: 2", file = report.name , sep = "\n", append = TRUE)
        cat("---", file = report.name , sep = "\n", append = TRUE)
        cat("\n\n", file = report.name , sep = "\n", append = TRUE)
      }
      
      
      
      
      ## First chunk to get the data in the report
      
      cat("```{r setup, include = FALSE, echo = FALSE, warning = FALSE, message = FALSE}", file = report.name , sep = "\n", append = TRUE)
      
      if (output == "pptx") {
        cat("knitr::opts_chunk$set(echo = FALSE, fig.height = 9, fig.width = 18, dpi = 300, comment = \"\"  )", file = report.name , sep = "\n", append = TRUE)
      }
      
      cat("mainDir <- getwd()", file = report.name , sep = "\n", append = TRUE)
      cat("mainDirroot <- substring(mainDir, 0 , nchar(mainDir) - 5)", file = report.name , sep = "\n", append = TRUE)
      
      cat("using <- function(...) {", file = report.name , sep = "\n", append = TRUE)
      cat("libs <- unlist(list(...))", file = report.name , sep = "\n", append = TRUE)
      cat("req <- unlist(lapply(libs,require,character.only = TRUE))", file = report.name , sep = "\n", append = TRUE)
      cat("      need <- libs[req == FALSE]", file = report.name , sep = "\n", append = TRUE)
      cat("        if (length(need) > 0) { ", file = report.name , sep = "\n", append = TRUE)
      cat("        install.packages(need, repos = 'http://cran.us.r-project.org')", file = report.name , sep = "\n", append = TRUE)
      cat("         lapply(need,require,character.only = TRUE)", file = report.name , sep = "\n", append = TRUE)
      cat("    }", file = report.name , sep = "\n", append = TRUE)
      cat("  }", file = report.name , sep = "\n", append = TRUE)
      cat("\n\n", file = report.name , sep = "\n", append = TRUE)
      
      cat("## Load all required packages", file = report.name , sep = "\n", append = TRUE)
      cat("using('tidyverse', 'ggthemes', 'plyr', 'ggrepel', 'viridis', 'RColorBrewer', 'extrafont', 'corrplot', 'corrgram', 'lsr', 'reshape2',", file = report.name , sep = "\n", append = TRUE)
      cat("  'viridis', 'OpenStreetMap', 'sp', 'ggmap', 'rgdal',    'scales', 'survey', 'knitr', 'rmarkdown', 'ggpubr', 'grid', 'jtools', 'moments', 'koboloadeR')", file = report.name , sep = "\n", append = TRUE)
      cat("options(scipen = 999) # turn-off scientific notation like 1e+48", file = report.name , sep = "\n", append = TRUE)
   
      
      cat("## Provide below the name of the form in xsl form - format should be xls not xlsx", file = report.name , sep = "\n", append = TRUE)
      cat(paste0("form <- \"",form,"\""), file = report.name , sep = "\n", append = TRUE)
      cat("dico <- utils::read.csv(paste0(mainDirroot,\"/data/dico_\",form,\".csv\"), encoding = \"UTF-8\", na.strings = \"\")", file = report.name , sep = "\n", append = TRUE)
      
      
      ## TO DO: Use config file to load the different frame
      
      
      cat("MainDataFrameMap <- utils::read.csv(paste0(mainDirroot,\"/data/MainDataFrame_edited_map.csv\"), encoding = \"UTF-8\", na.strings = \"\")", file = report.name , sep = "\n", append = TRUE)
      cat("MainDataFrame <- utils::read.csv(paste0(mainDirroot,\"/data/MainDataFrame_encoded.csv\"), encoding = \"UTF-8\", na.strings = \"\")", file = report.name , sep = "\n", append = TRUE)
      cat("\n", file = report.name , sep = "\n", append = TRUE)
      cat("## label Variables", file = report.name , sep = "\n", append = TRUE)
      cat("MainDataFrameMap <- kobo_label(MainDataFrameMap , dico)", file = report.name , sep = "\n", append = TRUE)
      

      
      cat("## getting correct district and gov from coordinates", file = report.name  , sep = "\n", append = TRUE)
      cat(paste0("geofile <- rgdal::readOGR(paste0(mainDirroot,\"/data/",configInfo[configInfo$name == "geofile", c("value")],"\"))" ), file = report.name  , sep = "\n", append = TRUE)
      cat(paste0("geofile$id <- geofile$",configInfo[configInfo$name == "geofileid", c("value")] ), file = report.name  , sep = "\n", append = TRUE) 
     
      cat("\n", file = report.name  , sep = "\n", append = TRUE)
      cat("## Fortify", file = report.name  , sep = "\n", append = TRUE)
      cat(paste0("geofile.fort <- fortify(geofile, region = \"id\")"), file = report.name  , sep = "\n", append = TRUE)
      cat("\n", file = report.name  , sep = "\n", append = TRUE)
 
      cat(paste0("MainDataFrameMap$id <- MainDataFrameMap$aggregVar1" ), file = report.name  , sep = "\n", append = TRUE) 
      cat("MainDataFrameMap <- MainDataFrameMap[!(is.na(MainDataFrameMap$id)), ]", file = report.name  , sep = "\n", append = TRUE)
      cat("geofile.map.fort <- dplyr::left_join( x = MainDataFrameMap, y = geofile.fort, by = \"id\")", file = report.name  , sep = "\n", append = TRUE)
      cat("\n", file = report.name  , sep = "\n", append = TRUE)
      cat("## get extend", file = report.name  , sep = "\n", append = TRUE)
      

      cat("geofile2 <- geofile[ geofile$id %in% MainDataFrameMap$id, ]", file = report.name  , sep = "\n", append = TRUE)
      cat("bbox <- geofile2 %>%  sf::st_bbox()", file = report.name  , sep = "\n", append = TRUE)


      
      #cat("xmin <- as.data.frame(geofile@bbox)[1,1]", file = report.name  , sep = "\n", append = TRUE)
      #cat("xmax <- as.data.frame(geofile@bbox)[1,2]", file = report.name  , sep = "\n", append = TRUE)
      #cat("ymin <- as.data.frame(geofile@bbox)[2,1]", file = report.name  , sep = "\n", append = TRUE)
      #cat("ymax <- as.data.frame(geofile@bbox)[2,2]", file = report.name  , sep = "\n", append = TRUE)
      #cat("\n", file = report.name  , sep = "\n", append = TRUE)
      #cat("## Extend the extend", file = report.name  , sep = "\n", append = TRUE)
      #cat("xmin <- xmin - ((xmax - xmin)/10)", file = report.name  , sep = "\n", append = TRUE)
      #cat("xmax <- xmax + ((xmax - xmin)/10)", file = report.name  , sep = "\n", append = TRUE)
      #cat("ymin <- ymin - ((ymax - ymin)/10)", file = report.name  , sep = "\n", append = TRUE)
      #cat("ymax <- ymax + ((ymax - ymin)/10)", file = report.name  , sep = "\n", append = TRUE)
      cat("\n", file = report.name  , sep = "\n", append = TRUE)
      cat("\n", file = report.name  , sep = "\n", append = TRUE)
      cat("## Map background", file = report.name  , sep = "\n", append = TRUE)
      
      cat(" map <- get_map(c(left = as.numeric(bbox$xmin -  ((bbox$xmax - bbox$xmin)/10)), ", file = report.name  , sep = "\n", append = TRUE)
      cat("                       bottom = as.numeric(bbox$ymin -  ((bbox$ymax - bbox$ymin)/10)), ", file = report.name  , sep = "\n", append = TRUE)
      cat("                       right = as.numeric(bbox$xmax +  ((bbox$xmax - bbox$xmin)/10)), ", file = report.name  , sep = "\n", append = TRUE)
      cat("                       top = as.numeric(bbox$ymax +   ((bbox$ymax - bbox$ymin)/10))), ", file = report.name  , sep = "\n", append = TRUE)
      cat("                     maptype = \"osm\")", file = report.name  , sep = "\n", append = TRUE)
      
      #cat("map <- OpenStreetMap::openmap(c(lat = ymax, lon = xmin ), c(lat = ymin, lon = xmax), type = \"osm\")", file = report.name  , sep = "\n", append = TRUE)
      #cat("mapLatLon <- OpenStreetMap::openproj(map, projection = \"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs\")", file = report.name  , sep = "\n", append = TRUE)
      
      
      cat("\n", file = report.name  , sep = "\n", append = TRUE)
      cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)
      
      if (output == "pptx") {
        
        if (lang == "eng") { 
          
          ### To DO : Offer option to insert in the report skeleton interpretation questions
          ### Intro text####################################################################
          cat(paste("# Crunching step\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("\n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("This data crunching report allows to quickly explore the results of the survey that can be regenerated as needed.\n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("The objective of this report is to allow to quickly identify potential patterns in your dataset.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("A quick screening of this initial report should allow to select the most meaningful graphs.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("The crunching process produces a lot of visuals. Therefore, it is key to carefully select the most relevant visual that will be presented for potential interpretation in the next step. A typical data interpretation session shall not last more than 2hours and include more than 60 visuals to look at in order to keep participants with a good focus level.\n "),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("## Selecting contents  "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("In order to guide this selection phase, the data crunching expert and report designer, in collaboration with the data analysis group, can use the following elements:\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  For numeric value, check the frequency distributions of each variable to average, deviation, including outliers and oddities\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  For categorical variables, check for unexpected values: any weird results based on common sense expectations\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Use correlation analysis to check for potential contradictions in respondent's answers to different questions for identified associations (chi-square)\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Always, Check for missing data (NA) or \"%of respondent who answered\" that you cannot confidently explain\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Check unanswered questions, that corresponds to unused skip logic in the questionnaire: For instance, did a person who was never displaced answer displacement-related questions? Were employment-related answers provided for a toddler?\n "),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("## Interpretation sessions  "),file = report.name , sep = "\n", append = TRUE)
          cat(paste(" When analyzing those representations in a collective setting during data interpretation sessions, you may:  \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Reflect__: question data quality and/or make suggestions to adjust questions, identify additional cleaning steps;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Interpret__: develop qualitative interpretations of data patterns;     \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Recommend__: suggest recommendations in terms of programmatic adjustment;    \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Classify__: define level of sensitivity for certain topics if required;     \n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("## The report can be regenerated as needed by:  "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  adjusting the report configuration in the xlsform to break it into report and chapter;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  configuring disaggregation & correlation for each question;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  revising the data cleansing based on the cleaning log;   \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  appending calculated indicators to your data frame to reshape variable - also called feature engineering. \n\n"),file = report.name , sep = "\n", append = TRUE)
          
          
          
          
          cat(paste("## Dataset description\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Title of the study:__ ",configInfo[configInfo$name == "titl", c("value")]," \n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Abstract:__ ",configInfo[configInfo$name == "abstract", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
          # cat(paste("__Rights & Disclaimer:__ ",configInfo[configInfo$name == "disclaimer", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Country where the study took place:__ ",configInfo[configInfo$name == "Country", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Geographic Coverage for the study within the country:__ ",configInfo[configInfo$name == "geogCover", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Kind of Data:__ ",configInfo[configInfo$name == "dataKind", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Number of records in the main data frame__: `r nrow(MainDataFrame)`\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Period of data collection__: between `r min(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))` and `r max(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))`\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Documented cleaning__: ",configInfo[configInfo$name == "cleanOps", c("value")],"\n\n"),file = report.name , sep = "\n\n", append = TRUE)
          #cat(paste("__Entity being analyzed in the study:__ ",configInfo[configInfo$name == "AnalysisUnit", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          #cat(paste("__Procedure, technique, or mode of inquiry used to attain the data:__ ",configInfo[configInfo$name == "ModeOfCollection", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          #cat(paste("__Study Universe:__  (i.e. group of persons or other elements that are the object of research and to which any analytic results refer:",configInfo[configInfo$name == "universe", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          
        } else if (lang == "esp") {
          
          cat(paste("# Etapa de Crunching\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("\n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("El data crunching es el paso donde se procesan y estructuran los datos de la encuesta, y se generan visuales de los resultados de una manera automatizada y rápida.\n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste(" Este informe de análisis y crunching de datos permite explorar rápidamente los resultados de la encuesta. El informe puede ser regenerado según sea necesario. El objetivo de este informe es poder identificar rápidamente posibles tendencias y patrones  en el conjunto de datos.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Un repaso rápido de este informe inicial deberá permitir la selección de  los gráficos más significativos.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("El proceso de crunching produce una gran cantidad de imágenes. Por lo tanto, es importante seleccionar cuidadosamente los visuales y gráficos más relevantes que podrán ser presentados para su interpretación en la siguiente etapa. Usualmente una sesión de interpretación de datos no debe durar más de 2 horas y no debe incluir más de 60 visuales a examinar con el fin de mantener a los participantes con un buen nivel de enfoque. \n "),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("## Selección  "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("El experto en análisis de datos (DIMA del Bureau / IMs de la Operación) y el diseñador de reportes (de la operación, e.g. Oficial de reportes, PI), en colaboración con el grupo de análisis de datos, pueden utilizar los siguientes elementos para guiar esta fase de selección:\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  •	Para el valor numérico, compruebe las distribuciones de frecuencia de cada variable a la media, la desviación, incluidos los valores atípicos y las rarezas\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Para variables categóricas, compruebe si hay valores inesperados: cualquier resultado extraño basado en las expectativas de sentido común\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Utilice el análisis de correlación para comprobar posibles contradicciones en las respuestas de los encuestados a diferentes preguntas para asociaciones identificadas (chi-cuadrado)\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Siempre, Compruebe si faltan datos (NA) o \"%del encuestado que respondió\" que no puede explicar con confianza\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Compruebe las preguntas sin respuesta, que corresponde a la lógica de omisión no utilizada en el cuestionario: Por ejemplo, ¿respondió una persona que nunca fue desplazada a las preguntas relacionadas con el desplazamiento? ¿Se proporcionaron respuestas relacionadas con el empleo para un niño pequeño?\n "),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("## Interpretatcion sessions  "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Al analizar los visuales y gráficos en conjunto durante las sesiones de interpretación de datos, se puede:  \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Reflejar__: cuestionar la calidad de los datos y / o hacer sugerencias para ajustar las preguntas, identificar pasos de limpieza adicionales;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Interpretar__: desarrollar interpretaciones cualitativas de los patrones en los datos;     \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Recomendar__: sugerir recomendaciones en términos de ajuste programático;    \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Clasificar__: se define el nivel de sensibilidad para ciertos temas si es necesario;     \n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("## Este informe se puede regenerar según sea necesario:    "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  ajustando la configuración del informe en el  xlsform  para dividirlo en informe y capítulo;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  configurar la desagregación y correlación para cada pregunta;;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  revisar la limpieza de datos basada en el registro de limpieza;   \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  anexar indicadores calculados a su trama de datos para cambiar la forma de la variable - también llamada ingeniería de características. \n\n"),file = report.name , sep = "\n", append = TRUE)
          
          
          cat(paste("## Descripción del conjunto de datos\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Título del estudio:__ ",configInfo[configInfo$name == "titl", c("value")]," \n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Resumen:__ ",configInfo[configInfo$name == "abstract", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
          # cat(paste("__Derechos y Descargo de Responsabilidad::__ ",configInfo[configInfo$name == "disclaimer", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__País donde tuvo lugar el estudio:__ ",configInfo[configInfo$name == "Country", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Cobertura geográfica para el estudio dentro del país:__ ",configInfo[configInfo$name == "geogCover", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Tipo de datos:__ ",configInfo[configInfo$name == "dataKind", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Número de registros en el marco de datos principal__: `r nrow(MainDataFrame)`\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Período de recopilación de datos__: between `r min(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))` and `r max(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))`\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Documented cleaning__: ",configInfo[configInfo$name == "cleanOps", c("value")],"\n\n"),file = report.name , sep = "\n\n", append = TRUE)
          #cat(paste("__Entity being analyzed in the study:__ ",configInfo[configInfo$name == "AnalysisUnit", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          #cat(paste("__Procedure, technique, or mode of inquiry used to attain the data:__ ",configInfo[configInfo$name == "ModeOfCollection", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          #cat(paste("__Study Universe:__  (i.e. group of persons or other elements that are the object of research and to which any analytic results refer:",configInfo[configInfo$name == "universe", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          
        } else if (lang == "fre") {
          
          cat(paste("# Etape de Crunching\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Ce rapport d'analyse de données permet d'explorer rapidement les résultats de l'enquête qui peuvent être régénérés au besoin.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("L'objectif de ce rapport est de permettre d'identifier rapidement les potentielles  dans votre jeu de données.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Un examen rapide de ce rapport initial devrait permettre de sélectionner les graphiques les plus significatifs.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Le processus de crunching produit beaucoup de visuels. Par conséquent, il est essentiel de sélectionner soigneusement les visuels les plus pertinent qui seront par la suite présenté pour une interprétation potentielle à l'étape suivante. Une session typique d'interprétation des données ne doit pas durer plus de 2 heures et comprendre plus de 60 visuels à regarder afin de garder les participants avec un bon niveau de concentration.\n "),file = report.name , sep = "\n", append = TRUE)
          
          
          cat(paste("## Sélection de contenu  "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Afin de guider cette phase de sélection, l'expert en data crunching et le concepteur de rapports, en collaboration avec le groupe d'analyse des données, peuvent utiliser les éléments suivants:\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Pour la valeur numérique, vérifiez les distributions de fréquence de chaque variable en moyenne, écart, y compris les valeurs aberrantes et les bizarreries\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Pour les variables catégorielles, vérifiez les valeurs inattendues: tout résultat étrange basé sur des attentes de bon sens\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Utiliser l'analyse de corrélation pour vérifier les contradictions potentielles dans les réponses des répondants aux différentes questions pour les associations identifiées (chi carré)\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Toujours, vérifiez les données manquantes (NA) ou \"% du répondant qui a répondu\" que vous ne pouvez pas expliquer en toute confiance\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Vérifiez les questions sans réponse, qui correspondent à la logique de saut inutilisée dans le questionnaire: par exemple, une personne qui n'a jamais été déplacée a-t-elle répondu aux questions liées au déplacement? Des réponses liées à l'emploi ont-elles été fournies à un tout-petit?\n "),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("## Session d'interpretation  "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Lors de l'analyse de ces représentations dans un cadre collectif lors de sessions d'interprétation des données, vous pouvez:  \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Reflexion__: remettre en question la qualité des données et / ou faire des suggestions pour ajuster les questions, identifier des étapes de nettoyage supplémentaires;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Interpretation__: développer des interprétations qualitatives des modèles de données;     \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Recommendation__: proposer des recommandations en termes d'ajustement programmatique;    \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Classification__: définir le niveau de sensibilité pour certains sujets si nécessaire;     \n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("## Le rapport peut être régénéré au besoin en:  "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  ajustant la configuration du rapport dans le xlsform pour le diviser en rapport et chapitre;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  configurant la désagrégation et la corrélation pour chaque question;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  révisant le nettoyage des données en fonction du journal de nettoyage;   \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  ajoutant des indicateurs calculés à votre bloc de données pour remodeler la variable - également appelée ingénierie des fonctionnalités. \n\n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("## Description du jeu de données\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Titre de l'étude:__ ",configInfo[configInfo$name == "titl", c("value")]," \n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Résumé:__ ",configInfo[configInfo$name == "abstract", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
          #cat(paste("__Droits et avis de non-responsabilité:__ ",configInfo[configInfo$name == "disclaimer", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Pays où l'étude a eu lieu:__ ",configInfo[configInfo$name == "Country", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Couverture géographique de l'étude dans le pays:__ ",configInfo[configInfo$name == "geogCover", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Type de données:__ ",configInfo[configInfo$name == "dataKind", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Nombre d'enregistrements dans le bloc de données principal__: `r nrow(MainDataFrame)`\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Période de collecte des données__: entre le `r min(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))` et le `r max(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))`\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Nettoyage documenté__: ",configInfo[configInfo$name == "cleanOps", c("value")],"\n\n"),file = report.name , sep = "\n\n", append = TRUE)
          #cat(paste("__Entité analysée dans l'étude:__ ",configInfo[configInfo$name == "AnalysisUnit", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          # cat(paste("__Procédure, technique ou mode d'enquête utilisé pour obtenir les données:__ ",configInfo[configInfo$name == "ModeOfCollection", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          #cat(paste("__Univers d'étude:__  (c'est-à-dire un groupe de personnes ou d'autres éléments qui font l'objet d'une recherche et auxquels se réfèrent les résultats analytiques:",configInfo[configInfo$name == "universe", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)  
          
          
        }
        
      } else  {
        
        
        if (lang == "eng") { 
          cat(paste("# Crunching step\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("This data crunching report allows to quickly explore the results of the survey that can be regenerated as needed.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("The objective of this report is to allow to quickly identify potential patterns in your dataset.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("A quick screening of this initial report should allow to select the most meaningful graphs.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("The crunching process produces a lot of visuals. Therefore it is key to carefully select the most relevant visual that will be presented for potential interpretation in the next step. A typical data interpretation session shall not last more than 2hours and include more than 60 visuals to look at in order to keep participants with a good focus level.\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("In order to guide this selection phase, the data crunching expert and report designer, in collaboration with the data analysis group, can use the following elements:\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  For numeric value, check the frequency distributions of each variable to average, deviation, including outliers and oddities\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  For categorical variables, check for unexpected values: any weird results based on common sense expectations\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Use correlation analysis to check for potential contradictions in respondents answers to different questions for identified associations (chi-square)\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Always, Check for missing data (NA) or \"%of respondent who answered\" that you cannot confidently explain\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Check unanswered questions, that corresponds to unused skip logic in the questionnaire: For instance, did a person who was never displaced answer displacement-related questions? Were employment-related answers provided for a toddler?\n "),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("When analyzing those representations in a collective setting during data interpretation sessions, you may:  \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Reflect__: question data quality and/or make suggestions to adjust questions, identify additional cleaning steps;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Interpret__: develop qualitative interpretations of data patterns;     \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Recommend__: suggest recommendations in terms of programmatic adjustment;    \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Classify__: define level of sensitivity for certain topics if required;     \n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("The report can be regenerated as needed by:  "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  adjusting the report configuration in the xlsform to break it into report and chapter;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  configuring disaggregation & correlation for each question;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  revising the data cleansing based on the cleaning log;   \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  appending calculated indicators to your data frame to reshape variable - also called feature engineering. \n\n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("# Dataset description\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Title of the study:__ ",configInfo[configInfo$name == "titl", c("value")]," \n\n"),file = report.name , sep = "\n", append = TRUE)
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
          
        } else if (lang == "esp") {
          cat(paste("# Etapa de Crunching\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("\n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("El data crunching es el paso donde se procesan y estructuran los datos de la encuesta, y se generan visuales de los resultados de una manera automatizada y rápida.\n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste(" Este informe de análisis y crunching de datos permite explorar rápidamente los resultados de la encuesta. El informe puede ser regenerado según sea necesario. El objetivo de este informe es poder identificar rápidamente posibles tendencias y patrones  en el conjunto de datos.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Un repaso rápido de este informe inicial deberá permitir la selección de  los gráficos más significativos.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("El proceso de crunching produce una gran cantidad de imágenes. Por lo tanto, es importante seleccionar cuidadosamente los visuales y gráficos más relevantes que podrán ser presentados para su interpretación en la siguiente etapa. Usualmente una sesión de interpretación de datos no debe durar más de 2 horas y no debe incluir más de 60 visuales a examinar con el fin de mantener a los participantes con un buen nivel de enfoque. \n "),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("El experto en análisis de datos (DIMA del Bureau / IMs de la Operación) y el diseñador de reportes (de la operación, e.g. Oficial de reportes, PI), en colaboración con el grupo de análisis de datos, pueden utilizar los siguientes elementos para guiar esta fase de selección:\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  •	Para el valor numérico, compruebe las distribuciones de frecuencia de cada variable a la media, la desviación, incluidos los valores atípicos y las rarezas\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Para variables categóricas, compruebe si hay valores inesperados: cualquier resultado extraño basado en las expectativas de sentido común\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Utilice el análisis de correlación para comprobar posibles contradicciones en las respuestas de los encuestados a diferentes preguntas para asociaciones identificadas (chi-cuadrado)\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Siempre, Compruebe si faltan datos (NA) o \"%del encuestado que respondió\" que no puede explicar con confianza\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Compruebe las preguntas sin respuesta, que corresponde a la lógica de omisión no utilizada en el cuestionario: Por ejemplo, ¿respondió una persona que nunca fue desplazada a las preguntas relacionadas con el desplazamiento? ¿Se proporcionaron respuestas relacionadas con el empleo para un niño pequeño?\n "),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("Al analizar los visuales y gráficos en conjunto durante las sesiones de interpretación de datos, se puede:  \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Reflejar__: cuestionar la calidad de los datos y / o hacer sugerencias para ajustar las preguntas, identificar pasos de limpieza adicionales;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Interpretar__: desarrollar interpretaciones cualitativas de los patrones en los datos;     \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Recomendar__: sugerir recomendaciones en términos de ajuste programático;    \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Clasificar__: se define el nivel de sensibilidad para ciertos temas si es necesario;     \n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("Este informe se puede regenerar según sea necesario:    "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  ajustando la configuración del informe en el  xlsform  para dividirlo en informe y capítulo;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  configurar la desagregación y correlación para cada pregunta;;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  revisar la limpieza de datos basada en el registro de limpieza;   \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  anexar indicadores calculados a su trama de datos para cambiar la forma de la variable - también llamada ingeniería de características. \n\n"),file = report.name , sep = "\n", append = TRUE)
          
          
          
          cat(paste("## Descripción del conjunto de datos\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Título del estudio:__ ",configInfo[configInfo$name == "titl", c("value")]," \n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Resumen:__ ",configInfo[configInfo$name == "abstract", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Derechos y Descargo de Responsabilidad::__ ",configInfo[configInfo$name == "disclaimer", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__País donde tuvo lugar el estudio:__ ",configInfo[configInfo$name == "Country", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Cobertura geográfica para el estudio dentro del país:__ ",configInfo[configInfo$name == "geogCover", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Tipo de datos:__ ",configInfo[configInfo$name == "dataKind", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Número de registros en el marco de datos principal__: `r nrow(MainDataFrame)`\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Período de recopilación de datos__: entre `r min(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))` y `r max(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))`\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Limpieza documentada__: ",configInfo[configInfo$name == "cleanOps", c("value")],"\n\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Entidad analizada en el estudio:__ ",configInfo[configInfo$name == "AnalysisUnit", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Procedimiento, técnica o modo de investigación utilizado para lograr los datos:__ ",configInfo[configInfo$name == "ModeOfCollection", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Universo de estudio__ : (es decir, grupo de personas u otros elementos que son objeto de investigación y a los que se refieren los resultados analíticos: ",configInfo[configInfo$name == "universe", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          
        } else if (lang == "fre") {
          cat(paste("# Etape de Crunching\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Ce rapport d'analyse de données permet d'explorer rapidement les résultats de l'enquête qui peuvent être régénérés au besoin.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("L'objectif de ce rapport est de permettre d'identifier rapidement les potentielles  dans votre jeu de données.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Un examen rapide de ce rapport initial devrait permettre de sélectionner les graphiques les plus significatifs.\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Le processus de crunching produit beaucoup de visuels. Par conséquent, il est essentiel de sélectionner soigneusement les visuels les plus pertinent qui seront par la suite présenté pour une interprétation potentielle à l'étape suivante. Une session typique d'interprétation des données ne doit pas durer plus de 2 heures et comprendre plus de 60 visuels à regarder afin de garder les participants avec un bon niveau de concentration.\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Afin de guider cette phase de sélection, l'expert en data crunching et le concepteur de rapports, en collaboration avec le groupe d'analyse des données, peuvent utiliser les éléments suivants:\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Pour la valeur numérique, vérifiez les distributions de fréquence de chaque variable en moyenne, écart, y compris les valeurs aberrantes et les bizarreries\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Pour les variables catégorielles, vérifiez les valeurs inattendues: tout résultat étrange basé sur des attentes de bon sens\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Utiliser l'analyse de corrélation pour vérifier les contradictions potentielles dans les réponses des répondants aux différentes questions pour les associations identifiées (chi carré)\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  Toujours, vérifiez les données manquantes (NA) ou \"% du répondant qui a répondu\" que vous ne pouvez pas expliquer en toute confiance\n "),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("  *  Vérifiez les questions sans réponse, qui correspondent à la logique de saut inutilisée dans le questionnaire: par exemple, une personne qui n'a jamais été déplacée a-t-elle répondu aux questions liées au déplacement? Des réponses liées à l'emploi ont-elles été fournies à un tout-petit?\n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Lors de l'analyse de ces représentations dans un cadre collectif lors de sessions d'interprétation des données, vous pouvez:  \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Reflexion__: remettre en question la qualité des données et / ou faire des suggestions pour ajuster les questions, identifier des étapes de nettoyage supplémentaires;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Interpretation__: développer des interprétations qualitatives des modèles de données;     \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Recommendation__: proposer des recommandations en termes d'ajustement programmatique;    \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  __Classification__: définir le niveau de sensibilité pour certains sujets si nécessaire;     \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("Le rapport peut être régénéré au besoin en:  "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  ajustant la configuration du rapport dans le xlsform pour le diviser en rapport et chapitre;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  configurant la désagrégation et la corrélation pour chaque question;   \n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  révisant le nettoyage des données en fonction du journal de nettoyage;   \n "),file = report.name , sep = "\n", append = TRUE)
          cat(paste("  *  ajoutant des indicateurs calculés à votre bloc de données pour remodeler la variable - également appelée ingénierie des fonctionnalités. \n\n"),file = report.name , sep = "\n", append = TRUE)
          
          cat(paste("# Description du jeu de données\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Titre de l'étude:__ ",configInfo[configInfo$name == "titl", c("value")]," \n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Résumé:__ ",configInfo[configInfo$name == "abstract", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Droits et avis de non-responsabilité:__ ",configInfo[configInfo$name == "disclaimer", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Pays où l'étude a eu lieu:__ ",configInfo[configInfo$name == "Country", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Couverture géographique de l'étude dans le pays:__ ",configInfo[configInfo$name == "geogCover", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Type de données:__ ",configInfo[configInfo$name == "dataKind", c("value")],"\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Nombre d'enregistrements dans le bloc de données principal__: `r nrow(MainDataFrame)`\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Période de collecte des données__: entre le `r min(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))` et le `r max(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\"))`\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Nettoyage documenté__: ",configInfo[configInfo$name == "cleanOps", c("value")],"\n\n"),file = report.name , sep = "\n\n", append = TRUE)
          cat(paste("__Entité analysée dans l'étude:__ ",configInfo[configInfo$name == "AnalysisUnit", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Procédure, technique ou mode d'enquête utilisé pour obtenir les données:__ ",configInfo[configInfo$name == "ModeOfCollection", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("__Univers d'étude:__  (c'est-à-dire un groupe de personnes ou d'autres éléments qui font l'objet d'une recherche et auxquels se réfèrent les résultats analytiques:",configInfo[configInfo$name == "universe", c("value")],"\n\n"),file = report.name , sep = "\n", append = TRUE)  
          
          
        }
        
      }
      
      
      ## get list of chapters
      #  
      #  !(is.na(dico$mappoly)) &
      chapters <- as.data.frame(unique(dico[ , c("chapter","report", "mappoly")]))
      names(chapters)[2] <- "Report"
      names(chapters)[1] <- "Chapter"
      names(chapters)[3] <- "mappoly"
      
      chapters <- chapters[ !(is.na(chapters$mappoly)), ]
      
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
        # v <- 1
        chaptersname <- as.character(chapters[ v , 1])
        
        
        
        ## Getting chapter questions ####################################################################################################
        #chapterquestions <- dico[which(dico$chapter== chaptersname ), c("chapter", "name", "label", "type", "qrepeatlabel", "fullname","listname") ]
        chapterquestions <- dico[which(dico$chapter == chaptersname &
                                         !(is.na(dico$mappoly)) &
                                         dico$type %in% c("select_one_d","integer","select_multiple" , "numeric")),
                                 c("chapter", "name", "label", "labelReport","hintReport", "type", "qrepeatlabel", "fullname","listname","variable", "mappoly") ]
        # levels(as.factor(as.character(dico[which(!(is.na(dico$chapter)) & dico$formpart=="questions"), c("type") ])))
        
        ## Ensure selected variable are in the aggregated frame
        check <- names(MainDataFrameMap)
        chapterquestions <- chapterquestions[chapterquestions$fullname %in% check, ]
        
        ## add better slides separator
        if (output == "pptx") {
          cat(paste("---"),file = report.name , sep = "\n", append = TRUE)
          cat(paste("# ", chaptersname),file = report.name , sep = "\n", append = TRUE)
          # cat(paste0("Linked questions: ", as.character(chapterquestions$labelReport) ),file = report.name , sep = "\n", append = TRUE)
          cat(paste("---"),file = report.name , sep = "\n", append = TRUE)
        } else  {
          cat(paste("# ", chaptersname),file = report.name , sep = "\n", append = TRUE)
          # cat(paste("Linked questions: ", as.character(chapterquestions$labelReport) ),file = report.name , sep = "\n", append = TRUE)
        }
        if (app == "shiny") {
          progress$set(message = "Compilation of questions results in progress...")
          updateProgress()
        }
        
        ## Loop.questions 
        if (app == "shiny") {
          progress$set(message = "Getting levels for each questions in progress...")
          updateProgress()
        }
        
        if( nrow(chapterquestions) > 0 ) {
          for (j in 1:nrow(chapterquestions))
          {
            # j <- 1
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
            
            
            ## write question name 
            cat("\n ",file = report.name , sep = "\n", append = TRUE)
            cat(paste("## ", questions.label ,sep = ""),file = report.name , sep = "\n", append = TRUE)
            
            
            ## Now create para based on question type
            
            
            cat(paste(if (is.na(questions.hint)){paste0("")} else {paste0("__Interpretation Hint__: ", questions.hint)},"\n\n",sep = ""),file = report.name ,sep = "\n", append = TRUE)
            
               
            
            ## Open chunk
            cat(paste0("```{r ", questions.name, ".tab, echo=FALSE, warning=FALSE, cache=FALSE, tidy = TRUE, message=FALSE, comment = \"\", fig.height=10, size=\"small\"}\n", sep = '\n'), file = report.name, append = TRUE)
           cat("plot1 <- ggmap(map) +", file = report.name  , sep = "\n", append = TRUE)          
            #cat("autoplot(mapLatLon) +", file = report.name  , sep = "\n", append = TRUE)
            cat("  geom_polygon(data = geofile.map.fort,", file = report.name  , sep = "\n", append = TRUE)
            
            if (questions.type == "integer") {
              cat( paste0("               aes(x = long, y = lat, fill = ",questions.name,", group = group),"), file = report.name  , sep = "\n", append = TRUE) } else {
                cat( paste0("               aes(x = long, y = lat, fill = ",questions.name," *100, group = group),"), file = report.name  , sep = "\n", append = TRUE)}
            
            cat("               colour = \"white\", alpha = 0.7 ) +", file = report.name  , sep = "\n", append = TRUE)
            cat("  coord_equal() +", file = report.name  , sep = "\n", append = TRUE)
            cat("  theme_map() +", file = report.name  , sep = "\n", append = TRUE)
            # cat("  scale_fill_gradient(low = \"#ffffcc\", high = \"#ff4444\",", file = report.name  , sep = "\n", append = TRUE)
            cat("  scale_fill_viridis(", file = report.name  , sep = "\n", append = TRUE)
            
            if (questions.type == "integer") {
              cat("                      name = \"Average value\",", file = report.name  , sep = "\n", append = TRUE) } else {
                cat("                      name = \"Percentage for that modality\",", file = report.name  , sep = "\n", append = TRUE)}
            
            
            cat("                      guide = guide_legend( direction = \"horizontal\", label.position = \"bottom\",", file = report.name  , sep = "\n", append = TRUE)
            cat("                                            keyheight = unit(2, units = \"mm\"),  keywidth = unit(length(labels)*10, units = \"mm\"),", file = report.name  , sep = "\n", append = TRUE)
            cat("                                            title.position = 'top',  title.hjust = 0.5, label.hjust = 1, nrow = 1, byrow = T, reverse = T )) +", file = report.name  , sep = "\n", append = TRUE)
            cat( paste0("  labs(title = \"",questions.label ,"\" ,  x = NULL, y = NULL,"),file = report.name ,sep = "\n", append = TRUE)
           
            ## subtitle = \"\", 
            if (questions.type == "integer") {
              cat(paste0("subtitle = \"Aggregated numeric questions. The means value is displayed in this map.\","  ),file = report.name , sep = "\n", append = TRUE) }
            else if (questions.type == "select_one_d") {
              cat(paste0("subtitle = \"Aggregated  unique choice question. The proportion for one specific modality is displayed in this map.\"," ),file = report.name , sep = "\n", append = TRUE) }
            else if (questions.type == "select_multiple") {
              cat(paste0("subtitle = \"Aggregated multiple choice question. The proportion for one specific modality is displayed in this map.\"," ),file = report.name , sep = "\n", append = TRUE) }
            
            ## caption
            cat(paste0("caption = paste0(\"", configInfo[configInfo$name == "titl", c("value")], "- \", 
                              nrow(MainDataFrame), \" total records collected between \",
                              min(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\")), \" and \",
                              max(as.Date(MainDataFrame$today, format = \"%Y-%m-%d\")), \" \n in \", \" ",
                       configInfo[configInfo$name == "Country", c("value")]," \")) +"), file = report.name ,sep = "\n", append = TRUE) 
            
            if (output == "pptx") {
              cat(paste0("kobo_unhcr_style_map_big()"),file = report.name ,sep = "\n", append = TRUE)
              
            } else {
              cat(paste0("kobo_unhcr_style_map()"),file = report.name ,sep = "\n", append = TRUE)
            }
            cat(paste0("ggpubr::ggarrange(kobo_left_align(plot1, c(\"caption\", \"subtitle\", \"title\")), ncol = 1, nrow = 1)"),file = report.name ,sep = "\n", append = TRUE)
            
            cat(paste0("\n", sep = '\n'), file = report.name, append = TRUE)
            cat(paste0("\n```\n", sep = '\n'), file = report.name, append = TRUE)
  
          }
        }
      }
      
    }
    
    
    
    #### Last Step Rendering reports ###################
    
    
    
    if (render == "FALSE") {
      
      cat(" Rmd files are ready and available in the code folder... \n")
    } else {
      if (app == "shiny") {
        progress$set(message = "Render now all reports...")
        updateProgress()
      }
      cat(" Clean memory... \n")
      gc()
      #rm(list = ls())
      kobo_load_packages()
      mainDir <- kobo_getMainDirectory()
      reports <- utils::read.csv(paste(mainDir,"/data/mapreports.csv",sep = ""), encoding = "UTF-8", na.strings = "")
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
          rmarkdown::render(paste(mainDir,"/code/",i,"-", reportsname, "-atlas.Rmd", sep = ""))
          ## Put the report in the out folder
          mainDir <- kobo_getMainDirectory()
          file.rename(paste(mainDir,"/code/",i,"-", reportsname, "-atlas.docx", sep = ""), paste0(mainDir,"/out/crunching_reports/Crunching-",i,"-", reportsname,"-",Sys.Date(), "-atlas.docx"))
          ## Clean  memory
          gc()
          
        } else if (output == "html") {
          
          cat(paste(i, " - Render html output report for ",reportsname))
          mainDir <- kobo_getMainDirectory()
          rmarkdown::render(paste(mainDir,"/code/",i,"-", reportsname, "-atlas.Rmd", sep = ""))
          ## Put the report in the out folder
          mainDir <- kobo_getMainDirectory()
          file.rename(paste(mainDir,"/code/",i,"-", reportsname, "-atlas.html", sep = ""), paste0(mainDir,"/out/crunching_reports/Crunching-",i,"-", reportsname,"-",Sys.Date(), "-atlas.html"))
          ## Clean  memory
          gc()
          
        } else if (output == "aspx") {
          
          cat(paste(i, " - Render aspx output - for sharepoint hosting - report for ",reportsname))
          mainDir <- kobo_getMainDirectory()
          rmarkdown::render(paste(mainDir,"/code/",i,"-", reportsname, "-atlas.Rmd", sep = ""))
          ## Put the report in the out folder
          mainDir <- kobo_getMainDirectory()
          file.rename(paste(mainDir,"/code/",i,"-", reportsname, "-atlas.html", sep = ""), paste0(mainDir,"/out/crunching_reports/Crunching-",i,"-", reportsname,"-",Sys.Date(), "-atlas.aspx"))
          ## Clean  memory
          gc()
          
        } else if (output == "pptx") {
          
          cat(paste(i, " - Render PowerPoint output report for ",reportsname))
          mainDir <- kobo_getMainDirectory()
          rmarkdown::render(paste(mainDir,"/code/",i,"-", reportsname, "-atlas.Rmd", sep = ""))
          ## Put the report in the out folder
          mainDir <- kobo_getMainDirectory()
          file.rename(paste(mainDir,"/code/",i,"-", reportsname, "-atlas.pptx", sep = ""), paste0(mainDir,"/out/crunching_reports/Crunching-",i,"-", reportsname,"-",Sys.Date(), "-atlas.pptx"))
          ## Clean  memory
          gc()
        }
      }
      
      cat(" Done!! Reports are in the folder OUT")
      
    }
    if (app == "shiny") {
      updateProgress()
    }
    cat(" Review the report- Adjust your configuration files and you will be very soon ready to start the qualitative analysis and the analysis workshops...")
  }, error = function(err) {
    print("kobo_crunching_report_ERROR")
    return(structure(err, class = "try-error"))
  })
}



