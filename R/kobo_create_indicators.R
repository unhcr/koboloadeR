#' @name kobo_create_indicators
#' @rdname kobo_create_indicators
#' @title Create Indicators
#'
#' @description Function to compute indicators from indicator sheet
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#' 
#'
#' @return No return, all results will be saved inside new CSV files
#'
#' @author Edouard Legoupil, Maher Daoud
#'
#' @examples
#' kobo_create_indicators()
#'
#' @examples
#' \dontrun{
#' kobo_create_indicators("myform.xls")
#' }
#'
#' @export kobo_create_indicators
#'

kobo_create_indicators <- function(form = "form.xls") {
  tryCatch({
    mainDir <- kobo_getMainDirectory()
    form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")
    
    ## Load all required packages
    kobo_load_packages()
    library(koboloadeR)
    
    ## load all required data files #########################################
    cat("\n\nload all required data files..\n")
    dataBeginRepeat <- kobo_get_begin_repeat()
    dataBeginRepeat <- dataBeginRepeat$names
    for (dbr in dataBeginRepeat) {
      dataFrame <- read.csv(paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""),stringsAsFactors = F) 
      assign(dbr, dataFrame)
    }
    
    #### Load and test i indicators #############################################################################
    #library(readxl)
    tried <- try(read_excel(form_tmp, sheet = "indicator"),
                 silent = TRUE)
    if (inherits(tried, "try-error")) {
      writeLines("There was an error: You have not defined indicators within your xlsform file. \n")
      
    } else {
      
      rm(tried)
      indicator <- read_excel(form_tmp, sheet = "indicator")
      if(nrow(indicator)==0){
        writeLines("There was an error: You have not defined indicators within your xlsform file. \n")
        
      } else {
        ## Load data & dico #############################################################################
        #form <- "form.xls"
        ## Run this only after data cleaning
        dico <- read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")
        
        ## Create the dicotemp #############################################################################
        #names(dico)
        dicotemp <- data.frame(c("trigger"))
        names(dicotemp)[1] <- "type"
        #dicotemp$type <- "trigger"
        dicotemp$name <- "trigger"
        dicotemp$fullname <- "trigger"
        dicotemp$label <- "trigger"
        dicotemp$labelReport <- "trigger"
        dicotemp$chapter <- "trigger"
        dicotemp$disaggregation <- "trigger"
        dicotemp$correlate <- "trigger"
        dicotemp$anonymise <- "trigger"
        
        dicotemp$structuralequation <- "trigger"
        dicotemp$clean <- "trigger"
        dicotemp$cluster <- "trigger"
        dicotemp$predict <- "trigger"
        dicotemp$variable <- "trigger"
        dicotemp$mappoint <- "trigger"
        dicotemp$mappoly <- "trigger"
        
        
        dicotemp$listname <- "trigger"
        dicotemp$qrepeat <- "trigger"
        dicotemp$qrepeatlabel <- "trigger"
        dicotemp$qlevel <- "trigger"
        dicotemp$qgroup <- "trigger"
        dicotemp$labelchoice <- "trigger"
        dicotemp$variable <- "trigger"
        dicotemp$order <- "trigger"
        dicotemp$weight <- "trigger"
        dicotemp$score <- "trigger"
        dicotemp$recategorise <- "trigger"
        dicotemp$formpart <- "trigger"
        dicotemp$indic <- "feature"
        
        ####Load data analysis plan#############################################################################
        #library(readxl)
        indicator <- read_excel(form_tmp, sheet = "indicator")
        
        ## Need to check that all column are presents...
        
        
        ## Load indicator info #############################################################################
        
        for (i in 1:nrow(indicator))
          
        {
          indicator.type	<- as.character(indicator[ i, c("type")])
          indicator.fullname	<- as.character(indicator[ i, c("fullname")])
          indicator.label	<- as.character(indicator[ i, c("label")])
          indicator.report	<- as.character(indicator[ i, c("label")])
          indicator.chapter	<- as.character(indicator[ i, c("chapter")])
          indicator.disaggregation	<- as.character(indicator[ i, c("disaggregation")])
          indicator.correlate	<- as.character(indicator[ i, c("correlate")])
          indicator.sensitive	<- as.character(indicator[ i, c("sensitive")])
          indicator.anonymise	<- as.character(indicator[ i, c("anonymise")])
          indicator.frame	<- as.character(indicator[ i, c("frame")])
          indicator.listname <- as.character(indicator[ i, c("listname")])
          indicator.calculation	<- as.character(indicator[ i, c("calculation")])
          
          
          indicator.structuralequation 	<- as.character(indicator[ i, c("structuralequation")])
          indicator.cluster 	<- as.character(indicator[ i, c("cluster")])
          indicator.predict	<- as.character(indicator[ i, c("predict")])
          indicator.variable	<- as.character(indicator[ i, c("variable")])
          indicator.mappoint	<- as.character(indicator[ i, c("mappoint")])
          indicator.mappoly	<- as.character(indicator[ i, c("mappoly")])
          
          
          cat(paste0(i, "- Load  indicator: ", indicator.label," of type: ",indicator.type,"\n"))
          
          ## Build and run the formula to insert the indicator in the right frame  ###########################
          indic.formula <- paste0(indicator.frame,"$",indicator.fullname," <- ",indicator.calculation )
          if (file.exists(paste0(mainDir,"/code/temp.R") )) file.remove(paste0(mainDir,"/code/temp.R"))
          cat(paste('form <- "',form,'"',sep = ""), file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat("mainDir <- kobo_getMainDirectory()", file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat('form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat('dataBeginRepeat <- kobo_get_begin_repeat()', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat('dataBeginRepeat <- dataBeginRepeat$names', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat('household <- read.csv(paste(mainDir,"/data/household.csv",sep = ""), encoding = "UTF-8", na.strings = "NA")', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          
          cat('
          for (dbr in dataBeginRepeat) {
            dataFrame <- read.csv(paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""),stringsAsFactors = F) 
            assign(dbr, dataFrame)
          }
          ', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          
          cat(indic.formula, file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat("####", file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          
          ## do a check on indicator variable type
          indicator.type2 <- indicator.type
          ifelse(indicator.type == "select_one", indicator.type2 <- "character", indicator.type2 <- indicator.type)
          
          
          cat(paste0(indicator.frame,"$",indicator.fullname," <- as.",indicator.type2,"(",indicator.frame,"$",indicator.fullname,")"), file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat(paste0("str(",indicator.frame,"$",indicator.fullname,")"), file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat(paste0("summary(",indicator.frame,"$",indicator.fullname,")"), file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          
          if(indicator.frame == "household"){
            cat('write.csv(household, paste(mainDir,"/data/household.csv",sep = ""), row.names = FALSE, na = "")', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          }else{
            cat(paste('dbr<-"',indicator.frame,'"',sep = ""))
            cat('write.csv(eval(as.name(dbr)),paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""), row.names = FALSE, na = "")', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          }
          
          source(paste0(mainDir,"/code/temp.R"))
          cat(paste0(i, "- Executed  indicator: ", indicator.label,"\n"))
          if (file.exists(paste0(mainDir,"/code/temp.R"))) file.remove(paste0(mainDir,"/code/temp.R"))
          
          ## Insert the indicator in a temp dico frame to be appended to the full dico  ######################
          
          dicotemp1 <- data.frame(c("trigger"))
          names(dicotemp1)[1] <- "type"
          dicotemp1$type <- indicator.type
          dicotemp1$name <- indicator.fullname
          dicotemp1$fullname <- indicator.fullname
          dicotemp1$label <- indicator.label
          dicotemp1$labelReport <- indicator.report
          dicotemp1$chapter <- indicator.chapter
          dicotemp1$disaggregation <- indicator.disaggregation
          dicotemp1$correlate <- indicator.correlate
          dicotemp1$anonymise <- indicator.anonymise
          
          dicotemp1$structuralequation <- indicator.structuralequation
          dicotemp1$clean <- " "
          dicotemp1$cluster <- indicator.cluster
          dicotemp1$predict <- indicator.predict
          dicotemp1$variable <- indicator.variable
          dicotemp1$mappoint <- indicator.mappoint
          dicotemp1$mappoly <- indicator.mappoly
          
          dicotemp1$listname <- indicator.listname
          dicotemp1$qrepeat <- " "
          dicotemp1$qrepeatlabel <- indicator.frame
          dicotemp1$qlevel <- " "
          dicotemp1$qgroup <- " "
          dicotemp1$labelchoice <- " "
          dicotemp1$order <- " "
          dicotemp1$weight <- " "
          dicotemp1$score <- " "
          dicotemp1$recategorise <- " "
          dicotemp1$formpart <- " "
          dicotemp1$indic <- "feature"
          
          dicotemp <- rbind(dicotemp,dicotemp1)
          
        }
        ## Append indicators in the dico  #############################################################################
        
        ## removing first line
        dicotemp <- dicotemp[ 2:nrow(dicotemp), ]
        
        ### mergin choices from the newly created indicators #################################################################
        
        cat("\n\n\n It's assumed that the modalities for newly calculated categoric indicators are in the same xlsform - choices worksheet  \n\n\n\n")
        choices <- read_excel(form_tmp, sheet = "choices")
        
        #rm(choices)
        names(choices)[names(choices) == "label::English"] <- "label"
        names(choices)[names(choices) == "label::english"] <- "label"
        names(choices)[names(choices) == "list name"] <- "listname"
        names(choices)[names(choices) == "list_name"] <- "listname"
        
        ## Remove trailing space
        choices$listname <- trim(choices$listname)
        choices$label <- trim(choices$label)
        
        if ("labelReport" %in% colnames(choices))
        {
          cat("12 -  Good: You have a column `labelReport` in your `choices` worksheet.\n");
        } else
        {cat("12 -  No column `labelReport` in your `choices` worksheet. Creating a dummy one for the moment...\n");
          choices[,"labelReport"] <- substr(choices[,"label"],1,80)}
        
        if ("order" %in% colnames(choices))
        {
          cat(" Good: You have a column `order` in your `choices` worksheet.\n");
        } else
        {cat("  No column `order` in your `choices` worksheet. Creating a dummy one for the moment...\n");
          choices$order <- ""}
        
        if ("weight" %in% colnames(choices))
        {
          cat("  Good: You have a column `weight` in your `choices` worksheet.\n");
        } else
        {cat("13 -  No column `weight` in your `choices` worksheet. Creating a dummy one for the moment...\n");
          choices$weight <- ""}
        
        if ("recategorise" %in% colnames(choices))
        {
          cat("  Good: You have a column `recategorise` in your `choices` worksheet.\n");
        } else
        {cat("  No column `recategorise` in your `choices` worksheet. Creating a dummy one for the moment...\n");
          choices$recategorise <- ""}
        
        if ("score" %in% colnames(choices))
        {
          cat("  Good: You have a column `score` in your `choices` worksheet.\n");
        } else
        {cat("  No column `score` in your `choices` worksheet. Creating a dummy one for the moment...\n");
          choices$score <- ""}
        
        choices <- choices[,c("listname",  "name",  "label", "order", "weight","score","recategorise")]
        names(choices)[names(choices) == "label"] <- "labelchoice"
        #rm(choices)
        
        dicotemp.choice <- dicotemp[ !(is.na(dicotemp$listname)), c( "type",  "name",  "fullname", "label", "labelReport",
                                                                     "chapter",  "disaggregation","correlate", "anonymise",
                                                                     "structuralequation", "clean", "cluster",  "predict",
                                                                     "variable",
                                                                     "mappoint", "mappoly",  "listname",
                                                                     "qrepeat",  "qrepeatlabel","qlevel","qgroup" )]
        
        choices2 <- join(x = dicotemp.choice, y = choices,  by = "listname", type = "left")
        
        choices2$type <- with(choices2, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices2$type),
                                               paste0("select_one_d"),choices2$type))
        
        choices2$type <- with(choices2, ifelse(grepl("select_multiple_d", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices2$type),
                                               paste0("select_multiple"),choices2$type))
        
        names(choices2)[2] <- "nameq"
        names(choices2)[3] <- "nameqfull"
        names(choices2)[4] <- "labelq"
        choices2$labelfull <- paste0(choices2$labelq, sep = ": ", choices2$labelchoice)
        choices2$namefull <- paste0(choices2$nameqfull, sep = ".", choices2$name)
        
        
        
        #### Now Row bind questions & choices########################################################################################################
        choices3 <- choices2[ ,c("type", "name", "namefull",  "labelfull", "labelReport",
                                 "chapter",  "disaggregation","correlate", "anonymise",
                                 "structuralequation", "clean", "cluster", "predict",
                                 "variable", "mappoint", "mappoly",  "listname",
                                 "qrepeat",  "qrepeatlabel","qlevel","qgroup",
                                 "labelchoice", "order", "weight","score",
                                 "recategorise")]
        
        
        names(choices3)[names(choices3) == "namefull"] <- "fullname"
        names(choices3)[names(choices3) == "labelfull"] <- "label"
        
        
        dicotemp <-    dicotemp[,c( "type", "name", "fullname", "label", "labelReport",
                                    "chapter",  "disaggregation","correlate", "anonymise",
                                    "structuralequation", "clean", "cluster", "predict",
                                    "variable", "mappoint", "mappoly",  "listname",
                                    "qrepeat",  "qrepeatlabel","qlevel","qgroup",
                                    "labelchoice", "order", "weight","score",
                                    "recategorise")]
        
        ### Check -- normally there should not be duplicate
  
        
        dicotemp$formpart <- "questions"
        choices3$formpart <- "answers"
        
        dicotemp <- rbind(dicotemp,choices3)
        
        
        dicotemp$indic <- "feature"
        dico$indic <- "data"
        
        dico$structuralequation <- NA
        
        #names(dico)
        #names(dicotemp)
        dico <- dico[ , c( "type", "name", "fullname", "label", "labelReport",
                           "chapter",  "disaggregation","correlate", "anonymise",
                           "structuralequation", "clean", "cluster", "predict",
                           "variable", "mappoint", "mappoly",  "listname",
                           "qrepeat",  "qrepeatlabel","qlevel","qgroup",
                           "labelchoice", "order", "weight","score",
                           "recategorise", "formpart", "indic" )]
        dicotemp <- dicotemp[ , c( "type", "name", "fullname", "label", "labelReport",
                                   "chapter",  "disaggregation","correlate", "anonymise",
                                   "structuralequation", "clean", "cluster", "predict",
                                   "variable", "mappoint", "mappoly",  "listname",
                                   "qrepeat",  "qrepeatlabel","qlevel","qgroup",
                                   "labelchoice", "order", "weight","score",
                                   "recategorise", "formpart", "indic" )]
        
        
        dico <- rbind(dico,dicotemp)
        
        rm(dicotemp,dicotemp1, choices, choices2, choices3, dicotemp.choice)
        
        household <- read.csv(paste(mainDir,"/data/household.csv",sep = ""), encoding = "UTF-8", na.strings = "NA")
        ## label Variables
        cat("\n\n quick check on labeling\n")
        household <- kobo_label(household , dico)
        for (dbr in dataBeginRepeat) {
          dataFrame <- read.csv(paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""),stringsAsFactors = F) 
          dataFrame <- kobo_label(dataFrame, dico)
          write.csv(dataFrame,paste(mainDir,"/data/",dbr,"-edited.csv",sep = ""), row.names = FALSE, na = "")
        }
        cat("\n\nWrite dico\n")
        write.csv(dico, paste0(mainDir,"/data/dico_",form,".csv"), row.names = FALSE, na = "")
        write.csv(household, paste(mainDir,"/data/household.csv",sep = ""), row.names = FALSE, na = "")
        
      }
    }
  }, error = function(err) {
    print("kobo_create_indicators_ERROR")
    return(structure(err, class = "try-error"))
  })
}