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
#'
#' @examples
#' \dontrun{
#' kobo_create_indicators("myform.xls")
#' }
#'
#' @export kobo_create_indicators
#'

kobo_create_indicators <- function(form = "form.xls") {

  mainDir <- kobo_getMainDirectory()
  form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")

  tryCatch({
    #### Load and test i indicators #############################################################################
    #library(readxl)
    tried <- try(readxl::read_excel(form_tmp, sheet = "indicator"),
                 silent = TRUE)
    if (inherits(tried, "try-error")) {
      writeLines("Note that you have not defined (or defined correctly) indicators within your xlsform file. \n")

    } else {
      rm(tried)

      ## load all required data files #########################################
      cat("\n\nload all required data files..\n")
      dataBeginRepeat <- kobo_get_begin_repeat()
      dataBeginRepeat <- dataBeginRepeat$names
      for (dbr in dataBeginRepeat) {
        dataFrame <- utils::read.csv(paste(mainDir,"/data/",dbr,"_edited.csv",sep = ""),stringsAsFactors = F)
        assign(dbr, dataFrame)
      }


      indicator <- readxl::read_excel(form_tmp, sheet = "indicator")
      if (nrow(indicator) == 0) {
        writeLines("Note that you have not defined (or defined correctly) indicators within your xlsform file.  \n")

      } else {
        ## Load data & dico #############################################################################
        #form <- "form.xls"
        ## Run this only after data cleaning
        dico <- utils::read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")

        ## Create the dicotemp #############################################################################
        #names(dico)
        dicotemp <- data.frame(c("trigger"))
        names(dicotemp)[1] <- "type"
        #dicotemp$type <- "trigger"
        dicotemp$name <- "trigger"
        dicotemp$fullname <- "trigger"
        dicotemp$label <- "trigger"
        dicotemp$labelReport <- "trigger"
        dicotemp$hintReport <- "trigger"
        dicotemp$chapter <- "trigger"
        dicotemp$disaggregation <- "trigger"
        dicotemp$correlate <- "trigger"
        dicotemp$anonymise <- "trigger"

        dicotemp$structuralequation.risk <- "trigger"
        dicotemp$structuralequation.coping <- "trigger"
        dicotemp$structuralequation.resilience <- "trigger"

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
        dicotemp$indic <- "trigger"
        dicotemp$constraint <- "trigger"

        dicotemp$label <- "trigger"
        dicotemp$relevant <- "trigger"
        dicotemp$repeat_count <- "trigger"
        dicotemp$required <- "trigger"


        ## Need to check that all column are presents...


        ## Load indicator info #############################################################################

        for (i in 1:nrow(indicator))

        {
          # i <- 1
          indicator.type	<- as.character(indicator[ i, c("type")])
          indicator.fullname	<- as.character(indicator[ i, c("fullname")])
          indicator.label	<- as.character(indicator[ i, c("label")])
          indicator.labelReport	<- as.character(indicator[ i, c("labelReport")])
          indicator.hintReport	<- as.character(indicator[ i, c("hintReport")])
          indicator.chapter	<- as.character(indicator[ i, c("chapter")])
          indicator.disaggregation	<- as.character(indicator[ i, c("disaggregation")])
          indicator.correlate	<- as.character(indicator[ i, c("correlate")])
          indicator.anonymise	<- as.character(indicator[ i, c("anonymise")])
          indicator.frame	<- as.character(indicator[ i, c("frame")])
          indicator.listname <- as.character(indicator[ i, c("listname")])
          indicator.calculation	<- as.character(indicator[ i, c("calculation")])


          indicator.structuralequation.risk 	<- as.character(indicator[ i, c("structuralequation.risk")])
          indicator.structuralequation.coping 	<- as.character(indicator[ i, c("structuralequation.coping")])
          indicator.structuralequation.resilience 	<- as.character(indicator[ i, c("structuralequation.resilience")])
          indicator.cluster 	<- as.character(indicator[ i, c("cluster")])
          indicator.predict	<- as.character(indicator[ i, c("predict")])
          indicator.variable	<- as.character(indicator[ i, c("variable")])
          indicator.mappoint	<- as.character(indicator[ i, c("mappoint")])
          indicator.mappoly	<- as.character(indicator[ i, c("mappoly")])


          cat(paste0(i, "- Load  indicator: ", indicator.labelReport," of type: ",indicator.type,"\n"))

          ## Build and run the formula to insert the indicator in the right frame  ###########################
          indic.formula <- paste0(indicator.frame,"$",indicator.fullname," <- ",indicator.calculation )
          if (file.exists(paste0(mainDir,"/code/temp.R") )) file.remove(paste0(mainDir,"/code/temp.R"))


          cat(paste('### Script to generate indicator: ',indicator.labelReport,sep = ""), file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat(paste('form <- "',form,'"',sep = ""), file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat("mainDir <- kobo_getMainDirectory()", file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat('form_tmp <- paste(mainDir, "data", form, sep = "/", collapse = "/")', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat('dataBeginRepeat <- kobo_get_begin_repeat()', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat('dataBeginRepeat <- dataBeginRepeat$names', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)

          cat('MainDataFrame <- utils::read.csv(paste(mainDir,"/data/MainDataFrame_edited.csv",sep = ""), encoding = "UTF-8", na.strings = "NA")', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)

          cat('for (dbr in dataBeginRepeat) {
            dataFrame <- utils::read.csv(paste(mainDir,"/data/",dbr,"_edited.csv",sep = ""),stringsAsFactors = F)

            assign(paste0(dbr,"_edited"), dataFrame)
          }', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)

          cat(indic.formula, file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat("####", file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)

          ## do a check on indicator variable type
          indicator.type2 <- indicator.type
          ifelse(indicator.type == "select_one", indicator.type2 <- "character", indicator.type2 <- indicator.type)


          cat(paste0(indicator.frame,"$",indicator.fullname," <- as.",indicator.type2,"(",indicator.frame,"$",indicator.fullname,")"), file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat(paste0("str(",indicator.frame,"$",indicator.fullname,")"), file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
          cat(paste0("summary(",indicator.frame,"$",indicator.fullname,")"), file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)

           if (indicator.frame == "MainDataFrame_edited") {
             cat('utils::write.csv(MainDataFrame, paste(mainDir,"/data/MainDataFrame_edited.csv",sep = ""), row.names = FALSE, na = "")', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
           }else{
             cat(paste('dbr<-"',indicator.frame,'"',sep = ""))
             cat('utils::write.csv(eval(as.name(dbr)),paste(mainDir,"/data/",dbr,".csv",sep = ""), row.names = FALSE, na = "")', file = paste0(mainDir,"/code/temp.R") , sep = "\n", append = TRUE)
           }


          source(paste0(mainDir,"/code/temp.R"))
          cat(paste0(i, "- Executed  indicator: ", indicator.labelReport,"\n"))
          if (file.exists(paste0(mainDir,"/code/temp.R"))) file.remove(paste0(mainDir,"/code/temp.R"))

          ## Insert the indicator in a temp dico frame to be appended to the full dico  ######################

          dicotemp1 <- data.frame(c("trigger"))
          names(dicotemp1)[1] <- "type"
          dicotemp1$type <- indicator.type
          dicotemp1$name <- indicator.fullname
          dicotemp1$fullname <- indicator.fullname
          dicotemp1$label <- indicator.label
          dicotemp1$labelReport <- indicator.labelReport
          dicotemp1$hintReport <- indicator.hintReport
          dicotemp1$chapter <- indicator.chapter
          dicotemp1$disaggregation <- indicator.disaggregation
          dicotemp1$correlate <- indicator.correlate
          dicotemp1$anonymise <- indicator.anonymise

          dicotemp1$structuralequation.risk <- indicator.structuralequation.risk
          dicotemp1$structuralequation.coping <- indicator.structuralequation.coping
          dicotemp1$structuralequation.resilience <- indicator.structuralequation.resilience
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

          dicotemp1$constraint <- " "
          dicotemp1$label <- " "
          dicotemp1$relevant <- " "
          dicotemp1$repeat_count <- " "
          dicotemp1$required <- " "

          dicotemp <- rbind(dicotemp,dicotemp1)

        }
        ## Append indicators in the dico  #############################################################################

        ## removing first line
          dicotemp <- dicotemp[ 2:nrow(dicotemp), ]

          ### mergin choices from the newly created indicators #################################################################

          cat("\n\n\n It's assumed that the modalities for newly calculated categoric indicators are in the same xlsform - choices worksheet  \n\n\n\n")
          choices <- readxl::read_excel(form_tmp, sheet = "choices")

          #rm(choices)
          #names(choices)[names(choices) == "labelReport"] <- "label"
          names(choices)[names(choices) == "label::english"] <- "label"
          names(choices)[names(choices) == "label::English"] <- "label"
          names(choices)[names(choices) == "list name"] <- "listname"
          names(choices)[names(choices) == "list_name"] <- "listname"

          ## Remove trailing space
          choices$listname <- trim(choices$listname)
          choices$label <- trim(choices$label)



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
          {cat(" No column `weight` in your `choices` worksheet. Creating a dummy one for the moment...\n");
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
          {
            cat("  No column `score` in your `choices` worksheet. Creating a dummy one for the moment...\n");
            choices$score <- ""
            }

          choices <- choices[,c("listname",  "name",  "label",  "order", "weight","score","recategorise")]

          names(choices)[names(choices) == "label"] <- "labelchoice"
          #rm(choices)

        dicotemp.choice <- dicotemp[ !(is.na(dicotemp$listname)), c( "type", "name",
                                                                  "fullname","labelReport","hintReport","chapter",
                                                                  "disaggregation","correlate","anonymise",
                                                                  "structuralequation.risk","structuralequation.coping","structuralequation.resilience",
                                                                  "clean","cluster","predict","variable","mappoint","mappoly",
                                                                  "listname","qrepeat","qrepeatlabel","qlevel","qgroup",
                                                                 # "order","weight","score","recategorise",
                                                                  "formpart","indic","constraint","label","relevant","repeat_count","required" )]

        choices2 <- plyr::join(x = dicotemp.choice, y = choices,  by = "listname", type = "left")

        choices2$type <- with(choices2, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices2$type),
                                               paste0("select_one_d"),choices2$type))

        choices2$type <- with(choices2, ifelse(grepl("select_multiple_d", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices2$type),
                                               paste0("select_multiple"),choices2$type))

        names(choices2)[2] <- "nameq"
        names(choices2)[3] <- "nameqfull"
        names(choices2)[4] <- "labelq"
        # choices$labelchoice

        # View(choices2[ , c("labelchoice")])
        #choices2$labelchoice <- as.factor(choices2$labelchoice)
        #str(choices2$labelchoice)
        #names(choices2)
        # choices2$labelq
        choices2$labelfull <- paste(choices2$labelq, choices2$labelchoice, sep = ": ")
        choices2$namefull <- paste(choices2$nameqfull, choices2$name, sep = ".")

       # names(choices2)

        #### Now Row bind questions & choices########################################################################################################
        choices3 <- choices2[ ,c("type", "name", "namefull",  "labelfull","hintReport",
                                 "chapter",  "disaggregation","correlate", "anonymise",
                                 "structuralequation.risk","structuralequation.coping","structuralequation.resilience",
                                 "clean", "cluster", "predict",
                                 "variable", "mappoint", "mappoly",  "listname",
                                 "qrepeat",  "qrepeatlabel","qlevel","qgroup",
                                 "labelchoice", "order", "weight","score",
                                 "recategorise",
                                 "formpart","indic","constraint","label","relevant","repeat_count","required")]


        names(choices3)[names(choices3) == "namefull"] <- "fullname"
        names(choices3)[names(choices3) == "labelfull"] <- "labelReport"
        #names(choices3)[names(choices3) == "labelfull"] <- "label"


        dicotemp <-    dicotemp[,c( "type", "name", "fullname", #"label",
                                    "labelReport","hintReport",
                                    "chapter",  "disaggregation","correlate", "anonymise",
                                    "structuralequation.risk","structuralequation.coping","structuralequation.resilience",
                                    "clean", "cluster", "predict",
                                    "variable", "mappoint", "mappoly",  "listname",
                                    "qrepeat",  "qrepeatlabel","qlevel","qgroup",
                                    "labelchoice", "order", "weight","score",
                                    "recategorise",
                                    "formpart","indic","constraint","label","relevant","repeat_count","required")]

        ### Check -- normally there should not be duplicate


        dicotemp$formpart <- "questions"
        choices3$formpart <- "answers"

        # test1 <- as.data.frame(names(dicotemp))
        # test2 <- as.data.frame(names(choices3))

        dicotemp <- rbind(dicotemp,choices3)

        dicotemp$label <- dicotemp$labelReport

        dicotemp$indic <- "feature"
        dico$indic <- "data"


        dicotemp$relevant <- ""
        dicotemp$required <- ""
        dicotemp$constraint <- ""
        dicotemp$repeat_count <- ""


        #names(dico)
        #names(dicotemp)
        dico <- dico[ , c( "type", "name", "fullname", "label", "labelReport","hintReport",
                           "chapter",  "disaggregation","correlate", "anonymise",
                           "structuralequation.risk","structuralequation.coping","structuralequation.resilience",
                           "anonymise", "clean", "cluster", "predict", "variable", "mappoint", "mappoly",

                           "relevant",  "required", "constraint", "repeat_count",

                           "listname","qrepeat",  "qrepeatlabel","qlevel","qgroup",
                           "labelchoice", "order", "weight","score",
                           "recategorise", "formpart", "indic" )]


        dicotemp <- dicotemp[ , c("type", "name", "fullname", "label", "labelReport","hintReport",
                                  "chapter",  "disaggregation","correlate", "anonymise",
                                  "structuralequation.risk","structuralequation.coping","structuralequation.resilience",
                                  "anonymise", "clean", "cluster", "predict", "variable", "mappoint", "mappoly",

                                  "relevant",  "required", "constraint", "repeat_count",

                                  "listname","qrepeat",  "qrepeatlabel","qlevel","qgroup",
                                  "labelchoice", "order", "weight","score",
                                  "recategorise", "formpart", "indic" )]


        dico <- rbind(dico,dicotemp)

        rm(dicotemp,dicotemp1, choices, choices2, choices3, dicotemp.choice)


        MainDataFrame <- utils::read.csv(paste(mainDir,"/data/MainDataFrame_edited.csv",sep = ""), encoding = "UTF-8", na.strings = "NA")
        ## label Variables
        cat("\n\n quick check on labeling\n")
        MainDataFrame <- kobo_label(MainDataFrame , dico)
        for (dbr in dataBeginRepeat) {
          dataFrame <- utils::read.csv(paste(mainDir,"/data/",dbr,"_edited.csv",sep = ""),stringsAsFactors = F)

          dataFrame <- kobo_label(dataFrame, dico)
          utils::write.csv(dataFrame,paste(mainDir,"/data/",dbr,"_edited.csv",sep = ""), row.names = FALSE, na = "")
        }
        cat("\n\nWrite dico\n")
        utils::write.csv(dico, paste0(mainDir,"/data/dico_",form,".csv"), row.names = FALSE, na = "")

        utils::write.csv(MainDataFrame, paste(mainDir,"/data/MainDataFrame_edited.csv",sep = ""), row.names = FALSE, na = "")


      }
    }
  }, error = function(err) {
    print("There was an error in the indicator creation step!!! \n\n")
    return(structure(err, class = "try-error"))
  })
}
