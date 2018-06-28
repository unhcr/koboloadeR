
#########################
## Script to compute indicators

mainDir <- getwd()
## Load all required packages
source(paste0(mainDir,"/code/0-packages.R"))
source(paste0(mainDir,"/code/0-config.R"))
library(koboloadeR)


#### Load and test i indicators #############################################################################
#library(readxl)
tried <- try(read_excel(paste("data/",form,sep = ""), sheet = "indicator"),
             silent = TRUE)
if (inherits(tried, "try-error")) {
  writeLines("There was an error: You have not defined indicators within your xlsform file. \n")
  rm(tried)
} else {

  rm(tried)
  indicator <- read_excel(paste("data/",form,sep = ""), sheet = "indicator")

  #rm(list = ls())


  ## Load data & dico #############################################################################
  #form <- "form.xls"
  ## Run this only after data cleaning
  dico <- read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")
  household <- read.csv("data/household.csv", encoding = "UTF-8", na.strings = "NA")
  nbremenage <- read.csv("data/nbremenage.csv", encoding = "UTF-8", na.strings = "NA")


  ## Create the dicotemp #############################################################################
  #names(dico)
  dicotemp <- data.frame(c("trigger"))
  names(dicotemp)[1] <- "type"
  #dicotemp$type <- "trigger"
  dicotemp$name <- "trigger"
  dicotemp$fullname <- "trigger"
  dicotemp$label <- "trigger"
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
  indicator <- read_excel(paste("data/",form,sep = ""), sheet = "indicator")


  ## Load indicator info #############################################################################

  for (i in 1:nrow(indicator))

  {
    # i <-1
    indicator.type	<- as.character(indicator[ i, c("type")])
    indicator.fullname	<- as.character(indicator[ i, c("fullname")])
    indicator.label	<- as.character(indicator[ i, c("label")])
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
    if (file.exists("code/temp.R")) file.remove("code/temp.R")
    cat(indic.formula, file = "code/temp.R" , sep = "\n", append = TRUE)
    cat("####", file = "code/temp.R" , sep = "\n", append = TRUE)

    ## do a check on indicator variable type
    indicator.type2 <- indicator.type
    ifelse(indicator.type == "select_one", indicator.type2 <- "character", indicator.type2 <- indicator.type)

    cat(paste0(indicator.frame,"$",indicator.fullname," <- as.",indicator.type2,"(",indicator.frame,"$",indicator.fullname,")"), file = "code/temp.R" , sep = "\n", append = TRUE)
    cat(paste0("str(",indicator.frame,"$",indicator.fullname,")"), file = "code/temp.R" , sep = "\n", append = TRUE)
    cat(paste0("summary(",indicator.frame,"$",indicator.fullname,")"), file = "code/temp.R" , sep = "\n", append = TRUE)
    source("code/temp.R")
    cat(paste0(i, "- Executed  indicator: ", indicator.label,"\n"))
    if (file.exists("code/temp.R")) file.remove("code/temp.R")

    ## Insert the indicator in a temp dico frame to be appended to the full dico  ######################

    dicotemp1 <- data.frame(c("trigger"))
    names(dicotemp1)[1] <- "type"
    dicotemp1$type <- indicator.type
    dicotemp1$name <- indicator.fullname
    dicotemp1$fullname <- indicator.fullname
    dicotemp1$label <- indicator.label
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
  choices <- read_excel(paste("data/",form,sep = ""), sheet = "choices")

  #rm(choices)
  names(choices)[names(choices) == "label::English"] <- "label"
  names(choices)[names(choices) == "label::english"] <- "label"
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

  dicotemp.choice <- dicotemp[ !(is.na(dicotemp$listname)), c( "type",  "name",  "fullname", "label",
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
  choices3 <- choices2[ ,c("type", "name", "namefull",  "labelfull",
                           "chapter",  "disaggregation","correlate", "anonymise",
                           "structuralequation", "clean", "cluster", "predict",
                           "variable", "mappoint", "mappoly",  "listname",
                           "qrepeat",  "qrepeatlabel","qlevel","qgroup",
                           "labelchoice", "order", "weight","score",
                           "recategorise")]


  names(choices3)[names(choices3) == "namefull"] <- "fullname"
  names(choices3)[names(choices3) == "labelfull"] <- "label"


  dicotemp <-    dicotemp[,c( "type", "name", "fullname", "label",
                              "chapter",  "disaggregation","correlate", "anonymise",
                              "structuralequation", "clean", "cluster", "predict",
                              "variable", "mappoint", "mappoly",  "listname",
                              "qrepeat",  "qrepeatlabel","qlevel","qgroup",
                              "labelchoice", "order", "weight","score",
                              "recategorise")]

  ### Check -- normally there should not be duplicate
  #choices3 <- choices2[!duplicated(choices2$fullname), ]

  #  names(dicotemp)
  #  names(choices2)

  dicotemp$formpart <- "questions"
  choices3$formpart <- "answers"

  dicotemp <- rbind(dicotemp,choices3)


  dicotemp$indic <- "feature"
  dico$indic <- "data"

  #names(dico)
  #names(dicotemp)
  dico <- dico[ , c( "type", "name", "fullname", "label",
                     "chapter",  "disaggregation","correlate", "anonymise",
                     "structuralequation", "clean", "cluster", "predict",
                     "variable", "mappoint", "mappoly",  "listname",
                     "qrepeat",  "qrepeatlabel","qlevel","qgroup",
                     "labelchoice", "order", "weight","score",
                     "recategorise", "formpart", "indic" )]
  dicotemp <- dicotemp[ , c( "type", "name", "fullname", "label",
                             "chapter",  "disaggregation","correlate", "anonymise",
                             "structuralequation", "clean", "cluster", "predict",
                             "variable", "mappoint", "mappoly",  "listname",
                             "qrepeat",  "qrepeatlabel","qlevel","qgroup",
                             "labelchoice", "order", "weight","score",
                             "recategorise", "formpart", "indic" )]


  dico <- rbind(dico,dicotemp)

  rm(dicotemp,dicotemp1, choices, choices2, choices3, dicotemp.choice)

  ### check indicator type
  #household.check <- household[ , ((ncol(household.back)+1):ncol(household))]
  #summary(household.check)
  ## label Variables
  #household.check <- kobo_label(household.check , dico)

  ## Check that the join is correct by looking at total HH members
  #household$mf <- household$F +household$M
  #household$adultchild <- household$adult  +household$child
  #View(household[ , c("section2.total_hh", "mf", "adultchild")])

  ## label Variables
  cat("\n\n quick check on labeling\n")
  household <- kobo_label(household , dico)
 # nbremenage <- kobo_label(nbremenage, dico)

  # cat("\n\n Re-encoding now the data plus indicators based on the the full dictionnary\n")
  # household <- kobo_encode(household, dico)
  # nbremenage <- kobo_encode(nbremenage, dico)

  cat("\n\nWrite backup\n")
  write.csv(dico, paste0("data/dico_",form,"-indic.csv"), row.names = FALSE, na = "")


  write.csv(household, "data/household.csv", row.names = FALSE, na = "")
#  write.csv(nbremenage, "data/nbremenage.csv", row.names = FALSE, na = "")

}
