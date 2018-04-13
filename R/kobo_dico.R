#' @name kobo_dico
#' @rdname kobo_dico
#' @title  Data dictionnary
#'
#' @description  Produce a data dictionnary based on the xlsform for the project
#'
#' @param mainDir
#'
#' @return A "data.table" with the full data dictionnary. To be used in the rest of the analysis.
#'
#' @author Edouard Legoupil, Elliott Messeiller
#'
#' @examples
#' kobo_dico()
#'
#' @examples
#' \dontrun{
#' kobo_dico()
#' }
#'
#' @export kobo_dico
#'

kobo_dico <- function(mainDir='') {
  if (mainDir==''){
    mainDir <- getwd()
  }

  source(paste0(mainDir,"/code/0-config.R"), local=TRUE)
  # read the survey tab of ODK from
  form_tmp <- paste0(mainDir, "/data/",form)

  ###############################################################################################
  ### First review all questions first
  survey <- read_excel(form_tmp, sheet = "survey")

  ## Rename the variable label
  names(survey)[names(survey)=="label::English"] <- "label"
  cat("Checking now for additional information within your xlsform. Note that you can insert them in the xls and re-run the function! \n \n ")

  if("disaggregation" %in% colnames(survey))
  {
  cat("Good: You have a column `disaggregation` in your survey worksheet.\n");
  } else
  {cat("No column `disaggregation` in your survey worksheet. Creating a dummy one for the moment...\n");
    survey$disaggregation <- ""}


  if("correlate" %in% colnames(survey))
  {
    cat("Good: You have a column `correlate` in your survey worksheet. This will be used to define the variables that should be checked for correlation between each others.\n");
  } else
  {cat("No column `correlate` in your survey worksheet. Creating a dummy one for the moment...\n");
    survey$correlate <- ""}


  if("ordinal" %in% colnames(survey))
  {
    cat("Good: You have a column `ordinal` in your survey worksheet. \n");
  } else
  {cat("No column `ordinal` in your survey worksheet. Creating a dummy one for the moment ...\n");
    survey$ordinal <- ""}

  if("weight" %in% colnames(survey))
  {
    cat("Good: You have a column `weight` in your survey worksheet. \n");
  } else
  {cat("No column `weight` in your survey worksheet. Creating a dummy one for the moment ...\n");
    survey$weight <- ""}



  ## Avoid columns without names
  survey <- survey[ ,c("type","name","label", "disaggregation","correlate","ordinal")]

  ## need to delete empty rows from the form
  survey <- as.data.frame(survey[!is.na(survey$type), ])

  #str(survey)
  #levels(as.factor(survey$type))

  ### We can now extract the id of the list name to reconstruct the full label fo rthe question
  cat(" \n Now extracting list name from questions type.\n \n")
  survey$listname <- ""

  ## handle case where we have "or_other"
  #survey$listname <- with(survey, ifelse(grepl("or_other", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$listname) ,
  #                                       paste0( substr(survey$listname , 1, (nchar(survey$listname)-8 ))),survey$listname))

  ## handle case where we have "or_other"
  survey$listname <- with(survey, ifelse(grepl("or_other", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$listname) ,
                                         paste0( substr(survey$listname , 1, (nchar(survey$listname)-8 ))),survey$listname))


  ## Extract for select_one
  survey$listname <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type) ,
                                         paste0( substr(survey$type , (regexpr("select_one", survey$type , ignore.case=FALSE, fixed=TRUE))+10,250)),survey$listname))

  survey$type <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_one"),survey$type))

  ## Extract for select multiple & clean type field
  survey$listname <- with(survey,  ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type),
                                          paste0( substr(survey$type , (regexpr("select_multiple", survey$type , ignore.case=FALSE, fixed=TRUE))+16,250)),survey$listname ))


  survey$type <- with(survey, ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_multiple_d"),survey$type))


  ## Remove trailing space
  survey$listname <- trim(survey$listname)
  survey$label <- trim(survey$label)
  #str(survey)

  ### Get question levels in order to match the variable name
  survey$qlevel <- ""
  for(i in 2:nrow(survey))
  {      if(survey[ i, c("type")] =="begin group" && survey[ i-1, c("qlevel")]=="" )      {survey[ i, c("qlevel")]  <-  "level1"}
    else if(survey[ i, c("type")] =="begin_group" && survey[ i-1, c("qlevel")]=="" )      {survey[ i, c("qlevel")]  <-  "level1"}

    else if(survey[ i, c("type")] =="begin group" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")]  <-  "level2"}
    else if(survey[ i, c("type")] =="begin_group" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")]  <-  "level2"}

    else if(survey[ i, c("type")] =="begin group" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level3"}
    else if(survey[ i, c("type")] =="begin_group" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level3"}

    else if(survey[ i, c("type")] =="begin group" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qlevel")]  <-  "level4"}
    else if(survey[ i, c("type")] =="begin_group" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qlevel")]  <-  "level4"}

    else if(survey[ i, c("type")] =="begin group" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qlevel")]  <-  "level5"}
    else if(survey[ i, c("type")] =="begin_group" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qlevel")]  <-  "level5"}

    ## Now end of group

    else if(survey[ i, c("type")] =="end group" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")] <- "" }
    else if(survey[ i, c("type")] =="end_group" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")] <- "" }

    else if(survey[ i, c("type")] =="end group" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level1"}
    else if(survey[ i, c("type")] =="end_group" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level1"}

    else if(survey[ i, c("type")] =="end group" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qlevel")]  <-  "level2"}
    else if(survey[ i, c("type")] =="end_group" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qlevel")]  <-  "level2"}

    else if(survey[ i, c("type")] =="end group" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qlevel")]  <-  "level3"}
    else if(survey[ i, c("type")] =="end_group" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qlevel")]  <-  "level3"}

    else if(survey[ i, c("type")] =="end group" && survey[ i-1, c("qlevel")]=="level5") {survey[ i, c("qlevel")]  <-  "level4"}
    else if(survey[ i, c("type")] =="end_group" && survey[ i-1, c("qlevel")]=="level5") {survey[ i, c("qlevel")]  <-  "level4"}

    else   {survey[ i, c("qlevel")]  <-  survey[ i-1, c("qlevel")]}
  }

  ### Get question groups in order to match the variable name
  ## Concatenation ofqlevel & qrepeat & type
     survey$type2 <- survey$type
     survey$type2[survey$type2 %in% c("begin_group","begin group","end_group","end group")]
  ## We need to handle situation with both repeat & group
  ## set <- as.data.frame(unique(dico[c("qlevel","qrepeat", "type")]))
  ## So 12 cases to handle

  cat(" \n Now rebuilding the variable full path in order to match with variable name from the exported dataset. \n
      Note that there should not be any dots in the orginal variables. \n
      Double Check as well there's no duplicate for the name column in the survey worksheet\n \n")
  survey$qgroup <- ""
  for(i in 2:nrow(survey))
  {
    #i <- 54
    #i <- 20
    #survey[ 113, c("qgroup")]
            if(survey[ i, c("qlevel")]  %in% c("level1","level2","level3","level4","level5") &&
              !(survey[ i, c("type")]   %in% c("begin_group","begin group","end_group","end group","begin_repeat","begin repeat","end_repeat","end repeat")) )

      {survey[ i, c("qgroup")] <- survey[ i-1, c("qgroup")]


    } else if(survey[ i, c("qlevel")]   %in% c("level1") &&
              survey[ i, c("type")]     %in% c("begin_group","begin group")  )

       {survey[ i, c("qgroup")] <- survey[ i, c("name")]

    } else if(survey[ i, c("qlevel")]   %in% c("level2","level3","level4","level5") &&
              survey[ i, c("type")]     %in% c("begin_group","begin group") )

       {survey[ i, c("qgroup")] <- paste(survey[ i-1, c("qgroup")], survey[ i, c("name")],sep=".")

    } else if(survey[ i, c("qlevel")]   %in% c("level1","level2","level3","level4","level5")  &&
              survey[ i, c("type")]     %in% c("begin_repeat","begin repeat")   )

      {survey[ i, c("qgroup")] <- paste(survey[ i-1, c("qgroup")], survey[ i, c("qrepeatlabel")],sep=".")

    } else if(survey[ i, c("qlevel")]   %in% c("level1","level2","level3","level4","level5") &&
              survey[ i, c("type")]     %in% c("end_group","end group","end_repeat","end repeat") )

       {survey[ i, c("qgroup")] <- substr(survey[ i-1, c("qgroup")] ,0, regexpr("\\.[^\\.]*$", survey[ i-1, c("qgroup")] )-1)

    } else  {survey[ i, c("qgroup")]  <- ""}
  }




  survey$fullname <- ""
  ## levels(as.factor(survey$type))
  ## Need to loop around the data frame in order to concatenate full name as observed in data dump
  survey[ 1, c("fullname")]  <-  survey[ 1, c("name")]
  for(i in 2:nrow(survey))
  {
    if(survey[ i, c("qlevel")] =="") {survey[ i, c("fullname")]  <-  survey[ i, c("name")]}
    else {survey[ i, c("fullname")]  <-  paste(survey[ i, c("qgroup")],survey[ i, c("name")],sep=".") }
  }

  ## a few colummns to adjust to match questions & choices
  survey$labelchoice <- survey$label
  survey$weight <- NA


  #############################################################################################################
  #### Now looking at choices --
  #rm(choices)
  choices <- read_excel(form_tmp, sheet = "choices")
  names(choices)[names(choices)=="label::English"] <- "label"
  names(choices)[names(choices)=="list name"] <- "listname"
  names(choices)[names(choices)=="list_name"] <- "listname"

  ## Remove trailing space
  choices$listname <- trim(choices$listname)
  choices$label <- trim(choices$label)


  choices <- choices[,c("listname",  "name",  "label")]
  names(choices)[names(choices)=="label"] <- "labelchoice"
  #rm(choices)
  choices <- join(x=choices, y=survey, by="listname", type="left")

  choices$type <- with(choices, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices$type),
                                       paste0("select_one_d"),choices$type))

  choices$type <- with(choices, ifelse(grepl("select_multiple_d", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices$type),
                                       paste0("select_multiple"),choices$type))


  names(choices)[9] <- "nameq"
  names(choices)[10] <- "labelq"
  choices$labelfull <- paste0(choices$labelq, sep = ": ", choices$labelchoice)
  choices$namefull <- paste0(choices$fullname, sep = ".", choices$name)
  choices$correlate <-""
  choices$ordinal <-""
  choices$qlevel <- choices$fullname
  choices$weight <- NA

  #############################################################################################################
  #### Now Row bing questions & choices

  choices2 <- choices[ ,c("type", "name", "namefull", "labelfull", "disaggregation","correlate", "listname", "qlevel", "qgroup", "labelchoice","ordinal", "weight")]


  names(choices2)[names(choices2)=="namefull"] <- "fullname"
  names(choices2)[names(choices2)=="labelfull"] <- "label"


  survey2 <-    survey[,c("type", "name",  "fullname", "label", "disaggregation","correlate","listname","qlevel", "qgroup", "labelchoice","ordinal", "weight")]

  survey2$formpart <- "questions"
  choices2$formpart <- "answers"

  dico <- rbind(survey2,choices2)


  ## Remove trailing space
  dico$fullname <- trim(dico$fullname)
  dico$listname <- trim(dico$listname)


  ## A few fix on the dico
  dico <- dico[ !is.na(dico$name), ]
  dico <- dico[ !is.na(dico$type), ]

  write.csv(dico, paste0(mainDir,"/data/dico_",form,".csv"), row.names=FALSE, na = "")
  path.to.dico <- paste0(mainDir,"/data/dico_",form,".csv")

 # f_csv(dico)
#  return(dico)
cat("\n")
cat("###########################################################################\n")
cat("# Your dictionnary was created, now the data and forms are linked.        #\n")
cat("# You can found it in the data directory.                                 #\n")
cat("###########################################################################\n")
cat("\n")


#Fetching the directory
#Path to file
configfile<-paste(mainDir,"/code/0-config.R",sep="")
#Writting file
sink(configfile,append=TRUE)
cat("\n ### Name of the dictionnary: \n")

cat(paste('path.to.dico <- paste("',mainDir,'/data/dico_',form,'.csv",sep="") \n',sep=""))
cat("\n")
cat(paste('dico <- read.csv("',path.to.dico,'", sep=",") \n',sep = ""))

sink()

}
NULL

