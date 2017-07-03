#' @name kobo_dico
#' @rdname kobo_dico
#' @title  Data dictionnary
#'
#' @description  Produce a data dictionnary based on the xlsform for the project
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the data folder.
#'
#'
#' @return A "data.table" with the full data dictionnary. To be used in the rest of the analysis.
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_dico()
#'
#' @export kobo_dico
#' @examples
#' \dontrun{
#' kobo_dico("myform.xls")
#' }
#'
#' @export kobo_dico
#'

kobo_dico <- function(form) {

  #kobo_form(formid, user = user, api = api)

  # read the survey tab of ODK from
  form_tmp <- paste0("data/",form)

  ###############################################################################################
  ### First review all questions first
  survey <- read_excel(form_tmp, sheet = "survey")

  ## Rename the variable label
  names(survey)[names(survey)=="label::English"] <- "label"

  if("repeatsummarize" %in% colnames(survey))
  {
    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `repeatsummarize` in your survey worksheet.\n");
  } else
  {cat("Checking Data Analysis Plan within your xlsform -  No column `repeatsummarize` in your survey worksheet. Creating dummy one.\n");
    survey$repeatsummarize <- ""}

  if("variable" %in% colnames(survey))
  {
    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `variable` in your survey worksheet.\n");
  } else
  {cat("Checking Data Analysis Plan within your xlsform -  No column `variable` in your survey worksheet. Creating dummy one.\n");
    survey$variable <- ""}

  if("disaggregation" %in% colnames(survey))
  {
    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `disaggregation` in your survey worksheet.\n");
  } else
  {cat("Checking Data Analysis Plan within your xlsform -  No column `disaggregation` in your survey worksheet. Creating dummy one.\n");
    survey$disaggregation <- ""}

  if("report" %in% colnames(survey))
  {
    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `report` in your survey worksheet.\n");
  } else
  {cat("Checking Data Analysis Plan within your xlsform -  No column `report` in your survey worksheet. Creating dummy one.\n");
    survey$report <- ""}

#  if("indicator" %in% colnames(survey))
#  {
#    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `indicator` in your survey worksheet.\n");
#  } else
#  {cat("Checking Data Analysis Plan within your xlsform -  No column `indicator` in your survey worksheet. Creating dummy one.\n");
#    survey$indicator <- ""}

#  if("indicatorgroup" %in% colnames(survey))
#  {
#    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `indicatorgroup` in your survey worksheet.\n");
#  } else
#  {cat("Checking Data Analysis Plan within your xlsform -  No column `indicatorgroup` in your survey worksheet. Creating dummy one.\n");
#    survey$indicatorgroup <- ""}

#  if("indicatortype" %in% colnames(survey))
#  {
#    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `indicatortype` in your survey worksheet.\n");
#  } else
#  {cat("Checking Data Analysis Plan within your xlsform -  No column `indicatortype` in your survey worksheet. Creating dummy one.\n");
#    survey$indicatortype <- ""}

#  if("indicatorlevel" %in% colnames(survey))
#  {
#    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `indicatorlevel` in your survey worksheet.\n");
#  } else
#  {cat("Checking Data Analysis Plan within your xlsform -  No column `indicatorlevel` in your survey worksheet. Creating dummy one.\n");
#    survey$indicatorlevel <- ""}

#  if("dataexternal" %in% colnames(survey))
#  {
#    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `dataexternal` in your survey worksheet.\n");
#  } else
#  {cat("Checking Data Analysis Plan within your xlsform -  No column `dataexternal` in your survey worksheet. Creating dummy one.\n");
#    survey$dataexternal <- ""}

#  if("indicatorcalculation" %in% colnames(survey))
#  {
#    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `indicatorcalculation` in your survey worksheet.\n");
#  } else
#  {cat("Checking Data Analysis Plan within your xlsform -  No column `indicatorcalculation` in your survey worksheet. Creating dummy one.\n");
#    survey$indicatorcalculation <- ""}

 # if("indicatornormalisation" %in% colnames(survey))
#  {
#    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `indicatornormalisation` in your survey worksheet.\n");
#  } else
#  {cat("Checking Data Analysis Plan within your xlsform -  No column `indicatornormalisation` in your survey worksheet. Creating dummy one.\n");
#    survey$indicatornormalisation <- ""}


  ## Avoid columns without names
  survey <- survey[ ,c("type",   "name" ,  "label",
                       "repeatsummarize","variable","disaggregation", "report"
                      # "indicator","indicatorgroup","indicatortype",
                      # "indicatorlevel","dataexternal","indicatorcalculation","indicatornormalisation"
                       #"indicator","select", "Comment", "indicatordirect", "indicatorgroup" ## This indicator reference
                       # "label::English",
                       #"label::Arabic" ,"hint::Arabic",
                       # "hint::English", "relevant",  "required", "constraint",   "constraint_message::Arabic",
                       # "constraint_message::English", "default",  "appearance", "calculation",  "read_only"  ,
                       # "repeat_count"
  )]

  ## need to delete empty rows from the form
  survey <- as.data.frame(survey[!is.na(survey$type), ])

  #str(survey)
  #levels(as.factor(survey$type))

  ### We can now extract the id of the list name to reconstruct the full label fo rthe question
  cat("Extracting list name.\n")
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

  ## Now creating full name in order to match with data variables name

  ### identify Repeat questions
  survey$qrepeat <- ""
  for(i in 2:nrow(survey))
  {
    #i<-39
         if(survey[ i, c("type")] =="begin repeat")   {survey[ i, c("qrepeat")]  <- "repeat"}
    else if(survey[ i-1, c("qrepeat")]=="repeat" && survey[ i, c("type")] !="end repeat") {survey[ i, c("qrepeat")]  <-  "repeat"}
    else if(survey[ i, c("type")] =="end repeat" ) {survey[ i, c("qrepeat")]  <-  ""}

    ## Sometimes it seems that we get an underscore for type
    else if(survey[ i, c("type")] =="begin_repeat")   {survey[ i, c("qrepeat")]  <- "repeat"}
    else if(survey[ i-1, c("qrepeat")]=="repeat" && survey[ i, c("type")] !="end_repeat") {survey[ i, c("qrepeat")]  <-  "repeat"}
    else if(survey[ i, c("type")] =="end_repeat" ) {survey[ i, c("qrepeat")]  <-  ""}

    else   {survey[ i, c("qrepeat")]  <-  ""}
  }

  ### Get question levels in order to match the variable name
  survey$qlevel <- ""
  for(i in 2:nrow(survey))
  {      if(survey[ i, c("type")] =="begin group" && survey[ i-1, c("qlevel")]=="" )      {survey[ i, c("qlevel")]  <-  "level1"}
    else if(survey[ i, c("type")] =="begin_group" && survey[ i-1, c("qlevel")]=="" )      {survey[ i, c("qlevel")]  <-  "level1"}

    else if(survey[ i, c("type")] =="begin repeat" && survey[ i-1, c("qlevel")]=="" )      {survey[ i, c("qlevel")]  <-  "level1"}
    else if(survey[ i, c("type")] =="begin_repeat" && survey[ i-1, c("qlevel")]=="" )      {survey[ i, c("qlevel")]  <-  "level1"}


    else if(survey[ i, c("type")] =="begin group" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")]  <-  "level2"}
    else if(survey[ i, c("type")] =="begin_group" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")]  <-  "level2"}

    else if(survey[ i, c("type")] =="begin repeat" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")]  <-  "level2"}
    else if(survey[ i, c("type")] =="begin_repeat" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")]  <-  "level2"}


    else if(survey[ i, c("type")] =="begin group" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level3"}
    else if(survey[ i, c("type")] =="begin_group" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level3"}

    else if(survey[ i, c("type")] =="begin repeat" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level3"}
    else if(survey[ i, c("type")] =="begin_repeat" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level3"}



    else if(survey[ i, c("type")] =="begin group" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qlevel")]  <-  "level4"}
    else if(survey[ i, c("type")] =="begin_group" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qlevel")]  <-  "level4"}

    else if(survey[ i, c("type")] =="begin group" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qlevel")]  <-  "level5"}
    else if(survey[ i, c("type")] =="begin_group" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qlevel")]  <-  "level5"}


    ## Now end of group

    else if(survey[ i, c("type")] =="end group" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")] <- "" }
    else if(survey[ i, c("type")] =="end_group" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")] <- "" }

    else if(survey[ i, c("type")] =="end repeat" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")] <- "" }
    else if(survey[ i, c("type")] =="end_repeat" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qlevel")] <- "" }


    else if(survey[ i, c("type")] =="end group" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level1"}
    else if(survey[ i, c("type")] =="end_group" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level1"}

    else if(survey[ i, c("type")] =="end repeat" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level1"}
    else if(survey[ i, c("type")] =="end_repeat" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qlevel")]  <-  "level1"}


    else if(survey[ i, c("type")] =="end group" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qlevel")]  <-  "level2"}
    else if(survey[ i, c("type")] =="end_group" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qlevel")]  <-  "level2"}

    else if(survey[ i, c("type")] =="end repeat" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qlevel")]  <-  "level2"}
    else if(survey[ i, c("type")] =="end_repeat" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qlevel")]  <-  "level2"}


    else if(survey[ i, c("type")] =="end group" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qlevel")]  <-  "level3"}
    else if(survey[ i, c("type")] =="end_group" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qlevel")]  <-  "level3"}

    else if(survey[ i, c("type")] =="end repeat" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qlevel")]  <-  "level3"}
    else if(survey[ i, c("type")] =="end_repeat" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qlevel")]  <-  "level3"}

    else if(survey[ i, c("type")] =="end group" && survey[ i-1, c("qlevel")]=="level5") {survey[ i, c("qlevel")]  <-  "level4"}
    else if(survey[ i, c("type")] =="end_group" && survey[ i-1, c("qlevel")]=="level5") {survey[ i, c("qlevel")]  <-  "level4"}

    else if(survey[ i, c("type")] =="end repeat" && survey[ i-1, c("qlevel")]=="level5") {survey[ i, c("qlevel")]  <-  "level4"}
    else if(survey[ i, c("type")] =="end_repeat" && survey[ i-1, c("qlevel")]=="level5") {survey[ i, c("qlevel")]  <-  "level4"}

    else   {survey[ i, c("qlevel")]  <-  survey[ i-1, c("qlevel")]}
  }

  ### Get question groups in order to match the variable name
  survey$qgroup <- ""
  for(i in 2:nrow(survey))
  {
    #i <- 159
    if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="begin group" )    {survey[ i, c("qgroup")] <-survey[ i, c("name")]
    } else if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="begin_group" )    {survey[ i, c("qgroup")] <-survey[ i, c("name")]

    } else if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="begin repeat" )    { survey[ i, c("qgroup")] <-survey[ i, c("name")]
    } else if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="begin_repeat" )    { survey[ i, c("qgroup")] <-survey[ i, c("name")]

    } else if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="end group" ) { survey[ i, c("qgroup")] <-substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1)
    } else if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="end_group" ) { survey[ i, c("qgroup")] <-substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1)

    } else if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="end repeat" ) { survey[ i, c("qgroup")] <-substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1)
    } else if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="end_repeat" ) { survey[ i, c("qgroup")] <-substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1)

    } else if( survey[ i, c("qlevel")]=="level2" && survey[ i-1, c("qlevel")]=="level1") { survey[ i, c("qgroup")] <-paste(survey[ i-1, c("qgroup")], survey[ i, c("name")],sep=".")
    } else if( survey[ i, c("qlevel")]=="level2" && survey[ i-1, c("qlevel")]=="level2") { survey[ i, c("qgroup")] <-survey[ i-1, c("qgroup")]
    } else if( survey[ i, c("qlevel")]=="level2" && survey[ i-1, c("qlevel")]=="level3") { level2i <- substr(survey[ i-1, c("qgroup")] ,regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)+1,200 )
                                                                                           survey[ i, c("qgroup")] <- paste( substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1),
                                                                                                                             substr(level2i, 0, regexpr(".", level2i , ignore.case=FALSE, fixed=TRUE)-1),
                                                                                                                             sep=".")

    } else if(survey[ i, c("qlevel")]=="level3" && survey[ i-1, c("qlevel")]=="level2") { survey[ i, c("qgroup")] <-paste(survey[ i-1, c("qgroup")], survey[ i, c("name")],sep=".")
    } else if(survey[ i, c("qlevel")]=="level3" && survey[ i-1, c("qlevel")]=="level3") { survey[ i, c("qgroup")] <-survey[ i-1, c("qgroup")]
    } else if(survey[ i, c("qlevel")]=="level3" && survey[ i-1, c("qlevel")]=="level4") { level3i <- substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1)
                                                                                          level3ii <- substr(survey[ i-1, c("qgroup")] , regexpr(".", survey[ i-1, c("qgroup")], ignore.case=FALSE, fixed=TRUE)+1 ,500)
                                                                                          level3iii <-  substr(level3ii , regexpr(".", level3ii, ignore.case=FALSE, fixed=TRUE)+1 ,500)
                                                                                          survey[ i, c("qgroup")] <- paste( substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1),
                                                                                                                            substr(level3ii, 0, regexpr(".", level3ii , ignore.case=FALSE, fixed=TRUE)-1) ,
                                                                                                                            substr(level3iii, 0, regexpr(".", level3iii , ignore.case=FALSE, fixed=TRUE)-1) ,
                                                                                                                            sep=".")

    } else if( survey[ i, c("qlevel")]=="level4" && survey[ i-1, c("qlevel")]=="level3") { survey[ i, c("qgroup")] <-paste(survey[ i-1, c("qgroup")], survey[ i, c("name")],sep=".")
    } else if( survey[ i, c("qlevel")]=="level4" && survey[ i-1, c("qlevel")]=="level4") { survey[ i, c("qgroup")] <-survey[ i-1, c("qgroup")]
    } else if( survey[ i, c("qlevel")]=="level4" && survey[ i-1, c("qlevel")]=="level5") { level4i <- substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1)
                                                                                           level4ii <- substr(survey[ i-1, c("qgroup")] , regexpr(".", survey[ i-1, c("qgroup")], ignore.case=FALSE, fixed=TRUE)+1 ,500)
                                                                                           level4iii <-  substr(level4ii , regexpr(".", level4ii, ignore.case=FALSE, fixed=TRUE)+1 ,500)
                                                                                           level4iiii <-  substr(level4iii , regexpr(".", level4iii, ignore.case=FALSE, fixed=TRUE)+1 ,500)
                                                                                           survey[ i, c("qgroup")] <- paste( substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1),
                                                                                                                              substr(level4ii, 0, regexpr(".", level4ii , ignore.case=FALSE, fixed=TRUE)-1) ,
                                                                                                                              substr(level4iii, 0, regexpr(".", level4iii , ignore.case=FALSE, fixed=TRUE)-1) ,
                                                                                                                              substr(level4iiii, 0, regexpr(".", level4iiii , ignore.case=FALSE, fixed=TRUE)-1) ,
                                                                                                                              sep=".")


    } else if( survey[ i, c("qlevel")]=="level5" && survey[ i-1, c("qlevel")]=="level4") { survey[ i, c("qgroup")] <-paste(survey[ i-1, c("qgroup")], survey[ i, c("name")],sep=".")
    } else if( survey[ i, c("qlevel")]=="level5" && survey[ i-1, c("qlevel")]=="level5") { survey[ i, c("qgroup")] <-survey[ i-1, c("qgroup")]

    } else  {survey[ i, c("qgroup")]  <- survey[ i-1, c("qgroup")]}

    #survey[ 105, c("qlevel")]
    #survey[ 106, c("qlevel")]

    #cat(paste("pasted", as.character(survey[ i, c("qgroup")])))

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
  survey$order <- ""
  survey$weight <- ""
  survey$score <- ""
  survey$recategorise <- ""



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

  if("order" %in% colnames(choices))
  {
    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `order` in your survey worksheet.\n");
  } else
  {cat("Checking Data Analysis Plan within your xlsform -  No column `order` in your survey worksheet. Creating dummy one.\n");
    choices$order <- ""}

  if("weight" %in% colnames(choices))
  {
    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `weight` in your survey worksheet.\n");
  } else
  {cat("Checking Data Analysis Plan within your xlsform -  No column `weight` in your survey worksheet. Creating dummy one.\n");
    choices$weight <- ""}

  if("recategorise" %in% colnames(choices))
  {
    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `recategorise` in your survey worksheet.\n");
  } else
  {cat("Checking Data Analysis Plan within your xlsform -  No column `recategorise` in your survey worksheet. Creating dummy one.\n");
    choices$recategorise <- ""}

  if("score" %in% colnames(choices))
  {
    cat("Checking Data Analysis Plan within your xlsform -  Good: You have a column `score` in your survey worksheet.\n");
  } else
  {cat("Checking Data Analysis Plan within your xlsform -  No column `score` in your survey worksheet. Creating dummy one.\n");
    choices$score <- ""}

  choices <- choices[,c("listname",  "name",  "label", "order", "weight","score","recategorise")]
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


  #############################################################################################################
  #### Now Row bing questions & choices
  #
    #names(choices) -"type", "name", "namefull",  "labelfull", "listname", "qrepeat", "qlevel", "qgroup"
    ## not kept: "nameq"     "labelq"   ,"fullname", "label",
    #names(survey) - "type" "name",  "fullname", "label",  "listname", "qrepeat"m  "qlevel",   "qgroup"
  choices2 <- choices[ ,c("type", "name", "namefull",  "labelfull", "listname", "qrepeat", "qlevel", "qgroup", "labelchoice",
                         "repeatsummarize","variable","disaggregation", "report",
                         #"indicator","indicatorgroup","indicatortype", "indicatorlevel","dataexternal","indicatorcalculation","indicatornormalisation",
                         "order", "weight","score", "recategorise")]


  names(choices2)[names(choices2)=="namefull"] <- "fullname"
  names(choices2)[names(choices2)=="labelfull"] <- "label"


  survey2 <-    survey[,c("type", "name",  "fullname", "label",  "listname", "qrepeat",  "qlevel",   "qgroup", "labelchoice",
                          "repeatsummarize","variable","disaggregation", "report",
                          #"indicator","indicatorgroup","indicatortype", "indicatorlevel","dataexternal","indicatorcalculation","indicatornormalisation",
                          "order", "weight","score", "recategorise")]

  ### Check -- normally there should not be duplicate
  #choices3 <- choices2[!duplicated(choices2$fullname), ]

  # names(choices2)
  # names(survey2)

  survey2$formpart <- "questions"
  choices2$formpart <- "answers"

  dico <- rbind(survey2,choices2)


  ## Remove trailing space
  dico$fullname <- trim(dico$fullname)
  dico$listname <- trim(dico$listname)


  ## A few fix on the dico
  dico <- dico[ !is.na(dico$name), ]
  dico <- dico[ !is.na(dico$type), ]

  ## Exclude repeat questions -- still need more work
  #levels(as.factor(dico$qrepeat))
  ## Changing type for flatenned repeat questions

  #dico$type[dico$qrepeat=="repeat" & dico$type %in% c("integer")] <- "integerlist"
  #dico$type[dico$qrepeat=="repeat" & dico$type %in% c("text")] <- "textlist"
  #dico$type[dico$qrepeat=="repeat" & dico$type %in% c("select_one")] <- "select_onelist"

  #dico$type[dico$qrepeat=="repeat" & dico$type %in% c("select_one_d")] <- "integer"
  #dico$type[dico$qrepeat=="repeat" & dico$type %in% c("select_multiple")] <- "integer"

  #dico[dico$qrepeat=="repeat" & dico$type %in% c("select_multiple")]

  #if(dico$qrepeat=="repeat" && dico$type %in% c("select_one_d", "select_multiple")) {dico$type <- "integer"
  #                               cat("Note that select_one & select_multiple questions within REPEAT part are converted to integer (results are summed up).\n")
  #} else { dico$type <- dico$type
     cat("Note that select_one & select_multiple questions within REPEAT part are converted to integer (results are summed up).\n")

  write.csv(dico, paste0("data/dico_",form,".csv"), row.names=FALSE, na = "")

 # f_csv(dico)
#  return(dico)
}
NULL
