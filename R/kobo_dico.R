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
#' @examples
#' \dontrun{
#' kobo_dico("myform.xls")
#' }
#'
#' @export kobo_dico
#'

kobo_dico <- function(form) {

  #kobo_form(formid, user = user, api = api)
  cat("\n Your form should be placed within the `data` folder. \n \n")
  # read the survey tab of ODK from
  form_tmp <- paste0("data/",form)


  ### First review all questions from survey sheet #################################################
  survey <- read_excel(form_tmp, sheet = "survey")

  ## Rename the variable label
  names(survey)[names(survey) == "label::English"] <- "label"
  names(survey)[names(survey) == "label::english"] <- "label"
  cat("Checking now for additional information within your xlsform. Note that you can insert them in the xls and re-run the function! \n \n ")



  ### add column if not present #################################################
  if ("disaggregation" %in% colnames(survey))
  {
  cat("1- Good: You have a column `disaggregation` in your survey worksheet.\n");
  } else
  {cat("1- No column `disaggregation` in your survey worksheet. Creating a dummy one for the moment...\n");
    survey$disaggregation <- ""}

  if ("correlate" %in% colnames(survey))
  {
    cat("2- Good: You have a column `correlate` in your survey worksheet. This will be used to define the variables that should be checked for correlation between each others.\n");
  } else
  {cat("2- No column `correlate` in your survey worksheet. Creating a dummy one for the moment...\n");
    survey$correlate <- ""}

  if ("chapter" %in% colnames(survey))
  {
    cat("3- Good: You have a column `chapter` in your survey worksheet. This will be used to breakdown the generated report\n");
  } else
  {cat("3- No column `chapter` in your survey worksheet. Creating a dummy one for the moment ...\n");
    survey$chapter <- ""}

  if ("structuralequation" %in% colnames(survey))
  {
    cat("4- Good: You have a column `structuralequation` in your survey worksheet. This will be used to configure the vulnerability structural equation model\n");
  } else
  {cat("4- No column `structuralequation` in your survey worksheet. Creating a dummy one for the moment...\n");
    survey$structuralequation <- ""}


  if ("anonymise" %in% colnames(survey))
  {
    cat("5- Good: You have a column `anonymise` in your survey worksheet. This will be used to anonymise the dataset.\n");
  } else
  {cat("5- No column `anonymise` in your survey worksheet. Creating a dummy one for the moment filled as `non-anonymised`. Other options to record are `Remove`, `Reference`, `Mask`, `Generalise` (see readme file) ...\n");
    survey$anonymise <- "default-non-anonymised"}

  if ("variable" %in% colnames(survey))
  {
    cat("6- Good: You have a column `variable` in your survey worksheet. This will be used to flag ordinal variable.\n");
  } else
  {cat("6- No column `variable` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
    survey$variable <- ""}

  ## Adding 	clean cluster	predict
  if ("clean" %in% colnames(survey))
  {
    cat("7- Good: You have a column `clean` in your survey worksheet. This will be used to flag variables that shoudl be clean with kobo_clean function.\n");
  } else
  {cat("7- No column `clean` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
    survey$clean <- "no"}

  if ("cluster" %in% colnames(survey))
  {
    cat("8- Good: You have a column `cluster` in your survey worksheet. This will be used to flag variables to be used for clustering exploration.\n");
  } else
  {cat("8- No column `cluster` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
    survey$cluster <- ""}

  if ("predict" %in% colnames(survey))
  {
    cat("9- Good: You have a column `predict` in your survey worksheet. This will be used to flag variables to be used for clustering exploration.\n");
  } else
  {cat("9- No column `predict` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
    survey$predict <- ""}

  if ("mappoint" %in% colnames(survey))
  {
    cat("10- Good: You have a column `mappoint` in your survey worksheet. This will be used to flag variables to be used for clustering exploration.\n");
  } else
  {cat("10- No column `mappoint` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
    survey$mappoint <- ""}

  if ("mappoly" %in% colnames(survey))
  {
    cat("11- Good: You have a column `mappoly` in your survey worksheet. This will be used to flag variables to be used for clustering exploration.\n");
  } else
  {cat("11- No column `mappoly` in your survey worksheet. Creating a dummy one for the moment (see readme file). ...\n");
    survey$mappoly <- ""}

  ## Avoid columns without names
  survey <- survey[ ,c("type",   "name" ,  "label",
                       #"repeatsummarize",
                       "variable","disaggregation",  "chapter", "structuralequation","anonymise","correlate","clean","cluster","predict","mappoint","mappoly"
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
  cat(" \n Now extracting list name from questions type.\n \n")
  survey$listname <- ""


  ## Extract for select_one
  survey$listname <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type) ,
                                         paste0( substr(survey$type , (regexpr("select_one", survey$type , ignore.case=FALSE, fixed=TRUE))+10,250)),survey$listname))

  survey$type <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_one"),survey$type))

  ## Extract for select multiple & clean type field
  survey$listname <- with(survey,  ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type),
                                          paste0( substr(survey$type , (regexpr("select_multiple", survey$type , ignore.case=FALSE, fixed=TRUE))+16,250)),survey$listname ))


  survey$type <- with(survey, ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_multiple_d"),survey$type))

  ## handle case where we have "or_other"
  #survey$listname <- with(survey, ifelse(grepl("or_other", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$listname) ,
  #                                       paste0( substr(survey$listname , 1, (nchar(survey$listname)-8 ))),survey$listname))

  ## handle case where we have "or_other"
  survey$listname <- with(survey, ifelse(grepl("or_other", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$listname) ,
                                         paste0( substr(survey$listname , 1, (nchar(survey$listname)-8 ))),
                                         survey$listname))

  ## Remove trailing space
  survey$listname <- trim(survey$listname)
  survey$label <- trim(survey$label)
  #str(survey)

  ## Now creating full name in order to match with data variables name

  ### identify Repeat questions with nest levels
  cat("\n Be careful! The current function only support 2 levels of nested repeat - for instance household / Case / Individual. \n \n")
  survey$qrepeat <- ""
  for(i in 2:nrow(survey))
  {
    #Check based on repeat type
         if (survey[ i, c("type")] %in% c("begin repeat","begin_repeat") && survey[ i - 1, c("qrepeat")] == "")                  {survey[ i, c("qrepeat")]  <- "repeatnest1"}
    else if (survey[ i, c("type")] %in% c("begin repeat","begin_repeat") && survey[ i - 1, c("qrepeat")] == "repeatnest1")       {survey[ i, c("qrepeat")]  <-  "repeatnest2"}
    else if (!(survey[ i, c("type")] %in% c("end repeat","end_repeat"))  && survey[ i - 1, c("qrepeat")] == "repeatnest1")       {survey[ i, c("qrepeat")]  <-  "repeatnest1"}
    else if (!(survey[ i, c("type")] %in% c("end repeat","end_repeat"))  && survey[ i - 1, c("qrepeat")] == "repeatnest2")       {survey[ i, c("qrepeat")]  <-  "repeatnest2"}
    else if (survey[ i, c("type")] %in% c("end repeat","end_repeat")     && survey[ i - 1, c("qrepeat")] == "repeatnest1" )      {survey[ i, c("qrepeat")]  <-  ""}
    else if (survey[ i, c("type")] %in% c("end repeat","end_repeat")     && survey[ i - 1, c("qrepeat")] == "repeatnest2" )      {survey[ i, c("qrepeat")]  <-  "repeatnest1"}

    else   {survey[ i, c("qrepeat")]  <-  ""}
  }

  ### identify Repeat questions
  survey$qrepeatlabel <- "household"
  nestable <- survey[survey$type %in% c("begin_repeat","begin repeat") , c("name","qrepeat","type")]
  nestable$name <- as.character(nestable$name)
  for(i in 2:nrow(survey))
  {
    # Now insert the repeat label based on name
    # i <-230
         if ( survey[ i, c("type")] == "begin repeat" )                                                {survey[ i, c("qrepeatlabel")]  <- survey[ i, c("name")]}
    else if ( survey[ i, c("type")] !="end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }
    else if ( survey[ i, c("type")] !="end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }
    else if ( survey[ i, c("type")] == "end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1")    {survey[ i, c("qrepeatlabel")]  <-  "household"}
    else if ( survey[ i, c("type")] == "end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2")    { nestabove <- as.character(survey[ i - 1, c("qrepeatlabel")])
                                                                                                       nestabovenum <- as.integer(which(nestable$name == nestabove ) -1)
                                                                                                      survey[ i, c("qrepeatlabel")]  <-  as.character( nestable[ nestabovenum , 1] ) }

    ## Sometimes it seems that we get an underscore for type
    else if ( survey[ i, c("type")] == "begin_repeat" )                                                {survey[ i, c("qrepeatlabel")]  <- survey[ i, c("name")]}
    else if ( survey[ i, c("type")] !="end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }
    else if ( survey[ i, c("type")] !="end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }
    else if ( survey[ i, c("type")] == "end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1")    {survey[ i, c("qrepeatlabel")]  <-  "household"}
    else if ( survey[ i, c("type")] == "end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2")    { nestabove <- as.character(survey[ i - 1, c("qrepeatlabel")])
                                                                                                        nestabovenum <- as.integer(which(nestable$name == nestabove ) -1)
                                                                                                        survey[ i, c("qrepeatlabel")]  <-  as.character( nestable[ nestabovenum , 1] ) }

    else   {survey[ i, c("qrepeatlabel")]  <-  "household"}
  }

  ### Get question levels in order to match the variable name
  survey$qlevel <- ""
  for(i in 2:nrow(survey))
  {      if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "" )      {survey[ i, c("qlevel")]  <-  "level1"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "" )      {survey[ i, c("qlevel")]  <-  "level1"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")]  <-  "level2"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")]  <-  "level2"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level3"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level3"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level4"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level4"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level5"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level5"}

    ## Now end of group

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")] <- "" }
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")] <- "" }

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level1"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level1"}

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level2"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level2"}

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level3"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level3"}

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level5") {survey[ i, c("qlevel")]  <-  "level4"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level5") {survey[ i, c("qlevel")]  <-  "level4"}

    else   {survey[ i, c("qlevel")]  <-  survey[ i - 1, c("qlevel")]}
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
            if (survey[ i, c("qlevel")]  %in% c("level1","level2","level3","level4","level5") &&
               survey[ i, c("qrepeat")] %in% c("", "repeatnest1", "repeatnest2") &&
              !(survey[ i, c("type")]   %in% c("begin_group","begin group","end_group","end group","begin_repeat","begin repeat","end_repeat","end repeat")) )

      {survey[ i, c("qgroup")] <- survey[ i - 1, c("qgroup")]


    } else if (survey[ i, c("qlevel")]   %in% c("level1") &&
              survey[ i, c("qrepeat")]  %in% c("", "repeatnest1", "repeatnest2") &&
              survey[ i, c("type")]     %in% c("begin_group","begin group")  )

       {survey[ i, c("qgroup")] <- survey[ i, c("name")]

    } else if (survey[ i, c("qlevel")]   %in% c("level2","level3","level4","level5") &&
              survey[ i, c("qrepeat")]  %in% c("", "repeatnest1", "repeatnest2") &&
              survey[ i, c("type")]     %in% c("begin_group","begin group") )

       {survey[ i, c("qgroup")] <- paste(survey[ i - 1, c("qgroup")], survey[ i, c("name")],sep = ".")

    } else if (survey[ i, c("qlevel")]   %in% c("level1","level2","level3","level4","level5")  &&
              survey[ i, c("qrepeat")]  %in% c("repeatnest1", "repeatnest2") &&
              survey[ i, c("type")]     %in% c("begin_repeat","begin repeat")   )

      {survey[ i, c("qgroup")] <- paste(survey[ i - 1, c("qgroup")], survey[ i, c("qrepeatlabel")], sep = ".")

    } else if (survey[ i, c("qlevel")]   %in% c("level1","level2","level3","level4","level5") &&
              survey[ i, c("qrepeat")]  %in% c("", "repeatnest1", "repeatnest2") &&
              survey[ i, c("type")]     %in% c("end_group","end group","end_repeat","end repeat") )

       {survey[ i, c("qgroup")] <- substr(survey[ i - 1, c("qgroup")] ,0, regexpr("\\.[^\\.]*$", survey[ i - 1, c("qgroup")] ) - 1)

    } else  {survey[ i, c("qgroup")]  <- ""}
  }


  survey$fullname <- ""
  ## levels(as.factor(survey$type))
  ## Need to loop around the data frame in order to concatenate full name as observed in data dump
  survey[ 1, c("fullname")]  <-  survey[ 1, c("name")]
  for (i in 2:nrow(survey))
  {
    if (survey[ i, c("qlevel")] == "") {survey[ i, c("fullname")]  <-  survey[ i, c("name")]}
    else {survey[ i, c("fullname")]  <-  paste(survey[ i, c("qgroup")],survey[ i, c("name")],sep = ".") }
  }

  ## a few colummns to adjust to match questions & choices
  survey$labelchoice <- survey$label
  survey$order <- ""
  survey$weight <- ""
  survey$score <- ""
  survey$recategorise <- ""



  ####
  #### Now looking at choices --#########################################################################################################
  #rm(choices)
  choices <- read_excel(form_tmp, sheet = "choices")
  names(choices)[names(choices) == "label::English"] <- "label"
  names(choices)[names(choices) == "label::english"] <- "label"
  names(choices)[names(choices) == "list name"] <- "listname"
  names(choices)[names(choices) == "list_name"] <- "listname"

  ## Remove trailing space
  choices$listname <- trim(choices$listname)
  choices$label <- trim(choices$label)

  if ("order" %in% colnames(choices))
  {
    cat("12 -  Good: You have a column `order` in your `choices` worksheet.\n");
  } else
  {cat("12 -  No column `order` in your `choices` worksheet. Creating a dummy one for the moment...\n");
    choices$order <- ""}

  if ("weight" %in% colnames(choices))
  {
    cat("13 -  Good: You have a column `weight` in your `choices` worksheet.\n");
  } else
  {cat("13 -  No column `weight` in your `choices` worksheet. Creating a dummy one for the moment...\n");
    choices$weight <- ""}

  if ("recategorise" %in% colnames(choices))
  {
    cat("14 -  Good: You have a column `recategorise` in your `choices` worksheet.\n");
  } else
  {cat("14 -  No column `recategorise` in your `choices` worksheet. Creating a dummy one for the moment...\n");
    choices$recategorise <- ""}

  if ("score" %in% colnames(choices))
  {
    cat("13 -  Good: You have a column `score` in your `choices` worksheet.\n");
  } else
  {cat("13 -  No column `score` in your `choices` worksheet. Creating a dummy one for the moment...\n");
    choices$score <- ""}

  choices <- choices[,c("listname",  "name",  "label", "order", "weight","score","recategorise")]
  names(choices)[names(choices) == "label"] <- "labelchoice"
  #rm(choices)
  choices <- join(x = choices, y = survey, by = "listname", type = "left")

  choices$type <- with(choices, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices$type),
                                       paste0("select_one_d"),choices$type))

  choices$type <- with(choices, ifelse(grepl("select_multiple_d", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices$type),
                                       paste0("select_multiple"),choices$type))


  names(choices)[9] <- "nameq"
  names(choices)[10] <- "labelq"
  choices$labelfull <- paste0(choices$labelq, sep = ": ", choices$labelchoice)
  choices$namefull <- paste0(choices$fullname, sep = ".", choices$name)



  #### Now Row bing questions & choices########################################################################################################
  #
    #names(choices) -"type", "name", "namefull",  "labelfull", "listname", "qrepeat", "qlevel", "qgroup"
    ## not kept: "nameq"     "labelq"   ,"fullname", "label",
    #names(survey) - "type" "name",  "fullname", "label",  "listname", "qrepeat"m  "qlevel",   "qgroup"
  choices2 <- choices[ ,c("type", "name", "namefull",  "labelfull", "chapter","disaggregation","correlate", "structuralequation","anonymise",
                          "clean","cluster","predict","mappoint","mappoly",
                          "listname", "qrepeat","qrepeatlabel",  "qlevel", "qgroup", "labelchoice",
                         #"repeatsummarize",
                         "variable",
                         #"indicator","indicatorgroup","indicatortype", "indicatorlevel","dataexternal","indicatorcalculation","indicatornormalisation",
                         "order", "weight","score", "recategorise")]


  names(choices2)[names(choices2) == "namefull"] <- "fullname"
  names(choices2)[names(choices2) == "labelfull"] <- "label"


  survey2 <-    survey[,c("type", "name",  "fullname", "label", "chapter", "disaggregation","correlate",  "structuralequation","anonymise",
                          "clean","cluster","predict","mappoint","mappoly",
                          "listname", "qrepeat","qrepeatlabel",  "qlevel",   "qgroup", "labelchoice",
                          #"repeatsummarize",
                          "variable",

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


  ## Trim long label...
  dico$label <- substring(dico$label, 0, 85)


  ## A few fix on the dico
  dico <- dico[ !is.na(dico$name), ]
  dico <- dico[ !is.na(dico$type), ]

  ## Exclude repeat questions -- still need more work
  #levels(as.factor(dico$qrepeat))
  ## Changing type for flatenned repeat questions

  #dico$type[dico$qrepeat== "repeat" & dico$type %in% c("integer")] <- "integerlist"
  #dico$type[dico$qrepeat== "repeat" & dico$type %in% c("text")] <- "textlist"
  #dico$type[dico$qrepeat== "repeat" & dico$type %in% c("select_one")] <- "select_onelist"

  #dico$type[dico$qrepeat== "repeat" & dico$type %in% c("select_one_d")] <- "integer"
  #dico$type[dico$qrepeat== "repeat" & dico$type %in% c("select_multiple")] <- "integer"

  #dico[dico$qrepeat== "repeat" & dico$type %in% c("select_multiple")]

  #if (dico$qrepeat== "repeat" && dico$type %in% c("select_one_d", "select_multiple")) {dico$type <- "integer"
  #                               cat("Note that select_one & select_multiple questions within REPEAT part are converted to integer (results are summed up).\n")
  #} else { dico$type <- dico$type
   #  cat("Note that select_one & select_multiple questions within REPEAT part are converted to integer (results are summed up).\n")

  write.csv(dico, paste0("data/dico_",form,".csv"), row.names = FALSE, na = "")

 # f_csv(dico)
#  return(dico)
}
NULL

