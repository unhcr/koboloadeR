#' @name kobo_dico
#' @rdname kobo_dico
#' @title  Produce a data dictionnary based on the xlsform for the project
#'
#'
#'
#' @param formname The filename of the form to be accessed (xls file).
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
#' kobo_dico("15051")
#' kobo_dico("31511", user = userpwd, api = "unhcr", check = TRUE)
#' }
#'
#' @export kobo_dico
kobo_dico <- function(form) {
  
  #kobo_form(formid, user = user, api = api)

  # read the survey tab of ODK from
  form_tmp <- paste0("data/",form,".xls")
  
  ###############################################################################################
  ### First review all questions first
  survey <- read_excel(form_tmp, sheet = "survey")
  #survey <-readWorksheet(loadWorkbook(form_tmp,sheet="survey",check.names=F))
  
  ### Now opening & parsing the form
  #surveyindic <- survey[ , c( "name", "indicator","indicatordirect", "indicatorgroup")]
  #surveyindic <-surveyindic[!(is.na(surveyindic$indicator)), ]
  #surveyindic <-surveyindic[!rowSums(is.na(surveyindic["indicator"])), ]
  
  ## Rename the variable label
  names(survey)[names(survey)=="label::English"] <- "label"
  
  ## Avoid columns without names
  survey <- survey[ ,c("type",   "name" ,  "label"
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
  survey$listname <- ""
  ## Extract for select_one 
  survey$listname <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type) ,                                        
                                         paste0( substr(survey$type , (regexpr("select_one", survey$type , ignore.case=FALSE, fixed=TRUE))+10,250)),survey$listname))
  survey$type <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_one"),survey$type))
  
  ## Extract for select multiple & clean type field
  survey$listname <- with(survey,  ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type),
                                          paste0( substr(survey$type , (regexpr("select_multiple", survey$type , ignore.case=FALSE, fixed=TRUE))+16,250)),survey$listname ))
  survey$type <- with(survey, ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_multiple_d"),survey$type))
  

  ## Remove space
  survey$listname <- trim(survey$listname)
  #str(survey)
  
  ## Now creating full name in order to match with data variables name

  ### identify Repeat questions
  survey$qrepeat <- ""
  for(i in 2:nrow(survey))
  { 
    if(survey[ i, c("type")] =="begin repeat")   {survey[ i, c("qrepeat")]  <-  "repeat"}
    else if(survey[ i, c("type")] =="begin_repeat")   {survey[ i, c("qrepeat")]  <-  "repeat"}
    else if(survey[ i-1, c("qrepeat")]=="repeat" && survey[ i, c("type")] !="end repeat") {survey[ i, c("qrepeat")]  <-  "repeat"}
    else if(survey[ i-1, c("qrepeat")]=="repeat" && survey[ i, c("type")] !="end_repeat") {survey[ i, c("qrepeat")]  <-  "repeat"}
    else if(survey[ i, c("type")] =="end repeat" ) {survey[ i, c("qrepeat")]  <-  ""}
    else if(survey[ i, c("type")] =="end_repeat" ) {survey[ i, c("qrepeat")]  <-  ""}
    else   {survey[ i, c("qrepeat")]  <-  ""}
  }  
  
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
  survey$qgroup <- ""
  for(i in 2:nrow(survey))
  { 
    if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="begin group" )    {survey[ i, c("qgroup")] <-survey[ i, c("name")]}
    else if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="begin_group" )    {survey[ i, c("qgroup")] <-survey[ i, c("name")]}
    
    else if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="end group" ) {survey[ i, c("qgroup")] <-substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1)}
    else if(survey[ i, c("qlevel")]=="level1" && survey[ i, c("type")] =="end_group" ) {survey[ i, c("qgroup")] <-substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1)}
    
    else if( survey[ i, c("qlevel")]=="level2" && survey[ i-1, c("qlevel")]=="level1") {survey[ i, c("qgroup")] <-paste(survey[ i-1, c("qgroup")], survey[ i, c("name")],sep=".")}
    else if( survey[ i, c("qlevel")]=="level2" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qgroup")] <-survey[ i-1, c("qgroup")]}
    else if( survey[ i, c("qlevel")]=="level2" && survey[ i-1, c("qlevel")]=="level3") {rm(level2i)
                                                                                        level2i <- substr(survey[ i-1, c("qgroup")] ,regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)+1,200 )
                                                                                        survey[ i, c("qgroup")] <- paste( substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1),
                                                                                          substr(level2i,0, regexpr(".", level2i , ignore.case=FALSE, fixed=TRUE)-1), sep=".")}
    
    else if( survey[ i, c("qlevel")]=="level3" && survey[ i-1, c("qlevel")]=="level2") {survey[ i, c("qgroup")] <-paste(survey[ i-1, c("qgroup")], survey[ i, c("name")],sep=".")}
    else if( survey[ i, c("qlevel")]=="level3" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qgroup")] <-survey[ i-1, c("qgroup")]}
    else if( survey[ i, c("qlevel")]=="level3" && survey[ i-1, c("qlevel")]=="level4") {
                                                                                        level3i <- substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1)
                                                                                        survey[ i, c("qgroup")] <- substr(level3i,0, regexpr(".", level3i , ignore.case=FALSE, fixed=TRUE)-1)}
    
    else if( survey[ i, c("qlevel")]=="level4" && survey[ i-1, c("qlevel")]=="level3") {survey[ i, c("qgroup")] <-paste(survey[ i-1, c("qgroup")], survey[ i, c("name")],sep=".")}
    else if( survey[ i, c("qlevel")]=="level4" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qgroup")] <-survey[ i-1, c("qgroup")]}
    else if( survey[ i, c("qlevel")]=="level4" && survey[ i-1, c("qlevel")]=="level5") {level4i <- substr(survey[ i-1, c("qgroup")] ,0, regexpr(".", survey[ i-1, c("qgroup")] , ignore.case=FALSE, fixed=TRUE)-1)
                                                                                        survey[ i, c("qgroup")] <- substr(level4i,0, regexpr(".", level4i , ignore.case=FALSE, fixed=TRUE)-1)}
    
    else if( survey[ i, c("qlevel")]=="level5" && survey[ i-1, c("qlevel")]=="level4") {survey[ i, c("qgroup")] <-paste(survey[ i-1, c("qgroup")], survey[ i, c("name")],sep=".")}
    else if( survey[ i, c("qlevel")]=="level5" && survey[ i-1, c("qlevel")]=="level5") {survey[ i, c("qgroup")] <-survey[ i-1, c("qgroup")]}
    
    else  {survey[ i, c("qgroup")]  <- survey[ i-1, c("qgroup")]}
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
  
  
  
  
  #############################################################################################################
  #### Now looking at choices --
  choices <- read_excel(form_tmp, sheet = "choices")
  names(choices)[names(choices)=="label::English"] <- "label"
  names(choices)[names(choices)=="list name"] <- "listname"
  names(choices)[names(choices)=="list_name"] <- "listname"
  
  choices <- choices[,c("listname",   "name" ,  "label")]
  #rm(choices1)
  choices1 <- join(x=choices, y=survey, by="listname", type="inner")
  choices1$type <- with(choices1, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices1$type), paste0("select_one_d"),choices1$type))
  choices1$type <- with(choices1, ifelse(grepl("select_multiple_d", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices1$type), paste0("select_mutiple"),choices1$type))
  
  names(choices1)[5] <- "nameq"
  names(choices1)[6] <- "labelq"
  choices1$labelfull <- paste0(choices1$labelq, sep = ": ", choices1$label)
  choices1$namefull <- paste0(choices1$fullname, sep = ".", choices1$name)
  
  #############################################################################################################
  #### Now Row bing questions & choices 
  # 
    #names(choices1) -"type", "name", "namefull",  "labelfull", "listname", "qrepeat", "qlevel", "qgroup"     
    ## not kept: "nameq"     "labelq"   ,"fullname""label",
    #names(survey) - "type" "name",  "fullname", "label",  "listname", "qrepeat"m  "qlevel",   "qgroup"  
  choices2 <- choices1[,c("type", "name", "namefull",  "labelfull", "listname", "qrepeat", "qlevel", "qgroup")]
  survey2 <-    survey[,c("type", "name",  "fullname", "label",  "listname", "qrepeat",  "qlevel",   "qgroup")]
  
  names(choices2)[names(choices2)=="namefull"] <- "fullname"
  names(choices2)[names(choices2)=="labelfull"] <- "label"
  
  ### Check -- normally there should not be duplicate
  #choices3 <- choices2[!duplicated(choices2$fullname), ]
  
  dico <- rbind(survey2,choices2)
  
  write.csv (dico, paste0("data/dico_",form,".csv"))
  
  ## get variable name from data
 # rm(datalabel)
 # datalabel <- as.data.frame( names(data))
 # names(datalabel)[1] <- "nameor"
  
 # f_csv(dico)
  return(dico)
}
NULL