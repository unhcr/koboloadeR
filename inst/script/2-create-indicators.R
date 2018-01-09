
#########################
## Script to compute indicators

rm(list = ls())


mainDir <- getwd()
## Load all required packages
source(paste0(mainDir,"/code/0-packages.R"))
source(paste0(mainDir,"/code/0-config.R"))
library(koboloadeR)

## Load data & dico #############################################################################
#form <- "form.xls"
## Run this only after data cleaning
dico <- read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding = "UTF-8", na.strings = "")
household <- read.csv("data/household.csv", encoding = "UTF-8", na.strings = "NA")
#CaseInformation <- read.csv("data/CaseInformation.csv", encoding="UTF-8", na.strings="NA")
#IndividaulBioData <- read.csv("data/IndividaulBioData.csv", encoding="UTF-8", na.strings="NA")
#InformationNotRegFamilies <- read.csv("data/InformationNotRegFamilies.csv", encoding="UTF-8", na.strings="NA")


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
dicotemp$sensitive <- "trigger"
dicotemp$anonymise <- "trigger"
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
  # i <-2
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
  dicotemp1$sensitive <- indicator.sensitive
  dicotemp1$anonymise <- indicator.anonymise
  dicotemp1$listname <- indicator.listname
  dicotemp1$qrepeat <- " "
  dicotemp1$qrepeatlabel <- indicator.frame
  dicotemp1$qlevel <- " "
  dicotemp1$qgroup <- " "
  dicotemp1$labelchoice <- " "
  dicotemp1$variable <- " "
  dicotemp1$order <- " "
  dicotemp1$weight <- " "
  dicotemp1$score <- " "
  dicotemp1$recategorise <- " "
  dicotemp1$formpart <- " "
  dicotemp1$indic <- "feature"

  dicotemp <- rbind(dicotemp,dicotemp1)

}
## Append indicators in the dico  #############################################################################

dico$indic <- "data"
## removing first line
dicotemp <- dicotemp[ 2:nrow(dicotemp), ]

#names(dico)
#names(dicotemp)

dico <- rbind(dico,dicotemp)

rm(dicotemp,dicotemp1)

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
CaseInformation <- kobo_label(CaseInformation, dico)
IndividaulBioData <- kobo_label(IndividaulBioData, dico)

cat("\n\n Re-encoding now the data plus indicators based on the the full dictionnary\n")
household <- kobo_encode(household, dico)
CaseInformation <- kobo_encode(CaseInformation, dico)
IndividaulBioData <- kobo_encode(IndividaulBioData, dico)
InformationNotRegFamilies <- kobo_encode(InformationNotRegFamilies, dico)

cat("\n\nWrite backup\n")
write.csv(dico, paste0("data/dico2_",form,".csv"), row.names = FALSE, na = "")


write.csv(household, "data/household2.csv", row.names = FALSE, na = "")
#write.csv(CaseInformation, "data/CaseInformation2.csv", row.names = FALSE, na = "")
#write.csv(IndividaulBioData , "data/IndividaulBioData2.csv", row.names = FALSE, na = "")
#write.csv(InformationNotRegFamilies, "data/InformationNotRegFamilies2.csv", row.names = FALSE, na = "")

