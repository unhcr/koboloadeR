mainDir <- getwd()
## Load all required packages
source(paste0(mainDir,"/code/0-packages.R"))
source(paste0(mainDir,"/code/0-config.R"))
library(koboloadeR)

## Load data & dico #############################################################################
#form <- "form.xls"
## Run this only after data cleaning
dico <- read.csv(paste0(mainDir,"/data/dico_",form,".csv"), encoding="UTF-8", na.strings="")
household <- read.csv(paste0(mainDir,"/data/household-clean-weight.csv"), encoding="UTF-8", na.strings="NA")
#household.back <- household
#case_number_details <- read.csv(paste0(mainDir,"/data/case_number_details.csv"), encoding="UTF-8", na.strings="NA")
#individual_biodata <- read.csv(paste0(mainDir,"/data/individual_biodata.csv"), encoding="UTF-8", na.strings="NA")


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
dicotemp$repeatsummarize <- "trigger"
dicotemp$variable <- "trigger"
dicotemp$order <- "trigger"
dicotemp$weight <- "trigger"
dicotemp$score <- "trigger"
dicotemp$recategorise <- "trigger"
dicotemp$formpart <- "trigger"
dicotemp$indic <- "feature"

####Load data analysis plan#############################################################################
#library(readxl)
#indicator <- read_excel("data/form.xls", sheet = "indicator")


## Load indicator info #############################################################################

for(i in 1:nrow(indicator))
{
  # i <-5
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
  cat(paste0(i, "- Load  indicator: ", indicator.label,"\n"))

  ## Build and run the formula to insert the indicator in the right frame  ###########################
  indic.formula <- paste0(indicator.frame,"$",indicator.fullname,"<-",indicator.calculation )
  if (file.exists("code/temp.R")) file.remove("code/temp.R")
  cat(indic.formula, file="code/temp.R" , sep="\n", append=TRUE)
  source("code/temp.R")
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
  dicotemp1$repeatsummarize <- " "
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
dico <- rbind(dico,dicotemp)

rm(dicotemp,dicotemp1)

### check indicator type
#household.check <- household[ , ((ncol(household.back)+1):ncol(household))]
#summary(household.check)
## label Variables
household.check <- kobo_label(household.check , dico)

## Check that the join is correct by looking at total HH members
#household$mf <- household$F +household$M
#household$adultchild <- household$adult  +household$child
#View(household[ , c("section2.total_hh", "mf", "adultchild")])

## label Variables
household <- kobo_label(household , dico)

cat("\n\nWrite backup\n")

write.csv(household, "data/household2.csv")
write.csv(case_number_details, "data/case_number_details2.csv")
write.csv(individual_biodata , "data/individual_biodata2.csv")

