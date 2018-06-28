rm(list = ls())

## Load all required packages         #############################################
source("code/0-packages.R")
source("code/0-config.R")

### Double Check that you have the last version
#source("https://raw.githubusercontent.com/unhcr/koboloadeR/master/inst/script/install_github.R")
#install.packages("devtools")
#library("devtools")
#install_github("unhcr/koboloadeR")

library(koboloadeR)

## kobo_projectinit()
## Now Position your form & your data in the data folder



## Load form and building dictionnary #############################################
#rm(form)
#form <- "form.xls"
## Generate & Load dictionnary
cat("\n\n\n Generate dictionnary from the xlsform \n\n\n\n")
kobo_dico(form)
dico <- read.csv(paste("data/dico_",form,".csv",sep = ""), encoding = "UTF-8", na.strings = "")
#rm(form)


# Load data #######################################################################
cat("\n\n\n Load original dataset \n\n\n\n")
data.or <- read.csv(path.to.data, sep = ";", encoding = "UTF-8", na.strings = "")

## Account for case when separator is a coma....
if (ncol(data.or) == 1) {
  data.or <- read.csv(path.to.data, sep = ",", encoding = "UTF-8", na.strings = "") } else {
    cat("\n")
  }

#names(data.or)
### Need to replace slash by point in the variable name
## get variable name from data
#datalabel <- as.data.frame( names(data.or))
#names(datalabel)[1] <- "nameor"
#datalabel$nameor <- as.character(datalabel$nameor)

## new variables name without /
#datalabel$namenew <- str_replace_all(datalabel$nameor, "/", ".")
## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
#names(data.or) <- datalabel[, 2]


### Generate anonymisation report  ################################################
##Uncomment ot generate an anonymisation report
#kobo_anonymisation_report(data.or)


## Check to split select_multiple if data is extracted from ODK ###################
cat("\n\n\n Now split select_multiple  variables \n\n\n\n")
household <- kobo_split_multiple(data.or, dico)


## Clean variable if any ##########################################################
cat("\n\n\n Clean variable if any \n\n\n\n")
household <- kobo_clean(household, dico)

## Build anonymised version of the frame ##########################################
cat("\n\n\n Anonymise Household \n\n\n\n")
#kobo_anonymise(household, dico)

## Save preliminary version before encoding or adding indicators ##################
cat("\n\nWrite backup before encoding or indicators calculation..\n")
write.csv(household,"data/household.csv", row.names = FALSE, na = "")


## Compute indicators if defined ##################################################
source("code/2-create-indicators.R")
## Writing to a new version of the dico
write.csv(dico, paste0("data/dico_",form,"-indic.csv"), row.names = FALSE, na = "")

## Re-encoding data now based on the dictionnary -- ##############################
## the xlsform dictionnary can be adjusted this script re-runned till satisfaction
cat("\n\n\n Now  re-encode data  \n\n\n\n")
household <- kobo_encode(household, dico)


## Cheking the labels matching... #################################################
## household is the default root data componnents to be used -- in order to deal with nested dataset
cat("\n\n\n Now  labeling variables \n\n\n\n")
household <- kobo_label(household, dico)


## We now save a back up in the data folder to be used for the Rmd  ###############
cat("\n\nWrite backup ready for report generation \n")
write.csv(household,"data/data2.csv", row.names = FALSE, na = "")

