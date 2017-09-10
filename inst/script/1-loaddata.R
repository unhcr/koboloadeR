rm(list = ls())


################################################################
## Load all required packages
source("code/0-packages.R")
source("code/0-config.R")

### Double Check that you have the last version
#source("https://raw.githubusercontent.com/Edouard-Legoupil/koboloadeR/master/inst/script/install_github.R")
#install.packages("devtools")
#library("devtools")
#install_github("Edouard-Legoupil/koboloadeR")

library(koboloadeR)

## kobo_projectinit()

############################################################
#                                                          #
#   Position your form & your data in the data folder
#                                                          #
############################################################

#rm(data)

## Might need to be tweaked -- double check
data.or <- read.csv(path.to.data, sep=";", encoding="UTF-8", na.strings="")

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

##############################################
## Load form
#rm(form)
#form <- "form.xls"
## Generate & Load dictionnary
kobo_dico(form)
dico <- read.csv(paste("data/dico_",form,".csv",sep = ""), encoding = "UTF-8", na.strings = "")
#rm(form)


#################################################################################
##### Re-encode correctly the dataset

## Check to split select_multiple if data is extracted from ODK
data <- kobo_split_multiple(data.or, dico)
## Re-encoding data now based on the dictionnary -- the xlsform dictionnary can be adjusted this script re-runned till satisfaction
data <- kobo_encode(data, dico)

## household is the default root data componnents to be used -- in order to deal with nested dataset
household <- kobo_label(data, dico)
## We now save a back up in the data folder to be used for the Rmd
write.csv(household,"data/data2.csv")

## Save another version in order to add indicators
write.csv(household,"data/household.csv")
