rm(list = ls())


################################################################
## Load all required packages
source("code/0-packages.R")
library(koboloadeR)

## kobo_projectinit()

############################################################
#                                                          #
#   Position your form & your data in the data folder  
#                                                          #
############################################################

rm(data)

data.or <- read.csv("data/data.csv", sep=";", encoding="UTF-8", na.strings="n/a")

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
rm(form)
form <- "form.xls"


##########################################################################################
## Generate & Load dictionnary
kobo_dico(form)
dico <- read.csv(paste("data/dico_",form,".csv",sep=""), encoding="UTF-8", na.strings="")




#################################################################################
##### Re-encode correctly the dataset

data <- kobo_encode(data.or, dico)
data <- kobo_label(data, dico)
#########################################################################################
## Produce graphs of all select_one questions
kobo_bar_one(data,dico)

#########################################################################################
## Produce graphs of all select_multiple questions
kobo_bar_multi(data,dico)


#########################################################################################
## Produce histogramme for all numeric variable
kobo_histo(data,dico)

#########################################################################################
## Produce graphs based on date

kobo_trend(data,"date",dico)

########################################################################################
### Produce faceted chart select_one

kobo_bar_one_facet(data,dico)

########################################################################################
### Produce correlation

kobo_correlation(data,  dico)


########################################################################################
### Produce boxplot
kobo_boxplot_facet(data,  dico)


#################################################################################
## Generating maps
xmax <- 27
xmin <- 16
ymax <- 34
ymin <- 21

#35.903519, 32.039009, 35.87809, 31.984039,
#39.3012981,34.8844372,33.3751558,29.1850356,

kobo_map_int(data,xmax,xmin,ymax,ymin, dico)
kobo_map_cat(data,xmax,xmin,ymax,ymin, dico)