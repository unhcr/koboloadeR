rm(list = ls())


################################################################
## Load all required packages
source("code/0-packages.R")
library(koboloadeR)

############################################################
#                                                          #
#                   Key Informant Data                     #
#                                                          #
############################################################

rm(data)

data <- read.csv("data/data.csv", sep=";", encoding="UTF-8", na.strings="n/a")
rm(form_temp)
form <- "form.xls"

kobo_dico(form)

##########################################################################################
## Load dictionnary
dico <- read.csv("data/dico_form.xls.csv", encoding="UTF-8", na.strings="")


dico <- dico[ !is.na(dico$name), ]

## Exclude repeat questions -- still need more work
dico <- dico[ is.na(dico$qrepeat), ]

#################################################################################
##### Re-encode correctly the dataset

data <- kobo_encode(data, dico)
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