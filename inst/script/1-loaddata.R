rm(list = ls())


################################################################
## Load all required packages
source("code/0-packages.R")

############################################################
#                                                          #
#                   Key Informant Data                     #
#                                                          #
############################################################

rm(data)
data <- read.csv("data/data.csv", encoding="UTF-8", na.strings="n/a")
rm(form_temp)
form_tmp <- "data/form.xls"

