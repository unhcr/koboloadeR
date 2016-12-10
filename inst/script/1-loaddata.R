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
################################################################
## Special script to generate lables based on the data & form definition 
source("code/1-get-labels-from-form.R")
## Now we can apply better names & labels to the datframe
names(data) <- datalabel[ , 7]
attributes(data)$variable.labels <- datalabel[ , 15]
#names(data)


surveyindiccomm <- surveyindic
datacomm <- data
datalabelcomm <- datalabel
############################################################
#                                                          #
#                       Observation  data                  #
#                                                          #
############################################################


rm(dataobs)
data <- read.csv("data/dataobs.csv", encoding="UTF-8", na.strings="n/a")
rm(form_temp)
form_tmp <- "data/formobs.xls"
################################################################
## Special script to generate lables based on the data & form definition 
source("code/1-get-labels-from-form.R")
## Now we can apply better names & labels to the datframe
names(data) <- datalabel[ , 7]
attributes(data)$variable.labels <- datalabel[ , 15]
#names(data)
surveyindicobs <- surveyindic
dataobs <- data
datalabelobs <- datalabel




############################################################
#                                                          #
#         Code below is not relevant - no GPS loc          ####
#                                                          #
############################################################



################################################################
### Recognise date from format: "01-01-2009 03:41:25"
#str(data$end)
#data$start1 <- as.Date(as.character(data$start), "%d-%m-%Y %h:%m:%s")
#data$end1 <- as.Date(as.character(data$end), format = "%d-%m-%Y %h:%m:%s")

################################################################
####  extracting coordinates from dataset
#data$geo <- as.character(data$GPS_location)
#options(digits = 15)
#data$lat <- as.numeric(substr(data$geo , 1,13))
#data$long <- as.numeric(substr( data$geo, 15,27))

#str(data)

################################################################
### Set up the correct order for the factor variables so that graphs are correctly displayed



################################################################
### Eliminate record without coordinates & create a specific Dataframe for maps
#datasp <-data[!rowSums(is.na(data["lat"])), ]
