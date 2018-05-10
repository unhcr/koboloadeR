#### Config file ###

### Can be manualy edited or interactively rewritten using the function kobo_projectconfig()

### 1. Place the form in xslform format - saved as .xls - not xlsx - in the data folder ######

form <- "form.xls"

#### 2. flat file load *********************

## Might need to be tweaked -- double check
path.to.data <- "data/data.csv"

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

###### 2a. case of nested dataframe

### We will set all configuration within a dataframe

## 1st column: name is the name of the dataframe - per convention the first one is call household
## 2nd column: path is the path to the csv file - extracted with ODK briefcase
## 3rd column: sep is the type of the separator used in the header format - default is '.' - if not '.' it will be converted
## 4rd column: framid is the name of the unique ID for the frame
## 6th column:parent is the name of the parent frame
## 7th column:parentid is the name of the corresponding ID to be used for the join in the parent frame
## 8th column: suffix string used if nested frame header does not include full xml path

frame1 <- c("name" = "",
            "path" = "",
            "sep" = "",
            "framid" = "",
            "parent" = "",
            "parentid" = "",
            "suffix" = ""  )

frame2 <- c("name" = "",
            "path" = "",
            "sep" = "",
            "framid" = "",
            "parent" = "",
            "parentid" = "",
            "suffix" = ""  )

frame3 <- c("name" = "",
            "path" = "",
            "sep" = "",
            "framid" = "",
            "parent" = "",
            "parentid" = "",
            "suffix" = ""  )

# path.to.data <- rbind(t(frame1), t(frame2), t(frame3))

#### 3. data cleaning *********************
cleaning.log <- "no"

#############################
### if cleaning.log <- "yes" please document the following
## path.to.cleaning.script <- "code/cleanscript.R"
##  path.to.data.update <- "data/datadelete.csv"
##  path.to.data.delete <- "data/datadelete.csv"


#### 4. sample weight *********************
sample.weight <- "no"

#############################
### if sample.weight <- "yes" please document the following
# path.to.weight <- "data/dataweight.csv"
# id.to.join.weight <- "weight.id"
# sampling.type <-  ## take numeric value 1:cluster sampling 2: stratified sampling

#### 5. Indicator *********************
indicator.calculation <- "no"

#############################
### if indicator.calculation <- "yes" please document the following
#library(readxl)
#indicator <- read_excel("data/form.xls", sheet = "indicator")

