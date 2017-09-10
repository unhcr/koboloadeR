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


#### 3. data cleaning *********************
## Might need to be tweaked -- double check
#path.to.data.update <- "data/datadelete.csv"
## Might need to be tweaked -- double check
#path.to.data.delete <- "data/datadelete.csv"


#### 4. sample weight *********************
## Might need to be tweaked -- double check
#path.to.weight <- "data/dataweight.csv"


#### 5. Indicator *********************
## Might need to be tweaked -- double check
#library(readxl)
#indicator <- read_excel("data/form.xls", sheet = "indicator")


