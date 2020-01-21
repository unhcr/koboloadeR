########################
### Console Script #####
########################

library(koboloadeR)

#### Phase 1: Project Configurationn #############
## Configure name of the xlsform that exist under data folder
# - Change if required but better to keep the defautl one
form <- "form.xls"

## Extend xlsform with necessary column
kobo_prepare_form(form)

### Eventually Generate dummy data (uncomment below if required)
# kobo_dummy(form)

######################################################################################
### At this stage you can start working on your xlsform and fill it accordingly!!! ###
######################################################################################


#### Phase 2: Analysis Plan ###############################
kobo_check_analysis_plan(form)


#### Phase 3:  Load & process Data ###############################
kobo_load_data(form)



#### Phase 4: Generate Analysis Reports ###############################

##  Generate Crunching Report --> Describe
## For disaggregation of variable indicate "facet","stak", "fill" or "dodge"
## for test of correlation on select_one variable use correlation = TRUE
kobo_crunching_report(form)

## Generate Cluster Report  --> Discover
## Report will based on variable cluster == "TRUE" - you need to set one variable as "id"
## We use the root data frame -
# Assumption is that information from sub-hierachical frame was included through calculated indicators
MainDataFrame <- read.csv( "data/MainDataFrame_edited.csv")
kobo_cluster_report(frame = MainDataFrame, form)

## Generate Prediction Report  --> Predict

## Generate Scoring Report  --> Prescribe


#### Phase 5: Statistical Disclosure control & Indicator sharing ###############################
## Generate Anonymisation Report
## We use the root data frame -
# Assumption is that information from sub-hierachical frame was included through calculated indicators
MainDataFrame <- read.csv( "data/MainDataFrame_edited.csv")
kobo_anonymisation_report(frame = MainDataFrame, form)

