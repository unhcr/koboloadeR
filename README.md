# koboloadeR

The `koboloadeR` package allows to:

* connect to the [KoBo API (v1)](https://kc.kobotoolbox.org/api/v1/) for the [KoBo Toolbox](http://www.kobotoolbox.org/) project. 

* compute a data dictionnary based on [xlsform](http://xlsform.org). It implies ot have a few additionnal column in the xlsform in order to better define how data shoudl be analysid (cf infra). as it based on a standard, this part and the following should work for any [xlsform compatible server](http://xlsform.org/#xlsform-tools) such as [OpenDataKit](https://opendatakit.org/) or [ONA](https://ona.io/home/)

* generate automatically of a series of charts & maps based on the data dictionnary

* access to a Shiny data viewer accessible using:

```
kobo_apps("data_viewer")
```


## Walk Through

 1. Install the package
```
source("https://raw.githubusercontent.com/Edouard-Legoupil/koboloadeR/master/inst/script/install_github.R")
install.packages(devtools)
library(devtools)
install_github("Edouard-Legoupil/koboloadeR")
```
(This version of `install_github` via [@jtilly](https://github.com/jtilly/install_github).)

 2. Start a project within Rstudio
 
 3. Launch the `kobo_projectinit` functions in order to organise your project. It also starts a series of question to set up a configuration file to access a kobo server.
 
 4. Either: 
   *  Grab your data with `kobo_data_downloader` & Get your form with `kobo_form`
   * or simply copy your data in `csv` format and your xlsform in `xls` format in the `data` folder that was created during the project initiation
 
 5. Open the `1-loaddata.R` script in the code folder, replace the name of the dataset and the name of the form.
 
 6. Create your dictionnary with `kobo_dico`
 
 7. Generate your graphs with `kobo_bar_one`, `kobo_bar_multi`, `kobo_histo`, `kobo_trend`, `kobo_bar_one_facet`, `kobo_correlate`, `kobo_boxplot_facet` (see below for explanation)


## Data Analysis Plan within your `xlsfrom`

`Xlsform` is a convenient format to build advance form using any spreadsheet software such as [Libreoffice](https://www.libreoffice.org/download/libreoffice-fresh/) or MsExcel. 

In order to build an an analysis plan within the form, the columns described in the tables below needs to be added. Note that if the column are not present, the script will create dummy ones. It's always possible to add your analysis plan to an existing form and relaunch `kobo_dico` in order to regenerate the correct analysis plan.

Note that for charting purpose, it's recommanded that labels for questions & choices should not exceed 70 characters. It's possible again to re-edit directly your xlsform and regenerate a new `dico`.

### In the `survey` worksheet:

Column | Description
------|--------------
`repeatsummarize`| used to summarize repeat questions 
`variable`| used to flag `ordinal` variables
`disaggregation`| used to flag variables used for  `facet` or `correlate`
`indicator`| used to map the question with an indicator
`indicatorgroup`| used to reference the the group the indicator should be associated to
`indicatortype`| define wether the indicator is  `Measurement`: variable used to quantify other indicators, `Disaggregation`: variable that describes certain groups, `Predictor`: Indicator that describes the cause of a situation, `Outcome`: Indicator that describes the consequence of a situation or `Judgment`: indicator that translates a subjective assessment
`indicatorlevel`| used to define the geographic aggregation to be used for indicator calculation
`Indicatorexternal`| used to reference an external dataset to be used to calculate the indicators. Could be for instance a population dataset.
`indicatorcalculation`| used to reference the calculation method to be used for the indicator: `Percentage`, `Sum`, `Max/Min`, `Average`, `Score`, `Denominator`, `Numerator`, `Numerator.external` (i.e. linked to an external value)
`indicatornomalisation`| used to reference the normalisation method to be used for the indicator


### In the `choices` worksheet:

Column | Description
------|--------------
`order`| used to define order for ordinal variables
`weight`| used to define weight for each answers in case it's used for some specific indicator calculation
`recategorise`| used to recategorise quickly choices for a question

## Core Functions

The package contains the following core functions:

Function | Description
------|--------------
`kobo_datasets`| Lists the datasets available for a given user. Returns a `data.table` with the basic metadata about the available datasets.
`kobo_submission_count`|Lists the number of submissions for a particular data collection project. A single integer. This function is mostly for use within the `kobo_data_downloader` function.
`kobo_data_downloader`|Downloads a specified dataset via the KoBo API. Returns a `data.table` of the entire dataset requested.

For all of the above functions, the default is to use the UNHCR KoBo Toolbox API URLs. However, it should be possible to specify the API URL to use if you have a custom installation of the toolbox.

## Chart Generation functions

* Adding a `kobo_bar_one` function to generate bar chart - frequency for all `select_one` questions

* Adding a `kobo_bar_multi` function to generate bar chart - frequency for all `select_multiple` questions

* Adding a `kobo_histo` function to generate histogramme for all `integer` questions

* Adding a `kobo_trend` function to generate histogramme for all `select_one` and `select_multiple` questions based 

* Adding a `kobo_bar_one_facet` function to generate bar chart for all `select_one` questions facetted on questions tagged as `facet` in the data analysis plan 

* Adding a `kobo_correlate` function to generate dot plot for all `integer` questions correlated with integer questions tagged as `correlate` in the data analysis plan 

* Adding a `kobo_boxplot_facet` function to generate box plot for all `integer` questions faceted with categorical questions tagged as `facet` in the data analysis plan 

## Shiny Apps

The package contains the following Shiny apps, accessible via `kobo_apps("app_name")`:

App | Description
---|---
`"data_viewer"` | The `"data_viewer"` app provides a basic login screen to authenticate against the specified API. Once authenticated, the datasets available via the specified login are displayed, and a dropdown list is populated with which one can select the dataset they want to view. The dataset is also made available in the users Global Environment.

Here's [a blog post introducing the package](http://news.mrdwab.com/post/koboloader/)!

### Exported Utility Functions

The package contains the following exported utility functions:

Function|Description
----|----
`kobo_time_parser_UTC`|Converts a date/time character string into a POSIXct time object.
`kobo_time_parser`|Formats a date/time character string into a character string for a specified timezone. Convert using `as.POSIXct` if you need an actual time object.

---------------

## Examples

The following examples access the public data available via KoBo Toolbox. Note that all of the functions have been set with defaults of `user = NULL` and `api = 'kobo'`.

```
kobo_datasets()[, c("description", "id"), with = FALSE] ## Just show the first two columns
#                                                    description    id
#   1:                                关于“西装微定制现状的调查“ 10427
#   2:                زانیاری لەسەر كۆمپانیاكانی نەوت لە گەرمیان 11190
#   3:                           מיפוי שדרות צ'רצ'יל - ורד ויואב 12568
#   4:                                                      Test 39717
#   5:                                             Market Survey  7640
#  ---                                                                
# 403: Webuy_Stock lot Business (No.1 Stock Bazar in Bangladesh) 30792
# 404:                               WWF Zambia [Field Reporter]  4163
# 405:                                         xls_form_training 41820
# 406:                                    Mwanza KAP SURVEY 2015 25206
# 407:                                    Elisha Zelina, GST6109  1857

kobo_submission_count(4163)
# [1] 37

kobo_data_downloader("4163")
# No local dataset found.
# Downloading remote file.
# ... The contents would normally be printed here

### On a subsequent run, if the file is already there and no changes have been made
kobo_data_downloader("4163")
# Number of rows in local and remote file match.
# Using local file.
```

The `kobo_data_downloader` automatically checks for the existence of an object in your workspace named "data_####" (where "####" is the numeric form ID). If such an object is found, it then uses `kobo_submission_count` to compare the number of rows in the local dataset against the number of rows in the remote dataset. If the number is found to be different, the remote dataset is re-downloaded. If they are found to be the same, the local dataset is used. 

In the future, it is intended that there would be a more robust and efficient method rather than redownloading the entire dataset each time a change has been detected.

--------------

Run the examples at the help pages to get a sense of some of the other features:

```
example("kobo_datasets")
example("kobo_submission_count")
example("kobo_data_downloader")
```

## Authentication

These functions all use basic HTTP authentication. The easiest way to enter the password details is the common `"username:password"` approach. Thus, when accessing form data using authentication, the function would be used in the following manner:

```
kobo_data_downloader("123456", "username:password")
```

