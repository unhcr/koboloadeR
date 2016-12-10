# koboloadeR

The `koboloadeR` package is a simple R connection to the [KoBo API (v1)](https://kc.kobotoolbox.org/api/v1/) for the [KoBo Toolbox](http://www.kobotoolbox.org/) project. It also provides a Shiny data viewer accessible using:

```
kobo_apps("data_viewer")
```

## Installation

In this early version of the package, `koboloadeR` will only available on GitHub. It can be installed using:

```
source("http://news.mrdwab.com/install_github.R")
install.packages(devtools)
library(devtools)
install_github("Edouard-Legoupil/koboloadeR")
```

(This version of `install_github` via [@jtilly](https://github.com/jtilly/install_github).)

## Functions

The package contains the following core functions:

Function | Description
------|--------------
`kobo_datasets`| Lists the datasets available for a given user. Returns a `data.table` with the basic metadata about the available datasets.
`kobo_submission_count`|Lists the number of submissions for a particular data collection project. A single integer. This function is mostly for use within the `kobo_data_downloader` function.
`kobo_data_downloader`|Downloads a specified dataset via the KoBo API. Returns a `data.table` of the entire dataset requested.

For all of the above functions, the default is to use the KoBo Toolbox API URLs. However, it should be possible to specify the API URL to use if you have a custom installation of the toolbox.

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

