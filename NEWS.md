# NEWS

## koboloadeR 0.1.3

Adding a `kobo_projectinit` function to set the data anlysis project folders, write the config file and add some analysis scripts.

Adding a `kobo_dico` function to get & parse the form and build a data dictionnary. The dictionnary is used to automatically generate graphs and maps.

Added UNHCR Kobotoolbox server API and put it per default


## koboloadeR 0.1.2

Testing `read_csv` wrapped in `setDT` to see whether it resolves issue #4.

## koboloadeR 0.1.1

The "data_viewer" Shiny app was added. 

## koboloadeR 0.1.0

KoBo Toolbox offers a convenient way to collect data using web and mobile forms. This package facilitates the retrieval of data entered using these tools using the KoBo Toolbox API (v1).

**MAIN FUNCTIONS**

* `kobo_datasets`
* `kobo_submission_count`
* `kobo_data_downloader`

**UTILITY FUNCTIONS**

* `kobo_time_parser_UTC`
* `kobo_time_parser`

**HELPER FUNCTIONS**

These are generally non-exported functions. Thus, you will need to prefix the functions with `koboloadeR:::` if you want to access them directly.

* `host`
* `get_me`
* `pwd_parse`
