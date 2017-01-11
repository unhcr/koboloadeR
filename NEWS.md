# NEWS

## koboloadeR 0.1.3

Forked from [https://github.com/mrdwab/koboloadeR](https://github.com/mrdwab/koboloadeR)

* Adding a `kobo_projectinit` function to set the data anlysis project folders, write the config file and add some analysis scripts.

* Adding a `kobo_form` function to get the form from the api. This includes 2 subfunctions: `kobo_dataset2` to have a better view of used forms & `kobo_forminfo` in order to get the form creater user name and pull it correctly from the API.

* Adding a `kobo_dico` function to parse the form and build a data dictionnary. The dictionnary is used to automatically generate graphs and maps. A inked functions is `kobo_label` that allows to recreate labels easily using the dictionnary.

* Adding a `kobo_label` function to add labels to variable using dictionnary

* Adding a `kobo_encode` function to re-encode variables as per dictionnary

* Adding a `kobo_bar_one` function to generate bar chart - frequency for all `select_one` questions

* Adding a `kobo_bar_multi` function to generate bar chart - frequency for all `select_omultiple` questions

* Adding a `kobo_histo` function to generate histogramme for all `integer` questions

* Adding a `kobo_trend` function to generate histogramme for all `select_one` and `select_multiple` questions based 

* Added UNHCR Kobotoolbox server API and put it per default


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
