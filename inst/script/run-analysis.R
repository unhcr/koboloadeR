#' @name build_analysis_package
#' @rdname build_analysis_package
#' @title Build the analysis project
#'
#' @description Function to build the analysis project.
#' In order to start, you need to put both the data and the original xlsform within the data-raw folder
#' you can adjust the function below and add as many output format as required
#'
#' @param form The full file name of the form to be accessed (not that it has to be an .xls format).
#'
#' @examples
#' \dontrun{
#' build_analysis_package(form = "form.xlsx")
#' }

build_analysis_package <- function(form = "form.xlsx") {
   
  ## Load & process Data - data in csv from the data-raw will be then save as .rda file in the data folder
  koboloadeR::kobo_load_data(form)
  #### Generate Exploration Reports
  koboloadeR::kobo_crunching_report(form,
                        output ="html", ## other options are  "docx", "pptx", "aspx"
                        render = "TRUE", ## can put to false in case you just need to refresh the Rmd based on your xlsform config
                        lang = "eng", ## other options is to have report language in Spanish "esp" or french "fre"
                        unhcRstyle = "TRUE", ## put false to have a non-UNHCR neutral theme
                        use_pct = "TRUE", ## use count rather than % in charts label
                        add_error_bar = "TRUE"  ## put false to supress error bar
                        )

  # add as many report generation format required: html, docx, pptx
  ## if you use aspx (in case you want to upload to sharepoint folder set to serve aspx) but will need before to edit file header and add manually - <meta http-equiv="Content-type" content="text/html; charset=utf-8" />

  ## Generate Cluster Report  --> Discover
  ## Report will based on variable cluster == "TRUE" - you need to set one variable as "id"
  ## We use the root data frame -
  # Assumption is that information from sub-hierachical frame was included through calculated indicators
  # MainDataFrame <- load( "data/MainDataFrame_edited.rda")
  # kobo_cluster_report(frame = MainDataFrame, form)
}


## Change here the precise name of the form if required
form <- "form.xlsx"
## Extend xlsform with required column if necessary - done only once!
# koboloadeR::kobo_prepare_form(form)

## Now generate everything
build_analysis_package(form)


