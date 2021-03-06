% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kobo_crunching_report.R
\name{kobo_crunching_report}
\alias{kobo_crunching_report}
\title{Generate Data Crunching Report}
\usage{
kobo_crunching_report(
  form = "form.xlsx",
  app = "console",
  output = "html",
  render = "TRUE",
  lang = "eng",
  unhcRstyle = "TRUE",
  use_pct = "TRUE",
  add_error_bar = "TRUE"
)
}
\arguments{
\item{form}{The full filename of the form to be accessed (xls or xlsx file).

It is assumed that the form is stored in the data-raw folder.}

\item{app}{The place where the function has been executed, the default is the console and the second option is the shiny app}

\item{output}{The output format html or aspx if you need to upload on sharepoint), docx (to quickly cut non interesting vz and take note during data interpretation session), pptx (to quickly cut non interesting vz and persent during data interpretation session), Default is html}

\item{render}{TRUE or FALSE - Tells whether to only produce Rmd or to also knit it in the required output format. Default is TRUE. Useful for testing as rending takes time.}

\item{lang}{eng, fre or esp - Change the language of the intro to the report - default is "eng" for english}

\item{unhcRstyle}{TRUE or FALSE tells whether to use UNHCR style fo rendering}

\item{use_pct}{TRUE or FALSE - Tells whether to add percent or absolute value in the chart - default is TRUE}

\item{add_error_bar}{TRUE or FALSE - Tells whether to whiskers for error in the chart - default is TRUE}
}
\value{
No return, All results will be saved on RMD files and Word files
}
\description{
Generate crunching Report that contains all descriptive statistics, correlation analysis, tabulation and data visualization for variables and indicators.
 For disaggregation of variable indicate "facet","stak", "fill" or "dodge". for test of correlation on select_one variable use correlation = TRUE
}
\examples{
\dontrun{
kobo_crunching_report("myform.xlsx")
}

}
\author{
Edouard Legoupil, Maher Daoud
}
