# ui.R

## Required packages
library(shiny)
library(DT)

## Create the user interface
## One row across the top for logging in and retrieving the datasets
##   available via those login credentials.
## A sidebar panel that lets you select a dataset to load.
## A main panel that has two tabs, one that shows the ID and names of
##   the datasets available, one that will show the contents of the
##   requested dataset.

shinyUI(fluidPage(
  headerPanel("KoBo Data Viewer"),

  ## Full-width row, with four columns

  fluidRow(
    column(3, textInput("username", label = "Username", value = "")),
    column(3, passwordInput("password", label = "Password", value = "")),
    column(4, textInput("api", label = "API", value = "kobo")),
    column(2, actionButton("listDatasets", "List Available Datasets"))),
  hr(),

  ## The bottom part is a conditional full-width row. Checks on the condition
  ##   that the listDatasets button has not yet been pressed. If it has not yet
  ##   been pressed, then basic instructions are shown....

  conditionalPanel(
    condition = "input.listDatasets == 0",
    fluidRow(
      column(
        6, h3("Usage"),
        p("Enter your", em("username"), ", ", em("password"), ", ", "and the ",
          em("API"), " that you want to use and click",
          code("List Available Datasets"), ". This will load a table of the IDs
          and titles of the datasets available, as well as a drop-down select
          menu with which you can select the dataset that you want to load."),
        p("To view a list of publicly available datasets, use ", code("NULL"),
          "for both the username and password fields."),
        h3("API"),
        p("API URLs are made available for KoBo Toolbox (", code('"kobo"'), "),",
          a(href = "https://kc.kobotoolbox.org/api/v1/",
            "https://kc.kobotoolbox.org/api/v1/"), ", KoBo Humanitarian Response (",
          code('"kobohr"'), "),",
          a(href = "https://kc.humanitarianresponse.info/api/v1/",
            "https://kc.humanitarianresponse.info/api/v1/"),
          ", and Ona (", code('"ona"'), "),",
          a(href = "https://ona.io/api/v1/", "https://ona.io/api/v1/"),
          ". For your own installation, or other installations using the same API
          but accessed at a different URL, enter the", em("full URL.")))
      )
    ),

  ## This is the alternative condition, for when the button has been pressed. The
  ##   space will now be filled with a two-column layout.

  conditionalPanel(
    condition = "input.listDatasets != 0",
    fluidRow(

      ## `uiOutput` is dynamically created in `server.R`

      column(
        2, uiOutput("select_dataset"),
        actionButton("loadDataset", "Load Requested Dataset"), br(),
        helpText("NOTE: The requested dataset would be downloaded to your Global
                 Environment. You may save it for further offline use."),
        helpText("The object would be named in the form of:"),
        helpText("'data_formid'")),
      column(
        10, tabsetPanel(tabPanel("Available Datasets",
                                 dataTableOutput("datasetsAvailable")),
                        tabPanel("Requested Dataset",
                                 dataTableOutput("datasetRequested"))
                        )
        )
      )
    ),

  ## This is to fix the alignment of the "listDatasets" button with the rest of
  ##   the login details.

  tags$style(type='text/css',
             "#listDatasets { width:100%; margin-top: 25px;}")
  )
)
