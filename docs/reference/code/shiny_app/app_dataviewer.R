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

ui <- fluidPage(
  #theme = "bootstrap.min.css",
  navbarPage("koboloadeR: Data Viewer", position = "static-top",
             ## Introduction#####
             tabPanel("Introduction",
                      mainPanel(
                        fluidRow(
                          h2("Instruction"),
                          h3(" 1- Export data from Kobo"),
                          p("Both your form and data should be downloaded directly from kobo."),
                          p("When exporting your data from Kobo:")))
             ),

             ## Project Configuration#####
             tabPanel("Data Viewer",
                      titlePanel("Configuring a few elements for your project"),
                      mainPanel(
                        fluidRow(

                fluidRow(
                  column(3, textInput("username", label = "Username", value = "")),
                  column(3, passwordInput("password", label = "Password", value = "")),
                  column(4, textInput("api", label = "API", value = "kobo"))
                  ),

                fluidRow( column(2, actionButton("listDatasets", "List Available Datasets"))),    hr(),

                ## The bottom part is a conditional full-width row. Checks on the condition that the listDatasets button has not yet been pressed. If it has not yet
                ##   been pressed, then basic instructions are shown....
                conditionalPanel(
                  condition = "input.listDatasets == 0",
                  fluidRow(
                    column(
                      6, h3("Usage"),
                      p("Enter your", em("username"), ", ", em("password"), ", ", "and the ",
                        em("API"), " that you want to use and click",
                        code("List Available Datasets"), ". This will load a table of the IDs and titles of the datasets available, as well as a drop-down select menu with which you can select the dataset that you want to load."),
                      p("To view a list of publicly available datasets, use ", code("NULL"), "for both the username and password fields."),
                      h3("API"),
                      p("API URLs are made available for
                          KoBo UNHCR (",code('"unhcr"'), "),",  a(href = "https://kobocat.unhcr.org/api/v1/", "https://kobocat.unhcr.org/api/v1/"), ",
                          KoBo Humanitarian Response (",code('"kobohr"'), "),",  a(href = "https://kc.humanitarianresponse.info/api/v1/", "https://kc.humanitarianresponse.info/api/v1/"), ",
                          KoBo Toolbox (", code('"kobo"'), "),",a(href = "https://kc.kobotoolbox.org/api/v1/", "https://kc.kobotoolbox.org/api/v1/"), ",
                          and Ona (", code('"ona"'), "),", a(href = "https://ona.io/api/v1/", "https://ona.io/api/v1/"),".

                        For your own installation, or other installations using the same API but accessed at a different URL, enter the", em("full URL.")))
                    )
                  ),

                ## This is the alternative condition, for when the button has been pressed. The
                ##   space will now be filled with a two-column layout.

                conditionalPanel(
                  condition = "input.listDatasets != 0",
                  fluidRow(

                    ## `uiOutput` is dynamically created in `server.R`

                    column( 2, uiOutput("select_dataset"),
                               actionButton("loadDataset", "Load Requested Dataset"), br(),
                               helpText("NOTE: The requested dataset would be downloaded to your Global Environment. You may save it for further offline use."),
                               helpText("The object would be named in the form of:"),
                               helpText("'data_formid'")),
                    column( 10, tabsetPanel(tabPanel("Available Datasets", dataTableOutput("datasetsAvailable")),
                                            tabPanel("Requested Dataset", dataTableOutput("datasetRequested"))) )
                    )
                  ), # End conditional panel
                  ## This is to fix the alignment of the "listDatasets" button with the rest of
                  ##   the login details.

                  tags$style(type = 'text/css', "#listDatasets { width:100%; margin-top: 25px;}") )
                )  # end tabpanel
             )
) # End nav bar

) # end fluid page


server <- function(input, output) {

  ## Start by creating a reactive version of the dataset listing. This
  ##   will then let us access the data for use in dynamically creating
  ##   the listing of the available datasets. We only need the "id"
  ##   and "title" datasets.

  my_data <- reactive({
    input$listDatasets
    isolate({
      user_name <- if (input$username == "NULL") NULL else input$username
      password <- if (input$password == "NULL") NULL else input$password
    })

    ## We want to wait on the execution of the download request until the button
    ##   to list available datasets has actually been pressed.

    if (input$listDatasets == 0) {
      return()
    } else {
      isolate({
        out <- kobo_datasets(
          user = c(user_name, password),
          api = input$api)[, c("id", "title"), with = FALSE]
      })
    }
  })

  ## This is for the datatable output

  output$datasetsAvailable <- renderDataTable({
    datatable(my_data())
  })

  ## This creates the dropdown UI for the sidebar. The values are
  ##   automatically populated with the "id" and "title" values from
  ##   the my_data dataset, which must be accessed using my_data()

  output$select_dataset <- renderUI({
    dat <- my_data()
    selectInput("select", label = "Select Dataset",
                choices = setNames(dat$id, dat$title),
                selected = 1, selectize = TRUE)
  })

  ## This downloads the requested dataset to your global environment,
  ##   and displays it in the "Requested Dataset" tab in the UI.

  output$datasetRequested <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "NULL") NULL else input$username
      password <- if (input$password == "NULL") NULL else input$password
      out <- kobo_data_downloader(
        input$select, c(user_name, password), input$api)
    })
    datatable(out, filter = "top")
  })
}



shinyApp(ui = ui, server = server)
