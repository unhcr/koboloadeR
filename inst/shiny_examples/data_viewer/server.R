# server.R

library(shiny)

shinyServer(function(input, output) {

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
})
