
library(shiny)
library(readxl)

ui <-
  fluidPage(theme = "bootstrap.min.css",
    tabsetPanel(
      tabPanel("Sampling Frame",
               titlePanel("Sampling"),
                   fluidRow(
                       column(3,
                              selectInput("sampling_meth", h5("What kind of sampling method do you want to use?"),
                                          choices = c("Simple random" = "srs", "2 stages random -st1" = "strat2st", "cluster sampling" = "cluster"))
                       ),
                       column(2,
                              numericInput("confidence_level",h6("Confidence level"),95,min = 0, max = 100, step = 5)),
                       column(2,
                              numericInput("margin_error",h6("Margin of error"),5,min = 0, max = 100, step = 5)),
                       column(2,
                              numericInput("proportion",h6("Proportion"),50,min = 0, max = 100, step = 5)),
                       column(2,
                              numericInput("buffer",h6("Survey buffer"),5,min = 0,max = 100, step = 1))

                   ),

                 fluidRow(
                   column(5,
                          fileInput("datafile", label = h4("Select the file with your sampling frame"),accept = c(".xlsx",".xls")),
                          uiOutput("data_sheet")),
                   column(2,
                          checkboxInput("strat", label = "Stratified")),
                   column(4,
                          uiOutput("input_strata"),
                          uiOutput("input_pop")
                          )

            ),
               fluidRow(
                   column(10,
                          DT::dataTableOutput("sample_frame")
                          )
                  ),

               fluidRow(
                 actionButton("sample_b","Sample!"),
                 dataTableOutput("sampling_frame_dt"),
                 downloadButton("sample", "Download sample frame")
               )

    )
  )
)

server <- function(input, output,session) {
  mainDir <- getwd()
  mainDir <- dirname(dirname(mainDir))

  output$data_sheet  <- renderUI({
    inFile <- input$datafile
    if (is.null(inFile)) return(NULL)
    data_sheets <- excel_sheets(inFile$datapath)
    radioButtons("data_sheet","Select the sheet with your data",data_sheets)
  })

  output$input_strata <- renderUI({
      inFile <- input$datafile
      stratification <- input$strat
      if (is.null(inFile)) return(NULL)
      if (stratification == F) return(NULL)
      SamplingFrame <- read_excel(inFile$datapath, sheet = input$data_sheet)
      columns <- names(SamplingFrame)
      selectInput("strata","Select input strata", choices = as.character(columns))

  })

  output$input_pop <- renderUI({
      inFile <- input$datafile
      if (is.null(inFile)) return(NULL)
      SamplingFrame <- read_excel(inFile$datapath, sheet = input$data_sheet)
      columns <- names(SamplingFrame)
      selectInput("pop","Select input population", choices = as.character(columns))

  })

  output$sample_frame <- DT::renderDataTable({
      inFile <- input$datafile
      if (is.null(inFile)) return(NULL)
      SamplingFrame <- data.frame(read_excel(inFile$datapath, sheet = input$data_sheet))
      DT::datatable(SamplingFrame, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))

  })

  sample_frame_f <- observeEvent(input$sample_b,{
      inFile <- input$datafile
      if (is.null(inFile)) return("Please upload your sampling frame")
      strat <- as.character(input$strata)
      col_pop <- as.character(input$pop)
      SamplingFrame <- data.frame(read_excel(inFile$datapath, sheet = input$data_sheet))
      conf_level <- input$confidence_level/100
      mar_erro <- input$margin_error/100
      prop <- input$proportion/100
      buff <- input$buffer/100
      sample_frame_f <- kobo_samplingframe(data = SamplingFrame,
                                      strata = strat,
                                      pop_col = col_pop,
                                      confidence_level = conf_level,
                                      margin_error = mar_erro,
                                      proportion = prop,
                                      method = input$sampling_meth,
                                      buffer = buff)
      output$sampling_frame_dt <- renderDataTable(sample_frame_f, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
      output$sample <- downloadHandler(
          filename = function(){
              paste0("sampling_frame.csv")
          },
          content = function(file) {
              write.csv(sample_frame_f(), file)
          })

      output$sample <- downloadHandler(
          filename = function(){
              paste0("sampling_frame.csv")
          },
          content = function(file) {
              write.csv(sample_frame_f, file)
          })


  })




}

shinyApp(ui = ui, server = server)
