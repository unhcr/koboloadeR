
library(shiny)
library(readxl)

ui <- fluidPage(
  #theme = "bootstrap.min.css",
  navbarPage("koboloadeR: Data crunching", position = "static-top",

             ## Introduction#####
             tabPanel("Introduction",
                      mainPanel(
                        fluidRow(
                        h2("Instruction"),
                        h3(" 1- Export data from Kobo"),
                        p("Both your form and data should be downloaded directly from kobo."),
                        p("When exporting your data from Kobo:"),
                        tags$ol(
                          tags$li("Select 'export type' as XLS"),
                          tags$li('XML values and headers'),
                          tags$li('Include groups in headers'),
                          tags$li("and 'Group seperator' as dot ('.') (see image below) ")
                        ),
                        p(strong("DO NOT"),"change the name of the columns or any cells in the data. Everything should be exactly as in the form."),
                        img(src = "exportformat.png", width = 200),

                        h3(" 2- Use the XLSform template supplied"),
                        p("In the folder 'code', you will find a XLSform template.",
                          br(),
                          "You will see that this te
                          mplate has three extra sheets:",
                          tags$ol(
                            tags$li("'analysis_plan': where you can define your indicators or new variables if need"),
                            tags$li("'sampling_frame': where you can specify your sampling frame to weight your data according to the method given in Project Configuration"),
                            tags$li("'instruction': help on how to fill the new XLSform.")
                          ),
                          "Please see the 'instruction' on how to setup your XLSform for koboloadeR")
                      ))
             ),

             ## Project Configuration#####
             tabPanel("Project configuration",
                      titlePanel("Configuring a few elements for your project"),
                      mainPanel(


                        fluidRow(
                          h2("Information about the reports"),
                          column(10,
                                 textInput("report_name", label =  "Name of the report", value = "Data Crunching Report"),
                                 textInput("location", label =  "Where is the report written", value = "out"),
                                 textInput("author", label =  "What is your name (author)?", value = "Edouard"),
                                 textInput("organisation", label =  "What is your organisation?"), value = "UNHCR" )
                        ),

                        fluidRow(
                          column(5, offset = 2, fileInput("form", label = h4("Select the file with the form"),accept = c(".xlsx")))
                        ),

                        fluidRow(
                          #list files in "data" and make dropdown of choices. then select sheets to be used for data, analysis plan and sampling
                          column(5,  fileInput("datafile", label = h4("Select the file with the data"),accept = c(".xlsx",".xls",".csv")),  uiOutput("data_sheet"))
                        ),

                        fluidRow(
                          column(5, radioButtons("weighting_sys", h4("What kind of weighting system did you use?"),
                                                 choices = c("None" = "none", "Sampling Frame" = "sampling_frame")) ),
                          column(4, conditionalPanel(condition = 'input.weighting_sys == "sampling_frame"',
                                                     radioButtons("sampling", h5("What kind of sampling methode did you use?"),
                                                                  choices = c("Simple random" = "simple_random",
                                                                              "2 stages random -st1" = "2_stages",
                                                                              "cluster sampling" = "cluster_sampling"))))
                        ),
                        fluidRow(
                          radioButtons("analysis_plan", h4("Did you use the indicators calculation sheet?"), choices = c("No" = "n", "Yes" = "y")),
                          uiOutput("files_uploaded")
                        )
                      ),

                      sidebarPanel(
                        h3("When you are all set, upload the files!"),  br(),
                        actionButton("upload_files",label = "Upload files"),  br(), br(),
                        htmlOutput("textmessage"), br(),   br(),
                        uiOutput("dico"), br(), br(),
                        htmlOutput("dicomessage")
                      )
             ),

             ## Indicators calculation #####
             tabPanel("Indicators calculation",
                      titlePanel("Build additional indicators from your dataset"),
                      mainPanel(
                        fluidRow(
                          p("It's often relevant to create new indicators.",
                            br(),
                            "Most common case are:",
                            tags$ol(
                              tags$li("'analysis_plan': where you can define your indicators or new variables if need"),
                              tags$li("'sampling_frame': where you can specify your sampling frame to weight your data according to the method given in Project Configuration"),
                              tags$li("'instruction': help on how to fill the new XLSform.")
                            ),
                            "Please see the 'instruction' on how to setup your XLSform for koboloadeR"))
                      )
             ),

             ## Output #####
             tabPanel("Output",
                      titlePanel("Output"),
                      sidebarLayout(position = "left",
                                    mainPanel(
                                      h4("Bar graphs for select_one questions"), actionButton("bar_one", "Generate graphs"),
                                      h4("Bar graphs for select_one questions with disaggregation"),  actionButton("bar_one_facet", "Generate graphs"),
                                      h4("Bar graphs for select_multiple questions"), actionButton("bar_multi", "Generate graphs"),
                                      h4("Bar graphs for select_multiple questions with disaggregation"), actionButton("bar_multi_facet", "Generate graphs"),
                                      h4("Histograms for integer,decimal, and calculate questions with disaggregation"), actionButton("histo", "Generate graphs")
                                    ),
                                    sidebarPanel(
                                      h4("Download outputs"), downloadButton("report", "Generate report") ))
             )
  )
)


server <- function(input, output,session) {
  mainDir <- getwd()
  mainDir <- dirname(dirname(mainDir))

  #Move the data and form to the data folder
  observeEvent(input$upload_files,{
    inFile_data <- input$datafile
    file.copy(inFile_data$datapath,file.path(mainDir,"/data/",inFile_data$name),overwrite = T)
    inFile_form <- input$form
    file.copy(inFile_form$datapath,file.path(mainDir,"/data/",inFile_form$name),overwrite = T)
    inFile_sheet <- input$data_sheet
    inFile_weight <- input$weighting_sys
    inFile_analysis <- input$analysis_plan
    inFile_sampling <- input$sampling
    inFile_report <- input$report_name
    inFile_location <- input$location
    inFile_author <- input$author
    inFile_organisation <- input$organisation

    #Path to file
    configfile <- paste(mainDir,"/code/0-config.R", sep = "")
    #Writting file
    sink(configfile)
    cat("#### Config file ###\n")
    cat("\n")
    cat("### Can be manualy edited or interactively rewritten using the function kobo_projectconfig() or kobo_shiny('app_koboloadeR.R') \n")
    cat("\n")
    cat("### 1. Form   ###\n")
    cat(paste("form <- '",inFile_form$name,"'", sep = ""))
    cat("\n")
    cat(paste('path.to.form <- ("',mainDir,'/data/',inFile_form$name,'") \n', sep = ""))
    cat("\n")


    cat("\n### 2. Main dataframe  ###\n")
    cat(paste('path.to.data <- ("',mainDir,'/data/',inFile_data$name,'") \n', sep = ""))
    cat(paste('datafile <- "',inFile_data$name,'"', sep = ""))
    cat("\n")
    cat(paste('sheet <- "',inFile_sheet,'"', sep = ""))
    cat("\n")
    cat(paste0('data <- read_excel("', inFile_data$name,'", sheet = "',inFile_sheet,'")\n\n') )
    cat("\n")

    cat("### 1. Weighting system   ###\n")
    cat(paste("usedweight <- '",inFile_weight,"'", sep = ""))
    cat("\n")
    cat("####### 3.1 - Type of sampling used ###\n")
    cat(paste('usedsampling <- "', inFile_sampling,'"', sep = ""))
    cat("\n")
    cat("###  4.- Used the data analysis plan\n")
    cat(paste('analysis_plan <- "', inFile_analysis,'"', sep = ""))
    cat("\n")
    cat("###  5. General info on the project\n")
    cat(paste('report_name <- "', inFile_report,'"', sep = ""))
    cat("\n")
    cat(paste('location <- "',inFile_location,'"', sep = ""))
    cat("\n")
    cat(paste('author <- "',inFile_author,'"', sep = ""))
    cat("\n")
    cat(paste('organisation <- "',inFile_organisation,'"', sep = ""))
    cat("\n")
    cat(paste('mainDir <- "',mainDir,'"', sep = ""))
    cat("\n")
    sink()

    output$textmessage <- renderUI(HTML(paste(p(strong("Files uploaded and config files written")),
                                              p("Now, build the dictionnary to link the form with the data"))))
    output$dico <- renderUI({
      actionButton("dico",label = "Build Data Dictionnary")
    })
  })

  observeEvent(input$dico,{
    isolate({source(paste0(mainDir,"/code/0-config.R"), local = TRUE)})
    kobo_dico(mainDir)
    if (input$analysis_plan == 'y') {
      kobo_indicator(mainDir)
    }
    if (input$weighting_sys != 'none') {
      kobo_weight(mainDir)
    }
    output$dicomessage <- renderUI(HTML(paste(p(strong("Dictionnary built, data weighted, and analysis_plan applied!"),
                                                p("All good, now go to the ",strong("'output' tab!"))))))

  })

  output$data_sheet  <- renderUI({
    inFile <- input$datafile
    if (is.null(inFile)) return(NULL)
    data_sheets <- excel_sheets(inFile$datapath)
    radioButtons("data_sheet","Select the sheet with your data",data_sheets)
  })

  observeEvent(input$bar_one,{
    isolate({source(paste0(mainDir,"/code/0-config.R"), local = TRUE)})
    kobo_bar_one(mainDir)
  })

  observeEvent(input$bar_one_facet,{
    isolate({source(paste0(mainDir,"/code/0-config.R"), local = TRUE)})
    kobo_bar_one_facet(mainDir)
  })

  observeEvent(input$bar_multi_facet,{
    isolate({source(paste0(mainDir,"/code/0-config.R"), local = TRUE)})
    kobo_bar_multi_facet(mainDir)
  })

  observeEvent(input$bar_multi,{
    isolate({source(paste0(mainDir,"/code/0-config.R"), local = TRUE)})
    kobo_bar_multi(mainDir)
  })

  observeEvent(input$histo,{
    isolate({source(paste0(mainDir,"/code/0-config.R"), local = TRUE)})
    kobo_histo(mainDir)
  })

  output$report <- downloadHandler(
    filename = "report.docx",
    content = function(file) {
      withProgress(message = "Preparing report...",value = 0,{
        file.copy(paste0(mainDir,"/code/report.Rmd"), paste0(mainDir,"/out"),overwrite = TRUE)
        tempReport <- paste0(mainDir,"/code/report.Rmd")
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          envir = new.env(parent = globalenv())
        )
      }
      )
    }
  )
}

shinyApp(ui = ui, server = server)
