#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
header <- dashboardHeader(title = NULL, disable = TRUE,
                          titleWidth = 0
)

sidebar <- dashboardSidebar(width = 0, disable = TRUE, collapsed = TRUE
)

body <- dashboardBody(useShinyalert(),
                      navbarPage(id = "navbar", title = "koboloadeR Package",
                                 position = c("fixed-top"), inverse=FALSE, collapsible=TRUE,
                                 #theme = "bootstrap.css",
                                 header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),

                                 # Project Configuration --------------------------------------------

                                 tabPanel(value = "pc", title = div(span("1"),span("Project Configuration"), class = "arrow_box"),
                                          uiOutput("projectConfiguration")
                                 ),

                                 # Analysis Plan Configuration --------------------------------------------

                                 tabPanel(value = "apc", title = div(span("2"),span("Analysis Plan Configuration"), class = "arrow_box"),
                                          uiOutput("analysisPlanConfiguration")

                                 ),

                                 # Data Processing ----------------------------------------

                                 tabPanel(value = "dp", title = div(span("3"),span("Data Processing"), class = "arrow_box"),
                                          uiOutput("dataProcessing")

                                 ),

                                 # Reports Generation ----------------------------------------

                                 tabPanel(value = "rg", title = div(span("4"),span("Reports Generation"), class = "arrow_box"),
                                          uiOutput("reportsGeneration")

                                 ),

                                 # Reports Generation ----------------------------------------

                                 tabPanel(value = "dd", title = div(span("5"),span("Data Dissemination"), class = "arrow_box"),
                                          uiOutput("dataDissemination")

                                 )

                      )
)


ui <- dashboardPage(
  header=header,
  sidebar=sidebar,
  body=body
)


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=10000*1024^2) #make the limit up to 10GB
  mainDir <- reactive({
    kobo_getMainDirectory()
  })

  projectConfigurationInfo <- reactiveValues(log = list(), data = list())
  projectConfigurationTheme <- reactiveValues(
    questionsWidth = 6,
    yesNoInputWidth = 3,
    warningBlockWidth = 3
  )
  tracker <- reactiveValues(value=0)

  observe({#initial code that run to check if there is xls form and to upload it
    tryCatch({
      if(tracker$value == 0 ){
        projectConfigurationInfo$log[["scenario"]] <- ""
        projectConfigurationInfo$log[["isPrepared"]] <- FALSE
        projectConfigurationInfo$log[["isGenerated"]] <- FALSE
        projectConfigurationInfo$log[["subAndMainfiles"]] <- FALSE
        projectConfigurationInfo$log[["isRecordSettingsSaved"]] <- FALSE
        projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        if(file.exists(paste(mainDir(), "data", "/form.xls", sep = "/", collapse = "/"))  ){
          #projectConfigurationInfo$log[["xlsForm"]] <- TRUE
          result <- kobo_get_begin_repeat()
          projectConfigurationInfo$data[["beginRepeatList"]] = c("MainDataFrame",result$names)
          projectConfigurationInfo$log[["beginRepeatList"]] = TRUE
          projectConfigurationInfo$log[["isPrepared"]] <- FALSE
          tracker$value = tracker$value + 1
        }
        if(file.exists(paste(mainDir(), "data", "/MainDataFrame.csv", sep = "/", collapse = "/")) ){
          dataFile <- read.csv( paste(mainDir(), "data", "/MainDataFrame.csv", sep = "/", collapse = "/")
                                , header=TRUE , stringsAsFactors = FALSE)
          projectConfigurationInfo$log[["data"]] <- TRUE
          projectConfigurationInfo$data[["data"]] <- dataFile
          projectConfigurationInfo$log[["isGenerated"]] <- FALSE
          tracker$value = tracker$value + 1
        }
        tracker$value = tracker$value + 1
      }
    }, error = function(err) {
      print("hnjfgigf09gs0")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  observe({# the main controller
    projectConfigurationInfo$log[["doYouHaveFormSelectInput"]] <- input$doYouHaveFormSelectInput
    projectConfigurationInfo$log[["doYouWantGenerateFormSelectInput"]] <- input$doYouWantGenerateFormSelectInput
    projectConfigurationInfo$log[["doYouHaveDataSelectInput"]] <- input$doYouHaveDataSelectInput
    projectConfigurationInfo$log[["formIncludeSettingsSelectInput"]] <- input$formIncludeSettingsSelectInput
    projectConfigurationInfo$log[["doYouHaveDatasetsSelectInput"]] <- input$doYouHaveDatasetsSelectInput


    if(is.null(projectConfigurationInfo$log[["data"]])){
      projectConfigurationInfo$log[["data"]] <- FALSE
    }
    if(is.null(projectConfigurationInfo$log[["xlsForm"]])){
      projectConfigurationInfo$log[["xlsForm"]] <- FALSE
    }

    if(is.null(projectConfigurationInfo$log[["beginRepeatList"]])){
      projectConfigurationInfo$log[["beginRepeatList"]]  <- FALSE
    }

    if(sum(input$doYouHaveFormSelectInput=="Yes")==1 && sum(input$doYouHaveDatasetsSelectInput =="Yes")==1 && sum(input$formIncludeSettingsSelectInput =="Yes")==1 ){
      projectConfigurationInfo$log[["scenario"]] <- "Scenario-1: has xls form, the main data file(s) and settings sheet"

    }else if(sum(input$doYouHaveFormSelectInput=="Yes")==1 && sum(input$doYouHaveDatasetsSelectInput =="Yes")==1 && sum(input$formIncludeSettingsSelectInput =="No")==1 ){
      projectConfigurationInfo$log[["scenario"]] <- "Scenario-2: has xls form and the main data file(s). But, does not has settings sheet"

    }else if(sum(input$doYouHaveFormSelectInput=="No")==1 && sum(input$doYouWantGenerateFormSelectInput =="Yes")==1 && sum(input$doYouHaveDataSelectInput =="Yes")==1  ){
      projectConfigurationInfo$log[["scenario"]] <- "Scenario-3: does not has xls form. But, has the main data file to generate xlsform."
    }else{
      projectConfigurationInfo$log[["scenario"]] <- ""
    }
  })

  ####################################### Project Configuration page ############################################
  output$projectConfiguration <- renderUI({
    s <- ""
    cpg <- kobo_check_project_configuration()
    if(cpg$flag){
      return(
        fluidRow(
          box(id="doYouWantUseExProjectBox",
              width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
              column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                     h4("Do you want to use the existing project?")
              ),
              column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                     selectInput("doYouWantUseExProjectSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
              )
          ),
          conditionalPanel(
            condition = "input.doYouWantUseExProjectSelectInput == 'Yes'",
            infoBox(
              width = 12,strong("Done!"),h4("You can start the Analysis Plan Configuration...", align = "center")
              ,icon = icon("check-circle"),
              color = "green"
            )
          ),
          conditionalPanel(
            condition = "input.doYouWantUseExProjectSelectInput == 'No'",
            box(id="doYouHaveFormBox",
                width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
                column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                       h4("Do you have the xlsform?")
                ),
                column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                       selectInput("doYouHaveFormSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
                ),
                column(width = projectConfigurationTheme$warningBlockWidth, offset = 0,
                       if(file.exists(paste(mainDir(), "data", "/form.xls", sep = "/", collapse = "/"))){
                         div(class = "warningBlock",
                             span(class = "warningTitle","WARNING!"),
                             span(class = "warningBody","Be careful, there is already xlsform file (form.xls) in the data directory, once you upload the new file, it will be overridden.")
                         )
                       }
                       
                ),
                column(width = 9,
                       conditionalPanel(
                         condition = "input.doYouHaveFormSelectInput == 'Yes'",
                         fileInput('xlsFormUploadedFile', 'Choose your xls form',
                                   accept=c('.xls'))
                       )
                ),
                column(width = 3,
                       conditionalPanel(
                         condition = "input.doYouHaveFormSelectInput == 'Yes'",
                         actionButton("uploadxlsButton", "Upload xlsform", icon("upload"),
                                      style="width:100%; margin-top: 25px;", class = "uploadButton" )
                       )
                )
            )
          ),
          conditionalPanel(
            condition = "input.doYouHaveFormSelectInput == 'Yes' & input.doYouWantUseExProjectSelectInput == 'No'",
            uiOutput("dataDDISamplingUI")
          )
        )
      )
                  
    }else{
      return(
        fluidRow(
          box(id="doYouHaveFormBox",
              width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
              column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                     h4("Do you have the xlsform?")
              ),
              column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                     selectInput("doYouHaveFormSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
              ),
              column(width = projectConfigurationTheme$warningBlockWidth, offset = 0,
                     if(file.exists(paste(mainDir(), "data", "/form.xls", sep = "/", collapse = "/"))){
                       div(class = "warningBlock",
                           span(class = "warningTitle","WARNING!"),
                           span(class = "warningBody","Be careful, there is already xlsform file (form.xls) in the data directory, once you upload the new file, it will be overridden.")
                       )
                     }
                     
              ),
              column(width = 9,
                     conditionalPanel(
                       condition = "input.doYouHaveFormSelectInput == 'Yes'",
                       fileInput('xlsFormUploadedFile', 'Choose your xls form',
                                 accept=c('.xls'))
                     )
              ),
              column(width = 3,
                     conditionalPanel(
                       condition = "input.doYouHaveFormSelectInput == 'Yes'",
                       actionButton("uploadxlsButton", "Upload xlsform", icon("upload"),
                                    style="width:100%; margin-top: 25px;", class = "uploadButton" )
                     )
              )
          ),
          conditionalPanel(
            condition = "input.doYouHaveFormSelectInput == 'Yes'",
            uiOutput("dataDDISamplingUI")
          )
          
        )
      )
    }
    
  })
  
  
  output$dataDDISamplingUI <- renderText({
    if(projectConfigurationInfo$log[["xlsForm"]]){
      return(
        paste0("", 
               box(id="doYouHaveDatasetsBox", title = "File(s) related to project (Mandatory)", status = "danger", 
                   width=12, solidHeader = TRUE, collapsible = TRUE,
                   column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                          h4("Do you have the Data file(s)?")
                   ),
                   column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                          selectInput("doYouHaveDatasetsSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
                   ),
                   
                   column(width = 12,
                          conditionalPanel(
                            condition = "input.doYouHaveDatasetsSelectInput == 'Yes'",
                            column(width = 12,
                                   uiOutput("dataInputsUI")
                            ),
                            column(width = 3,
                                   actionButton("saveDataFilesButton", "Upload and Save files", icon("upload"), class = "uploadButton", style="margin: 15px 0px; height:45px; width:100%;")
                            )
                            
                          )
                   ),
                   conditionalPanel(
                     condition = "input.doYouHaveDatasetsSelectInput == 'No'",
                     column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                            h4("Do you want to generate Data?")
                     ),
                     column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                            selectInput("doYouWantGenerateDataSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
                     )
                   )
                   
               ),
               box(id="recordSettingsBox", title = "Record Settings Configuration (Optional)", status = "primary",
                   width=12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   column(width = 12, align="left",
                          uiOutput("recordSettingsUI")
                   )
               ),
               box(id="ddiBox", title = "DDI informations (Optional)", status = "primary",
                   width=12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   column(width = 12, align="left",
                          uiOutput("ddiUI")
                   )
               )
        )
      )
    }
  })
  
  output$dataInputsUI <- renderText({
    s <- ""
    for(i in 1:(length(projectConfigurationInfo$data[["beginRepeatList"]]))){
      s <- paste(s , box(class = "uploadFilesBox",title = projectConfigurationInfo$data[["beginRepeatList"]][i],  status = "danger",
                         fluidRow(
                           column(10, offset = 1,
                                  fileInput(inputId=paste("fileInput",projectConfigurationInfo$data[["beginRepeatList"]][i],sep = ""), NULL,
                                            accept=c('text/csv',
                                                     'text/comma-separated-values,text/plain',
                                                     '.csv'))
                           ),
                           column(width = 10, offset = 1, style = "border-top: 1px solid lightgray; margin-top: 10px; padding-top: 15px",
                                  radioButtons(inputId=paste("separator",projectConfigurationInfo$data[["beginRepeatList"]][i],sep = ""), 'Separator',
                                               c(Comma=',',
                                                 Semicolon=';',
                                                 Tab='\t'),
                                               ',', inline =TRUE)
                           )
                         )
                         ,collapsible = FALSE ,width = 3),sep="" )
    }
    
    return(s)
  })
  
  output$recordSettingsUI <- renderUI({
    fluidRow(
      column(12, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px; margin-bottom: 20px; background-color: ghostwhite; padding-top: 20px;",
             column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px dotted lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                    h4("What sampling do you have?")
             ),
             column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                    selectInput("samplingSelectInput", label = NULL,choices = c("-- select --",
                                                                                "No sampling (type 1)",
                                                                                "Cluster sample (type 2)",
                                                                                "Stratified sample (type 3)"
                    ))
                    
             ),
             conditionalPanel(
               condition = "input.samplingSelectInput == 'Cluster sample (type 2)'",
               column(width = 12, style="margin: 15px 0px 15px; border-top: 1px solid lightgray; padding: 20px 10px 0px;",
                      column(width = 4,
                             selectizeInput("variableNameCluster", label = "Select the name of cluster variable",choices = projectConfigurationInfo$data[["xlsFormFields"]]
                                            ,options = list(placeholder = '-- select --', onInitialize = I('function() { this.setValue(""); }'))
                             )
                      ),
                      column(width = 8,
                             column(width = 9, style = "padding-left: 0px;",
                                    fileInput('weightsClusterFileInput', 'Choose weights file for Cluster sample',
                                              accept=c('text/csv',
                                                       'text/comma-separated-values,text/plain',
                                                       '.csv'))),
                             column(width = 3, style = "border-left: 1px solid lightgray; margin-top: 10px;",
                                    radioButtons('weightsClusterSep', 'Separator',
                                                 c(Comma=',',
                                                   Semicolon=';',
                                                   Tab='\t'),
                                                 ',', inline =TRUE)),
                             column(width = 3, offset = 9,
                                    if(file.exists(paste(mainDir(), "data", "/weightsCluster.csv", sep = "/", collapse = "/"))){
                                      div(class = "warningBlock",
                                          span(class = "warningTitle","WARNING!"),
                                          span(class = "warningBody","Be careful, there is already weightsCluster.csv file in the data directory, once you upload the new file, it will be overridden.")
                                      )
                                    }
                                    
                             )
                      )
               )
               
             ),
             conditionalPanel(
               condition = "input.samplingSelectInput == 'Stratified sample (type 3)'",
               column(width = 12, style="margin: 15px 0px 15px; border-top: 1px solid lightgray; padding: 20px 10px 0px;",
                      column(width = 4,
                             selectizeInput("variableNameStratified", label = "Select the name of stratified variable",choices = projectConfigurationInfo$data[["xlsFormFields"]]
                                            ,options = list(placeholder = '-- select --', onInitialize = I('function() { this.setValue(""); }'))
                             )
                      ),
                      column(width = 8,
                             column(width = 9, style = "padding-left: 0px;",
                                    fileInput('weightsStratifiedFileInput', 'Choose weights file for Stratified sample',
                                              accept=c('text/csv',
                                                       'text/comma-separated-values,text/plain',
                                                       '.csv'))),
                             column(width = 3, style = "border-left: 1px solid lightgray; margin-top: 10px;",
                                    radioButtons('weightsStratifiedSep', 'Separator',
                                                 c(Comma=',',
                                                   Semicolon=';',
                                                   Tab='\t'),
                                                 ',', inline =TRUE)),
                             column(width = 3, offset = 9,
                                    if(file.exists(paste(mainDir(), "data", "/weightsStratified.csv", sep = "/", collapse = "/"))){
                                      div(class = "warningBlock",
                                          span(class = "warningTitle","WARNING!"),
                                          span(class = "warningBody","Be careful, there is already weightsStratified.csv file in the data directory, once you upload the new file, it will be overridden.")
                                      )
                                    }
                                    
                             )
                      )
               )
               
             )
      ),
      
      column(12, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px; margin-bottom: 20px;",
             column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px dotted lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                    h4("Do you have data cleaning log?")
             ),
             column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                    selectInput("cleaningLogSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
             ),
             column(width = projectConfigurationTheme$warningBlockWidth, offset = 0,
                    if(file.exists(paste(mainDir(), "data", "/cleaningLog.csv", sep = "/", collapse = "/"))){
                      div(class = "warningBlock",
                          span(class = "warningTitle","WARNING!"),
                          span(class = "warningBody","Be careful, there is already cleaningLog.csv file in the data directory, once you upload the new file, it will be overridden.")
                      )
                    }
                    
             ),
             conditionalPanel(
               condition = "input.cleaningLogSelectInput == 'Yes'",
               column(width = 12, style="margin: 15px 0px 15px; border-top: 1px solid lightgray; padding: 20px 10px 0px;",
                      column(width = 9, style = "padding-left: 0px;",
                             fileInput('cleaningLogFileInput', 'Choose cleaning Log file',
                                       accept=c('text/csv',
                                                'text/comma-separated-values,text/plain',
                                                '.csv'))),
                      column(width = 3, style = "border-left: 1px solid lightgray; margin-top: 10px;",
                             radioButtons('cleaningLogSep', 'Separator',
                                          c(Comma=',',
                                            Semicolon=';',
                                            Tab='\t'),
                                          ',', inline =TRUE))
               )
             )
             
      ),
      column(3, style = "margin-bottom: 20px; padding-top: 0px;",
             actionButton("saveRecordSettingsConfigurationButton", "Save Settings", icon("save"), class = "uploadButton", style="margin: 15px 0px; height:45px; width:100%;")
      )
      
    )
  })
  
  output$ddiUI <- renderUI({
    fluidRow(
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Title of the study:")
        ),
        column(width = 6, offset = 0,
               textInput("titlDDIInput", label = NULL, width = "100%", placeholder = "Free Text", value = "Refugee Survey in Country x")
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Abstract:")
        ),
        column(width = 6, offset = 0,
               textInput("abstractDDIInput", label = NULL, width = "100%", placeholder = "Free Text", value="Blablablablablabla")
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Rights & Disclaimer:")
        ),
        column(width = 6, offset = 0,
               textInput("disclaimerDDIInput", label = NULL, width = "100%", placeholder = "Free Text - adjust if necessary", value="UNHCR does not warrant in any way the accuracy of the information and data contained in the datasets and shall not be held liable for any loss caused by reliance on the accuracy or reliability thereof.")
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Country where the study took place:")
        ),
        column(width = 6, offset = 0,
               selectizeInput("countryDDIInput", label = NULL,
                           choices = c("Afghanistan" = "AFG", "Albania" = "ALB", "Algeria" = "DZA", "American Samoa" = "ASM", "Andorra" = "AND", "Angola" = "AGO", "Anguilla" = "AIA", "Antarctica" = "ATA", "Antigua and Barbuda" = "ATG", "Argentina" = "ARG", "Armenia" = "ARM", "Aruba" = "ABW", "Australia" = "AUS", "Austria" = "AUT", "Azerbaijan" = "AZE", "Bahamas (the)" = "BHS", "Bahrain" = "BHR", "Bangladesh" = "BGD", "Barbados" = "BRB", "Belarus" = "BLR", "Belgium" = "BEL", "Belize" = "BLZ", "Benin" = "BEN", "Bermuda" = "BMU", "Bhutan" = "BTN", "Bolivia (Plurinational State of)" = "BOL", "Bonaire, Sint Eustatius and Saba" = "BES", "Bosnia and Herzegovina" = "BIH", "Botswana" = "BWA", "Bouvet Island" = "BVT", "Brazil" = "BRA", "British Indian Ocean Territory (the)" = "IOT", "Brunei Darussalam" = "BRN", "Bulgaria" = "BGR", "Burkina Faso" = "BFA", "Burundi" = "BDI", "Cabo Verde" = "CPV", "Cambodia" = "KHM", "Cameroon" = "CMR", "Canada" = "CAN", "Cayman Islands (the)" = "CYM", "Central African Republic (the)" = "CAF", "Chad" = "TCD", "Chile" = "CHL", "China" = "CHN", "Christmas Island" = "CXR", "Cocos (Keeling) Islands (the)" = "CCK", "Colombia" = "COL", "Comoros (the)" = "COM", "Congo (the Democratic Republic of the)" = "COD", "Congo (the)" = "COG", "Cook Islands (the)" = "COK", "Costa Rica" = "CRI", "Croatia" = "HRV", "Cuba" = "CUB", "Curaçao" = "CUW", "Cyprus" = "CYP", "Czechia" = "CZE", "Côte d'Ivoire" = "CIV", "Denmark" = "DNK", "Djibouti" = "DJI", "Dominica" = "DMA", "Dominican Republic (the)" = "DOM", "Ecuador" = "ECU", "Egypt" = "EGY", "El Salvador" = "SLV", "Equatorial Guinea" = "GNQ", "Eritrea" = "ERI", "Estonia" = "EST", "Eswatini" = "SWZ", "Ethiopia" = "ETH", "Falkland Islands (the) [Malvinas]" = "FLK", "Faroe Islands (the)" = "FRO", "Fiji" = "FJI", "Finland" = "FIN", "France" = "FRA", "French Guiana" = "GUF", "French Polynesia" = "PYF", "French Southern Territories (the)" = "ATF", "Gabon" = "GAB", "Gambia (the)" = "GMB", "Georgia" = "GEO", "Germany" = "DEU", "Ghana" = "GHA", "Gibraltar" = "GIB", "Greece" = "GRC", "Greenland" = "GRL", "Grenada" = "GRD", "Guadeloupe" = "GLP", "Guam" = "GUM", "Guatemala" = "GTM", "Guernsey" = "GGY", "Guinea" = "GIN", "Guinea-Bissau" = "GNB", "Guyana" = "GUY", "Haiti" = "HTI", "Heard Island and McDonald Islands" = "HMD", "Holy See (the)" = "VAT", "Honduras" = "HND", "Hong Kong" = "HKG", "Hungary" = "HUN", "Iceland" = "ISL", "India" = "IND", "Indonesia" = "IDN", "Iran (Islamic Republic of)" = "IRN", "Iraq" = "IRQ", "Ireland" = "IRL", "Isle of Man" = "IMN", "Israel" = "ISR", "Italy" = "ITA", "Jamaica" = "JAM", "Japan" = "JPN", "Jersey" = "JEY", "Jordan" = "JOR", "Kazakhstan" = "KAZ", "Kenya" = "KEN", "Kiribati" = "KIR", "Korea (the Democratic People's Republic of)" = "PRK", "Korea (the Republic of)" = "KOR", "Kuwait" = "KWT", "Kyrgyzstan" = "KGZ", "Lao People's Democratic Republic (the)" = "LAO", "Latvia" = "LVA", "Lebanon" = "LBN", "Lesotho" = "LSO", "Liberia" = "LBR", "Libya" = "LBY", "Liechtenstein" = "LIE", "Lithuania" = "LTU", "Luxembourg" = "LUX", "Macao" = "MAC", "Macedonia (the former Yugoslav Republic of)" = "MKD", "Madagascar" = "MDG", "Malawi" = "MWI", "Malaysia" = "MYS", "Maldives" = "MDV", "Mali" = "MLI", "Malta" = "MLT", "Marshall Islands (the)" = "MHL", "Martinique" = "MTQ", "Mauritania" = "MRT", "Mauritius" = "MUS", "Mayotte" = "MYT", "Mexico" = "MEX", "Micronesia (Federated States of)" = "FSM", "Moldova (the Republic of)" = "MDA", "Monaco" = "MCO", "Mongolia" = "MNG", "Montenegro" = "MNE", "Montserrat" = "MSR", "Morocco" = "MAR", "Mozambique" = "MOZ", "Myanmar" = "MMR", "Namibia" = "NAM", "Nauru" = "NRU", "Nepal" = "NPL", "Netherlands (the)" = "NLD", "New Caledonia" = "NCL", "New Zealand" = "NZL", "Nicaragua" = "NIC", "Niger (the)" = "NER", "Nigeria" = "NGA", "Niue" = "NIU", "Norfolk Island" = "NFK", "Northern Mariana Islands (the)" = "MNP", "Norway" = "NOR", "Oman" = "OMN", "Pakistan" = "PAK", "Palau" = "PLW", "Palestine, State of" = "PSE", "Panama" = "PAN", "Papua New Guinea" = "PNG", "Paraguay" = "PRY", "Peru" = "PER", "Philippines (the)" = "PHL", "Pitcairn" = "PCN", "Poland" = "POL", "Portugal" = "PRT", "Puerto Rico" = "PRI", "Qatar" = "QAT", "Romania" = "ROU", "Russian Federation (the)" = "RUS", "Rwanda" = "RWA", "Réunion" = "REU", "Saint Barthélemy" = "BLM", "Saint Helena, Ascension and Tristan da Cunha" = "SHN", "Saint Kitts and Nevis" = "KNA", "Saint Lucia" = "LCA", "Saint Martin (French part)" = "MAF", "Saint Pierre and Miquelon" = "SPM", "Saint Vincent and the Grenadines" = "VCT", "Samoa" = "WSM", "San Marino" = "SMR", "Sao Tome and Principe" = "STP", "Saudi Arabia" = "SAU", "Senegal" = "SEN", "Serbia" = "SRB", "Seychelles" = "SYC", "Sierra Leone" = "SLE", "Singapore" = "SGP", "Sint Maarten (Dutch part)" = "SXM", "Slovakia" = "SVK", "Slovenia" = "SVN", "Solomon Islands" = "SLB", "Somalia" = "SOM", "South Africa" = "ZAF", "South Georgia and the South Sandwich Islands" = "SGS", "South Sudan" = "SSD", "Spain" = "ESP", "Sri Lanka" = "LKA", "Sudan (the)" = "SDN", "Suriname" = "SUR", "Svalbard and Jan Mayen" = "SJM", "Sweden" = "SWE", "Switzerland" = "CHE", "Syrian Arab Republic" = "SYR", "Taiwan (Province of China)" = "TWN", "Tajikistan" = "TJK", "Tanzania, United Republic of" = "TZA", "Thailand" = "THA", "Timor-Leste" = "TLS", "Togo" = "TGO", "Tokelau" = "TKL", "Tonga" = "TON", "Trinidad and Tobago" = "TTO", "Tunisia" = "TUN", "Turkey" = "TUR", "Turkmenistan" = "TKM", "Turks and Caicos Islands (the)" = "TCA", "Tuvalu" = "TUV", "Uganda" = "UGA", "Ukraine" = "UKR", "United Arab Emirates (the)" = "ARE", "United Kingdom of Great Britain and Northern Ireland (the)" = "GBR", "United States Minor Outlying Islands (the)" = "UMI", "United States of America (the)" = "USA", "Uruguay" = "URY", "Uzbekistan" = "UZB", "Vanuatu" = "VUT", "Venezuela (Bolivarian Republic of)" = "VEN", "Viet Nam" = "VNM", "Virgin Islands (British)" = "VGB", "Virgin Islands (U.S.)" = "VIR", "Wallis and Futuna" = "WLF", "Western Sahara" = "ESH", "Yemen" = "YEM", "Zambia" = "ZMB", "Zimbabwe" = "ZWE", "Åland Islands" = "ALA" ))
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Geographic Coverage for the study within the country:")
        ),
        column(width = 6, offset = 0,
               textInput("geogCoverDDIInput", label = NULL, width = "100%", placeholder = "Free Text", value="Blablablablablabla")
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Kind of Data: Sample survey data [ssd] or Census/enumeration data [cen]")
        ),
        column(width = 6, offset = 0,
               selectInput("analysisUnitDDIInput", label = NULL,choices = c("ssd", "cen"))
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Describes the entity being analyzed in the study or in the variable:")
        ),
        column(width = 6, offset = 0,
               selectInput("analysisUnitDDIInput", label = NULL,choices = c("HousingUnit", "GeographicUnit")))
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("The procedure, technique, or mode of inquiry used to attain the data:")
        ),
        column(width = 6, offset = 0,
               selectInput("modeOfCollectionDDIInput", label = NULL,choices = c("Interview.FaceToFace.CAP", "Interview.Telephone.CATI", "SelfAdministeredQuestionnaire.FixedForm.WebBased", "FocusGroup.FaceToFace", "Observation"))
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Description of the study Universe: The group of persons or other elements that are the object of research and to which any analytic results refer:")
        ),
        column(width = 6, offset = 0,
               textInput("universeDDIInput", label = NULL, width = "100%", placeholder = "Free Text", value="Refugee Survey in Country x")
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Do you have a file describing the universe that can be joined to the survey (for instance registration data)?")
        ),
        column(width = 6, offset = 0,
               selectInput("universeyesDDIInput", label = NULL,choices = c("No","Yes"))
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Name of the csv file with universe data:")
        ),
        column(width = 6, offset = 0,
               textInput("universefileDDIInput", label = NULL, width = "100%", placeholder = "Free Text", value="universe.csv")
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Name of the variable within universe to do the join with the survey:")
        ),
        column(width = 6, offset = 0,
               textInput("universeidDDIInput", label = NULL, width = "100%", placeholder = "Free Text", value="progres.id")
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Name of the variable within survey to do the join with the universe:")
        ),
        column(width = 6, offset = 0,
               textInput("universesurveyidDDIInput", label = NULL, width = "100%", placeholder = "Free Text", value="progres.id")
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Description of the Sampling Procedure in the context of the study:")
        ),
        column(width = 6, offset = 0,
               textInput("sampProcDDIInput", label = NULL, width = "100%", placeholder = "Free Text", value="Blablablablablabla")
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Description of the generation of the final weight - for instance usage of post-stratification and other calibration:")
        ),
        column(width = 6, offset = 0,
               textInput("weightDDIInput", label = NULL, width = "100%", placeholder = "Free Text", value="Blablablablablabla")
        )
      ),
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Data Editing and Cleaning Operation: Description of the cleaning procedure:")
        ),
        column(width = 6, offset = 0,
               textInput("cleanOpsDDIInput", label = NULL, width = "100%", placeholder = "Free Text", value="Blablablablablabla")
        )
      ),
      column(3, style = "margin-bottom: 20px; padding-top: 0px;",
             actionButton("saveDDIButton", "Save DDI information", icon("save"), class = "uploadButton", style="margin: 15px 0px; height:45px; width:100%;")
      )
      
    )
  })
  
  observeEvent(input$saveDDIButton,{
    tryCatch({
      progress <- shiny::Progress$new()
      progress$set(message = "Saving DDI's info in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()
      if(sum(trimws(input$titlDDIInput) == "")){
        print("titlDDIInputEmpty")
        shinyalert("Title is required",
                   "'Title' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      if(sum(trimws(input$abstractDDIInput) == "")){
        print("abstractDDIInputEmpty")
        shinyalert("Abstract is required",
                   "'Abstract' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      if(sum(trimws(input$disclaimerDDIInput) == "")){
        print("disclaimerDDIInputEmpty")
        shinyalert("Rights & Disclaimer is required",
                   "'Rights & Disclaimer' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      if(sum(trimws(input$countryDDIInput) == "")){
        print("countryDDIInputEmpty")
        shinyalert("Country is required",
                   "'Country' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      if(sum(trimws(input$geogCoverDDIInput) == "")){
        print("geogCoverDDIInputEmpty")
        shinyalert("Geographic Coverage is required",
                   "'Geographic Coverage' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      if(sum(trimws(input$dataKindDDIInput) == "")){
        print("dataKindDDIInputEmpty")
        shinyalert("Kind of Data is required",
                   "'Kind of Data' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      if(sum(trimws(input$analysisUnitDDIInput) == "")){
        print("analysisUnitDDIInputEmpty")
        shinyalert("analysisUnit is required",
                   "'Describes the entity being analyzed in the study or in the variable' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      if(sum(trimws(input$modeOfCollectionDDIInput) == "")){
        print("modeOfCollectionDDIInputEmpty")
        shinyalert("modeOfCollection is required",
                   "'The procedure, technique, or mode of inquiry used to attain the data' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      if(sum(trimws(input$universeDDIInput) == "")){
        print("universeDDIInputEmpty")
        shinyalert("universe is required",
                   "'Description of the study Universe' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      if(sum(trimws(input$universeyesDDIInput) == "")){
        print("universeyesDDIInputEmpty")
        shinyalert("universeyes is required",
                   "'Do you have a file describing the universe that can be joined to the survey (for instance registration data)' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      
      if(sum(trimws(input$universefileDDIInput) == "" & sum(trimws(input$universeyesDDIInput) == "Yes"))){
        print("universefileDDIInputEmpty")
        shinyalert("universefile is required",
                   "'Name of the csv file with universe data' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      if(sum(trimws(input$universeidDDIInput) == "")){
        print("universeidDDIInputEmpty")
        shinyalert("universeid is required",
                   "'Name of the variable within universe to do the join with the survey' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      
      if(sum(trimws(input$universesurveyidDDIInput) == "")){
        print("universesurveyidDDIInputEmpty")
        shinyalert("universesurveyid is required",
                   "'Name of the variable within survey to do the join with the universe' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      
      if(sum(trimws(input$sampProcDDIInput) == "")){
        print("sampProcDDIInputEmpty")
        shinyalert("sampProc is required",
                   "'Description of the Sampling Procedure in the context of the study' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      
      if(sum(trimws(input$weightDDIInput) == "")){
        print("weightDDIInputEmpty")
        shinyalert("weight is required",
                   "'Description of the generation of the final weight - for instance usage of post-stratification and other calibration' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      if(sum(trimws(input$cleanOpsDDIInput) == "")){
        print("cleanOpsDDIInputEmpty")
        shinyalert("cleanOps is required",
                   "'Data Editing and Cleaning Operation' is empty! please fill it with a proper text",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      configInfo <- kobo_get_config()
      configInfo[configInfo$name=="titl", "value"] = trimws(input$titlDDIInput)
      configInfo[configInfo$name=="abstract", "value"] = trimws(input$abstractDDIInput)
      configInfo[configInfo$name=="disclaimer", "value"] = trimws(input$disclaimerDDIInput)
      configInfo[configInfo$name=="Country", "value"] = trimws(input$countryDDIInput)
      configInfo[configInfo$name=="geogCover", "value"] = trimws(input$geogCoverDDIInput)
      configInfo[configInfo$name=="dataKind", "value"] = trimws(input$dataKindDDIInput)
      configInfo[configInfo$name=="AnalysisUnit", "value"] = trimws(input$analysisUnitDDIInput)
      configInfo[configInfo$name=="ModeOfCollection", "value"] = trimws(input$modeOfCollectionDDIInput)
      configInfo[configInfo$name=="universe", "value"] = trimws(input$universeDDIInput)
      configInfo[configInfo$name=="universeyes", "value"] = trimws(input$universeyesDDIInput)
      configInfo[configInfo$name=="universefile", "value"] = trimws(input$universefileDDIInput)
      configInfo[configInfo$name=="universeid", "value"] = trimws(input$universeidDDIInput)
      configInfo[configInfo$name=="universesurveyid", "value"] = trimws(input$universesurveyidDDIInput)
      configInfo[configInfo$name=="sampProc", "value"] = trimws(input$sampProcDDIInput)
      configInfo[configInfo$name=="weight", "value"] = trimws(input$weightDDIInput)
      configInfo[configInfo$name=="cleanOps", "value"] = trimws(input$cleanOpsDDIInput)
      updateProgress()
      result <- kobo_edit_form(analysisSettings = configInfo )
      
      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      
    }, error = function(err) {
      print("jkfhg8fsdjksdjioerf")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })
  
  
  
  
  
  
  output$projectConfiguration2 <- renderUI({
    fluidRow(
      box(id="doYouHaveFormBox",
          width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
          column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                 h4("Do you have the xlsform?")
          ),
          column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                 selectInput("doYouHaveFormSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
          ),
          column(width = projectConfigurationTheme$warningBlockWidth, offset = 0,
                 if(file.exists(paste(mainDir(), "data", "/form.xls", sep = "/", collapse = "/"))){
                   div(class = "warningBlock",
                       span(class = "warningTitle","WARNING!"),
                       span(class = "warningBody","Be careful, there is already xlsform file (form.xls) in the data directory, once you upload the new file, it will be overridden.")
                   )
                 }

          ),
          column(width = 9,
                 conditionalPanel(
                   condition = "input.doYouHaveFormSelectInput == 'Yes'",
                   fileInput('xlsFormUploadedFile', 'Choose your xls form',
                             accept=c('.xls'))
                 )
          ),
          column(width = 3,
                 conditionalPanel(
                   condition = "input.doYouHaveFormSelectInput == 'Yes'",
                   actionButton("uploadxlsButton", "Upload xlsform", icon("upload"),
                                style="width:100%; margin-top: 25px;", class = "uploadButton" )
                 )
          )
      ),
      conditionalPanel(
        condition = "input.doYouHaveFormSelectInput == 'No'",
        box(id="doYouWantGenerateFormBox",
            width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
            conditionalPanel(
              condition = "input.doYouHaveFormSelectInput == 'No'",
              column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                     h4("Do you want to generate xlsform from your Data file?")
              ),
              column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                     selectInput("doYouWantGenerateFormSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
              )
            )
        )
      ),
      conditionalPanel(
        condition = "input.doYouHaveFormSelectInput != 'Yes' && input.doYouWantGenerateFormSelectInput == 'Yes' && input.doYouHaveFormSelectInput != '-- select --'",
        box(id="doYouHaveDataBox",
            width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
            column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                   h4("Do you have the Data file?")
            ),
            column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                   selectInput("doYouHaveDataSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
            ),
            column(width = projectConfigurationTheme$warningBlockWidth, offset = 0,
                   if(file.exists(paste(mainDir(), "data", "/MainDataFrame.csv", sep = "/", collapse = "/"))){
                     div(class = "warningBlock",
                         span(class = "warningTitle","WARNING!"),
                         span(class = "warningBody","Be careful, there is already MainDataFrame.csv file in the data directory, once you upload the new file, it will be overridden.")
                     )
                   }

            ),
            column(width = 12,
                   conditionalPanel(
                     condition = "input.doYouHaveDataSelectInput == 'Yes'",
                     column(width = 9,
                            fileInput('dataUploadedFile', 'Choose your Data file',
                                      accept=c('text/csv',
                                               'text/comma-separated-values,text/plain',
                                               '.csv'))),
                     column(width = 3, style = "border-left: 1px solid lightgray; margin-top: 10px;",
                            radioButtons('dataUploadedFileSep', 'Separator',
                                         c(Comma=',',
                                           Semicolon=';',
                                           Tab='\t'),
                                         ',', inline =TRUE)),
                     column(width = 12,
                            actionButton("dataUploadFileButton", "Upload file", icon("upload"), class = "uploadButton",
                                         style="width:100%; margin-bottom: 20px; height:45px;")
                     )
                   )
            )
        )
      ),
      conditionalPanel(
        condition = "input.doYouHaveFormSelectInput == 'Yes'",
        div(id="doYouHaveDatasetsDiv",
            box(id="doYouHaveDatasetsBox", title = "File(s) related to project", status = "danger" , 
                width=12,status="primary", solidHeader = FALSE, collapsible = TRUE,
                column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                       h4("Do you have the Data file(s)?")
                ),
                column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                       selectInput("doYouHaveDatasetsSelectInput2", label = NULL,choices = c("-- select --","Yes","No"))
                ),

                column(width = 12,
                       conditionalPanel(
                         condition = "input.doYouHaveDatasetsSelectInput == 'Yes'",
                         column(width = 12,
                                uiOutput("dataInputsUI9")
                         ),
                         column(width = 12,
                                actionButton("saveDataFilesButton", "Upload and Save files", icon("upload"), class = "uploadButton", style="margin: 15px 0px; width:100%;")
                         )

                       )
                )
            )
        )
      ),
      conditionalPanel(
        condition = "input.doYouHaveFormSelectInput == 'Yes' && input.doYouHaveDatasetsSelectInput == 'Yes'",
        box(id="doesFormNeedToPrepareBox",
            width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
            column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                   h4("Does your xlsform already include information about the Sampling used for data collection and data Cleaning log?")
            ),
            column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                   selectInput("formIncludeSettingsSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
            )
        )
      ),
      conditionalPanel(
        condition = "input.doYouHaveDataSelectInput == 'No' || input.doYouHaveDatasetsSelectInput == 'No'",
        infoBox(
          width = 12,strong("Warning"),h4("You cannot proceed without data file", align = "center")
          ,icon = icon("exclamation-triangle"),
          color = "yellow"
        )
      ),
      conditionalPanel(
        condition = "input.doYouHaveFormSelectInput == 'No' && input.doYouWantGenerateFormSelectInput == 'No'",
        infoBox(
          width = 12,strong("Warning"),h4("You cannot proceed without xlsform", align = "center")
          ,icon = icon("exclamation-triangle"),
          color = "yellow"
        )
      ),
      conditionalPanel(
        condition = "(input.doYouHaveFormSelectInput == 'No' && input.doYouWantGenerateFormSelectInput == 'Yes' && input.doYouHaveDataSelectInput == 'Yes') ||
        (input.doYouHaveFormSelectInput == 'Yes' && input.doYouHaveDatasetsSelectInput == 'Yes' && input.formIncludeSettingsSelectInput == 'No')",

        box(id="generateAndPrepareBox",
            width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
            column(width = 2, align="left",
                   icon("arrow-right", "fa-14x")
            ),
            conditionalPanel(
              condition = "(input.doYouHaveFormSelectInput == 'No' && input.doYouWantGenerateFormSelectInput == 'Yes' && input.doYouHaveDataSelectInput == 'Yes')",
              column(width = 3, align="center",
                     actionButton("generateFormButton", "Generate xlsform", class = "processButton")
              ),
              column(width = 2, align="center",
                     icon("arrow-right", "fa-14x")
              )
            ),

            column(width = 3, align="center",
                   actionButton("prepareFormButton", "Prepare xlsform", class = "processButton")
            ),
            column(width = 2, align="right",
                   icon("arrow-down", "fa-14x")
            )
        )
      ),
      conditionalPanel(
        condition = "(input.doYouHaveFormSelectInput == 'No' && input.doYouWantGenerateFormSelectInput == 'Yes' && input.doYouHaveDataSelectInput == 'Yes') ||
        (input.doYouHaveFormSelectInput == 'Yes' && input.doYouHaveDatasetsSelectInput == 'Yes' && input.formIncludeSettingsSelectInput == 'No')",
        div(id="recordSettingsDiv",
            box(id="recordSettingsBox", title = "Record Settings Configuration",
                width=12,status="primary", solidHeader = FALSE, collapsible = TRUE,
                column(width = 12, align="left",
                       uiOutput("recordSettingsUI")
                )
            )
        )
      ),

      conditionalPanel(
        condition = "(input.doYouHaveFormSelectInput == 'Yes' && input.doYouHaveDatasetsSelectInput == 'Yes') ||
        (input.doYouHaveFormSelectInput == 'No' && input.doYouHaveDataSelectInput == 'Yes' && input.doYouWantGenerateFormSelectInput == 'Yes') ",
        uiOutput("informationBoxAboutNextStep")
      )

  )
  })

  observeEvent(input$doYouHaveFormSelectInput,{
    projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
    projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
    projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
  })

  observeEvent(input$formIncludeSettingsSelectInput, {
    projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
    projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
    projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
    if(sum(input$formIncludeSettingsSelectInput=="No")==1){
      if(sum(input$doYouHaveFormSelectInput == 'Yes')==1){
        projectConfigurationInfo$log[["isPrepared"]] <- FALSE
      }else if(sum(input$doYouHaveFormSelectInput == 'No')==1){
        projectConfigurationInfo$log[["isPrepared"]] <- FALSE
        projectConfigurationInfo$log[["isGenerated"]] <- FALSE
      }
    }
    if(sum(input$formIncludeSettingsSelectInput=="Yes")==1 && projectConfigurationInfo$log[["xlsForm"]] && projectConfigurationInfo$log[["subAndMainfiles"]] ){
      shinyalert("Wooooow",
                 "Go to phase two: Analysis Plan Configrution",
                 type = "success",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#28A8E2",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    }

  })

  output$informationBoxAboutNextStep <- renderText({
    s <-""
    if(
      (sum(input$doYouHaveFormSelectInput == 'Yes')==1 && projectConfigurationInfo$log[["xlsForm"]] && projectConfigurationInfo$log[["subAndMainfiles"]] && (sum(input$formIncludeSettingsSelectInput == 'Yes') == 1 || (sum(input$formIncludeSettingsSelectInput == 'No') == 1 && projectConfigurationInfo$log[["isPrepared"]] && projectConfigurationInfo$log[["isRecordSettingsSaved"]]) ) ) ||
      (sum(input$doYouHaveFormSelectInput == 'No')==1 && projectConfigurationInfo$log[["data"]] && projectConfigurationInfo$log[["isGenerated"]] && projectConfigurationInfo$log[["isPrepared"]] && projectConfigurationInfo$log[["isRecordSettingsSaved"]] && sum(input$doYouHaveFormSelectInput == 'No') == 1)
    ){

      projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- TRUE
      s <- paste(
        div(
          infoBox(
            width = 12,strong("Perfect!"),h4("Go to the second phase: the Analysis Plan Configrution", align = "center")
            ,icon = icon("check"),
            color = "green"
          )
        )
        , s ,sep="" )
      return(s)
    }
  })

  observeEvent(input$uploadxlsButton, {
    tryCatch({
      progress <- shiny::Progress$new()
      progress$set(message = "Uploading xlsform in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()
      
      inFile <- input$xlsFormUploadedFile
      if (!is.null(inFile)){
        wb <- xlsx::loadWorkbook(inFile$datapath)
        xlsx::saveWorkbook(wb, paste(mainDir(), "data", "/form.xls", sep = "/", collapse = "/"))
        updateProgress()
        projectConfigurationInfo$log[["xlsForm"]] <- TRUE
        result <- kobo_get_begin_repeat()
        projectConfigurationInfo$data[["beginRepeatList"]] = c("MainDataFrame",result$names)
        projectConfigurationInfo$log[["beginRepeatList"]] = TRUE
        shinyalert("Done, xlsform has been successfully uploaded",
                   paste(
                     result$message,
                     "\n\n",
                     paste("You can find the xlsform file in",paste(mainDir(), "data", sep = "/", collapse = "/") )
                   ),
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        updateProgress()
        result <- kobo_prepare_form()
        if(class(result) == "try-error"){
          shinyalert("Error",
                     result,
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        updateProgress()
        projectConfigurationInfo$log[["isPrepared"]] <- TRUE
        projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        return(TRUE)
      }
      shinyalert("You have to select file before uploading process",
                 "To select the file click on Browse button.",
                 type = "info",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#28A8E2",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    }, error = function(err) {
      print("ksdfljiogbdf89")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  observeEvent(input$saveDataFilesButton, {
    tryCatch({
      progress <- shiny::Progress$new()
      progress$set(message = "Uploading files in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()
      if(!projectConfigurationInfo$log[["beginRepeatList"]]){
        shinyalert("xlsform is required",
                   "You have to upload xlsform before saving or uploading data files",
                   type = "info",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      check <- 0
      for(i in 1:(length(projectConfigurationInfo$data[["beginRepeatList"]]))){
        inFile <- input[[paste("fileInput",projectConfigurationInfo$data[["beginRepeatList"]][i],sep = "")]]
        if (!is.null(inFile)){
          check <- check + 1
        }
      }
      updateProgress()
      if(check != length(projectConfigurationInfo$data[["beginRepeatList"]])){
        shinyalert("Failed",
                   "You can't save and upload the files until you upload all requested files",
                   type = "info",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      settingsDF <- data.frame(name = character(),
                               label = character(),
                               options = character(),
                               value = character(),
                               path = character(),
                               stringsAsFactors = FALSE
      )

      for(i in 1:(length(projectConfigurationInfo$data[["beginRepeatList"]]))){
        inFile <- input[[paste("fileInput",projectConfigurationInfo$data[["beginRepeatList"]][i],sep = "")]]
        if (!is.null(inFile)){
          dataFile <- read.csv(inFile$datapath, header=TRUE, sep=input[[paste("separator",projectConfigurationInfo$data[["beginRepeatList"]][i],sep = "")]], stringsAsFactors = FALSE)
          fileName <- paste("/",projectConfigurationInfo$data[["beginRepeatList"]][i], ".csv", sep="")
          settingsDF <- rbind(settingsDF,  data.frame(name = projectConfigurationInfo$data[["beginRepeatList"]][i],
                                                      label = paste("Name and the path of", projectConfigurationInfo$data[["beginRepeatList"]][i]),
                                                      options = "",
                                                      value = paste(projectConfigurationInfo$data[["beginRepeatList"]][i], ".csv", sep=""),
                                                      path = paste0(mainDir(), "/data", fileName),
                                                      stringsAsFactors = FALSE
                                            )
                              )
          if(projectConfigurationInfo$data[["beginRepeatList"]][i]=="MainDataFrame"){
            projectConfigurationInfo$log[["data"]] <- TRUE
            projectConfigurationInfo$data[["data"]] <- dataFile
            write.csv(dataFile,  paste(mainDir(), "data", "/MainDataFrame_edited.csv", sep = "/", collapse = "/"), row.names = FALSE)
          }
          write.csv(dataFile,  paste(mainDir(), "data", fileName, sep = "/", collapse = "/"), row.names = FALSE)
        }
      }

      configInfo <- kobo_get_config()
      configInfo <- configInfo[!configInfo$name %in% settingsDF$name, ]
      settingsDF <- rbind(configInfo, settingsDF)
      settingsDF <- settingsDF[!is.na(settingsDF$name),]

      result <- kobo_edit_form(analysisSettings = settingsDF )

      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }


      updateProgress()
      projectConfigurationInfo$log[["subAndMainfiles"]] <- TRUE
      projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
      projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
      projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
      shinyalert("Done, all files have been successfully uploaded",
                 paste("You can find the files in",paste(mainDir(), "data", sep = "/", collapse = "/") )
                 ,
                 type = "success",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#28A8E2",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
      updateProgress()
      progress$close()

      showModal(showInputOfInstanceID())


    }, error = function(err) {
      print("903gjrvgof")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  showInputOfInstanceID <- function() {
    tryCatch({
      return(modalDialog(id="showInputOfInstanceIDPopUp",
                         title = "Instance ID and cluster ID",
                         uiOutput("showInputOfInstanceIDBody"),
                         size = "l",
                         footer = tagList(
                           modalButton("Exit", icon("sign-out-alt")),
                           actionButton("saveInstanceIDButton", "Save", class = "toolButton", style="height: 35px;")
                         )
      ))
    }, error = function(err) {
      print("8gvffdijiods")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  }

  output$showInputOfInstanceIDBody <- renderText({
    s <- ""
    levelsOfDF <- kobo_get_dataframes_levels()
    levelsOfDF <- levelsOfDF[levelsOfDF$name!="MainDataFrame",]

    if(nrow(levelsOfDF)!=0){
      for (i in 1:nrow(levelsOfDF)) {
        child  <- levelsOfDF[i, "name"]
        parent <- levelsOfDF[i, "parent"]
        colnamesOfChild <-  colnames(read.csv(paste0(mainDir(), "/data/", child, ".csv"), stringsAsFactors = FALSE))
        colnamesOfParent <- colnames(read.csv(paste0(mainDir(), "/data/", parent, ".csv"), stringsAsFactors = FALSE))
        style <- ""
        if( as.integer(i/2) == i/2 ){
          style <- "border-bottom: 1px solid lightgray;padding: 20px 0px;"
        }else{
          style <- "border-bottom: 1px solid lightgray; background-color: #f8f8ff; padding: 20px 0px;"
        }
        s <- paste(s,
                   column(width = 12, style = style,
                          column(width = 3,
                                 h4(paste("The instanceID between the child (",child,")",sep = ""))
                                 ),
                          column(width = 3, offset = 0,
                                 selectInput(paste("instanceIDInput", child, "child", parent, "parent", sep = ""), label = NULL,choices = c("-- select --",colnamesOfChild))
                                 ),

                          column(width = 3,
                                   h4(paste("and the parent (",parent,")",sep = ""))
                          ),
                          column(width = 3, offset = 0,
                                 selectInput(paste("instanceIDInput", parent, "parent", child, "child", sep = ""), label = NULL,choices = c("-- select --",colnamesOfParent))
                                 )
                     )
                   ,sep = "")
      }

      s <- box(width = 12, title = "Instance ID", status = "primary", solidHeader = T, collapsible = T,
                         HTML(s)
               )
    }
    s <- paste(s,
          box(width = 12, title = "Cluster ID", status = "primary", solidHeader = T, collapsible = T,
                column(width = 8, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                       h4("Select the ID variable for cluster report?"),
                       div(class = "help-tip-small",style="top: 0px; right: 25px;",
                           p(
                             span("This is the ID that will be used for kobo_cluster_report function.",style="display: block;")
                           )
                       )
                ),
                column(width = 4, offset = 0,
                       selectInput("clusterIDInput", label = NULL,choices = c("-- select --",colnames(projectConfigurationInfo$data[["data"]])))
                )
              )
    ,sep = "")
    return(s)

  })

  observeEvent(input$saveInstanceIDButton, {
    tryCatch({
      if(sum(input$clusterIDInput == "-- select --")){
        print("hgdtrhrfthdg")
        shinyalert("Cluster ID is required",
                   "You can't save the settings without selecting one of the variables",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      progress <- shiny::Progress$new()
      progress$set(message = "Save IDs in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()

      configInfo <- kobo_get_config()
      configInfo <- configInfo[!is.na(configInfo$name),]
      configInfo <- configInfo[!startsWith(tolower(configInfo$name), "instanceid"),]
      levelsOfDF <- kobo_get_dataframes_levels()
      levelsOfDF <- levelsOfDF[levelsOfDF$name!="MainDataFrame",]

      if(nrow(levelsOfDF)!=0){
        for (i in 1:nrow(levelsOfDF)) {
          child  <- levelsOfDF[i, "name"]
          parent <- levelsOfDF[i, "parent"]

          if(sum(input[[paste("instanceIDInput", child, "child", parent, "parent", sep = "")]] == "-- select --")){
            print("gfdhfgfdfhjhjd")
            shinyalert("Instance ID is required",
                       "You can't save the settings without selecting one of the variables",
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
            return(FALSE)
          }

          if(sum(input[[paste("instanceIDInput", parent, "parent", child, "child", sep = "")]] == "-- select --")){
            print("fgdjhgjkmhlkjl")
            shinyalert("Instance ID is required",
                       "You can't save the settings without selecting one of the variables",
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
            return(FALSE)
          }

          configInfo <- rbind(configInfo,
                              c(
                                paste0("instanceID_", child, "_", parent),
                                paste0("The instanceID between the child (",child,")", " and the parent (",parent,")"),
                                options = "",
                                input[[paste("instanceIDInput", child, "child", parent, "parent", sep = "")]],
                                path = ""
                              )
                              )

          configInfo <- rbind(configInfo,
                              c(
                                paste0("instanceID_", parent, "_", child),
                                paste0("The instanceID between the parent (",parent,")", " and the child (",child,")"),
                                options = "",
                                input[[paste("instanceIDInput", parent, "parent", child, "child", sep = "")]],
                                path = ""
                              )

          )
        }
      }
      updateProgress()
      mainDir <- kobo_getMainDirectory()
      form_tmp <- paste(mainDir, "data", "form.xls", sep = "/", collapse = "/")
      survey <- tryCatch({
        as.data.frame(read_excel(form_tmp, sheet = "survey"),
                      stringsAsFactors = FALSE) #read survey sheet from the form
      }, error = function(err) {
        data.frame( #if it doesn't exist, we need to create empty dataframe with those fields
          type = character(),
          name = character(),
          label = character(),
          "labelReport" = character(),
          variable = character(),
          disaggregation = character(),
          chapter = character(),
          structuralequation.risk = character(),
          structuralequation.coping = character(),
          structuralequation.resilience = character(),
          anonymise = character(),
          correlate = character(),
          clean = character(),
          cluster = character(),
          predict = character(),
          mappoint = character(),
          mappoly = character(),
          stringsAsFactors = FALSE,
          check.names = F
        )
      })

      if(!"cluster" %in% colnames(survey)){
        survey$cluster <- NA
      }else{
        ind <- which(tolower(survey$cluster)=="id")
        survey[ind, "cluster"] <- NA
      }
      if(length(survey[!is.na(survey$name) & survey$name=="X_index", "cluster"]) == 0){
        survey[length(survey)+1, "type"] <- "text"
        survey[length(survey)+1, "name"] <- input$clusterIDInput
        survey[length(survey)+1, "cluster"] <- "id"
      }else{
        survey[!is.na(survey$name) & survey$name=="X_index", "cluster"] <- "id"
      }

      updateProgress()

      result <- kobo_edit_form(survey = survey, analysisSettings = configInfo)

      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }


      removeModal()
    }, error = function(err) {
      print("jkfhg8fsdjksdjioerf")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  observeEvent(input$dataUploadFileButton, {
    tryCatch({
      inFile <- input$dataUploadedFile
      if (!is.null(inFile)){
        dataFile <- read.csv(inFile$datapath, header=TRUE, sep=input$dataUploadedFileSep, stringsAsFactors = FALSE)
        write.csv(dataFile,  paste(mainDir(), "data", "/MainDataFrame.csv", sep = "/", collapse = "/"), row.names = FALSE)
        write.csv(dataFile,  paste(mainDir(), "data", "/MainDataFrame_edited.csv", sep = "/", collapse = "/"), row.names = FALSE)
        projectConfigurationInfo$log[["data"]] <- TRUE
        projectConfigurationInfo$data[["data"]] <- dataFile
        projectConfigurationInfo$log[["isGenerated"]] <- FALSE
        projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        shinyalert(paste ("Done,", inFile$name ,"has been successfully uploaded"),
                   paste("You can find the file in",paste(mainDir(), "data", sep = "/", collapse = "/") )
                   ,
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )

        return(TRUE)
      }
      shinyalert("You have to select file before uploading process",
                 "To select the file click on Browse button.",
                 type = "info",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#28A8E2",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    }, error = function(err) {
      print("vsdoja89430kd")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  observeEvent(input$generateFormButton, {
    if(!projectConfigurationInfo$log[["data"]]){
      print("lppodcsfodi8")
      shinyalert("Error",
                 "You can't run this function without uploading MainDataFrame.csv file",
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    }
    if(
      sum( input$doYouHaveFormSelectInput == "No") &&
      sum( input$doYouWantGenerateFormSelectInput == "Yes") &&
      sum( input$doYouHaveDataSelectInput == "Yes") &&
      projectConfigurationInfo$log[["data"]]

    ){
      tryCatch({
        progress <- shiny::Progress$new()
        progress$set(message = "Generating xlsform in progress...", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
        updateProgress()
        kobo_to_xlsform(projectConfigurationInfo$data[["data"]])
        updateProgress()
        progress$close()
        projectConfigurationInfo$log[["xlsForm"]] <- TRUE
        projectConfigurationInfo$log[["isGenerated"]] <- TRUE
        projectConfigurationInfo$log[["isPrepared"]] <- FALSE
        projectConfigurationInfo$log[["isRecordSettingsSaved"]] <- FALSE
        projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        shinyalert("Done, xlsform created using 'kobo_to_xlsform' function",
                   "You can creates and save a xlsform skeleton from a data file in your data folder\nThe form.xls will be saved in the data folder of your project",
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )

      }, error = function(err) {
        print("893uedlkfmklsdlv")
        shinyalert("Error",
                   err$message,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
      })

    }
  })

  observeEvent(input$prepareFormButton, {
    tryCatch({
      if(!projectConfigurationInfo$log[["xlsForm"]] ||
         (sum(input$doYouHaveFormSelectInput == "No") &&  sum(input$doYouWantGenerateFormSelectInput == "Yes") && projectConfigurationInfo$log[["isGenerated"]] == FALSE)
      ){
        print("klgbvf89d8sad")
        shinyalert("Error",
                   "You can't run this function without uploading or generating xlsform file",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }

      if(
        (sum(input$doYouHaveFormSelectInput == "No") &&
         sum(input$doYouWantGenerateFormSelectInput == "Yes") &&
         sum(input$doYouHaveDataSelectInput == "Yes") &&
         projectConfigurationInfo$log[["xlsForm"]]) ||

        (sum(input$doYouHaveFormSelectInput == "Yes") &&
         sum(input$doYouHaveDatasetsSelectInput == "Yes") &&
         sum(input$formIncludeSettingsSelectInput == "No") &&
         projectConfigurationInfo$log[["xlsForm"]])
      ){

        progress <- shiny::Progress$new()
        progress$set(message = "Preparing xlsform in progress...", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
        updateProgress()

        result <- kobo_prepare_form()

        if(class(result) == "try-error"){
          shinyalert("Error",
                     result,
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }

        updateProgress()

        survey <- tryCatch({
          as.data.frame(read_excel(paste(mainDir(), "data", "/form.xls", sep = "/", collapse = "/"), sheet = "survey"), stringsAsFactors=FALSE) #read survey sheet from the form
        }, error = function(err) {
          NULL
        })
        projectConfigurationInfo$data[["xlsFormFields"]] <- survey[!survey$type %in% c("begin repeat", "end repeat", "end_repeat",  "begin_repeat",
                                                                                       "begin group", "end group", "end_group", "begin_group"
        ), "name"]

        #progress$close()
        shinyalert("Done, xlsform prepared using 'kobo_prepare_form' function",
                   "Prepare XLSform by adding chapter, disaggregation, correlate, variable, anonymise, structuralequation, clean, cluster, predict, mappoint, mappoly in case if those fields are not exist; the function will create dummy column for each one. Also, coloring all rows that have type equal to 'begin group', 'end group', 'begin repeat' or 'end repeat'.",
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )

        projectConfigurationInfo$log[["isPrepared"]] <- TRUE
        projectConfigurationInfo$log[["isRecordSettingsSaved"]] <- FALSE
        projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
      }
    }, error = function(err) {
      print("vdskljdc8t34")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  

  

  observeEvent(input$saveRecordSettingsConfigurationButton, {
    tryCatch({
      settingsDF <- data.frame(name = character(),
                               label = character(),
                               options = character(),
                               value = character(),
                               path = character(),
                               stringsAsFactors = FALSE
      )
      lastRow <- 1

      progress <- shiny::Progress$new()
      progress$set(message = "Saving settings sheet", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()

      if(sum(input$samplingSelectInput == "-- select --")){
        print("ldsaj329ujdsssds")
        shinyalert("Error",
                   "You can't save the settings without selecting one of the sampling's options\n please select one.",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      updateProgress()
      if(sum(input$samplingSelectInput == "No sampling (type 1)")){
        settingsDF[lastRow,"name"] <- "sample_type"
        settingsDF[lastRow,"label"] <- "Sample type of the project"
        settingsDF[lastRow,"options"] <- "1. No sampling (type 1) 2. Cluster sample (type 2) 3. Stratified sample (type 3)"
        settingsDF[lastRow,"value"] <- "No sampling (type 1)"
      }
      else if(sum(input$samplingSelectInput == "Cluster sample (type 2)")){
        settingsDF[lastRow,"name"] <- "sample_type"
        settingsDF[lastRow,"label"] <- "Sample type of the project"
        settingsDF[lastRow,"options"] <- "1. No sampling (type 1) 2. Cluster sample (type 2) 3. Stratified sample (type 3)"
        settingsDF[lastRow,"value"] <- input$samplingSelectInput

        if(sum(input$variableNameCluster == "")){
          print("dkjchd8s97ydsj")
          shinyalert("Error",
                     "You need to select the name of cluster variable",
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        updateProgress()
        lastRow <- lastRow+1
        settingsDF[lastRow,"name"] <- "variable_name"
        settingsDF[lastRow,"label"] <- "The name of cluster variable that will be used to join the weight file with the main file, please make sure the name of this variable exists in both files"

        settingsDF[lastRow,"value"] <- input$variableNameCluster


        lastRow <- lastRow+1
        updateProgress()
        inFileWeightsCluster<- input$weightsClusterFileInput
        if(is.null(inFileWeightsCluster)){
          print("knmxcjxgvy78")
          shinyalert("Error",
                     "You need to choose weights file for Cluster sample",
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }else if(!is.null(inFileWeightsCluster)){
          dataFile <- read.csv(inFileWeightsCluster$datapath, header=TRUE, sep=input$weightsClusterSep, stringsAsFactors = FALSE)
          write.csv(dataFile,  paste(mainDir(), "data", "/weightsCluster.csv", sep = "/", collapse = "/"), row.names = FALSE)
          lastRow <- lastRow+1
          settingsDF[lastRow,"name"] <- "weights_info"
          settingsDF[lastRow,"label"] <- "Weights that will be used in cluster sample"
          settingsDF[lastRow,"value"] <- "weightsCluster.csv"
          settingsDF[lastRow,"path"] <-  paste(mainDir(), "data", "weightsCluster.csv", sep = "/", collapse = "/")
        }
      }
      else if(sum(input$samplingSelectInput == "Stratified sample (type 3)")){
        settingsDF[lastRow,"name"] <- "sample_type"
        settingsDF[lastRow,"label"] <- "Sample type of the project"
        settingsDF[lastRow,"options"] <- "1. No sampling (type 1) 2. Cluster sample (type 2) 3. Stratified sample (type 3)"
        settingsDF[lastRow,"value"] <- input$samplingSelectInput
        updateProgress()
        if(sum(input$variableNameStratified == "")){
          print("09e3klkfdslsss")
          shinyalert("Error",
                     "You need to select the name of Stratified variable",
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        lastRow<-lastRow+1
        settingsDF[lastRow,"name"] <- "variable_name"
        settingsDF[lastRow,"label"] <- "The name of Stratified variable"
        settingsDF[lastRow,"value"] <- input$variableNameStratified
        updateProgress()
        lastRow <- lastRow+1


        inFileWeightsStratified<- input$weightsStratifiedFileInput
        if(is.null(inFileWeightsStratified)){
          print("9cvdsjfdifd")
          shinyalert("Error",
                     "You need to choose weights file for Stratified sample",
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }else if(!is.null(inFileWeightsStratified)){
          dataFile <- read.csv(inFileWeightsStratified$datapath, header=TRUE, sep=input$weightsStratifiedSep, stringsAsFactors = FALSE)
          write.csv(dataFile,  paste(mainDir(), "data", "/weightsStratified.csv", sep = "/", collapse = "/"), row.names = FALSE)
          lastRow <- lastRow+1
          settingsDF[lastRow,"name"] <- "weights_info"
          settingsDF[lastRow,"label"] <- "Weights file that will be used in Stratified sample"
          settingsDF[lastRow,"value"] <- "weightsStratified.csv"
          settingsDF[lastRow,"path"] <-  paste(mainDir(), "data", "weightsStratified.csv", sep = "/", collapse = "/")
        }
      }

      updateProgress()

      if(sum(input$cleaningLogSelectInput == "-- select --")){
        print("atywfqdhufe7")
        shinyalert("Error",
                   "You can't save the settings before answering the 'cleaning log' question",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      if(sum(input$cleaningLogSelectInput == "No")){
        lastRow <- lastRow+1
        settingsDF[lastRow,"name"] <- "cleaning_log"
        settingsDF[lastRow,"label"] <- "cleaning log plan for the project"
        settingsDF[lastRow,"options"] <- "1. Yes 2. No, 3. csv filename"
        settingsDF[lastRow,"value"] <- "No"
      }else{
        inFilecleaningLog <- input$cleaningLogFileInput
        if(is.null(inFilecleaningLog) && sum(input$cleaningLogSelectInput == "Yes")){
          print("fdsgftrrr554")
          shinyalert("Error",
                     "You need to upload 'cleaning log' file before saving the settings",
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }else if(!is.null(inFilecleaningLog) && sum(input$cleaningLogSelectInput == "Yes")){
          dataFile <- read.csv(inFilecleaningLog$datapath, header=TRUE, sep=input$cleaningLogSep, stringsAsFactors = FALSE)
          write.csv(dataFile,  paste(mainDir(), "data", "/cleaningLog.csv", sep = "/", collapse = "/"), row.names = FALSE)
          lastRow <- lastRow+1
          settingsDF[lastRow,"name"] <- "cleaning_log"
          settingsDF[lastRow,"label"] <- "cleaning log plan for the project"
          settingsDF[lastRow,"options"] <- "1. Yes 2. No, 3. csv filename"
          settingsDF[lastRow,"value"] <- "cleaningLog.csv"
          settingsDF[lastRow,"path"] <-  paste(mainDir(), "data", "/cleaningLog.csv", sep = "/", collapse = "/")
        }
      }

      configInfo <- kobo_get_config()
      configInfo <- configInfo[!is.na(configInfo$name),]
      configInfo <- configInfo[!configInfo$name %in% settingsDF$name, ]
      settingsDF <- rbind(settingsDF, configInfo)

      settingsDF <- settingsDF[!is.na(settingsDF$name),]

      result <- kobo_edit_form(analysisSettings = settingsDF )

      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }


      projectConfigurationInfo$log[["isRecordSettingsSaved"]] <- TRUE
      updateProgress()
      if(sum(input$samplingSelectInput != "No sampling (type 1)")){
        showModal(showSamplingMoreParm())
      }else{
        shinyalert("Done, Record Settings Configuration has been successfully saved",
                   "You can find the Settings in 'analysisSettings' sheet in xlsform file",
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = FALSE
        )
        Sys.sleep(3)
        shinyalert("Wooooow",
                   "You can start the Analysis Plan Configrution",
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
      }


    }, error = function(err) {
      print("dsuyhg78w90")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  showSamplingMoreParm <- function() {
    tryCatch({
      return(modalDialog(id="showSamplingMoreParmPopUp",
                         title = "More required parameters for sampling method",
                         uiOutput("showSamplingMoreParmBody"),
                         size = "l",
                         footer = tagList(
                           actionButton("saveSamplingMoreParmButton", "Save", class = "toolButton", style="height: 35px;")
                         )
      ))
    }, error = function(err) {
      print("8gvffdijiods")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  }

  output$showSamplingMoreParmBody <- renderText({
    mainDir <- kobo_getMainDirectory()
    configInfo <- kobo_get_config()
    configInfo <- configInfo[!is.na(configInfo$name),]
    s <- ""
    if(configInfo[configInfo$name=="sample_type","value"] == "Cluster sample (type 2)"){
      path <- configInfo[configInfo$name=="weights_info", "path"]
      print(path)
      weight <- read.csv(file=path,stringsAsFactors = F)
      s <- paste(s,
                 column(width = 12,
                        column(width = 8, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                               h4("Select the variable that contains the weights for cluster sample:")
                        ),
                        column(width = 4, offset = 0,
                               selectInput("weightsClusterVariableInput", label = NULL, choices = c("-- select --",colnames(weight)))
                        ),

                        column(width = 8, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                               h4("Enter number of clusters:")
                        ),
                        column(width = 4, offset = 0,
                               numericInput("numberOfClustersInput", label = NULL, value=1, min = 1, max=10000, step = 1)
                        )
                 )
                 ,sep="")

    }else if(configInfo[configInfo$name=="sample_type","value"] == "Stratified sample (type 3)"){
      path <- configInfo[configInfo$name=="weights_info", "value"]
      weight <- read.csv(path,stringsAsFactors = F)
      s <- paste(s,
                 column(width = 12,
                        column(width = 8, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                               h4("Select the variable that contains the weights for Stratified sample:")
                        ),
                        column(width = 4, offset = 0,
                               selectInput("weightsStratifiedVariableInput", label = NULL,choices = c("-- select --",colnames(weight)))
                        )
                 )
                 ,sep="")

    }

    return(s)
  })

  observeEvent(input$saveSamplingMoreParmButton,{
    tryCatch({
      progress <- shiny::Progress$new()
      progress$set(message = "Saving settings sheet", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()

      configInfo <- kobo_get_config()
      configInfo <- configInfo[!configInfo$name%in% c("weightsVariable", "numberOfClusters"),]

      if(configInfo[configInfo$name=="sample_type","value"] == "Cluster sample (type 2)"){
        if(sum(input$weightsClusterVariableInput == "-- select --")){
          print("gfdhfjukli")
          shinyalert("Weights Cluster Variable is required",
                     "You can't save the settings without selecting one of the variables for Weights",
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        if(length(configInfo[configInfo$name=="weightsVariable","value"]) == 0){
          configInfo <- rbind(configInfo, c("weightsVariable", "The variable that contains the weights",NA,input$weightsClusterVariableInput,NA ))
        }else{
          if(is.na(configInfo[configInfo$name=="weightsVariable","value"])){
            configInfo <- rbind(configInfo, c("weightsVariable", "The variable that contains the weights",NA,input$weightsClusterVariableInput,NA ))
          }else{
            configInfo[configInfo$name=="weightsVariable","value"] <- input$weightsClusterVariableInput
          }

        }
        updateProgress()
        if(length(configInfo[configInfo$name=="numberOfClusters","value"]) == 0){
          configInfo <- rbind(configInfo, c("numberOfClusters", "Number of clusters",NA,input$numberOfClustersInput,NA ))
        }else{
          if(is.na(configInfo[configInfo$name=="numberOfClusters","value"])){
            configInfo <- rbind(configInfo, c("numberOfClusters", "Number of clusters",NA,input$numberOfClustersInput,NA ))
          }else{
            configInfo[configInfo$name=="numberOfClusters","value"] <- input$numberOfClustersInput
          }

        }
        path <- configInfo[configInfo$name=="weights_info", "path"]
        weight <- read.csv(path,stringsAsFactors = F)
        weight$fpc <- input$numberOfClustersInput
        write.csv(weight, path, row.names = F)
        updateProgress()
      }else if(configInfo[configInfo$name=="sample_type","value"] == "Stratified sample (type 3)"){
        if(sum(input$weightsStratifiedVariableInput == "-- select --")){
          print("htydhghf")
          shinyalert("Weights Stratified Variable is required",
                     "You can't save the settings without selecting one of the variables for Weights",
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        if(length(configInfo[configInfo$name=="weightsVariable","value"]) == 0){
          configInfo <- rbind(configInfo, c("weightsVariable", "The variable that contains the weights",NA,input$weightsStratifiedVariableInput,NA ))
        }else{
          if(is.na(configInfo[configInfo$name=="weightsVariable","value"])){
            configInfo <- rbind(configInfo, c("weightsVariable", "The variable that contains the weights",NA,input$weightsStratifiedVariableInput,NA ))
          }else{
            configInfo[configInfo$name=="weightsVariable","value"] <- input$weightsStratifiedVariableInput
          }

        }
        updateProgress()
      }
      updateProgress()

      result <- kobo_edit_form(analysisSettings = configInfo)

      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }


      removeModal()
      shinyalert("Done, Record Settings Configuration has been successfully saved",
                 "You can find the Settings in 'analysisSettings' sheet in xlsform file",
                 type = "success",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#28A8E2",
                 animation = FALSE,
                 showConfirmButton = FALSE
      )
      Sys.sleep(3)
      shinyalert("Wooooow",
                 "You can start Analysis Plan Configrution",
                 type = "success",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#28A8E2",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    }, error = function(err) {
      print("dgtryhehfyd")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  #######################################           End               ############################################

  ####################################### Analysis Plan Configuration page ############################################
  sheets <- reactiveValues()

  lastMenuItem <- reactiveValues(v=NULL)

  output$analysisPlanConfiguration <- renderUI({
    if(!projectConfigurationInfo$log[["isRecordSettingsCompleted"]]){
      infoBox(
        width = 12,strong("Warning"),h4("You cannot proceed without completing Project Configuration section", align = "center")
        ,icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    }else{
      fluidRow(
        box(id="doYouHaveAnalysisPlanBox",
            width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
            column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                   h4("Does xlsform include documented analysis plan?")
            ),
            column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                   selectInput("doYouHaveAnalysisPlanSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
            )
        ),
        column(width = 12,
               div(id="wellMenuForAPC",
                   sidebarLayout(
                     sidebarPanel(width = 2,
                                  sidebarMenu(id="sidebarMenuForAP",
                                              menuItem(div(span("1",class = "numberStep"),span("Re-Labeling Survey Sheet")), tabName = "relabelingSurvey"),
                                              menuItem(div(span("2",class = "numberStep"),span("Re-Labeling Choices Sheet")), tabName = "relabelingChoices"),
                                              #menuItem(div(span("3",class = "numberStep"),span("Text type")), tabName = "textType"),
                                              menuItem(div(span("3",class = "numberStep"),span("Select_one type")), tabName = "selectOneType"),
                                              menuItem(div(span("4",class = "numberStep"),span("Order Ordinal Variables")), tabName = "orderOrdinalVariables"),
                                              menuItem(div(span("5",class = "numberStep"),span("Select_multiple type")), tabName = "selectMultipleType"),
                                              menuItem(div(span("6",class = "numberStep"),span("Numeric type")), tabName = "numericType"),
                                              #menuItem(div(span("8",class = "numberStep"),span("Integer type")), tabName = "integerType"),
                                              menuItem(div(span("7",class = "numberStep"),span("Date type")), tabName = "dateType"),
                                              #menuItem(div(span("10",class = "numberStep"),span("Decimal type")), tabName = "decimalType"),
                                              menuItem(div(span("8",class = "numberStep"),span("Indicators Sheet")), tabName = "indicatorsSheet"),
                                              menuItem(div(span("9",class = "numberStep"),span("Chapter")), tabName = "chapter"),
                                              menuItem(NULL, icon = icon("info-circle"), tabName = "infoAPC")
                                  ),
                                  div(id="styleFormDiv", style="background-color: #ecf0f5;",
                                      actionButton("styleFormButton", "Add style to xlsform", style="width: 100%; margin: 10px 0% 0px; line-height: 35px;", class = "uploadButton"))
                     ),
                     mainPanel(width = 10,
                               div(id = "analysisPlanTab",
                                   tabItems(
                                     tabItem(tabName = "relabelingSurvey",
                                             uiOutput("relabelingSurveyUI")
                                     ),
                                     tabItem(tabName = "relabelingChoices",
                                             uiOutput("relabelingChoicesUI")
                                     ),
                                     tabItem(tabName = "selectOneType",
                                             uiOutput("selectOneTypeUI")
                                     ),
                                     tabItem(tabName = "orderOrdinalVariables",
                                             uiOutput("orderOrdinalVariablesUI")
                                     ),
                                     tabItem(tabName = "selectMultipleType",
                                             uiOutput("selectMultipleTypeUI")
                                     ),
                                     tabItem(tabName = "numericType",
                                             uiOutput("numericTypeUI")
                                     ),
                                     tabItem(tabName = "dateType",
                                             uiOutput("dateTypeUI")
                                     ),
                                     tabItem(tabName = "indicatorsSheet",
                                             uiOutput("indicatorsSheetUI")
                                     ),
                                     tabItem(tabName = "chapter",
                                             uiOutput("chapterUI")
                                     ),
                                     tabItem(tabName = "infoAPC",
                                             uiOutput("infoAPCUI")
                                     )
                                   )
                               )
                     )
                   )
               )
        ),
        box(id="decoAndCheckplanBox",
              width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
              column(width = 2, align="left",
                     icon("arrow-right", "fa-24x")
              ),
              column(width = 2, align="center",
                     icon("arrow-right", "fa-24x")
              ),
              column(width = 3, align="center",
                     actionButton("checkPlanButton", "Check the Plan", class = "processButtonLarge")
              ),
              column(width = 2, align="center",
                     icon("arrow-right", "fa-24x")
              ),
              column(width = 2, align="right",
                     icon("arrow-right", "fa-24x")
              )
          )
        )
    }
  })

  observeEvent(input$checkPlanButton, {
    tryCatch({
      progress <- shiny::Progress$new()
      progress$set(message = "Checking analysis plan in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()
      result <- kobo_check_analysis_plan()

      if(result$flag){
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- TRUE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        shinyalert("Wooooow",
                   "You can start Data Processing",
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
      }else{
        shinyalert("You can't start Data Processing\nyour plan contains some errors",
                   result$message,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
      }


    }, error = function(err) {
      print("ghdhkkjdasfdsa")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  observeEvent(input$styleFormButton, {
    tryCatch({
      progress <- shiny::Progress$new()
      progress$set(message = "Styling xlsform in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()

      result <- kobo_prepare_form()

      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }


      updateProgress()

      shinyalert("Done, xlsform styled using 'kobo_prepare_form' function",
                 "Prepare XLSform by adding chapter, disaggregation, correlate, variable, anonymise, structuralequation, clean, cluster, predict, mappoint, mappoly in case if those fields are not exist; the function will create dummy column for each one. Also, coloring all rows that have type equal to 'begin group', 'end group', 'begin repeat' or 'end repeat'.",
                 type = "success",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#28A8E2",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )


    }, error = function(err) {
      print("fdsg54tu7j")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  #################### Relabeling Survey#############################
  output$relabelingSurveyUI <- renderUI({
    box(id="relabelingSurveyBox",
        width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        rHandsontableOutput("relabelingSurveyTable")
    )
  })
  output$relabelingSurveyTable <- renderRHandsontable({
    tryCatch({
      temp <- rhandsontable(sheets[["relabelingSurvey"]], stretchH = "all", height = 600, useTypes = TRUE) %>%
        hot_col("type", readOnly = TRUE, width = 200) %>%
        hot_col("name", readOnly = TRUE, width = 200) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)


      if("label" %in% colnames(sheets[["relabelingSurvey"]])){
        temp <- temp %>% hot_col("label", readOnly = TRUE, width = 200) %>%
          hot_col("labelReport", width = 400, allowInvalid = FALSE,
                  validator = "
                  function (value, callback) {
                  setTimeout(function(){
                  if(value.length >= 85){
                  alert('Please make sure that the length of the string less than 85 characters');
                  }
                  if(value.length <= 0){
                  alert('Please make sure that the length of the string greater than 0 characters');
                  }
                  callback(value.length < 85 && value.length > 0);
                  }, 700)
                  }")
      }else{
        temp <- temp %>% hot_col("labelReport", width = 400, allowInvalid = FALSE,
                                 validator = "
                                 function (value, callback) {
                                 setTimeout(function(){
                                 if(value.length >= 85){
                                 alert('Please make sure that the length of the string less than 80 characters');
                                 }
                                 callback(value.length < 85);
                                 }, 700)
                                 }"
                                 )
      }

      if("hint" %in% colnames(sheets[["relabelingSurvey"]])){
        temp <- temp %>% hot_col("hint", readOnly = TRUE, width = 200) %>%
          hot_col("hintReport", width = 400, allowInvalid = FALSE,
                  validator = "
                  function (value, callback) {
                  setTimeout(function(){
                  if(value.length >= 85){
                  alert('Please make sure that the length of the string less than 85 characters');
                  }
                  if(value.length <= 0){
                  alert('Please make sure that the length of the string greater than 0 characters');
                  }
                  callback(value.length < 85 && value.length > 0);
                  }, 700)
                  }")
      }else{
        temp <- temp %>% hot_col("hintReport", width = 400, allowInvalid = FALSE,
                                 validator = "
                                 function (value, callback) {
                                 setTimeout(function(){
                                 if(value.length >= 85){
                                 alert('Please make sure that the length of the string less than 85 characters');
                                 }
                                 callback(value.length < 85);
                                 }, 700)
                                 }"
                                 )
      }
      temp
}, error = function(err) {
  print("1")
  shinyalert("Error",
             err$message,
             type = "error",
             closeOnClickOutside = FALSE,
             confirmButtonCol = "#ff4d4d",
             animation = FALSE,
             showConfirmButton = TRUE
  )
})
    })


  #################### Relabeling Choices#############################
  output$relabelingChoicesUI <- renderUI({
    box(id="relabelingChoicesBox",
        width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        rHandsontableOutput("relabelingChoicesTable")
    )
  })
  output$relabelingChoicesTable <- renderRHandsontable({
    tryCatch({
      temp <- rhandsontable(sheets[["relabelingChoices"]], stretchH = "all", height = 600, useTypes = TRUE) %>%
        hot_col("list_name", readOnly = TRUE, width = 200) %>%
        hot_col("name", readOnly = TRUE, width = 200) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)


      if("label" %in% colnames(sheets[["relabelingChoices"]])){
        temp <- temp %>% hot_col("label", readOnly = TRUE, width = 200) %>%
          hot_col("labelReport", width = 400, allowInvalid = FALSE,
                  validator = "
                  function (value, callback) {
                  setTimeout(function(){
                  if(value.length >= 80){
                  alert('Please make sure that the length of the string less than 80 characters');
                  }
                  if(value.length <= 0){
                  alert('Please make sure that the length of the string greater than 0 characters');
                  }
                  callback(value.length < 80 && value.length > 0);
                  }, 700)
                  }")
      }
      else{
        temp <- temp %>% hot_col("labelReport", width = 400, allowInvalid = FALSE,
                                 validator = "
                                 function (value, callback) {
                                 setTimeout(function(){
                                 if(value.length >= 80){
                                 alert('Please make sure that the length of the string less than 80 characters');
                                 }
                                 callback(value.length < 80);
                                 }, 700)
                                 }"
                                 )
      }
      temp
      }, error = function(err) {
        print("2")
        shinyalert("Error",
                   err$message,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
      })
    })



  ##################### SelectOne Type#############################
  output$selectOneTypeUI <- renderUI({
    box(id="selectOneTypeBox",
        width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        uiOutput("selectOneTypeBody")
        #rHandsontableOutput("selectOneTypeTable")
    )
  })

  output$selectOneTypeBody <- renderUI({

    form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
    selectOneType <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                   stringsAsFactors = FALSE)
    reqNames <- c("type",   "name" ,  "label", "variable",
                  "disaggregation", #"chapter",
                  "structuralequation.risk","structuralequation.coping","structuralequation.resilience","anonymise","correlate","clean","cluster","predict","mappoint","mappoly"

    )
    if(sum(sapply(reqNames, function(x){x %in% colnames(selectOneType)})) != length(reqNames)){
      print("fdsgtyh56ghtf")
      shinyalert("Error",
                 paste("You need to make sure that all required fields are existing in survey sheet\n",
                       reqNames
                 ),
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
      return(FALSE)
    }
    selectOneType <- selectOneType[reqNames]
    selectOneType <- selectOneType[startsWith(tolower(selectOneType$type), "select_one"),]


    if(nrow(selectOneType)>0){
      rHandsontableOutput("selectOneTypeTable")
    }else{
      infoBox(
        width = 12,strong("Info"),
        h4("There is no select_one type, you can start with the next step", align = "center")
        ,icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    }

  })

  output$selectOneTypeTable <- renderRHandsontable({
    tryCatch({
      temp <- rhandsontable(sheets[["selectOneType"]], stretchH = "all", height = 600, useTypes = TRUE) %>%
        hot_col("type", readOnly = TRUE, width = 200) %>%
        hot_col("name", readOnly = TRUE, width = 200) %>%
        hot_col("label", readOnly = TRUE, width = 200) %>%
        hot_col("variable", type = "dropdown", source = c("ordinal factor", "factor"), width = 120) %>%
        #hot_col("chapter",  width = 200) %>%
        hot_col("disaggregation",  width = 120, halign="htCenter") %>%
        hot_col("structuralequation.risk",  width = 200, halign="htCenter") %>%
        hot_col("structuralequation.coping",  width = 200, halign="htCenter") %>%
        hot_col("structuralequation.resilience",  width = 200, halign="htCenter") %>%
        hot_col("anonymise", type = "dropdown", source = c("default-non-anonymised", "key", "remove", "sensitive", "reference"), width = 120) %>%
        hot_col("correlate",  width = 120, halign="htCenter") %>%
        hot_col("clean",  width = 120, halign="htCenter") %>%
        hot_col("cluster",  width = 120, halign="htCenter") %>%
        hot_col("predict",  width = 120, halign="htCenter") %>%
        hot_col("mappoint",  width = 120, halign="htCenter") %>%
        hot_col("mappoly",  width = 120, halign="htCenter") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)

      temp
    }, error = function(err) {
      print("3")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })


  #####################Order Ordinal Variables#############################
  output$orderOrdinalVariablesUI <- renderUI({
    box(id = "orderOrdinalVariablesBox",
        width = 12,status = "primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        uiOutput("orderOrdinalVariablesBody")
        #rHandsontableOutput("orderOrdinalVariablesTable")
    )
  })
  output$orderOrdinalVariablesBody <- renderUI({

    if (is.null(sheets[["orderOrdinalVariables"]])) {
      return(FALSE)
    }
    if (nrow(sheets[["orderOrdinalVariables"]])>0) {
      rHandsontableOutput("orderOrdinalVariablesTable")
    } else {
      infoBox(
        width = 12,strong("Info"),
        h4("There is no Ordinal Variables type, you can start with the next step", align = "center")
        ,icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    }

  })

  output$orderOrdinalVariablesTable <- renderRHandsontable({
    tryCatch({
      temp <- rhandsontable(sheets[["orderOrdinalVariables"]], stretchH = "all", height = 600, useTypes = TRUE) %>%
        hot_col("list_name", readOnly = TRUE, width = 200) %>%
        hot_col("name", readOnly = TRUE, width = 200) %>%
        hot_col("label", readOnly = TRUE, width = 200) %>%
        hot_col("order",  width = 30, type = "autocomplete", source = 1:100 ) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)

      temp
    }, error = function(err) {
      print("4")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })


  #####################Select_multiple type#############################
  output$selectMultipleTypeUI <- renderUI({
    box(id = "selectMultipleTypeBox",
        width = 12,status = "primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        uiOutput("selectMultipleTypeBody")
        #rHandsontableOutput("selectMultipleTypeTable")
    )
  })
  output$selectMultipleTypeBody <- renderUI({

    form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
    selectMultipleType <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                        stringsAsFactors = FALSE)
    reqNames <- c("type",   "name" ,  "label", "variable",
                  "disaggregation", #"chapter",
                  "structuralequation.risk","structuralequation.coping","structuralequation.resilience","anonymise","correlate","clean","cluster","predict","mappoint","mappoly"

    )
    if (sum(sapply(reqNames, function(x){x %in% colnames(selectMultipleType)})) != length(reqNames)) {
      print("dgtshfgfhdfAS")
      shinyalert("Error",
                 paste("You need to make sure that all required fields are existing in survey sheet\n",
                       reqNames
                 ),
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
      return(FALSE)
    }
    selectMultipleType <- selectMultipleType[reqNames]
    selectMultipleType <- selectMultipleType[startsWith(tolower(selectMultipleType$type), "select_multiple"),]
    if (nrow(selectMultipleType)>0) {
      rHandsontableOutput("selectMultipleTypeTable")
    }else{
      infoBox(
        width = 12,strong("Info"),
        h4("There is no select_Multiple type, you can start with the next step", align = "center")
        ,icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    }

  })

  output$selectMultipleTypeTable <- renderRHandsontable({
    tryCatch({
      temp <- rhandsontable(sheets[["selectMultipleType"]], stretchH = "all", height = 600, useTypes = TRUE) %>%
        hot_col("type", readOnly = TRUE, width = 200) %>%
        hot_col("name", readOnly = TRUE, width = 200) %>%
        hot_col("label", readOnly = TRUE, width = 200) %>%
        hot_col("variable", readOnly = TRUE, width = 200) %>%
        #hot_col("chapter",  width = 200) %>%
        hot_col("disaggregation",  width = 120, halign="htCenter") %>%
        hot_col("structuralequation.risk",  width = 200, halign="htCenter") %>%
        hot_col("structuralequation.coping",  width = 200, halign="htCenter") %>%
        hot_col("structuralequation.resilience",  width = 200, halign="htCenter") %>%
        hot_col("anonymise", type = "dropdown", source = c("default-non-anonymised", "key", "remove", "sensitive", "reference"), width = 120) %>%
        hot_col("correlate",  width = 120, halign="htCenter") %>%
        hot_col("clean",  width = 120, halign="htCenter") %>%
        hot_col("cluster",  width = 120, halign="htCenter") %>%
        hot_col("predict",  width = 120, halign="htCenter") %>%
        hot_col("mappoint",  width = 120, halign="htCenter") %>%
        hot_col("mappoly",  width = 120, halign="htCenter") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)

      temp
    }, error = function(err) {
      print("5")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })
  ###################################################################

  #####################Numeric type#############################
  output$numericTypeUI <- renderUI({
    box(id="numericTypeBox",
        width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        uiOutput("numericTypeBody")
        #rHandsontableOutput("numericTypeTable")
    )
  })
  output$numericTypeBody <- renderUI({

    form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
    numericType <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                 stringsAsFactors = FALSE)
    reqNames <- c("type",   "name" ,  "label", "variable",
                  "disaggregation", #"chapter",
                  "structuralequation.risk","structuralequation.coping","structuralequation.resilience","anonymise","correlate","clean","cluster","predict","mappoint","mappoly"

    )
    if(sum(sapply(reqNames, function(x){x %in% colnames(numericType)})) != length(reqNames)){
      print("fdgfsdg54GDF")
      shinyalert("Error",
                 paste("You need to make sure that all required fields are existing in survey sheet\n",
                       reqNames
                 ),
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
      return(FALSE)
    }
    numericType <- numericType[reqNames]
    numericType <- numericType[startsWith(tolower(numericType$type), "integer") |
                                 startsWith(tolower(numericType$type), "decimal") |
                                 startsWith(tolower(numericType$type), "geopoint") |
                                 startsWith(tolower(numericType$type), "calculate")
                               ,]
    if(nrow(numericType)>0){
      rHandsontableOutput("numericTypeTable")
    }else{
      infoBox(
        width = 12,strong("Info"),
        h4("There is no Numeric type, you can start with the next step", align = "center")
        ,icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    }

  })

  output$numericTypeTable <- renderRHandsontable({
    tryCatch({
      temp <- rhandsontable(sheets[["numericType"]], stretchH = "all", height = 600, useTypes = TRUE) %>%
        hot_col("type", readOnly = TRUE, width = 200) %>%
        hot_col("name", readOnly = TRUE, width = 200) %>%
        hot_col("label", readOnly = TRUE, width = 200) %>%
        hot_col("variable", readOnly = TRUE, width = 200) %>%
        #hot_col("chapter",  width = 200) %>%
        hot_col("disaggregation",  width = 120, halign="htCenter") %>%
        hot_col("structuralequation.risk",  width = 200, halign="htCenter") %>%
        hot_col("structuralequation.coping",  width = 200, halign="htCenter") %>%
        hot_col("structuralequation.resilience",  width = 200, halign="htCenter") %>%
        hot_col("anonymise", type = "dropdown", source = c("default-non-anonymised", "key", "outlier"), width = 120) %>%
        hot_col("correlate",  width = 120, halign="htCenter") %>%
        hot_col("clean",  width = 120, halign="htCenter") %>%
        hot_col("cluster",  width = 120, halign="htCenter") %>%
        hot_col("predict",  width = 120, halign="htCenter") %>%
        hot_col("mappoint",  width = 120, halign="htCenter") %>%
        hot_col("mappoly",  width = 120, halign="htCenter") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)

      temp
    }, error = function(err) {
      print("6")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })
  ###################################################################

  #####################Date type#############################
  output$dateTypeUI <- renderUI({
    box(id="dateTypeBox",
        width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        uiOutput("dateTypeBody")
        #rHandsontableOutput("dateTypeTable")
    )
  })
  output$dateTypeBody <- renderUI({

    form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
    dateType <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                              stringsAsFactors = FALSE)
    reqNames <- c("type",   "name" ,  "label", "variable",
                  "disaggregation", #"chapter",
                  "structuralequation.risk","structuralequation.coping","structuralequation.resilience","anonymise","correlate","clean","cluster","predict","mappoint","mappoly"

    )
    if(sum(sapply(reqNames, function(x){x %in% colnames(dateType)})) != length(reqNames)){
      print("dsjfksfj4t")
      shinyalert("Error",
                 paste("You need to make sure that all required fields are existing in survey sheet\n",
                       reqNames
                 ),
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
      return(FALSE)
    }
    dateType <- dateType[reqNames]
    dateType <- dateType[startsWith(tolower(dateType$type), "date") |
                           startsWith(tolower(dateType$type), "time") |
                           startsWith(tolower(dateType$type), "datetime")
                         ,]
    if(nrow(dateType)>0){
      rHandsontableOutput("dateTypeTable")
    }else{
      infoBox(
        width = 12,strong("Info"),
        h4("There is no Date type, you can start with the next step", align = "center")
        ,icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    }

  })

  output$dateTypeTable <- renderRHandsontable({
    tryCatch({
      temp <- rhandsontable(sheets[["dateType"]], stretchH = "all", height = 600, useTypes = TRUE) %>%
        hot_col("type", readOnly = TRUE, width = 200) %>%
        hot_col("name", readOnly = TRUE, width = 200) %>%
        hot_col("label", readOnly = TRUE, width = 200) %>%
        hot_col("variable", readOnly = TRUE, width = 200) %>%
        #hot_col("chapter",  width = 200) %>%
        hot_col("disaggregation",  width = 120, halign="htCenter") %>%
        hot_col("structuralequation.risk",  width = 200, halign="htCenter") %>%
        hot_col("structuralequation.coping",  width = 200, halign="htCenter") %>%
        hot_col("structuralequation.resilience",  width = 200, halign="htCenter") %>%
        hot_col("anonymise", type = "dropdown", source = c("default-non-anonymised", "key", "outlier"), width = 120) %>%
        hot_col("correlate",  width = 120, halign="htCenter") %>%
        hot_col("clean",  width = 120, halign="htCenter") %>%
        hot_col("cluster",  width = 120, halign="htCenter") %>%
        hot_col("predict",  width = 120, halign="htCenter") %>%
        hot_col("mappoint",  width = 120, halign="htCenter") %>%
        hot_col("mappoly",  width = 120, halign="htCenter") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)

      temp
    }, error = function(err) {
      print("7")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })
  ###################################################################

  ################################## Indicators Sheet #########################################
  indicatorsInfo <- reactiveValues(data=NULL, selectedIndicator=NULL, operationType=NULL)

  output$indicatorsSheetUI <- renderUI({
    box(id="indicatorsSheetBox",
        width=12, solidHeader = FALSE, collapsible = FALSE,height = 650,
        column(width = 12,style="border-bottom: 1px solid lightgray; margin: 10px 0px 35px;",
               actionButton("addIndicatorButton", "Add Indicator", icon = icon("plus"), class = "uploadButton",
                            style="height: 50px; margin-bottom:20px; font-size: large; margin-left: 1%; margin-right: 1%;", width="98%")
        ),
        column(width = 12,style="overflow: auto; height: 500px;",
               uiOutput("indicatorsBoxes")
        )
    )
  })

  output$indicatorsBoxes <- renderText({
    tryCatch({
      indicatorsIF <- indicatorsInfo[["data"]]
      s <- ""
      indicators <- indicatorsIF[,"fullname"]
      for (ind in indicators) {

        rowInd <- indicatorsIF[indicatorsIF$fullname == ind, ]


        textInfoOfRow <- ""
        for(curCol in colnames(rowInd)){
          textInfoOfRow <- paste(textInfoOfRow,
                                 column(width = ifelse(curCol=="calculation",8,4),
                                        style="border: 1px solid lightgray; line-height: 35px;margin: 10px 0px;",
                                        span(paste(curCol,":"),class='colNameInd'),
                                        span(rowInd[1,curCol],class='colValInd')
                                 )
                                 ,sep=" ")
        }
        textInfoOfRow<-HTML(textInfoOfRow)
        s <- paste(s,
                   box(id=paste("box",ind,sep = ""), title = ind, status="primary",
                       width=12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                       div(
                         textInfoOfRow , class = "divOFIndRow"
                       ),
                       column(width = 7,
                              actionButton(paste("editIndicatorButton", ind, sep = ""), "Edit Indicator", icon = icon("edit"), class = "toolButton", style="height: 50px; margin-bottom:20px;", width="100%")
                       ),
                       column(width = 5,
                              actionButton(paste("deleteIndicatorButton", ind, sep = ""), "Delete Indicator", icon = icon("trash-alt"), class = "deleteButton", style="height: 50px; margin-bottom:20px;", width="100%")
                       )
                   )
                   ,sep="")
      }

      lapply(1:length(indicators), function(j) {
        observeEvent(input[[paste("editIndicatorButton", indicators[j] ,sep = "")]] , {
          tryCatch({
            if(!is.null(indicatorsInfo$selectedIndicator)){
              if(indicatorsInfo$selectedIndicator == "return"){
                indicatorsInfo$selectedIndicator <- indicators[j]
                indicatorsInfo$operationType <- "Edit"
                return(FALSE)
              }
            }
            indicatorsInfo$selectedIndicator <- indicators[j]
            indicatorsInfo$operationType <- "Edit"
            showModal(showIndicatorsTool("Edit",indicators[j]))
          }, error = function(err) {
            print("ewfkh8943%34")
            shinyalert("Error",
                       err$message,
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
          })
        },ignoreInit = TRUE)
      })

      lapply(1:length(indicators), function(j) {
        observeEvent(input[[paste("deleteIndicatorButton", indicators[j] ,sep = "")]] , {
          tryCatch({
            progress <- shiny::Progress$new()
            progress$set(message = "Deleting indicator in progress...", value = 0)
            on.exit(progress$close())
            updateProgress <- function(value = NULL, detail = NULL) {
              if (is.null(value)) {
                value <- progress$getValue()
                value <- value + (progress$getMax() - value) / 5
              }
              progress$set(value = value, detail = detail)
            }
            updateProgress()

            form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
            indicator <- as.data.frame(read_excel(form_tmp, sheet = "indicator"),
                                       stringsAsFactors = FALSE)
            updateProgress()
            #indicator[!is.na(indicator$fullname) & indicator$fullname==indicators[j],] <- NA
            indicator <-indicator[!is.na(indicator$fullname) & indicator$fullname!=indicators[j],]
            updateProgress()

            result <- kobo_edit_form(indicator = indicator)

            if(class(result) == "try-error"){
              shinyalert("Error",
                         result,
                         type = "error",
                         closeOnClickOutside = FALSE,
                         confirmButtonCol = "#ff4d4d",
                         animation = FALSE,
                         showConfirmButton = TRUE
              )
              return(FALSE)
            }

            indicatorsIF <- indicatorsInfo[["data"]]
            indicatorsIF <- indicatorsIF[indicatorsIF$fullname != indicators[j],]
            updateProgress()
            indicatorsIF <- indicatorsIF %>% arrange(fullname)
            updateProgress()
            indicatorsInfo[["data"]] <- indicatorsIF
          }, error = function(err) {
            print("fdgtogkjf90sdfd")
            shinyalert("Error",
                       err$message,
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
          })
        } ,ignoreInit = TRUE,once = TRUE)
      })

      s
    }, error = function(err) {
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })

  })

  observeEvent(input$addIndicatorButton,{
    tryCatch({
      indicatorsInfo$selectedIndicator <- ""
      indicatorsInfo$operationType <- "Add"
      showModal(showIndicatorsTool("Add",NULL))
    }, error = function(err) {
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  showIndicatorsTool <- function(type, indicatorName) {
    tryCatch({
      return(modalDialog(id="showIndicatorToolPopUp",
                         title = ifelse(type=="Add","Add Indicator", paste("Edit",indicatorName,"Indicator")),
                         uiOutput("indicatorToolBody"),
                         size = "l",
                         footer = tagList(
                           #modalButton("Cancel", icon("sign-out-alt")),
                           actionButton("cancelIndicatorButton", "Cancel", icon = icon("sign-out-alt")),

                           actionButton("saveIndicatorButton", ifelse(type=="Add","Add the Indicator", "Edit the Indicator"), class = "toolButton", style="height: 35px;")
                         )
      ))
    }, error = function(err) {
      print("8")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  }

  output$indicatorToolBody <- renderText({
    tryCatch({
      selInd <- indicatorsInfo$selectedIndicator
      indicatorsIF <- indicatorsInfo[["data"]]

      rowInd <- indicatorsIF[indicatorsIF$fullname == selInd, ]

      form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")

      list_name <- c()
      choices <- as.data.frame(read_excel(form_tmp, sheet = "choices"),
                               stringsAsFactors = FALSE)
      if ("list_name" %in% colnames(choices)) {
        list_name <- choices$list_name
        list_name <- list_name[!is.na(list_name) | trimws(list_name) != '']
        list_name <- list_name[!duplicated(list_name)]
        list_name <- sort(list_name)
      }

      s <- paste("",
                 box(id="mandatoryInputsIndicatorBox",title = "Mandatory Inputs...",
                     width=12, solidHeader = TRUE, collapsible = FALSE, status = "danger",
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Enter indicator's fullname:")
                       ),
                       column(width = 6, offset = 0,
                              textInput("indicatorFullnameInput", label = NULL, value = rowInd[1,"fullname"], width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Enter indicator's label:")
                       ),
                       column(width = 5, offset = 0,
                              textInput("indicatorLabelInput", label = NULL, value = rowInd[1,"labelReport"], width = "100%")
                       ),
                       column(width = 1, offset = 0, align="center",
                              uiOutput("indicatorLabelInputLengthUI")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Select indicator's frame:")
                       ),
                       column(width = 6, offset = 0,
                              selectizeInput("indicatorFrameInput", label = NULL, selected = rowInd[1,"frame"], choices = c("-- select --",projectConfigurationInfo$data[["beginRepeatList"]]),
                                             options = list(placeholder = "-- select --"),
                                             width = "100%")
                       )
                     ),

                     uiOutput("calculationNeed"),

                     conditionalPanel(
                       condition = "input.useCalculation=='No'",
                       column(
                         width=12,
                         uiOutput("calculationBuilderToolBody")
                       )
                     )


                 ),

                 box(id="moreOptionsIndicatorBox",title = "Optional Inputs...",
                     width=12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status = "info",
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Enter indicator's listname:")
                       ),
                       column(width = 6, offset = 0,
                              selectizeInput("indicatorListnameInput", label = NULL, selected = rowInd[1,"listname"], choices = c("-- select --",list_name),
                                             options = list(placeholder = "-- select --", create = TRUE),
                                             width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Apply disaggregation?")
                       ),
                       column(width = 6, offset = 0,
                              selectInput("indicatorDisaggregationInput", label = NULL, selected = ifelse(rowInd[1,"disaggregation"]=="TRUE","Yes",ifelse(rowInd[1,"disaggregation"]=="FALSE","No","-- select --")),
                                          choices = c("-- select --","Yes","No"),
                                          width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Apply correlate?")
                       ),
                       column(width = 6, offset = 0,
                              selectInput("indicatorCorrelateInput", label = NULL, selected = ifelse(rowInd[1,"correlate"]=="TRUE","Yes",ifelse(rowInd[1,"correlate"]=="FALSE","No","-- select --")),
                                          choices = c("-- select --","Yes","No"),
                                          width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Select the anonymise way:")
                       ),
                       column(width = 6, offset = 0,
                              selectInput("indicatorAnonymiseInput", label = NULL, selected = rowInd[1,"anonymise"],
                                          choices = c("-- select --","key", "outlier", "sensitive", "remove", "reference"),
                                          width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Apply cluster?")
                       ),
                       column(width = 6, offset = 0,
                              selectInput("indicatorClusterInput", label = NULL, selected = ifelse(rowInd[1,"cluster"]=="TRUE","Yes",ifelse(rowInd[1,"cluster"]=="FALSE","No","-- select --")),
                                          choices = c("-- select --","Yes","No"),
                                          width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Apply predict?")
                       ),
                       column(width = 6, offset = 0,
                              selectInput("indicatorPredictInput", label = NULL, selected = ifelse(rowInd[1,"predict"]=="TRUE","Yes",ifelse(rowInd[1,"predict"]=="FALSE","No","-- select --")),
                                          choices = c("-- select --","Yes","No"),
                                          width = "100%")
                       )
                     ),


                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Apply mappoint?")
                       ),
                       column(width = 6, offset = 0,
                              selectInput("indicatorMappointInput", label = NULL, selected = ifelse(rowInd[1,"mappoint"]=="TRUE","Yes",ifelse(rowInd[1,"mappoint"]=="FALSE","No","-- select --")),
                                          choices = c("-- select --","Yes","No"),
                                          width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Apply mappoly?")
                       ),
                       column(width = 6, offset = 0,
                              selectInput("indicatorMappolyInput", label = NULL, selected = ifelse(rowInd[1,"mappoly"]=="TRUE","Yes",ifelse(rowInd[1,"mappoly"]=="FALSE","No","-- select --")),
                                          choices = c("-- select --","Yes","No"),
                                          width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Apply structuralequation risk?")
                       ),
                       column(width = 6, offset = 0,
                              selectInput("indicatorstructuralequationRiskInput", label = NULL, selected = ifelse(rowInd[1,"structuralequation.risk"]=="TRUE","Yes",ifelse(rowInd[1,"structuralequation.risk"]=="FALSE","No","-- select --")),
                                          choices = c("-- select --","Yes","No"),
                                          width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Apply structuralequation coping?")
                       ),
                       column(width = 6, offset = 0,
                              selectInput("indicatorstructuralequationCopingInput", label = NULL, selected = ifelse(rowInd[1,"structuralequation.coping"]=="TRUE","Yes",ifelse(rowInd[1,"structuralequation.coping"]=="FALSE","No","-- select --")),
                                          choices = c("-- select --","Yes","No"),
                                          width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Apply structuralequation resilience?")
                       ),
                       column(width = 6, offset = 0,
                              selectInput("indicatorstructuralequationResilienceInput", label = NULL, selected = ifelse(rowInd[1,"structuralequation.resilience"]=="TRUE","Yes",ifelse(rowInd[1,"structuralequation.resilience"]=="FALSE","No","-- select --")),
                                          choices = c("-- select --","Yes","No"),
                                          width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Enter indicator's hint:")
                       ),
                       column(width = 6, offset = 0,
                              textInput("indicatorHintInput", label = NULL, value = rowInd[1,"hintReport"], width = "100%")
                       )
                     )

                 )
                 ,sep = "")

      s
    }, error = function(err) {
      print("jkfdnvk")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  output$calculationNeed <- renderText({
    m <- ""
    if(indicatorsInfo$operationType=="Edit"){
      m <- paste(m,column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Use previous calculation?")
        ),
        column(width = 6, offset = 0,
               selectInput("useCalculation", label = NULL, selected = "No",
                           choices = c("Yes","No"),
                           width = "100%")
        )
      ),sep = "")

    }else{
      m <- paste(m,column(style = "display: none;",
                          width=12,
                          column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                                 h4("Use previous calculation?")
                          ),
                          column(width = 6, offset = 0,
                                 selectInput("useCalculation", label = NULL, selected = "No",
                                             choices = c("No"),
                                             width = "100%")
                          )
      ),sep = "")
    }
    m
  })

  observeEvent(input$saveIndicatorButton,{
    tryCatch({
      selInd <- indicatorsInfo$selectedIndicator
      indicatorsIF <- indicatorsInfo[["data"]]

      if(sum(input$indicatorFullnameInput=="")){
        shinyalert("Fullname is required",
                   'Please make sure that you entered the "Fullname" for this Indicator',
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }

      if(sum(input$indicatorFullnameInput %in% indicatorsIF$fullname) & sum(input$indicatorFullnameInput != selInd)){
        shinyalert("Fullname is not available",
                   'This name is reserved by another indicator, please use another one.',
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }

      if(sum(input$indicatorLabelInput=="")){
        shinyalert("Label is required",
                   'Please make sure that you entered the "Label" for this Indicator',
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }

      if(sum(str_length(input$indicatorLabelInput)>85)){
        shinyalert("Label length",
                   'Please make sure that the length of the label string is less than 85',
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }

      if(sum(input$indicatorFrameInput=="-- select --") ){
        shinyalert("Frame is required",
                   'Please make sure that you entered the "Frame" for this Indicator',
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }

      if(sum(input$indicatorCaseSelectInput == "-- select --") & sum(input$useCalculation=="No")){
        print("kflg")
        shinyalert("Error",
                   "Please make sure that you select the Indicator.",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        resultDVUIValue$text <- ""
        resultFRUIValue$text <- ""
        resultSUUIValue$text <- ""
        resultMMAUIValue$text <- ""
        resultD2UIValue$text <- ""
        return(FALSE)
      }

      progress <- shiny::Progress$new()
      progress$set(message = "Process in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()

      ##################Calculation Builder Tool######################

      calculationResult <- c()
      if(sum(input$indicatorCaseSelectInput=="Discretize a value")){
        tryCatch({
          ############################        Validation        ############################
          if (
            (sum(input$frameDVSelectInput == "-- select --") ||
             sum(input$variableDVSelectInput == "-- select --") ||
             sum(input$breaksDVTextInput == "")& sum(input$useCalculation=="No"))
          ) {
            print("lkfdbjg9")
            shinyalert("Error",
                       "Please make sure that you enter all required inputs.",
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
            resultDVUIValue$text <- ""
            return(FALSE)
          }
          ############################        END        ############################


          pre<-as.numeric(strsplit(input$breaksDVTextInput,",")[[1]])
          pre <- pre[!is.na(pre)]
          pre <- paste( pre ,sep = "," ,collapse=",")
          if(pre==""){
            resultDVUIValue$text <- ""
            shinyalert("Info",
                       "Only numerical values are allowed",
                       type = "info",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#28A8E2",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
            return(FALSE)
          }
          calculationResult <- "cut("
          calculationResult <- paste(calculationResult,paste0(input$frameDVSelectInput,"_edited"),"$",input$variableDVSelectInput, " ", sep="")
          calculationResult <- paste(calculationResult, ",c(", pre,"))" , sep = ""  )
        }, error = function(err) {
          calculationResult <- structure(c, class = "try-error")
        })
      }
      else if(sum(input$indicatorCaseSelectInput=="Re categorize a categorical variable by re coding modalities")){
        tryCatch({
          ############################        Validation        ############################
          if (
            (sum(input$frameFRSelectInput == "-- select --") ||
             sum(input$variableFRSelectInput == "-- select --") ||
             sum(input$listnameFRSelectInput == "-- select --")) & sum(input$useCalculation=="No")
          ) {
            print("kfdjsgt54")
            shinyalert("Error",
                       "Please make sure that you enter all required inputs.",
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
            resultFRUIValue$text <- ""
            return(FALSE)
          }
          ############################        END        ############################

          calculationResult <- "fct_recode("
          calculationResult <- paste(calculationResult,paste0(input$frameFRSelectInput,"_edited"),"$",input$variableFRSelectInput, ", ", sep="")
          factorValues <- choicesSheetFR()[choicesSheetFR()$list_name==input$listnameFRSelectInput,c("list_name", "name", "label")]
          for(i in 1:nrow(factorValues)){

            temp <- input[[paste(i,factorValues[i,"name"],sep = "-")]]

            calculationResult <- paste(calculationResult,"\"",temp,"\""," = ","\"",factorValues[i,"name"],"\"",sep = "")

            if(i!=nrow(factorValues)){
              calculationResult <- paste(calculationResult,", ",sep = "")
            }
          }
          calculationResult <- paste(calculationResult,")",sep = "")
        }, error = function(err) {
          calculationResult <- structure(c, class = "try-error")
        })
      }
      else if(sum(input$indicatorCaseSelectInput=="Sum up different numeric or integer variables") ){
        tryCatch({
          ############################        Validation        ############################
          if (
            sum(input$frameSUSelectInput == "-- select --") & sum(input$useCalculation=="No")
          ) {
            print("lkghfgiojdft90i")
            shinyalert("Error",
                       "Please make sure that you enter all required inputs.",
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
            resultSUUIValue$text <- ""
            return(FALSE)
          }
          ############################        END        ############################

          calculationResult <- "psum("
          calculationResult <- paste(calculationResult,
                                     paste0(input$frameSUSelectInput,"_edited"),
                                     "$",
                                     input$varSU1,
                                     ", ",
                                     paste0(input$frameSUSelectInput,"_edited"),
                                     "$",
                                     input$varSU2,
                                     ifelse(length(variablesToUseSU$idOfVar)>0,
                                            ", ",""), sep="")



          counter <- 1
          for(i in variablesToUseSU$idOfVar){
            val <- input[[paste("varSU", i, sep = "")]]
            calculationResult <- paste(calculationResult,
                                       paste0(input$frameSUSelectInput,"_edited"),
                                       "$",
                                       val
                                       , sep="")

            if(counter != length(variablesToUseSU$idOfVar)){
              calculationResult <- paste(calculationResult,", ", sep="")
            }
            counter <- counter + 1
          }


          calculationResult <- paste(calculationResult,")",sep = "")
        }, error = function(err) {
          calculationResult <- structure(c, class = "try-error")
        })
      }
      else if(sum(input$indicatorCaseSelectInput=="Calculate min, max or avg value for multiple integer or numeric variables")){
        tryCatch({
          ############################        Validation        ############################
          if (
            (sum(input$frameMMASelectInput == "-- select --") ||
             sum(input$variableMMASelectInput == "-- select --") ||
             sum(input$statisticalFunctionsMMASelectInput == "-- select --")) & sum(input$useCalculation=="No")
          ) {
            print("lklfgjifyuqw")
            shinyalert("Error",
                       "Please make sure that you enter all required inputs.",
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
            resultMMAUIValue$text <- ""
            return(FALSE)
          }
          ############################        END        ############################


          varFrame <- paste(paste0(input$frameMMASelectInput,"_edited"),"$",input$variableMMASelectInput, sep="")
          if(sum(input$statisticalFunctionsMMASelectInput=="Minimum")){
            calculationResult <- paste("min(",varFrame," ,na.rm = TRUE)",sep = "")
          }else if(sum(input$statisticalFunctionsMMASelectInput=="Maximum")){
            calculationResult <- paste("max(",varFrame," ,na.rm = TRUE)",sep = "")
          }else if(sum(input$statisticalFunctionsMMASelectInput=="Average")){
            calculationResult <- paste("mean(",varFrame," ,na.rm = TRUE)",sep = "")
          }else if(sum(input$statisticalFunctionsMMASelectInput=="Median")){
            calculationResult <- paste("median(",varFrame," ,na.rm = TRUE)",sep = "")
          }else if(sum(input$statisticalFunctionsMMASelectInput=="Mode")){
            calculationResult <- paste("uniqv[which.max(tabulate(match(",varFrame, ", unique(",varFrame,"))))",sep = "")
          }else if(sum(input$statisticalFunctionsMMASelectInput=="Standard Deviation")){
            calculationResult <- paste("sd(",varFrame," ,na.rm = TRUE)",sep = "")
          }else if(sum(input$statisticalFunctionsMMASelectInput=="Interquartile Range")){
            calculationResult <- paste("IQR(",varFrame," ,na.rm = TRUE)",sep = "")
          }
        }, error = function(err) {
          calculationResult <- structure(c, class = "try-error")
        })
      }
      else if(sum(input$indicatorCaseSelectInput=="Calculate ratio by dividing 2 numeric or integer variables")){
        tryCatch({
          ############################        Validation        ############################
          if (
            (sum(input$frameD2SelectInput == "-- select --") ||
             sum(input$variableD2SelectInput1 == "-- select --") ||
             sum(input$variableD2SelectInput2 == "-- select --")) & sum(input$useCalculation=="No")
          ) {
            print("idf85reij")
            shinyalert("Error",
                       "Please make sure that you enter all required inputs.",
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
            resultD2UIValue$text <- ""
            return(FALSE)
          }
          ############################        END        ############################

          form_tmp <- paste(mainDir(), "data", paste(paste(input$frameD2SelectInput,"_edited"), ".csv", sep = ""), sep = "/", collapse = "/")
          temp <- read.csv(form_tmp, stringsAsFactors = TRUE)

          if(input$variableD2SelectInput1 %in% colnames(temp)){
            calculationResult <- paste(paste0(input$frameD2SelectInput,"_edited"),"$",input$variableD2SelectInput1, " / ", sep="")
          }else{
            calculationResult <- paste(input$variableD2SelectInput1, " / ", sep="")
          }

          if(input$variableD2SelectInput2 %in% colnames(temp)){
            calculationResult <- paste(calculationResult, paste0(input$frameD2SelectInput,"_edited"),"$",input$variableD2SelectInput2, sep="")
          }else{
            calculationResult <- paste(calculationResult, input$variableD2SelectInput2, sep="")
          }


        }, error = function(err) {
          calculationResult <- structure(c, class = "try-error")
        })
      }
      else if(sum(input$indicatorCaseSelectInput=="Set condition on specific variables")){
        mainDf <- ifVariables$mainDataFrame
        blocksId <- c(1, ifVariables$blocksId)
        calculation <- ""
        for(i in 1:length(blocksId) ){
          conditionsId <- c(1,ifVariables[[paste("conditionsIdOfBlock", blocksId[i], sep="")]])
          conditionString <- ""
          resultOfCondition <- ""
          for (j in 1:length(conditionsId)) {
            linkBy <- input[[paste("linkByCond", conditionsId[j], "Block", blocksId[i], sep = "")]]
            withCond <- input[[paste("withCond", conditionsId[j], "Block", blocksId[i], sep = "")]]
            leftSide <- input[[paste("leftSideCond", conditionsId[j], "Block", blocksId[i], sep = "")]]
            rightSide <- input[[paste("rightSideCond", conditionsId[j], "Block", blocksId[i], sep = "")]]
            logicalOperators <- input[[paste("logicalOperatorsCond", conditionsId[j], "Block", blocksId[i], sep = "")]]
            ##############################       validation        ##############################
            if(conditionsId[j]!=1){
              if((linkBy=="-- select --" | trimws(linkBy)=="" )){
                shinyalert("link By is required",
                           paste('Please make sure that you select the "link By" for condition', j, " ,Block", i, sep = ""),
                           type = "error",
                           closeOnClickOutside = FALSE,
                           confirmButtonCol = "#ff4d4d",
                           animation = FALSE,
                           showConfirmButton = TRUE
                )
                return(FALSE)
              }
              if((withCond=="-- select --" | trimws(withCond)=="" )){
                shinyalert("with is required",
                           paste('Please make sure that you select the "with" for condition', j, " ,Block", i, sep = ""),
                           type = "error",
                           closeOnClickOutside = FALSE,
                           confirmButtonCol = "#ff4d4d",
                           animation = FALSE,
                           showConfirmButton = TRUE
                )
                return(FALSE)
              }
              linkBy <- ifelse(linkBy=="and","&","|")
            }
            else{
              linkBy <- NA
              withCond <- NA
            }
            if((leftSide=="-- select --" | trimws(leftSide)=="" )){
              shinyalert("left Side of condition is required",
                         paste('Please make sure that you select the "left Side" for condition', j, " ,Block", i, sep = ""),
                         type = "error",
                         closeOnClickOutside = FALSE,
                         confirmButtonCol = "#ff4d4d",
                         animation = FALSE,
                         showConfirmButton = TRUE
              )
              return(FALSE)
            }
            if((rightSide=="-- select or enter --" | trimws(rightSide)=="" )){
              shinyalert("right Side of condition is required",
                         paste('Please make sure that you select the "right Side" for condition', j, " ,Block", i, sep = ""),
                         type = "error",
                         closeOnClickOutside = FALSE,
                         confirmButtonCol = "#ff4d4d",
                         animation = FALSE,
                         showConfirmButton = TRUE
              )
              return(FALSE)
            }
            if((logicalOperators=="-- select --" | trimws(logicalOperators)=="" )){
              shinyalert("logical Operator is required",
                         paste('Please make sure that you select the "logical Operator" for condition', j, " ,Block", i, sep = ""),
                         type = "error",
                         closeOnClickOutside = FALSE,
                         confirmButtonCol = "#ff4d4d",
                         animation = FALSE,
                         showConfirmButton = TRUE
              )
              return(FALSE)
            }
            ###################################################################################

            conditionString <- "("
            if(leftSide %in% colnames(mainDf)){
              leftSide <- paste(paste0(input$frameIFSelectInput,"_edited") ,"['",leftSide,"']", sep = "")
            }
            else{
              checker <- as.numeric(leftSide)
              if(is.na(checker)){
                leftSide <- paste("'", leftSide, "'", sep="")
              }else{
                leftSide <- checker
              }
            }
            if(rightSide %in% colnames(mainDf)){
              rightSide <- paste(paste0(input$frameIFSelectInput,"_edited") ,"['",rightSide,"']", sep = "")
            }
            else{
              if(logicalOperators == "in" | logicalOperators == "not in"){
                rightSide <- strsplit(rightSide,",")[[1]]
                tempStr <-""
                for (z in 1:length(rightSide)) {
                  rightSide[z] <- trimws(rightSide[z])
                  if(z==1){
                    tempStr <- paste("'",rightSide[z],"'", sep = "")
                  }else{
                    tempStr <- paste(tempStr,",","'",rightSide[z],"'", sep = "")
                  }
                }
                rightSide <- tempStr
              }else{
                checker <- as.numeric(rightSide)
                if(is.na(checker)){
                  rightSide <- paste("'", rightSide, "'", sep="")
                }else{
                  rightSide <- checker
                }
              }
            }

            if(logicalOperators == "in"){
              rightSide <- paste("c(",rightSide,")")
            }
            else if(logicalOperators == "not in"){
              conditionString <- "(!"
              rightSide <- paste("c(",rightSide,")")
              logicalOperators <- "in"
            }
            conditionString <- paste(conditionString, leftSide, logicalOperators, rightSide, ")" )
            if(conditionsId[j]!=1){
              if(withCond == paste("Condition",j-1)){
                resultOfCondition <- paste( resultOfCondition, linkBy , conditionString)
              }else{
                resultOfCondition <- paste( "(" , resultOfCondition ,")", linkBy , conditionString)
              }
            }
            else{
              resultOfCondition <- conditionString
            }

          }
          resultOfBlock <- input[[paste("resultOfBlock" , blocksId[i], sep = "")]]
          if((resultOfBlock=="-- select or enter --" | trimws(resultOfBlock)=="" )){
            shinyalert("Result of condition is required",
                       paste('Please make sure that you select or enter the "result" for Block', i, sep = ""),
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
            return(FALSE)
          }
          if(resultOfBlock %in% colnames(mainDf)){
            resultOfBlock <- paste(paste0(input$frameIFSelectInput,"_edited") ,"['",resultOfBlock,"']", sep = "")
          }
          else{
            checker <- as.numeric(resultOfBlock)
            if(is.na(checker)){
              resultOfBlock <- paste("'", resultOfBlock, "'", sep="")
            }else{
              resultOfBlock <- checker
            }
          }

          calculationResult <- paste(calculationResult,"ifelse(",resultOfCondition,",",resultOfBlock, ",")

        }

        resultOfElse <- input[["resultOfElse"]]
        if((resultOfElse=="-- select or enter --" | trimws(resultOfElse)=="" )){
          shinyalert("Result of else is required",
                     'Please make sure that you select or enter the "result" for Else',
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        if(resultOfElse %in% colnames(mainDf)){
          resultOfElse <- paste(paste0(input$frameIFSelectInput,"_edited") ,"['",resultOfElse,"']", sep = "")
        }
        else{
          checker <- as.numeric(resultOfElse)
          if(is.na(checker)){
            resultOfElse <- paste("'", resultOfElse, "'", sep="")
          }else{
            resultOfElse <- checker
          }
        }
        calculationResult <- paste(calculationResult,resultOfElse)

        for(i in 1:length(blocksId) ){
          calculationResult <- paste(calculationResult,")")
        }
        calculationResult <- trimws(calculationResult)
      }


      ################################################################

      if(class(calculationResult) == "try-error"){
        print("ioi435ti")
        shinyalert("Error",
                   calculationResult$message,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }

      ########################Detect the Type#########################
      typeOfInd <- c()

      if(sum(input$indicatorCaseSelectInput=="Discretize a value")){
        tryCatch({
          form_tmp <- paste(mainDir(), "data", paste(paste0(input$frameDVSelectInput,"_edited"), ".csv", sep = ""), sep = "/", collapse = "/")
          assign(paste0(input$frameDVSelectInput,"_edited"),read.csv(form_tmp, stringsAsFactors = TRUE))
          typeOfInd <- eval(parse(text=calculationResult))
          if(is.character(typeOfInd)){
            typeOfInd <- "factor"
          }else if(is.integer(typeOfInd)){
            typeOfInd <- "integer"
          }else if(is.numeric(typeOfInd)){
            typeOfInd <- "numeric"
          }else{
            typeOfInd <- "factor"
          }
          rm(paste0(input$frameDVSelectInput,"_edited"))
        }, error = function(err) {
          typeOfInd <- structure(c, class = "try-error")
        })
      }
      else if(sum(input$indicatorCaseSelectInput=="Re categorize a categorical variable by re coding modalities")){
        tryCatch({

          form_tmp <- paste(mainDir(), "data", paste(paste0(input$frameFRSelectInput,"_edited"), ".csv", sep = ""), sep = "/", collapse = "/")
          assign(paste0(input$frameFRSelectInput,"_edited"),read.csv(form_tmp, stringsAsFactors = TRUE))
          typeOfInd <- eval(parse(text=calculationResult))
          if(is.character(typeOfInd)){
            typeOfInd <- "factor"
          }else if(is.integer(typeOfInd)){
            typeOfInd <- "integer"
          }else if(is.numeric(typeOfInd)){
            typeOfInd <- "numeric"
          }else{
            typeOfInd <- "factor"
          }
          rm(paste0(input$frameFRSelectInput,"_edited"))

        }, error = function(err) {
          typeOfInd <- structure(c, class = "try-error")
        })
      }
      else if(sum(input$indicatorCaseSelectInput=="Sum up different numeric or integer variables") ){
        tryCatch({

          form_tmp <- paste(mainDir(), "data", paste(paste0(input$frameSUSelectInput,"_edited"), ".csv", sep = ""), sep = "/", collapse = "/")
          assign(paste0(input$frameSUSelectInput,"_edited"),read.csv(form_tmp, stringsAsFactors = TRUE))
          typeOfInd <- eval(parse(text=calculationResult))
          if(is.character(typeOfInd)){
            typeOfInd <- "factor"
          }else if(is.integer(typeOfInd)){
            typeOfInd <- "integer"
          }else if(is.numeric(typeOfInd)){
            typeOfInd <- "numeric"
          }else{
            typeOfInd <- "factor"
          }
          rm(paste0(input$frameSUSelectInput,"_edited"))

        }, error = function(err) {
          typeOfInd <- structure(c, class = "try-error")
        })
      }
      else if(sum(input$indicatorCaseSelectInput=="Calculate min, max or avg value for multiple integer or numeric variables")){
        tryCatch({

          form_tmp <- paste(mainDir(), "data", paste(paste0(input$frameMMASelectInput,"_edited"), ".csv", sep = ""), sep = "/", collapse = "/")
          assign(paste0(input$frameMMASelectInput,"_edited"),read.csv(form_tmp, stringsAsFactors = TRUE))
          typeOfInd <- eval(parse(text=calculationResult))
          if(is.character(typeOfInd)){
            typeOfInd <- "factor"
          }else if(is.integer(typeOfInd)){
            typeOfInd <- "integer"
          }else if(is.numeric(typeOfInd)){
            typeOfInd <- "numeric"
          }else{
            typeOfInd <- "factor"
          }
          rm(paste0(input$frameMMASelectInput,"_edited"))

        }, error = function(err) {
          typeOfInd <- structure(c, class = "try-error")
        })
      }
      else if(sum(input$indicatorCaseSelectInput=="Calculate ratio by dividing 2 numeric or integer variables")){
        tryCatch({

          form_tmp <- paste(mainDir(), "data", paste(paste0(input$frameD2SelectInput,"_edited"), ".csv", sep = ""), sep = "/", collapse = "/")
          assign(paste0(input$frameD2SelectInput,"_edited"),read.csv(form_tmp, stringsAsFactors = TRUE))
          typeOfInd <- eval(parse(text=calculationResult))
          if(is.character(typeOfInd)){
            typeOfInd <- "factor"
          }else if(is.integer(typeOfInd)){
            typeOfInd <- "integer"
          }else if(is.numeric(typeOfInd)){
            typeOfInd <- "numeric"
          }else{
            typeOfInd <- "factor"
          }
          rm(paste0(input$frameD2SelectInput,"_edited"))

        }, error = function(err) {
          typeOfInd <- structure(c, class = "try-error")
        })
      }
      else if(sum(input$indicatorCaseSelectInput=="Set condition on specific variables")){
        tryCatch({

          form_tmp <- paste(mainDir(), "data", paste(paste0(input$frameIFSelectInput,"_edited"), ".csv", sep = ""), sep = "/", collapse = "/")
          assign(paste0(input$frameIFSelectInput,"_edited"),read.csv(form_tmp, stringsAsFactors = TRUE))
          typeOfInd <- eval(parse(text=calculationResult))
          if(is.character(typeOfInd)){
            typeOfInd <- "factor"
          }else if(is.integer(typeOfInd)){
            typeOfInd <- "integer"
          }else if(is.numeric(typeOfInd)){
            typeOfInd <- "numeric"
          }else{
            typeOfInd <- "factor"
          }
          rm(paste0(input$frameIFSelectInput,"_edited"))

        }, error = function(err) {
          typeOfInd <- structure(c, class = "try-error")
        })
      }else{
        if(sum(input$useCalculation=="Yes")){
          form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
          indicator <- as.data.frame(read_excel(form_tmp, sheet = "indicator"),
                                     stringsAsFactors = FALSE)

          frame <- indicator[!is.na(indicator$fullname) & indicator$fullname==selInd, "frame"]

          form_tmp <- paste(mainDir(), "data", paste(frame, ".csv", sep = ""), sep = "/", collapse = "/")
          assign(frame, read.csv(form_tmp, stringsAsFactors = TRUE))
          typeOfInd <- eval(parse(text=indicator[!is.na(indicator$fullname) & indicator$fullname==selInd, "calculation"]))
          if(is.character(typeOfInd)){
            typeOfInd <- "factor"
          }else if(is.integer(typeOfInd)){
            typeOfInd <- "integer"
          }else if(is.numeric(typeOfInd)){
            typeOfInd <- "numeric"
          }else{
            typeOfInd <- "factor"
          }
          rm(frame)
        }
      }
      if(is.null(typeOfInd)){
        typeOfInd <- F
      }
      ################################################################
      updateProgress()

      form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
      indicator <- as.data.frame(read_excel(form_tmp, sheet = "indicator"),
                                 stringsAsFactors = FALSE)
      updateProgress()
      preTyp <- indicator[!is.na(indicator$fullname) & indicator$fullname==selInd, "type"]
      preChp <- indicator[!is.na(indicator$fullname) & indicator$fullname==selInd, "chapter"]
      preVar <- indicator[!is.na(indicator$fullname) & indicator$fullname==selInd, "variable"]
      preCal <- indicator[!is.na(indicator$fullname) & indicator$fullname==selInd, "calculation"]
      updateProgress()
      if(length(preTyp)==0){
        preTyp <- NA
      }
      if(length(preChp)==0){
        preChp <- NA
      }
      if(length(preVar)==0){
        preVar <- NA
      }
      if(length(preCal)==0){
        preCal <- NA
      }
      indicator <- indicator[!is.na(indicator$fullname) & indicator$fullname!=selInd,]
      frame <- paste0(input$indicatorFrameInput, "_edited")

      newRow <- data.frame(
        type = ifelse(sum(input$useCalculation=="No"),ifelse(is.null(typeOfInd),NA,ifelse(typeOfInd=="factor", "select_one",typeOfInd)),preTyp),
        fullname = input$indicatorFullnameInput,
        labelReport = input$indicatorLabelInput,
        hintReport = input$indicatorHintInput,
        chapter = preChp,
        disaggregation = ifelse(sum(input$indicatorDisaggregationInput=="Yes"),"TRUE",ifelse(sum(input$indicatorDisaggregationInput=="No"),"False",NA)),
        correlate = ifelse(sum(input$indicatorCorrelateInput=="Yes"),"TRUE",ifelse(sum(input$indicatorCorrelateInput=="No"),"False",NA)),
        anonymise = ifelse(sum(input$indicatorAnonymiseInput=="-- select --"),NA,input$indicatorAnonymiseInput),
        cluster = ifelse(sum(input$indicatorClusterInput=="Yes"),"TRUE",ifelse(sum(input$indicatorClusterInput=="No"),"False",NA)),
        predict = ifelse(sum(input$indicatorPredictInput=="Yes"),"TRUE",ifelse(sum(input$indicatorPredictInput=="No"),"False",NA)),
        variable = ifelse(typeOfInd=="factor",preVar,NA),
        mappoint = ifelse(sum(input$indicatorMappointInput=="Yes"),"TRUE",ifelse(sum(input$indicatorMappointInput=="No"),"False",NA)),
        mappoly = ifelse(sum(input$indicatorMappolyInput=="Yes"),"TRUE",ifelse(sum(input$indicatorMappolyInput=="No"),"False",NA)),
        structuralequation.risk = ifelse(sum(input$indicatorstructuralequationRiskInput=="Yes"),"TRUE",ifelse(sum(input$indicatorstructuralequationRiskInput=="No"),"False",NA)),
        structuralequation.coping = ifelse(sum(input$indicatorstructuralequationCopingInput=="Yes"),"TRUE",ifelse(sum(input$indicatorstructuralequationCopingInput=="No"),"False",NA)),
        structuralequation.resilience = ifelse(sum(input$indicatorstructuralequationResilienceInput=="Yes"),"TRUE",ifelse(sum(input$indicatorstructuralequationResilienceInput=="No"),"False",NA)),
        frame = ifelse(sum(input$indicatorFrameInput=="-- select --"),NA,frame) ,
        listname = ifelse(sum(input$indicatorListnameInput=="-- select --"),NA,input$indicatorListnameInput),
        calculation =  ifelse(sum(input$useCalculation=="No"),gsub("MainDataFrame", "MainDataFrame_edited", calculationResult),preCal),
        stringsAsFactors = FALSE
      )
      updateProgress()
      indicator <- rbind(indicator, newRow)


      result <- kobo_edit_form(indicator = indicator)

      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }

      updateProgress()

      indicator <- indicator %>% arrange(fullname)
      indicatorsInfo[["data"]] <- indicator

      indicatorsInfo$selectedIndicator <- newRow$fullname
      updateProgress()
      if(newRow$type == "select_one" & (sum(input$useCalculation=="No") | !is.na(newRow[1,"variable"] ) | !is.na(newRow[1,"listname"]) ) ){
        removeModal()
        showModal(showSubIndicatorsTool(input$indicatorFullnameInput))
      }else{
        removeModal()
      }

    }, error = function(err) {
      print("ibosd8gt")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  observe({
    output$indicatorLabelInputLengthUI <- renderUI({
      if(str_length(input$indicatorLabelInput)>85){
        verbatimTextOutput("indicatorLabelInputLengthRed")
      }else{
        verbatimTextOutput("indicatorLabelInputLengthBlack")
      }
    })
  })
  output$indicatorLabelInputLengthRed <- renderText({
    str_length(input$indicatorLabelInput)
  })
  output$indicatorLabelInputLengthBlack <- renderText({
    str_length(input$indicatorLabelInput)
  })


  showSubIndicatorsTool <- function(indicatorName) {
    tryCatch({
      return(modalDialog(id="showSubIndicatorToolPopUp",
                         title = paste("More inputs for", indicatorName, "indicator"),
                         uiOutput("subIndicatorToolBody"),
                         size = "l",
                         footer = tagList(
                           actionButton("completeSaveIndicatorButton", "Continue...", class = "toolButton", style="height: 35px;")
                         )
      ))
    }, error = function(err) {
      print("3454dfvd")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  }

  output$subIndicatorToolBody <- renderText({
    tryCatch({
      selInd <- indicatorsInfo$selectedIndicator
      indicatorsIF <- indicatorsInfo[["data"]]

      rowInd <- indicatorsIF[indicatorsIF$fullname == selInd, ]

      form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")

      list_name <- c()
      choices <- as.data.frame(read_excel(form_tmp, sheet = "choices"),
                               stringsAsFactors = FALSE)
      if ("list_name" %in% colnames(choices)) {
        list_name <- choices$list_name
        list_name <- list_name[!is.na(list_name) | trimws(list_name) != '']
        list_name <- list_name[!duplicated(list_name)]
        list_name <- sort(list_name)
      }

      s <- paste("",

                 box(id="mandatoryInputsSubIndicatorBox",title = "Mandatory Inputs...",
                     width=12, solidHeader = TRUE, collapsible = FALSE, status = "danger",
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Select the variable type?")
                       ),
                       column(width = 6, offset = 0,
                              selectInput("subIndicatorVariableInput", label = NULL, selected = rowInd[1,"variable"],
                                          choices = c("-- select --", "ordinal factor", "factor"),
                                          width = "100%")
                       )
                     ),
                     column(
                       width=12,
                       column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                              h4("Enter indicator's listname:")
                       ),
                       column(width = 6, offset = 0,
                              selectizeInput("subIndicatorListnameInput", label = NULL, selected = rowInd[1,"listname"], choices = c("-- select --",list_name),
                                             options = list(placeholder = "-- select --", create = TRUE),
                                             width = "100%")
                       )
                     )

                 )

                 ,sep="")

      s

    }, error = function(err) {
      print("qaasddsdc4")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  observeEvent(input$completeSaveIndicatorButton,{
    tryCatch({
      selInd <- indicatorsInfo$selectedIndicator
      indicatorsIF <- indicatorsInfo[["data"]]

      if(sum(input$subIndicatorVariableInput=="-- select --") ){
        shinyalert("Variable is required",
                   'Please make sure that you entered the "Variable" for this Indicator',
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      if(sum(input$subIndicatorListnameInput=="-- select --") ){
        shinyalert("Listname is required",
                   'Please make sure that you entered the "Listname" for this Indicator',
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }

      progress <- shiny::Progress$new()
      progress$set(message = "Process in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()

      indicatorsIF[!is.na(indicatorsIF$fullname) & indicatorsIF$fullname == selInd, "variable"] = input$subIndicatorVariableInput
      indicatorsIF[!is.na(indicatorsIF$fullname) & indicatorsIF$fullname == selInd, "listname"] = input$subIndicatorListnameInput
      updateProgress()


      result <- kobo_edit_form(indicator = indicatorsIF)

      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }

      updateProgress()
      indicatorsIF <- indicatorsIF %>% arrange(fullname)
      indicatorsInfo[["data"]] <- indicatorsIF
      removeModal()

    }, error = function(err) {
      print("zxmbcmxcbl")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  output$calculationBuilderToolBody <- renderUI({
    fluidRow(
      column(
        width=12,
        column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("Select the case of this Indicator?")
        ),
        column(width = 6, offset = 0,
               selectInput("indicatorCaseSelectInput", label = NULL,choices = c("-- select --",
                                                                                "Discretize a value",
                                                                                "Re categorize a categorical variable by re coding modalities",
                                                                                "Sum up different numeric or integer variables",
                                                                                "Calculate min, max or avg value for multiple integer or numeric variables",
                                                                                "Set condition on specific variables",
                                                                                "Calculate ratio by dividing 2 numeric or integer variables",
                                                                                "Aggregate variables from nested frame"
               ),width = "100%")
        )
      ),
      conditionalPanel(
        condition = "input.indicatorCaseSelectInput == 'Discretize a value'",
        column(
          width=12,
          column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                 h4("Select the frame that contains the variable")
          ),
          column(width = 6, offset = 0,
                 selectInput("frameDVSelectInput", label = NULL,choices = c("-- select --", projectConfigurationInfo$data[["beginRepeatList"]]),width = "100%")
          )
        )
      ),
      conditionalPanel(
        condition = "input.frameDVSelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Discretize a value'",
        uiOutput("variableDVUI")
      ),
      conditionalPanel(
        condition = "input.variableDVSelectInput != '-- select --' && input.frameDVSelectInput != '-- select --'  && input.indicatorCaseSelectInput == 'Discretize a value'",
        uiOutput("breaksDVUI")
      ),


      conditionalPanel(
        condition = "input.indicatorCaseSelectInput == 'Re categorize a categorical variable by re coding modalities'",
        column(
          width=12,
          column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                 h4("Select the frame that contains the variable")
          ),
          column(width = 6, offset = 0,
                 selectInput("frameFRSelectInput", label = NULL,choices = c("-- select --", projectConfigurationInfo$data[["beginRepeatList"]]),width = "100%")
          )
        )
      ),
      conditionalPanel(
        condition = "frameFRSelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Re categorize a categorical variable by re coding modalities'",
        uiOutput("variableFRUI")
      ),

      conditionalPanel(
        condition = "input.variableFRSelectInput != '-- select --' && input.indicatorCaseSelectInput ==  'Re categorize a categorical variable by re coding modalities'",
        uiOutput("listnameFRUI")
      ),
      conditionalPanel(
        condition = "input.listnameFRSelectInput != '-- select --'  && input.indicatorCaseSelectInput == 'Re categorize a categorical variable by re coding modalities'",
        uiOutput("factorValuesFRUI")
      ),


      conditionalPanel(
        condition = "input.indicatorCaseSelectInput == 'Sum up different numeric or integer variables'",
        column(
          width=12,
          column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                 h4("Select the frame that contains the variable(s)")
          ),
          column(width = 6, offset = 0,
                 selectInput("frameSUSelectInput", label = NULL,choices = c("-- select --", projectConfigurationInfo$data[["beginRepeatList"]]),width = "100%")
          )
        )
      ),
      conditionalPanel(
        condition = "input.frameSUSelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Sum up different numeric or integer variables'",
        column(
          width=12, style="margin-top: 20px; border-top: 1px solid #e5e5e5; padding-top: 30px;",
          conditionalPanel(
            condition = "input.frameSUSelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Sum up different numeric or integer variables'",
            uiOutput("mainTwoVariablesSUUI")
          ),
          conditionalPanel(
            condition = "input.frameSUSelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Sum up different numeric or integer variables'",
            uiOutput("otherVariablesSUUI")
          )
        )
      ),


      conditionalPanel(
        condition = "input.indicatorCaseSelectInput == 'Calculate min, max or avg value for multiple integer or numeric variables'",
        column(
          width=12,
          column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                 h4("Select the frame that contains the variable")
          ),
          column(width = 6, offset = 0,
                 selectInput("frameMMASelectInput", label = NULL,choices = c("-- select --", projectConfigurationInfo$data[["beginRepeatList"]]),width = "100%")
          )
        )
      ),
      conditionalPanel(
        condition = "input.frameMMASelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Calculate min, max or avg value for multiple integer or numeric variables'",
        uiOutput("variableMMAUI")
      ),
      conditionalPanel(
        condition = "input.variableMMASelectInput != '-- select --' && input.frameMMASelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Calculate min, max or avg value for multiple integer or numeric variables'",
        uiOutput("statisticalFunctionsMMAUI")
      ),


      conditionalPanel(
        condition = "input.indicatorCaseSelectInput == 'Calculate ratio by dividing 2 numeric or integer variables'",
        column(
          width=12,
          column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                 h4("Select the frame that contains the variable")
          ),
          column(width = 6, offset = 0,
                 selectInput("frameD2SelectInput", label = NULL,choices = c("-- select --", projectConfigurationInfo$data[["beginRepeatList"]]),width = "100%")
          )
        )
      ),
      conditionalPanel(
        condition = "input.frameD2SelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Calculate ratio by dividing 2 numeric or integer variables'",
        uiOutput("variablesD2AUI")
      ),


      conditionalPanel(
        condition = "input.indicatorCaseSelectInput == 'Set condition on specific variables'",
        column(
          width=12,
          column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                 h4("Select the frame that contains the variable")
          ),
          column(width = 6, offset = 0,
                 selectInput("frameIFSelectInput", label = NULL,choices = c("-- select --", projectConfigurationInfo$data[["beginRepeatList"]]),width = "100%")
          )
        )
      ),
      conditionalPanel(
        condition = "input.frameIFSelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Set condition on specific variables'",
        column(
          width=12,
          uiOutput("conditionsBlockIFUI")
        )
      )
    )
  })

  ##########Discretize a value##################

  output$variableDVUI <- renderUI({
    tryCatch({
      if(input$frameDVSelectInput != "-- select --"){
        if(file.exists(paste(mainDir(), "data", paste("/",input$frameDVSelectInput, ".csv", sep=""), sep = "/", collapse = "/"))){
          temp <- read.csv(
            paste(mainDir(), "data", paste("/",input$frameDVSelectInput, ".csv", sep=""), sep = "/", collapse = "/"),
            stringsAsFactors = FALSE, nrows = 1
          )
          selectedCol <- unlist(lapply(temp, is.numeric))
          column(
            width=12,
            column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                   h4("Select the variable")
            ),
            column(width = 6, offset = 0,
                   selectInput("variableDVSelectInput", label = NULL,choices = c("-- select --",
                                                                                 colnames(temp[ , selectedCol])
                   ),width = "100%")
            )
          )
        }else{
          infoBox(
            width = 12,strong("Warning"),
            h4(paste("You cannot proceed without",input$frameDVSelectInput,"file"), align = "center")
            ,icon = icon("exclamation-triangle"),
            color = "yellow"
          )
        }

      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })

  output$breaksDVUI <- renderUI({
    tryCatch({
      if(!is.null(input$variableDVSelectInput)){
        if(input$variableDVSelectInput != "-- select --"){
          column(
            width=12,
            column(width = 4, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                   h4("Enter the Breaks argument")
            ),
            column(width = 1,
                   div(class = "help-tip",style="top: 0px;left: 25px;",
                       p(
                         span("Enter either a numeric vector of two or more unique cut points EX: 0,25,50,75,100",style="display: block;"),
                         span("OR\n",style="display: block;text-align: center;margin: 20px 0px;font-weight: 900;border-top: 1px solid white;line-height: 0px;
                              font-size: 25px;color: #1aab8a;"),
                         span("Single number as Interval length EX: 5, so the length of the interval will be equal to max(x)/5, where x is the variable.",style="display: block;")
                         )
                   )
            ),
            column(width = 7, offset = 0,
                   textInput("breaksDVTextInput", label = NULL, width = "100%", placeholder="EX: 0,25,50,75,100 or 5")
            )
          )
        }
      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })

  resultDVUIValue <- reactiveValues(text="")

  observeEvent(input$frameDVSelectInput,{
    resultDVUIValue$text <- ""
  })

  observeEvent(input$variableDVSelectInput,{
    resultDVUIValue$text <- ""
  })

  observe({
    if(!is.null(input$breaksDVTextInput)){
      if(input$breaksDVTextInput == ""){
        resultDVUIValue$text = ""
      }
    }
  })
  ##########-----END----------##################


  ##########Re categorize a categorical variable by re coding modalities##################
  output$variableFRUI <- renderUI({
    tryCatch({
      if(input$frameFRSelectInput != "-- select --"){
        if(file.exists(paste(mainDir(), "data", paste("/",input$frameFRSelectInput, ".csv", sep=""), sep = "/", collapse = "/"))){
          temp <- read.csv(
            paste(mainDir(), "data", paste("/",input$frameFRSelectInput, ".csv", sep=""), sep = "/", collapse = "/"),
            stringsAsFactors = FALSE, nrows = 1
          )
          selectedCol <- unlist(lapply(temp, is.character))
          column(
            width=12,
            column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                   h4("Select the variable")
            ),
            column(width = 6, offset = 0,
                   selectInput("variableFRSelectInput", label = NULL,choices = c("-- select --",
                                                                                 colnames(temp[ , selectedCol])
                   ),width = "100%")
            )
          )
        }else{
          infoBox(
            width = 12,strong("Warning"),
            h4(paste("You cannot proceed without",input$frameDVSelectInput,"file"), align = "center")
            ,icon = icon("exclamation-triangle"),
            color = "yellow"
          )
        }
      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })

  choicesSheetFR <- reactive({
    tryCatch({
      form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
      choices <- as.data.frame(read_excel(form_tmp, sheet = "choices"),
                               stringsAsFactors = FALSE)
      choices <- choices[!is.na(choices$list_name),]
      choices <- choices[trimws(choices$list_name)!="",]
      choices
    }, error = function(err) {
      print("1x3wex")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
      data.frame(
        name = character(),
        label = character(),
        value = character(),
        path = character(),
        stringsAsFactors = FALSE
      )
    })
  })

  output$listnameFRUI <- renderText({
    tryCatch({
      if(!is.null(input$variableFRSelectInput)){
        if(input$variableFRSelectInput != "-- select --"){
          s <- ""
          s<-paste(
            column(
              width=12,
              column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                     h4("Select the listname that represent the factor")
              ),
              column(width = 6, offset = 0,
                     selectInput("listnameFRSelectInput", label = NULL,choices = c("-- select --",
                                                                                   choicesSheetFR()$list_name
                     ),width = "100%")
              )
            ),s,sep = "")
          return(s)
        }
      }
      return("")
    }, error = function(err) {
      return(infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      ))
    })
  })

  output$factorValuesFRUI <- renderText({
    tryCatch({
      if(!is.null(input$listnameFRSelectInput)){
        if(input$listnameFRSelectInput != "-- select --"){
          factorValues <- choicesSheetFR()[choicesSheetFR()$list_name==input$listnameFRSelectInput,c("list_name", "name", "label")]

          s <- column(width = 2,offset=10,style="margin-bottom:50px;",
                      div(class = "help-tip",style="top: 0px;left: 25px;",
                          p(
                            span("Enter the new name of level",style="display: block;"),
                            span("OR\n",style="display: block;text-align: center;margin: 20px 0px;font-weight: 900;border-top: 1px solid white;line-height: 0px;
                                 font-size: 25px;color: #1aab8a;"),
                            span("Keep it as is",style="display: block;"),
                            span("OR\n",style="display: block;text-align: center;margin: 20px 0px;font-weight: 900;border-top: 1px solid white;line-height: 0px;
                                 font-size: 25px;color: #1aab8a;"),
                            span("Replace it with 'NULL' value to remove it",style="display: block;")
                            )
                          )
          )


          s <- paste(s,
                     column(width = 12,offset = 0,style="margin-bottom: 10px;",
                            column(width = 3, style="background-color: black;color: white;text-align: center;border-right: 1px solid white;",
                                   h4("list_name")),
                            column(width = 5, style="background-color: black;color: white;text-align: center;border-right: 1px solid white;",
                                   h4("label")),
                            column(width = 4, style="background-color: black;color: white;text-align: center;",
                                   h4("name"))
                     ),sep="")
          for(i in 1:nrow(factorValues)){
            s <- paste(s,
                       column(width = 12,offset = 0,
                              column(width = 3,
                                     h5(factorValues[i,"list_name"]) ),
                              column(width = 5,style="border-left: 2px solid #1aab8a;border-right: 2px solid #1aab8a;",
                                     h5(factorValues[i,"label"]) ),
                              column(width = 4,
                                     textInput(paste(i,factorValues[i,"name"],sep = "-"),
                                               label = NULL,
                                               value = factorValues[i,"name"],
                                               placeholder = factorValues[i,"name"],
                                               width = "100%"
                                     )
                              )
                       )
                       ,sep = "")
          }
          return(s)
        }
      }
    }, error = function(err) {
      return(infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      ))
    })
  })

  resultFRUIValue <- reactiveValues(text="")

  observeEvent(input$frameFRSelectInput,{
    resultFRUIValue$text <- ""
  })

  observeEvent(input$variableFRSelectInput,{
    resultFRUIValue$text <- ""
  })

  observeEvent(input$listnameFRSelectInput,{
    resultFRUIValue$text <- ""
  })

  ##########-----END----------##################


  ##########Sum up different numeric or integer variables##################
  variablesToUseSU <- reactiveValues(col=NULL,counterOfVar=2, otherVariablesSUUI="", idOfVar=c())
  resultSUUIValue <- reactiveValues(text="")

  observeEvent(input$frameSUSelectInput,{
    tryCatch({
      if(sum(input$frameSUSelectInput != "-- select --") ){
        temp <- read.csv(
          paste(mainDir(), "data", paste("/",input$frameSUSelectInput, ".csv", sep=""), sep = "/", collapse = "/"),
          stringsAsFactors = FALSE, nrows = 1
        )
        selectedCol <- unlist(lapply(temp, is.numeric))
        selectedCol <- colnames(temp[ , selectedCol])
        selectedCol <- sort(selectedCol)
        variablesToUseSU$col <- selectedCol
      }
    }, error = function(err) {
      return(infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      ))
    })
  })

  output$mainTwoVariablesSUUI <- renderUI({
    column(
      width=12,
      column(width = 1, offset = 0, align = "center",
             icon("flag", "fa-2x")
      ),
      column(width = 3, offset = 0, align = "center",
             selectInput("varSU1", label = NULL,choices = variablesToUseSU$col,width = "100%", selected = 1)
      ),
      column(width = 1, offset = 0, align = "center",
             icon("plus", "fa-2x")
      ),
      column(width = 4, offset = 0, align = "center",
             selectInput("varSU2", label = NULL,choices = variablesToUseSU$col,width = "100%", selected = 15)
      ),
      column(width = 3, offset = 0, align = "center",
             actionButton("addVariablesSU", "Add Variable", class = "toolButton", style="height: 35px;", icon = icon("plus-circle","fa-1x"))
      )
    )
  })

  output$otherVariablesSUUI <- renderText({
    variablesToUseSU$otherVariablesSUUI
  })

  observeEvent(input$addVariablesSU,{
    tryCatch({
      resultSUUIValue$text <- ""
      if(length(variablesToUseSU$idOfVar)==0 || variablesToUseSU$idOfVar==0){
        variablesToUseSU$counterOfVar <- variablesToUseSU$counterOfVar + 1
        variablesToUseSU$idOfVar <- c(variablesToUseSU$counterOfVar)
      }else{
        variablesToUseSU$counterOfVar <- variablesToUseSU$counterOfVar + 1
        variablesToUseSU$idOfVar <- c(variablesToUseSU$idOfVar, variablesToUseSU$counterOfVar)
      }

      if( length(variablesToUseSU$idOfVar) > length(variablesToUseSU$col) ){
        shinyalert("Info",
                   "You can't add more variables, because you have enough inputs that cover all variables",
                   type = "info",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        variablesToUseSU$counterOfVar <- variablesToUseSU$counterOfVar-1
        return(NULL)
      }

      s <- ""
      for(i in variablesToUseSU$idOfVar){
        tempVal<-1

        if(!is.null(input[[paste("varSU", i, sep = "")]])){
          tempVal <- input[[paste("varSU", i, sep = "")]]
        }

        s <- paste(s,
                   column(
                     width=12,
                     column(width = 1, offset = 0, align = "center",
                            icon("plus", "fa-2x")
                     ),
                     column(width = 8, offset = 0, align = "center",
                            selectInput(paste("varSU", i, sep = ""), label = NULL,choices = variablesToUseSU$col,width = "100%", selected = tempVal)
                     ),
                     column(width = 2, offset = 0, align = "center",
                            actionButton(paste("deleteVariableSU", i ,sep = ""), NULL, class = "toolButtonDelete", style="height: 35px;", icon = icon("times","fa-2x"))
                     )
                   ),sep="")
      }


      lapply(1:length(variablesToUseSU$idOfVar), function(j) {
        observeEvent(input[[paste("deleteVariableSU", variablesToUseSU$idOfVar[j] ,sep = "")]] , {
          resultSUUIValue$text <- ""
          variablesToUseSU$idOfVar <- variablesToUseSU$idOfVar[ variablesToUseSU$idOfVar != variablesToUseSU$idOfVar[j] ]
          variablesToUseSU$idOfVar <- sort(variablesToUseSU$idOfVar)


          if(length(variablesToUseSU$idOfVar)==0 || variablesToUseSU$idOfVar==0){
            variablesToUseSU$idOfVar <- c()
          }

          s <- ""
          for(i in variablesToUseSU$idOfVar){
            tempVal<-1

            if(!is.null(input[[paste("varSU", i, sep = "")]])){
              tempVal <- input[[paste("varSU", i, sep = "")]]
            }

            s <- paste(s,
                       column(
                         width=12,
                         column(width = 1, offset = 0, align = "center",
                                icon("plus", "fa-2x")
                         ),
                         column(width = 8, offset = 0, align = "center",
                                selectInput(paste("varSU", i, sep = ""), label = NULL,choices = variablesToUseSU$col,width = "100%", selected = tempVal)
                         ),
                         column(width = 2, offset = 0, align = "center",
                                actionButton(paste("deleteVariableSU", i ,sep = ""), NULL, class = "toolButtonDelete", style="height: 35px;", icon = icon("times","fa-2x"))
                         )
                       ),sep="")
          }

          variablesToUseSU$otherVariablesSUUI <- s

        }, ignoreInit = TRUE,once = TRUE)
      })


      variablesToUseSU$otherVariablesSUUI <- s
    }, error = function(err) {
      print("7565hujfhn")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  observeEvent(input$varSU1,{
    resultSUUIValue$text <- ""
  })

  observeEvent(input$varSU2,{
    resultSUUIValue$text <- ""
  })

  observeEvent(input$frameSUSelectInput,{
    resultSUUIValue$text <- ""
  })

  ##########-----END----------##################


  ##########Calculate min, max or avg value for multiple integer or numeric variables##################
  resultMMAUIValue <- reactiveValues(text="")

  output$variableMMAUI <- renderUI({
    tryCatch({
      if(sum(input$frameMMASelectInput != "-- select --")){
        if(file.exists(paste(mainDir(), "data", paste("/",input$frameMMASelectInput, ".csv", sep=""), sep = "/", collapse = "/"))){
          temp <- read.csv(
            paste(mainDir(), "data", paste("/",input$frameMMASelectInput, ".csv", sep=""), sep = "/", collapse = "/"),
            stringsAsFactors = FALSE, nrows = 1
          )
          selectedCol <- unlist(lapply(temp, is.numeric))
          column(
            width=12,
            column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                   h4("Select the variable")
            ),
            column(width = 6, offset = 0,
                   selectInput("variableMMASelectInput", label = NULL,choices = c("-- select --",
                                                                                  colnames(temp[ , selectedCol])
                   ),width = "100%")
            )
          )
        }else{
          infoBox(
            width = 12,strong("Warning"),
            h4(paste("You cannot proceed without",input$frameMMASelectInput,"file"), align = "center")
            ,icon = icon("exclamation-triangle"),
            color = "yellow"
          )
        }

      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })

  output$statisticalFunctionsMMAUI <- renderUI({
    tryCatch({
      if(input$variableMMASelectInput != "-- select --"){
        column(
          width=12,
          column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                 h4("Select the Statistical Function")
          ),
          column(width = 6, offset = 0,
                 selectInput("statisticalFunctionsMMASelectInput", label = NULL,choices = c("-- select --",
                                                                                            "Minimum",
                                                                                            "Maximum",
                                                                                            "Average",
                                                                                            "Median",
                                                                                            "Mode",
                                                                                            "Standard Deviation",
                                                                                            "Interquartile Range"
                 )
                 ,width = "100%")
          )
        )

      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })

  observeEvent(input$frameMMASelectInput,{
    resultMMAUIValue$text <- ""
  })

  observeEvent(input$variableMMASelectInput,{
    resultMMAUIValue$text <- ""
  })

  observeEvent(input$variableMMASelectInput,{
    resultMMAUIValue$text <- ""
  })

  observeEvent(input$statisticalFunctionsMMASelectInput,{
    resultMMAUIValue$text <- ""
  })
  ##########-----END----------##################


  ##########Calculate ratio by dividing 2 numeric or integer variables##################
  resultD2UIValue <- reactiveValues(text="")

  output$variablesD2AUI <- renderUI({
    tryCatch({
      if(sum(input$frameD2SelectInput != "-- select --")){
        if(file.exists(paste(mainDir(), "data", paste("/",input$frameD2SelectInput, ".csv", sep=""), sep = "/", collapse = "/"))){
          temp <- read.csv(
            paste(mainDir(), "data", paste("/",input$frameD2SelectInput, ".csv", sep=""), sep = "/", collapse = "/"),
            stringsAsFactors = FALSE, nrows = 1
          )
          selectedCol <- unlist(lapply(temp, is.numeric))
          column(
            width=12,
            column(width = 12, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                   h4("Select from variables, enter numeric ,or integer value")
            ),
            column(width = 12,style="margin-top: 15px;",
                   column(width = 5, offset = 0, align="center",style="margin-top: 15px;",
                          selectizeInput("variableD2SelectInput1", label = NULL,choices = c("-- select --",
                                                                                            colnames(temp[ , selectedCol])
                          ),width = "100%",options = list(create = TRUE))
                   ),
                   column(width = 2, offset = 0, align="center",
                          h3("/")
                   ),
                   column(width = 5, offset = 0, align="center",style="margin-top: 15px;",
                          selectizeInput("variableD2SelectInput2", label = NULL,choices = c("-- select --",
                                                                                            colnames(temp[ , selectedCol])
                          ),width = "100%",options = list(create = TRUE))
                   )
            )
          )
        }else{
          infoBox(
            width = 12,strong("Warning"),
            h4(paste("You cannot proceed without",input$frameD2SelectInput,"file"), align = "center")
            ,icon = icon("exclamation-triangle"),
            color = "yellow"
          )
        }

      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })

  observeEvent(input$frameD2SelectInput,{
    resultD2UIValue$text <- ""
  })

  observeEvent(input$variableD2SelectInput1,{
    resultD2UIValue$text <- ""
  })

  observeEvent(input$variableD2SelectInput2,{
    resultD2UIValue$text <- ""
  })

  ##########-----END----------##################


  ##########Set condition on specific variables###########################
  ifVariables <- reactiveValues(conditionsIdOfBlock1=NULL,
                                mainDataFrame = NULL,
                                newBlock="",
                                blocksId = NULL,
                                lastIdConditionsBlock1 = NULL,
                                lastIdBlocks = NULL
  )
  blockWithNewId <- reactiveValues(lastIdBlocks=2)

  observeEvent(input$frameIFSelectInput,{
    blocksId <- c(1, ifVariables$blocksId)
    for(i in 1:length(blocksId) ){
      ifVariables[[paste("lastIdConditions", blocksId[i], sep="")]] <- 0
      ifVariables[[paste("conditionsIdOfBlock", blocksId[i], sep="")]] <- NULL
    }
    ifVariables$conditionsIdOfBlock1 <- NULL
    ifVariables$mainDataFrame <- NULL
    ifVariables$newBlock <- ""
    ifVariables$blocksId <- NULL
    ifVariables$lastIdConditionsBlock1 <- 0
    ifVariables$lastIdBlocks <- 0
  })

  observeEvent(input$cancelIndicatorButton,{
    blocksId <- c(1, ifVariables$blocksId)
    for(i in 1:length(blocksId) ){
      ifVariables[[paste("lastIdConditionsBlock", blocksId[i], sep="")]] <- ifVariables[[paste("lastIdConditionsBlock", blocksId[i], sep="")]] + 1
      ifVariables[[paste("conditionsIdOfBlock", blocksId[i], sep="")]] <- NULL
      ifVariables[[paste("moreConditionsBlock", blocksId[i], sep="")]] <- ""
    }

    ifVariables$mainDataFrame <- NULL
    ifVariables$newBlock <- ""
    ifVariables$lastIdBlocks <-  ifVariables$lastIdBlocks + 1
    blockWithNewId$lastIdBlocks <- ifVariables$lastIdBlocks
    ifVariables$blocksId <- NULL

    removeModal()
  })

  output$conditionsBlockIFUI <- renderUI({
    column(width = 12,
           column(width = 4, offset = 8,
                  actionButton("AddIfBlock" ,"Add if block", icon = icon("plus"), class = "toolButton", style="height: 40px; margin-bottom:20px;", width="100%")
           ),
           box(title = "Block 1", id="Block1",status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
               column(width = 2,
                      h3("if(")
               ),
               column(width = 4,offset = 6,
                      actionButton("addConditionBlock1", "Add condition for Block1", icon = icon("plus") ,class = "toolButton", style="height: 35px; margin-top: 20px;", width = "100%")
               ),
               column(width = 12,offset = 0, style="margin-top: 20px;" , align = "left",
                      box(title = "Condition 1", id="cond1Block1",status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                          uiOutput("bodyOfCondition1")
                      )
               ),
               column(width = 12,offset = 0 , align = "left",
                      uiOutput("moreConditionsBlock1")
               ),
               column(width = 1,
                      h3("){")
               ),
               column(width = 11,offset = 1,
                      uiOutput("bodyOfBlock1")
               ),
               column(width = 1,
                      h3("}")
               )
           ),
           uiOutput("newBlock"),
           box(title = "Else", id="elseBlock",status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
               column(width = 2,
                      h3("else(")
               ),
               column(width = 11,offset = 1,
                      uiOutput("bodyOfElse")
               ),
               column(width = 1,
                      h3("}")
               )
           )

    )
  })

  output$bodyOfCondition1 <- renderUI({
    tryCatch({
      if(sum(input$frameIFSelectInput != "-- select --")){
        if(file.exists(paste(mainDir(), "data", paste("/",input$frameIFSelectInput, ".csv", sep=""), sep = "/", collapse = "/"))){
          temp <- read.csv(
            paste(mainDir(), "data", paste("/",input$frameIFSelectInput, ".csv", sep=""), sep = "/", collapse = "/"),
            stringsAsFactors = FALSE, nrows = 1
          )
          ifVariables$mainDataFrame <- temp
          column(width=12,
                 column(width = 5, align = "center",
                        selectizeInput("leftSideCond1Block1", label = NULL,choices = c("-- select --",
                                                                                       colnames(temp)
                        ),width = "100%",options = list( placeholder = "-- select --"))
                 ),
                 column(width = 2, align = "center",
                        selectInput("logicalOperatorsCond1Block1",label = NULL, choices = c("-- select --", "<", "<=", ">", ">=", "==", "!=", "&", "|", "in", "not in"), width = "100%")
                 ),
                 column(width = 5, align = "center",
                        selectizeInput("rightSideCond1Block1", label = NULL,choices = c("-- select or enter --",
                                                                                        colnames(temp)
                        ),width = "100%",options = list(create = TRUE, placeholder = "-- select or enter --"))
                 )
          )
        }else{
          infoBox(
            width = 12,strong("Warning"),
            h4(paste("You cannot proceed without",input$frameIFSelectInput,"file"), align = "center")
            ,icon = icon("exclamation-triangle"),
            color = "yellow"
          )
        }

      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      )
    })

  })

  output$moreConditionsBlock1 <- renderText({
    ifVariables[["moreConditionsBlock1"]]
  })

  observeEvent(input$addConditionBlock1,{
    tryCatch({
      if(is.null(ifVariables$conditionsIdOfBlock1)){
        if(is.null(ifVariables$lastIdConditionsBlock1)){
          ifVariables$lastIdConditionsBlock1 <- 2
        }
        ifVariables$conditionsIdOfBlock1 <- ifVariables$lastIdConditionsBlock1
      }else{
        ifVariables$lastIdConditionsBlock1 <- ifVariables$lastIdConditionsBlock1 + 1
        ifVariables$conditionsIdOfBlock1 <- c(ifVariables$conditionsIdOfBlock1, ifVariables$lastIdConditionsBlock1)
      }
      print("**")
      print(ifVariables$conditionsIdOfBlock1)
      print("##")
      s <- ""
      for(i in 1:length(ifVariables$conditionsIdOfBlock1)){
        choices <- c(paste("Condition", i))
        track <- ""
        for (j in 1:i) {
          if(i>1){
            if(track==""){
              track <- paste("Condition", j)
            }else{
              track <- paste(track,paste("Condition", j), sep = " - ")
            }
          }
        }
        choices <- c(choices,track)
        s <- paste(s,
                   box(title = paste("Condition",i+1), id=paste("cond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep=""),status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                       if(input$frameIFSelectInput != "-- select --"){
                         temp <- ifVariables$mainDataFrame
                         column(width = 12,
                                column(width = 12, style="color: white; background-color: black; height: 70px; padding: 15px 0px 0px; margin-bottom: 30px; border-bottom: 5px solid lightgray;",
                                       column(width = 2, align = "left",
                                              h4("Link by")
                                       ),
                                       column(width = 3, align = "center",
                                              selectInput(paste("linkByCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep=""),label = NULL, choices = c("-- select --", "and", "or"),
                                                          selected = ifelse(!is.null(input[[paste("linkByCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]]), input[[paste("linkByCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]], "-- select --")

                                                          ,width = "100%")
                                       ),
                                       column(width = 2, align = "center",
                                              h4("With")
                                       ),
                                       column(width = 5, align = "center",
                                              selectInput(paste("withCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep=""),label = NULL, choices = c("-- select --", choices),
                                                          selected = ifelse(!is.null(input[[paste("withCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]]), input[[paste("withCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]], "-- select --")

                                                          ,width = "100%")
                                       )
                                ),

                                column(width = 5, align = "center",
                                       selectizeInput(paste("leftSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep=""), label = NULL,choices = c("-- select --",
                                                                                                                                                          colnames(temp)
                                       ),
                                       selected = ifelse(!is.null(input[[paste("leftSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep="")]]), input[[paste("leftSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep="")]], "-- select --")
                                       ,width = "100%",options = list(placeholder = "-- select --"))
                                ),
                                column(width = 2, align = "center",
                                       selectInput(paste("logicalOperatorsCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep=""),label = NULL, choices = c("-- select --", "<", "<=", ">", ">=", "==", "!=", "&", "|", "in", "not in"),
                                                   selected = ifelse(!is.null(input[[paste("logicalOperatorsCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]]), input[[paste("logicalOperatorsCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]], "-- select --")

                                                   ,width = "100%")
                                ),
                                column(width = 5, align = "center",
                                       selectizeInput(paste("rightSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep=""), label = NULL,choices = c("-- select or enter --",
                                                                                                                                                           colnames(temp)
                                       ),
                                       selected = ifelse(!is.null(input[[paste("rightSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep="")]]), input[[paste("rightSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep="")]], "-- select --")
                                       ,width = "100%",options = list(create = TRUE, placeholder = "-- select or enter --"))
                                ),
                                column(width = 12, align = "center",
                                       actionButton(paste("deleteCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep=""), "Delete Condition", icon = icon("trash-alt"), class = "deleteButton", style="height: 35px; margin-bottom:20px;", width="100%")
                                )
                         )

                       }
                   )

                   ,sep = "")
      }


      lapply(1:length(ifVariables$conditionsIdOfBlock1), function(k) {
        observeEvent(input[[paste("deleteCond",ifVariables$conditionsIdOfBlock1[k],"Block1",sep="")]] , {
          ifVariables$conditionsIdOfBlock1 <- ifVariables$conditionsIdOfBlock1[ ifVariables$conditionsIdOfBlock1 != ifVariables$conditionsIdOfBlock1[k] ]
          s <- ""
          if(length(ifVariables$conditionsIdOfBlock1)!=0){
            for(i in 1:length(ifVariables$conditionsIdOfBlock1)){
              choices <- c(paste("Condition", i))
              track <- ""
              for (j in 1:i) {
                if(i>1){
                  if(track==""){
                    track <- paste("Condition", j)
                  }else{
                    track <- paste(track,paste("Condition", j), sep = " - ")
                  }
                }
              }
              choices <- c(choices,track)
              s <- paste(s,
                         box(title = paste("Condition",i+1), id=paste("cond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep=""),status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                             if(input$frameIFSelectInput != "-- select --"){
                               temp <- ifVariables$mainDataFrame
                               column(width = 12,
                                      column(width = 12, style="color: white; background-color: black; height: 70px; padding: 15px 0px 0px; margin-bottom: 30px; border-bottom: 5px solid lightgray;",
                                             column(width = 2, align = "left",
                                                    h4("Link by")
                                             ),
                                             column(width = 3, align = "center",
                                                    selectInput(paste("linkByCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep=""),label = NULL, choices = c("-- select --", "and", "or"),
                                                                selected = ifelse(!is.null(input[[paste("linkByCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]]), input[[paste("linkByCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]], "-- select --")

                                                                ,width = "100%")
                                             ),
                                             column(width = 2, align = "center",
                                                    h4("With")
                                             ),
                                             column(width = 5, align = "center",
                                                    selectInput(paste("withCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep=""),label = NULL, choices = c("-- select --", choices),
                                                                selected = ifelse(!is.null(input[[paste("withCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]]), input[[paste("withCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]], "-- select --")

                                                                ,width = "100%")
                                             )
                                      ),

                                      column(width = 5, align = "center",
                                             selectizeInput(paste("leftSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep=""), label = NULL,choices = c("-- select --",
                                                                                                                                                                colnames(temp)
                                             ),
                                             selected = ifelse(!is.null(input[[paste("leftSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep="")]]), input[[paste("leftSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep="")]], "-- select --")
                                             ,width = "100%",options = list(placeholder = "-- select --"))
                                      ),
                                      column(width = 2, align = "center",
                                             selectInput(paste("logicalOperatorsCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep=""),label = NULL, choices = c("-- select --", "<", "<=", ">", ">=", "==", "!=", "&", "|", "in", "not in"),
                                                         selected = ifelse(!is.null(input[[paste("logicalOperatorsCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]]), input[[paste("logicalOperatorsCond",ifVariables$conditionsIdOfBlock1[i],"Block1", sep="")]], "-- select --")

                                                         ,width = "100%")
                                      ),
                                      column(width = 5, align = "center",
                                             selectizeInput(paste("rightSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep=""), label = NULL,choices = c("-- select or enter --",
                                                                                                                                                                 colnames(temp)
                                             ),
                                             selected = ifelse(!is.null(input[[paste("rightSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep="")]]), input[[paste("rightSideCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep="")]], "-- select --")
                                             ,width = "100%",options = list(create = TRUE, placeholder = "-- select or enter --"))
                                      ),
                                      column(width = 12, align = "center",
                                             actionButton(paste("deleteCond",ifVariables$conditionsIdOfBlock1[i],"Block1",sep=""), "Delete Condition", icon = icon("trash-alt"), class = "deleteButton", style="height: 35px; margin-bottom:20px;", width="100%")
                                      )
                               )

                             }
                         )

                         ,sep = "")
            }
          }else{
            ifVariables$lastIdConditionsBlock1 <- ifVariables$lastIdConditionsBlock1 +1
            ifVariables$conditionsIdOfBlock1 <- NULL
            s<-""
          }
          ifVariables[["moreConditionsBlock1"]] <- s
        }, ignoreInit = TRUE, once = TRUE)
      })


      ifVariables[["moreConditionsBlock1"]] <- s

    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })

  output$bodyOfBlock1 <- renderUI({
    tryCatch({
      if(input$frameIFSelectInput != "-- select --"){
        if(file.exists(paste(mainDir(), "data", paste("/",input$frameIFSelectInput, ".csv", sep=""), sep = "/", collapse = "/"))){
          temp <- ifVariables$mainDataFrame

          column(width=12,
                 column(width = 11, align = "center",
                        selectizeInput("resultOfBlock1", label = NULL,choices = c("-- select or enter --",
                                                                                  colnames(temp)
                        ),width = "100%",options = list(create = TRUE, placeholder = "-- select or enter --"))
                 )
          )
        }else{
          infoBox(
            width = 12,strong("Warning"),
            h4(paste("You cannot proceed without",input$frameIFSelectInput,"file"), align = "center")
            ,icon = icon("exclamation-triangle"),
            color = "yellow"
          )
        }

      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })

  output$bodyOfElse <- renderUI({
    tryCatch({
      if(input$frameIFSelectInput != "-- select --"){
        if(file.exists(paste(mainDir(), "data", paste("/",input$frameIFSelectInput, ".csv", sep=""), sep = "/", collapse = "/"))){
          temp <- ifVariables$mainDataFrame

          column(width=12,
                 column(width = 11, align = "center",
                        selectizeInput("resultOfElse", label = NULL,choices = c("-- select or enter --", "- none -",
                                                                                colnames(temp)
                        ),width = "100%",options = list(create = TRUE, placeholder = "-- select or enter --"))
                 )
          )
        }else{
          infoBox(
            width = 12,strong("Warning"),
            h4(paste("You cannot proceed without",input$frameIFSelectInput,"file"), align = "center")
            ,icon = icon("exclamation-triangle"),
            color = "yellow"
          )
        }

      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message, align = "center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })

  output$newBlock <- renderText({
    ifVariables$newBlock
  })


  observeEvent(input$AddIfBlock,{
    tryCatch({

      if (!input$AddIfBlock == ""){
        print("Before ***********")
        print(paste("last id block for ifVar", ifVariables$lastIdBlocks))
        print(paste("blocksId", ifVariables$blocksId))
        print(paste("last id block for blockWithNewId", ifVariables$blocksId))


        if(is.null(ifVariables$blocksId)){
          if(ifVariables$lastIdBlocks==0){
            ifVariables$lastIdBlocks <- blockWithNewId$lastIdBlocks
          }
          ifVariables$blocksId <- ifVariables$lastIdBlocks
        }
        else{
          ifVariables$lastIdBlocks <- ifVariables$lastIdBlocks + 1
          ifVariables$blocksId <- c(ifVariables$blocksId, ifVariables$lastIdBlocks)
        }
        print("After ***********")
        print(paste("last id block for ifVar", ifVariables$lastIdBlocks))
        print(paste("blocksId", ifVariables$blocksId))
        print(paste("last id block for blockWithNewId", ifVariables$blocksId))

        temp <- ifVariables$mainDataFrame
        s <- ""

        for (i in 1:length(ifVariables$blocksId)) {
          s <- paste(s,
                     box(title = paste("Block", i+1), id=paste("Block",ifVariables$blocksId[i], sep = ""),status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                         column(width = 2,
                                h3("if(")
                         ),
                         column(width = 4,offset = 6,
                                actionButton(paste("addConditionBlock",ifVariables$blocksId[i], sep = ""), paste("Add condition for Block", i+1, sep = ""), icon = icon("plus") ,class = "toolButton", style="height: 35px; margin-top: 20px;", width = "100%")
                         ),
                         column(width = 12,offset = 0, style="margin-top: 20px;" , align = "left",
                                box(title = "Condition 1", id=paste("cond1Block",ifVariables$blocksId[i], sep = ""),status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                                    column(width=12,
                                           column(width = 5, align = "center",
                                                  selectizeInput(paste("leftSideCond1Block",ifVariables$blocksId[i], sep = ""), label = NULL,choices = c("-- select --",
                                                                                                                                                         colnames(temp)
                                                  ),width = "100%",options = list( placeholder = "-- select --"))
                                           ),
                                           column(width = 2, align = "center",
                                                  selectInput(paste("logicalOperatorsCond1Block",ifVariables$blocksId[i], sep = ""),label = NULL, choices = c("-- select --", "<", "<=", ">", ">=", "==", "!=", "&", "|", "in", "not in"), width = "100%")
                                           ),
                                           column(width = 5, align = "center",
                                                  selectizeInput(paste("rightSideCond1Block",ifVariables$blocksId[i], sep = ""), label = NULL,choices = c("-- select or enter --",
                                                                                                                                                          colnames(temp)
                                                  ),width = "100%",options = list(create = TRUE, placeholder = "-- select or enter --"))
                                           )
                                    )
                                )
                         ),
                         column(width = 12,offset = 0 , align = "left",
                                uiOutput(paste("moreConditionsBlock",ifVariables$blocksId[i], sep = ""))
                         ),
                         column(width = 1,
                                h3("){")
                         ),
                         column(width = 11,offset = 1,
                                column(width=12,
                                       column(width = 11, align = "center",
                                              selectizeInput(paste("resultOfBlock",ifVariables$blocksId[i], sep = ""), label = NULL,choices = c("-- select or enter --",
                                                                                                                                                colnames(temp)
                                              ),width = "100%",options = list(create = TRUE, placeholder = "-- select or enter --"))
                                       )
                                )
                         ),
                         column(width = 1,
                                h3("}")
                         ),
                         column(width = 4,offset = 8,
                                actionButton(paste("deleteBlock",ifVariables$blocksId[i] ,sep=""), "Delete Block", icon = icon("trash-alt"), class = "deleteButton", style="height: 35px; margin-bottom:20px;", width="100%")
                         )
                     )
                     ,sep = "")
        }

        lapply(1:length(ifVariables$blocksId), function(m) {
          output[[paste("moreConditionsBlock",ifVariables$blocksId[m], sep = "")]] <- renderText({
            ifVariables[[paste("moreConditionsBlock",ifVariables$blocksId[m], sep = "")]]
          })
        })

        lapply(1:length(ifVariables$blocksId), function(w) {
          observeEvent(input[[paste("addConditionBlock", ifVariables$blocksId[w], sep="")]],{

            print(paste(paste("addConditionBlock", ifVariables$blocksId[w], sep="") ,"--- input -->", input[[paste("addConditionBlock", ifVariables$blocksId[w], sep="")]] ))
            print(paste("length of ids-->", length(ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]])))



            if(is.null(ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]] )){

              if(is.null(ifVariables[[paste("lastIdConditionsBlock", ifVariables$blocksId[w], sep="")]])){
                ifVariables[[paste("lastIdConditionsBlock", ifVariables$blocksId[w], sep="")]] <- 2
              }

              ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]] <- ifVariables[[paste("lastIdConditionsBlock", ifVariables$blocksId[w], sep="")]]

            }else{
              ifVariables[[paste("lastIdConditionsBlock", ifVariables$blocksId[w], sep="")]] <- ifVariables[[paste("lastIdConditionsBlock", ifVariables$blocksId[w], sep="")]] + 1
              ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]] <- c(ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]], ifVariables[[paste("lastIdConditionsBlock", ifVariables$blocksId[w], sep="")]])
            }

            print(paste("con1:",!input[[paste("addConditionBlock", ifVariables$blocksId[w], sep="")]] == "" ))
            print(paste("con2:",input[[paste("addConditionBlock", ifVariables$blocksId[w], sep="")]] == length(ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]])))

            if (!input[[paste("addConditionBlock", ifVariables$blocksId[w], sep="")]] == "" &
                input[[paste("addConditionBlock", ifVariables$blocksId[w], sep="")]] == length(ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]])){
              s <- ""
              for(i in 1:length(ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]])){
                choices <- c(paste("Condition", i))
                track <- ""
                for (j in 1:i) {
                  if(i>1){
                    if(track==""){
                      track <- paste("Condition", j)
                    }else{
                      track <- paste(track,paste("Condition", j), sep = " - ")
                    }
                  }
                }
                choices <- c(choices,track)
                s <- paste(s,
                           box(title = paste("Condition",i+1), id=paste("cond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i], sep=""),status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                               if(input$frameIFSelectInput != "-- select --"){
                                 temp <- ifVariables$mainDataFrame
                                 column(width = 12,
                                        column(width = 12, style="color: white; background-color: black; height: 70px; padding: 15px 0px 0px; margin-bottom: 30px; border-bottom: 5px solid lightgray;",
                                               column(width = 2, align = "left",
                                                      h4("Link by")
                                               ),
                                               column(width = 3, align = "center",
                                                      selectInput(paste("linkByCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep=""),label = NULL, choices = c("-- select --", "and", "or"),
                                                                  selected = ifelse(!is.null(input[[paste("linkByCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]]), input[[paste("linkByCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]], "-- select --")

                                                                  ,width = "100%")
                                               ),
                                               column(width = 2, align = "center",
                                                      h4("With")
                                               ),
                                               column(width = 5, align = "center",
                                                      selectInput(paste("withCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep=""),label = NULL, choices = c("-- select --", choices),
                                                                  selected = ifelse(!is.null(input[[paste("withCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]]), input[[paste("withCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]], "-- select --")

                                                                  ,width = "100%")
                                               )
                                        ),

                                        column(width = 5, align = "center",
                                               selectizeInput(paste("leftSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep=""), label = NULL,choices = c("-- select --",
                                                                                                                                                                                                                                                     colnames(temp)
                                               ),
                                               selected = ifelse(!is.null(input[[paste("leftSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep="")]]), input[[paste("leftSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep="")]], "-- select --")
                                               ,width = "100%",options = list(placeholder = "-- select --"))
                                        ),
                                        column(width = 2, align = "center",
                                               selectInput(paste("logicalOperatorsCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep=""),label = NULL, choices = c("-- select --", "<", "<=", ">", ">=", "==", "!=", "&", "|", "in", "not in"),
                                                           selected = ifelse(!is.null(input[[paste("logicalOperatorsCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]]), input[[paste("logicalOperatorsCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]], "-- select --")

                                                           ,width = "100%")
                                        ),
                                        column(width = 5, align = "center",
                                               selectizeInput(paste("rightSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep=""), label = NULL,choices = c("-- select or enter --",
                                                                                                                                                                                                                                                      colnames(temp)
                                               ),
                                               selected = ifelse(!is.null(input[[paste("rightSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep="")]]), input[[paste("rightSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep="")]], "-- select --")
                                               ,width = "100%",options = list(create = TRUE, placeholder = "-- select or enter --"))
                                        ),
                                        column(width = 12, align = "center",
                                               actionButton(paste("deleteCond", ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i] , paste("Block", ifVariables$blocksId[w], sep=""),sep=""), "Delete Condition", icon = icon("trash-alt"), class = "deleteButton", style="height: 35px; margin-bottom:20px;", width="100%")                                      )
                                 )

                               }
                           )

                           ,sep = "")
              }


              lapply(1:length(ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]]), function(k) {
                observeEvent(input[[paste("deleteCond", ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][k] , paste("Block", ifVariables$blocksId[w], sep=""),sep="")]] , {

                  ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]] <- ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][ ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]] != ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][k] ]
                  #print(ifVariables$conditionsIdOfBlock1)
                  s <- ""

                  if(length(ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]])!=0){
                    for(i in 1:length(ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]])){
                      choices <- c(paste("Condition", i))
                      track <- ""
                      for (j in 1:i) {
                        if(i>1){
                          if(track==""){
                            track <- paste("Condition", j)
                          }else{
                            track <- paste(track,paste("Condition", j), sep = " - ")
                          }
                        }
                      }
                      choices <- c(choices,track)
                      s <- paste(s,
                                 box(title = paste("Condition",i+1), id=paste("cond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i], sep=""),status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                                     if(input$frameIFSelectInput != "-- select --"){
                                       temp <- ifVariables$mainDataFrame
                                       column(width = 12,
                                              column(width = 12, style="color: white; background-color: black; height: 70px; padding: 15px 0px 0px; margin-bottom: 30px; border-bottom: 5px solid lightgray;",
                                                     column(width = 2, align = "left",
                                                            h4("Link by")
                                                     ),
                                                     column(width = 3, align = "center",
                                                            selectInput(paste("linkByCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep=""),label = NULL, choices = c("-- select --", "and", "or"),
                                                                        selected = ifelse(!is.null(input[[paste("linkByCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]]), input[[paste("linkByCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]], "-- select --")

                                                                        ,width = "100%")
                                                     ),
                                                     column(width = 2, align = "center",
                                                            h4("With")
                                                     ),
                                                     column(width = 5, align = "center",
                                                            selectInput(paste("withCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep=""),label = NULL, choices = c("-- select --", choices),
                                                                        selected = ifelse(!is.null(input[[paste("withCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]]), input[[paste("withCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]], "-- select --")

                                                                        ,width = "100%")
                                                     )
                                              ),

                                              column(width = 5, align = "center",
                                                     selectizeInput(paste("leftSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep=""), label = NULL,choices = c("-- select --",
                                                                                                                                                                                                                                                           colnames(temp)
                                                     ),
                                                     selected = ifelse(!is.null(input[[paste("leftSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep="")]]), input[[paste("leftSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep="")]], "-- select --")
                                                     ,width = "100%",options = list(placeholder = "-- select --"))
                                              ),
                                              column(width = 2, align = "center",
                                                     selectInput(paste("logicalOperatorsCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep=""),label = NULL, choices = c("-- select --", "<", "<=", ">", ">=", "==", "!=", "&", "|", "in", "not in"),
                                                                 selected = ifelse(!is.null(input[[paste("logicalOperatorsCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]]), input[[paste("logicalOperatorsCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""), sep="")]], "-- select --")

                                                                 ,width = "100%")
                                              ),
                                              column(width = 5, align = "center",
                                                     selectizeInput(paste("rightSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep=""), label = NULL,choices = c("-- select or enter --",
                                                                                                                                                                                                                                                            colnames(temp)
                                                     ),
                                                     selected = ifelse(!is.null(input[[paste("rightSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep="")]]), input[[paste("rightSideCond",ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i],paste("Block", ifVariables$blocksId[w], sep=""),sep="")]], "-- select --")
                                                     ,width = "100%",options = list(create = TRUE, placeholder = "-- select or enter --"))
                                              ),
                                              column(width = 12, align = "center",
                                                     actionButton(paste("deleteCond", ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]][i] , paste("Block", ifVariables$blocksId[w], sep=""),sep=""), "Delete Condition", icon = icon("trash-alt"), class = "deleteButton", style="height: 35px; margin-bottom:20px;", width="100%")                                      )
                                       )

                                     }
                                 )

                                 ,sep = "")
                    }
                  }else{
                    ifVariables[[paste("lastIdConditionsBlock", ifVariables$blocksId[w], sep="")]] <- ifVariables[[paste("lastIdConditionsBlock", ifVariables$blocksId[w], sep="")]] +1

                    ifVariables[[paste("conditionsIdOfBlock", ifVariables$blocksId[w], sep="")]] <- NULL
                    s<-""
                  }


                  ifVariables[[paste("moreConditionsBlock", ifVariables$blocksId[w], sep="")]] <- s
                })
              })

              ifVariables[[paste("moreConditionsBlock", ifVariables$blocksId[w], sep="")]] <- s
            }

          },ignoreInit = T)
        })

        lapply(1:length(ifVariables$blocksId), function(z) {
          observeEvent(input[[paste("deleteBlock",ifVariables$blocksId[z] ,sep="")]],{
            ifVariables$blocksId <- ifVariables$blocksId[ifVariables$blocksId != ifVariables$blocksId[z] ]
            #print(ifVariables$blocksId)
            s <- ""

            if(length(ifVariables$blocksId)!=0){
              temp <- ifVariables$mainDataFrame
              s <- ""
              for (i in 1:length(ifVariables$blocksId)) {
                s <- paste(s,
                           box(title = paste("Block", i+1), id=paste("Block",ifVariables$blocksId[i], sep = ""),status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                               column(width = 2,
                                      h3("if(")
                               ),
                               column(width = 4,offset = 6,
                                      actionButton(paste("addConditionBlock",ifVariables$blocksId[i], sep = ""), paste("Add condition for Block", i+1, sep = ""), icon = icon("plus") ,class = "toolButton", style="height: 35px; margin-top: 20px;", width = "100%")
                               ),
                               column(width = 12,offset = 0, style="margin-top: 20px;" , align = "left",
                                      box(title = "Condition 1", id=paste("cond1Block",ifVariables$blocksId[i], sep = ""),status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                                          column(width=12,
                                                 column(width = 5, align = "center",
                                                        selectizeInput(paste("leftSideCond1Block",ifVariables$blocksId[i], sep = ""), label = NULL,choices = c("-- select --",
                                                                                                                                                               colnames(temp)
                                                        ),width = "100%",options = list( placeholder = "-- select --"))
                                                 ),
                                                 column(width = 2, align = "center",
                                                        selectInput(paste("logicalOperatorsCond1Block",ifVariables$blocksId[i], sep = ""),label = NULL, choices = c("-- select --", "<", "<=", ">", ">=", "==", "!=", "&", "|", "in", "not in"), width = "100%")
                                                 ),
                                                 column(width = 5, align = "center",
                                                        selectizeInput(paste("rightSideCond1Block",ifVariables$blocksId[i], sep = ""), label = NULL,choices = c("-- select or enter --",
                                                                                                                                                                colnames(temp)
                                                        ),width = "100%",options = list(create = TRUE, placeholder = "-- select or enter --"))
                                                 )
                                          )
                                      )
                               ),
                               column(width = 12,offset = 0 , align = "left",
                                      uiOutput(paste("moreConditionsBlock",ifVariables$blocksId[i], sep = ""))
                               ),
                               column(width = 1,
                                      h3("){")
                               ),
                               column(width = 11,offset = 1,
                                      column(width=12,
                                             column(width = 11, align = "center",
                                                    selectizeInput(paste("resultOfBlock",ifVariables$blocksId[i], sep = ""), label = NULL,choices = c("-- select or enter --",
                                                                                                                                                      colnames(temp)
                                                    ),width = "100%",options = list(create = TRUE, placeholder = "-- select or enter --"))
                                             )
                                      )
                               ),
                               column(width = 1,
                                      h3("}")
                               ),
                               column(width = 4,offset = 8,
                                      actionButton(paste("deleteBlock",ifVariables$blocksId[i] ,sep=""), "Delete Block", icon = icon("trash-alt"), class = "deleteButton", style="height: 35px; margin-bottom:20px;", width="100%")
                               )
                           )
                           ,sep = "")
              }


            }else{
              ifVariables$lastIdBlocks <- ifVariables$lastIdBlocks + 1
              ifVariables$blocksId <- NULL
              s<-""
            }


            ifVariables$newBlock <- s


          })
        })

      }

      ifVariables$newBlock <- s

    }, error = function(err) {
      print("lzvmifv89zxvvfj3")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  },ignoreInit = T)








  ##########-----END----------##################
  #############################################################################################

  #####################    Chapter      #############################
  chaptersDataFrame <- reactiveValues(data=NULL, selectedChapter=NULL, operationType=NULL)

  output$chapterUI <- renderUI({
    box(id="chapterBox",
        width=12, solidHeader = FALSE, collapsible = FALSE,height = 650,
        column(width = 12,style="border-bottom: 1px solid lightgray; margin: 10px 0px 35px;",
               actionButton("addChapterButton", "Add Chapter", icon = icon("plus"), class = "uploadButton",
                            style="height: 50px; margin-bottom:20px; font-size: large; margin-left: 1%; margin-right: 1%;", width="98%")
        ),
        column(width = 12,style="overflow: auto; height: 500px;",
               uiOutput("chaptersBoxes")
        )
    )
  })

  output$chaptersBoxes <- renderText({
    tryCatch({
      chaptersDF <- chaptersDataFrame[["data"]]
      s <- ""
      cahpters <- unique(chaptersDF[,"chapter"])
      for (chp in cahpters) {
        varSur <- chaptersDF[chaptersDF$chapter == chp & chaptersDF$sheet == "survey", "var"]
        ind <- chaptersDF[chaptersDF$chapter == chp & chaptersDF$sheet == "indicator", "var"]



        if(length(varSur)==0){
          textSur <- span("there is no variables for this chapter")
        }else{
          textSur <- ""
          for(vs in varSur){
            textSur <- paste(textSur,
                             span(vs,class='tagVar')
                             ,sep=" ")
          }
          textSur<-HTML(textSur)
        }

        if(length(ind)==0){
          textInd <- span("there is no indicators for this chapter")
        }else{
          textInd <- ""
          for(inde in ind){
            textInd <- paste(textInd,
                             span(inde,class='tagInd')
                             ,sep=" ")
          }
          textInd<-HTML(textInd)
        }


        s <- paste(s,
                   box(id=paste("box",chp,sep = ""), title = chp, status="primary",
                       width=12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                       span("Variables: ", class = "titleTagVar"),div(
                         textSur , class = "divVar"
                       ),
                       span("Indicators: ", class = "titleTagInd"),div(
                         textInd , class = "divInd"
                       ),
                       column(width = 7,
                              actionButton(paste("editChapterButton", chp, sep = ""), "Edit Chapter", icon = icon("edit"), class = "toolButton", style="height: 50px; margin-bottom:20px;", width="100%")
                       ),
                       column(width = 5,
                              actionButton(paste("deleteChapterButton", chp, sep = ""), "Delete Chapter", icon = icon("trash-alt"), class = "deleteButton", style="height: 50px; margin-bottom:20px;", width="100%")
                       )
                   )
                   ,sep="")
      }


      lapply(1:length(cahpters), function(j) {
        observeEvent(input[[paste("editChapterButton", cahpters[j] ,sep = "")]] , {
          tryCatch({
            if(!is.null(chaptersDataFrame$selectedChapter)){
              if(chaptersDataFrame$selectedChapter == "return"){
                chaptersDataFrame$selectedChapter <- cahpters[j]
                chaptersDataFrame$operationType <- "Edit"
                return(FALSE)
              }
            }
            chaptersDataFrame$selectedChapter <- cahpters[j]
            chaptersDataFrame$operationType <- "Edit"
            showModal(showChapterTool("Edit",cahpters[j]))
          }, error = function(err) {
            print("n7886nhjh")
            shinyalert("Error",
                       err$message,
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
          })
        },ignoreInit = TRUE)
      })
      lapply(1:length(cahpters), function(j) {
        observeEvent(input[[paste("deleteChapterButton", cahpters[j] ,sep = "")]] , {
          tryCatch({
            progress <- shiny::Progress$new()
            progress$set(message = "Deleting chapter in progress...", value = 0)
            on.exit(progress$close())
            updateProgress <- function(value = NULL, detail = NULL) {
              if (is.null(value)) {
                value <- progress$getValue()
                value <- value + (progress$getMax() - value) / 5
              }
              progress$set(value = value, detail = detail)
            }
            updateProgress()
            form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
            survey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                    stringsAsFactors = FALSE)
            indicator <- as.data.frame(read_excel(form_tmp, sheet = "indicator"),
                                       stringsAsFactors = FALSE)
            updateProgress()
            survey[!is.na(survey$chapter) & survey$chapter==cahpters[j],"chapter"] <- NA
            indicator[!is.na(indicator$chapter) & indicator$chapter==cahpters[j],"chapter"] <- NA
            updateProgress()


            result <- kobo_edit_form(survey = survey, indicator = indicator)

            if(class(result) == "try-error"){
              shinyalert("Error",
                         result,
                         type = "error",
                         closeOnClickOutside = FALSE,
                         confirmButtonCol = "#ff4d4d",
                         animation = FALSE,
                         showConfirmButton = TRUE
              )
              return(FALSE)
            }

            chaptersDF <- chaptersDataFrame[["data"]]
            chaptersDF <- chaptersDF[chaptersDF$chapter != cahpters[j],]
            updateProgress()
            chaptersDF <- chaptersDF %>% arrange(chapter)
            updateProgress()
            chaptersDataFrame[["data"]] <- chaptersDF
          }, error = function(err) {
            print("bkgjlf89sd")
            shinyalert("Error",
                       err$message,
                       type = "error",
                       closeOnClickOutside = FALSE,
                       confirmButtonCol = "#ff4d4d",
                       animation = FALSE,
                       showConfirmButton = TRUE
            )
          })
        } ,ignoreInit = TRUE,once = TRUE)
      })

      s
    }, error = function(err) {
      print("lgfbkgf909f")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  observeEvent(input$addChapterButton,{
    tryCatch({
      chaptersDataFrame$selectedChapter <- ""
      chaptersDataFrame$operationType <- "Add"
      showModal(showChapterTool("Add",NULL))
    }, error = function(err) {
      print("fdoihyito90")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  showChapterTool <- function(type, chapterName) {
    tryCatch({
      form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
      survey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                              stringsAsFactors = FALSE)
      indicator <- as.data.frame(read_excel(form_tmp, sheet = "indicator"),
                                 stringsAsFactors = FALSE)

      survey <- survey[startsWith(tolower(survey$type), "integer") |
                         startsWith(tolower(survey$type), "decimal") |
                         startsWith(tolower(survey$type), "geopoint") |
                         startsWith(tolower(survey$type), "calculate") |
                         startsWith(tolower(survey$type), "text") |
                         startsWith(tolower(survey$type), "barcode") |
                         startsWith(tolower(survey$type), "select_multiple") |
                         startsWith(tolower(survey$type), "select_one") |
                         startsWith(tolower(survey$type), "date") |
                         startsWith(tolower(survey$type), "time") |
                         startsWith(tolower(survey$type), "datetime")
                       ,]


      varSer <- survey[is.na(survey$chapter),"name"]


      varInd <- indicator[is.na(indicator$chapter),"fullname"]

      if(length(c(varSer, varInd)) == 0){
        return(modalDialog(id="showChapterToolPopUp",
                           title = "No more Variables or Indicators",
                           column(offset = 1,width = 10,style="text-align: center;",
                                  icon("info-circle","big-info-circle")
                           ),
                           column(offset = 1,width = 11,
                                  h3("You can't add more chapters because there are no Variables or Indicators available to use.", style="color: gray; text-align: center; color")
                           ),
                           size = "l",
                           footer = tagList(
                             modalButton("Cancel", icon("sign-out-alt"))
                           )
        ))
      }
      else{
        return(modalDialog(id="showChapterToolPopUp",
                           title = ifelse(type=="Add","Add Chapter", paste("Edit",chapterName,"chapter")),
                           uiOutput("chapterToolBody"),
                           size = "l",
                           footer = tagList(
                             modalButton("Cancel", icon("sign-out-alt")),
                             actionButton("saveChapterButton", ifelse(type=="Add","Add the Chapter", "Edit the Chapter"), class = "toolButton", style="height: 35px;")
                           )
        ))
      }
    }, error = function(err) {
      print("lksjfk89")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  }

  output$chapterToolBody <- renderText({
    tryCatch({
      selChap <- chaptersDataFrame$selectedChapter
      chaptersDF <- chaptersDataFrame[["data"]]


      s <- paste("",
                 column(
                   width=12,
                   column(width = 6, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                          h4("Enter chapter's name:")
                   ),
                   column(width = 6, offset = 0,
                          textInput("chapterNameInput", label = NULL, value = selChap, width = "100%")
                   )
                 ),sep = "")

      form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
      survey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                              stringsAsFactors = FALSE)
      indicator <- as.data.frame(read_excel(form_tmp, sheet = "indicator"),
                                 stringsAsFactors = FALSE)

      survey <- survey[startsWith(tolower(survey$type), "integer") |
                         startsWith(tolower(survey$type), "decimal") |
                         startsWith(tolower(survey$type), "geopoint") |
                         startsWith(tolower(survey$type), "calculate") |
                         startsWith(tolower(survey$type), "text") |
                         startsWith(tolower(survey$type), "barcode") |
                         startsWith(tolower(survey$type), "select_multiple") |
                         startsWith(tolower(survey$type), "select_one") |
                         startsWith(tolower(survey$type), "date") |
                         startsWith(tolower(survey$type), "time") |
                         startsWith(tolower(survey$type), "datetime")
                       ,]


      varSer <- c(chaptersDF[chaptersDF$chapter == selChap & chaptersDF$sheet == "survey", "var"],
                  survey[is.na(survey$chapter),"name"]
      )

      varInd <- c(chaptersDF[chaptersDF$chapter == selChap & chaptersDF$sheet == "indicator", "var"],
                  indicator[is.na(indicator$chapter),"fullname"]
      )

      s <- paste(s,
                 column(
                   width=12,
                   column(width = 12, style = "margin: 15px 0px 10px; background-color: #1aab8a; color: white;",
                          h4("Select Variables of the chapter"),helpText("(To select multiple Variables, press on Ctrl + Click on the item)", style="color: black;")
                   ),
                   column(width = 12,style="margin-top: 10px;",
                          if(length(varSer)){
                            selectInput("selectedVar", label=NULL, choices = varSer, multiple = TRUE, selected = chaptersDF[chaptersDF$chapter == selChap & chaptersDF$sheet == "survey", "var"]
                                        , size=13, selectize = FALSE, width = "100%"
                            )
                          }else{
                            infoBox(
                              width = 12,strong("Info"),
                              h4(paste("There are no Variables available."), align = "center")
                              ,icon = icon("info-circle"),
                              color = "teal"
                            )
                          }

                   ),
                   column(width = 12, style = "margin: 15px 0px 10px; background-color: #1aab8a; color: white;",
                          h4("Select Indicators of the chapter"),helpText("(To select multiple Indicators, press on Ctrl + Click on the item)", style="color: black;")
                   ),
                   column(width = 12, style="margin-top: 10px;",
                          if(length(varInd)){
                            selectInput("selectedInd", label=NULL, choices = varInd, multiple = TRUE, selected = chaptersDF[chaptersDF$chapter == selChap & chaptersDF$sheet == "indicator", "var"]
                                        , size=13, selectize = FALSE, width = "100%"
                            )
                          }else{
                            infoBox(
                              width = 12,strong("Info"),
                              h4(paste("There are no Indicators available."), align = "center")
                              ,icon = icon("info-circle"),
                              color = "teal"
                            )
                          }
                   )
                 ),sep = "")

      s
    }, error = function(err) {
      print("lvcbjudfi8")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  observeEvent(input$saveChapterButton,{
    tryCatch({
      selChap <- chaptersDataFrame$selectedChapter
      chaptersDF <- chaptersDataFrame[["data"]]
      opeType <- chaptersDataFrame$operationType

      chaptersDataFrame$selectedChapter <- "return"
      if(is.null(input$chapterNameInput) || trimws(input$chapterNameInput) ==""){
        shinyalert("chapter's name is required",
                   "You need to enter the name of the chapter before saving",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      if(opeType == "Add"){
        if(input$chapterNameInput %in% chaptersDF$chapter){
          shinyalert("The name is reserved",
                     "You need to enter another name for this chapter because this name is reserved.",
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }

      }

      selectedVar <- input$selectedVar
      selectedInd <- input$selectedInd

      if(is.null(selectedVar) & is.null(selectedInd)){
        shinyalert("",
                   "You need to select at least one item from Variables or Indicators.",
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }


      progress <- shiny::Progress$new()
      progress$set(message = "Saving chapter in progress...", value = 0)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      updateProgress()

      form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
      survey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                              stringsAsFactors = FALSE)
      indicator <- as.data.frame(read_excel(form_tmp, sheet = "indicator"),
                                 stringsAsFactors = FALSE)
      updateProgress()
      if(nrow(survey)>0){
        survey[!is.na(survey$chapter) & survey$chapter==selChap,"chapter"] <- NA
      }
      if(nrow(indicator)>0){
        indicator[!is.na(indicator$chapter) & indicator$chapter==selChap,"chapter"] <- NA
      }
      updateProgress()
      if(nrow(survey)>0){
        survey[!is.na(survey$name) & survey$name %in% selectedVar,"chapter"] <- input$chapterNameInput
      }
      if(nrow(indicator)>0){
        indicator[!is.na(indicator$fullname) & indicator$fullname %in% selectedInd,"chapter"] <- input$chapterNameInput
      }
      updateProgress()


      result <- kobo_edit_form(survey = survey, indicator = indicator)

      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }

      updateProgress()
      chaptersDF <- chaptersDF[chaptersDF$chapter != selChap,]

      if(!is.null(selectedVar)){
        surveyDataFrame <- data.frame(
          chapter = input$chapterNameInput,
          sheet = "survey",
          var = selectedVar,
          stringsAsFactors=FALSE
        )
      }else{
        surveyDataFrame <- data.frame(
          chapter = character(),
          sheet = character(),
          var = character(),
          stringsAsFactors=FALSE
        )
      }
      if(!is.null(selectedInd)){
        indicatorDataFrame <- data.frame(
          chapter = input$chapterNameInput,
          sheet = "indicator",
          var = selectedInd,
          stringsAsFactors=FALSE
        )
      }else{
        indicatorDataFrame <- data.frame(
          chapter = character(),
          sheet = character(),
          var = character(),
          stringsAsFactors=FALSE
        )
      }
      updateProgress()
      chaptersDF <- rbind(chaptersDF, surveyDataFrame, indicatorDataFrame)
      chaptersDF <- chaptersDF %>% arrange(chapter)
      chaptersDataFrame[["data"]] <- chaptersDF
      updateProgress()

      Sys.sleep(4)

      removeModal()
    }, error = function(err) {
      print("ikfoi-dv-0")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })
  ###################################################################

  ###########################Saving Unit for Settings#######################
  observeEvent(input$sidebarMenuForAP,{
    tryCatch({
      las <- lastMenuItem$v
      lastMenuItem$v <-input$sidebarMenuForAP
      if(is.null(las)){
        return(FALSE)
      }
      if(las == "relabelingSurvey"){
        userSurvey <- sheets[["relabelingSurvey"]]
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        mainSurvey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                    stringsAsFactors = FALSE)

        origin <- mainSurvey[rownames(mainSurvey) %in% rownames(userSurvey),colnames(userSurvey)]
        newSur <- userSurvey
        origin <- as.data.frame(sapply(origin,as.character),stringsAsFactors = F)
        newSur <- as.data.frame(sapply(newSur,as.character),stringsAsFactors = F)

        if(identical(origin,newSur)){
          return(FALSE)
        }

        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        progress <- shiny::Progress$new()
        progress$set(message = "Saving sheet in progress...", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
        updateProgress()

        columnsMainNotUser <- colnames(mainSurvey)[!colnames(mainSurvey) %in% colnames(userSurvey)]
        userSurvey[,columnsMainNotUser]<-NA
        userSurvey<-userSurvey[colnames(mainSurvey)]

        updateProgress()


        newSurvey <- rbind(mainSurvey[!rownames(mainSurvey) %in% rownames(userSurvey),],
                           userSurvey
        )
        newSurvey <- newSurvey[ order(as.numeric(row.names(newSurvey))), ]
        updateProgress()

        for (field in columnsMainNotUser) {
          newSurvey[,field] <- mainSurvey[,field]
        }

        updateProgress()


        result <- kobo_edit_form(survey = newSurvey)

        if(class(result) == "try-error"){
          shinyalert("Error",
                     result,
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }

        updateProgress()
      }
      else if(las == "relabelingChoices"){
        userRelabelingChoices <- sheets[["relabelingChoices"]]
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        mainChoices <- as.data.frame(read_excel(form_tmp, sheet = "choices"),
                                     stringsAsFactors = FALSE)

        if(identical(as.character(mainChoices["labelReport"]),as.character(userRelabelingChoices["labelReport"]))){
          return(FALSE)
        }
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        progress <- shiny::Progress$new()
        progress$set(message = "Saving sheet in progress...", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
        updateProgress()

        mainChoices["labelReport"] <- userRelabelingChoices["labelReport"]



        result <- kobo_edit_form(choices = mainChoices)

        if(class(result) == "try-error"){
          shinyalert("Error",
                     result,
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }

        updateProgress()
      }
      else if(las == "selectOneType"){
        userSurvey <- sheets[["selectOneType"]]
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        mainSurvey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                    stringsAsFactors = FALSE)

        origin <- mainSurvey[rownames(mainSurvey) %in% rownames(userSurvey),colnames(userSurvey)]
        newSur <- userSurvey
        origin <- as.data.frame(sapply(origin,as.character),stringsAsFactors = F)
        newSur <- as.data.frame(sapply(newSur,as.character),stringsAsFactors = F)

        if(identical(origin,newSur)){
          return(FALSE)
        }
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        progress <- shiny::Progress$new()
        progress$set(message = "Saving sheet in progress...", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
        updateProgress()



        columnsMainNotUser <- colnames(mainSurvey)[!colnames(mainSurvey) %in% colnames(userSurvey)]
        userSurvey[,columnsMainNotUser]<-NA
        userSurvey<-userSurvey[colnames(mainSurvey)]

        updateProgress()


        newSurvey <- rbind(mainSurvey[!rownames(mainSurvey) %in% rownames(userSurvey),],
                           userSurvey
        )
        newSurvey <- newSurvey[ order(as.numeric(row.names(newSurvey))), ]
        updateProgress()

        for (field in columnsMainNotUser) {
          newSurvey[,field] <- mainSurvey[,field]
        }

        updateProgress()


        result <-  kobo_edit_form(survey = newSurvey)

        if(class(result) == "try-error"){
          shinyalert("Error",
                     result,
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }

      }
      else if(las == "orderOrdinalVariables"){
        userChoices <- c()
        if (!is.null(input$orderOrdinalVariablesTable)) {
          userChoices <- hot_to_r(input$orderOrdinalVariablesTable)#as.data.frame(do.call(rbind, input$indicatorsSheetUI$data))#
        }

        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        mainChoices <- as.data.frame(read_excel(form_tmp, sheet = "choices"),
                                     stringsAsFactors = FALSE)

        origin <- mainChoices[rownames(mainChoices) %in% rownames(userChoices),colnames(userChoices)]
        newCho <- userChoices
        origin <- as.data.frame(sapply(origin,as.character),stringsAsFactors = F)
        newCho <- as.data.frame(sapply(newCho,as.character),stringsAsFactors = F)

        if(identical(origin,newCho)){
          return(FALSE)
        }
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        progress <- shiny::Progress$new()
        progress$set(message = "Saving sheet in progress...", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
        updateProgress()



        columnsMainNotUser <- colnames(mainChoices)[!colnames(mainChoices) %in% colnames(userChoices)]
        userChoices[,columnsMainNotUser]<-NA
        userChoices<-userChoices[colnames(mainChoices)]

        updateProgress()


        newChoices <- rbind(mainChoices[!rownames(mainChoices) %in% rownames(userChoices),],
                            userChoices
        )
        newChoices <- newChoices[ order(as.numeric(row.names(newChoices))), ]
        updateProgress()

        for (field in columnsMainNotUser) {
          newChoices[,field] <- mainChoices[,field]
        }

        updateProgress()


        result <-  kobo_edit_form(choices = newChoices)

        if(class(result) == "try-error"){
          shinyalert("Error",
                     result,
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
      }
      else if(las == "selectMultipleType"){
        userSurvey <- sheets[["selectMultipleType"]]
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        mainSurvey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                    stringsAsFactors = FALSE)

        origin <- mainSurvey[rownames(mainSurvey) %in% rownames(userSurvey),colnames(userSurvey)]
        newSur <- userSurvey
        origin <- as.data.frame(sapply(origin,as.character),stringsAsFactors = F)
        newSur <- as.data.frame(sapply(newSur,as.character),stringsAsFactors = F)

        if(identical(origin,newSur)){
          return(FALSE)
        }
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        progress <- shiny::Progress$new()
        progress$set(message = "Saving sheet in progress...", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
        updateProgress()



        columnsMainNotUser <- colnames(mainSurvey)[!colnames(mainSurvey) %in% colnames(userSurvey)]
        userSurvey[,columnsMainNotUser]<-NA
        userSurvey<-userSurvey[colnames(mainSurvey)]

        updateProgress()


        newSurvey <- rbind(mainSurvey[!rownames(mainSurvey) %in% rownames(userSurvey),],
                           userSurvey
        )
        newSurvey <- newSurvey[ order(as.numeric(row.names(newSurvey))), ]
        updateProgress()

        for (field in columnsMainNotUser) {
          newSurvey[,field] <- mainSurvey[,field]
        }

        updateProgress()


        result <- kobo_edit_form(survey = newSurvey)

        if(class(result) == "try-error"){
          shinyalert("Error",
                     result,
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
      }
      else if(las == "numericType"){
        userSurvey <- sheets[["numericType"]]
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        mainSurvey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                    stringsAsFactors = FALSE)

        origin <- mainSurvey[rownames(mainSurvey) %in% rownames(userSurvey),colnames(userSurvey)]
        newSur <- userSurvey
        origin <- as.data.frame(sapply(origin,as.character),stringsAsFactors = F)
        newSur <- as.data.frame(sapply(newSur,as.character),stringsAsFactors = F)

        if(identical(origin,newSur)){
          return(FALSE)
        }
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        progress <- shiny::Progress$new()
        progress$set(message = "Saving sheet in progress...", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
        updateProgress()



        columnsMainNotUser <- colnames(mainSurvey)[!colnames(mainSurvey) %in% colnames(userSurvey)]
        userSurvey[,columnsMainNotUser]<-NA
        userSurvey<-userSurvey[colnames(mainSurvey)]

        updateProgress()


        newSurvey <- rbind(mainSurvey[!rownames(mainSurvey) %in% rownames(userSurvey),],
                           userSurvey
        )
        newSurvey <- newSurvey[ order(as.numeric(row.names(newSurvey))), ]
        updateProgress()

        for (field in columnsMainNotUser) {
          newSurvey[,field] <- mainSurvey[,field]
        }

        updateProgress()

        result <- kobo_edit_form(survey = newSurvey)

        if(class(result) == "try-error"){
          shinyalert("Error",
                     result,
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }


      }
      else if(las == "dateType"){
        userSurvey <- sheets[["dateType"]]
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        mainSurvey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                    stringsAsFactors = FALSE)

        origin <- mainSurvey[rownames(mainSurvey) %in% rownames(userSurvey),colnames(userSurvey)]
        newSur <- userSurvey
        origin <- as.data.frame(sapply(origin,as.character),stringsAsFactors = F)
        newSur <- as.data.frame(sapply(newSur,as.character),stringsAsFactors = F)

        if(identical(origin,newSur)){
          return(FALSE)
        }
        projectConfigurationInfo$log[["isAnalysisPlanCompleted"]] <- FALSE
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- FALSE
        progress <- shiny::Progress$new()
        progress$set(message = "Saving sheet in progress...", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
        updateProgress()



        columnsMainNotUser <- colnames(mainSurvey)[!colnames(mainSurvey) %in% colnames(userSurvey)]
        userSurvey[,columnsMainNotUser]<-NA
        userSurvey<-userSurvey[colnames(mainSurvey)]

        updateProgress()


        newSurvey <- rbind(mainSurvey[!rownames(mainSurvey) %in% rownames(userSurvey),],
                           userSurvey
        )
        newSurvey <- newSurvey[ order(as.numeric(row.names(newSurvey))), ]
        updateProgress()

        for (field in columnsMainNotUser) {
          newSurvey[,field] <- mainSurvey[,field]
        }

        updateProgress()


        result <- kobo_edit_form(survey = newSurvey)

        if(class(result) == "try-error"){
          shinyalert("Error",
                     result,
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
      }

    }, error = function(err) {
      print("ofgbif89r")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })
  #############################################################################

  observe({
    tryCatch({
      if(is.null(projectConfigurationInfo$log[["isRecordSettingsCompleted"]])){
        return(NULL)
      }
      if(!projectConfigurationInfo$log[["isRecordSettingsCompleted"]]){
        return(NULL)
      }
      relabelingSurvey <- c()
      relabelingChoices <- c()
      selectOneType <- c()
      selectMultipleType <- c()
      orderOrdinalVariables <- c()
      numericType <- c()
      dateType <- c()
      #################################relabeling Survey#######################################
      if (!is.null(input$relabelingSurveyTable)) {
        relabelingSurvey = hot_to_r(input$relabelingSurveyTable)
      }
      else{
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        relabelingSurvey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                          stringsAsFactors = FALSE)
        names(relabelingSurvey)[tolower(names(relabelingSurvey)) == "label::english"] <- "label"
        names(relabelingSurvey)[tolower(names(relabelingSurvey)) == "hint::english"] <- "hint"
        reqNames <- c("type", "name")
        if("label" %in% colnames(relabelingSurvey)){
          reqNames <- c(reqNames, "label", "labelReport")
          relabelingSurvey[,"label"] <- as.character(relabelingSurvey[,"label"])
        }else{
          reqNames <- c(reqNames, "labelReport")
        }

        if("hint" %in% colnames(relabelingSurvey)){
          reqNames <- c(reqNames, "hint", "hintReport")
          relabelingSurvey[,"hint"] <- as.character(relabelingSurvey[,"hint"])
        }else{
          reqNames <- c(reqNames, "hintReport")
        }

        if ("labelReport" %in% colnames(relabelingSurvey)) {
          if(mean(is.na(relabelingSurvey[,"labelReport"]))==1){
            if("label" %in% colnames(relabelingSurvey)){
              relabelingSurvey["labelReport"] = substr(relabelingSurvey[,"label"],1,80)
            }else{
              relabelingSurvey["labelReport"] = ""
            }
          }else{
            relabelingSurvey["labelReport"] = substr(relabelingSurvey[,"labelReport"],1,80)
          }
        }

        if (!"labelReport" %in% colnames(relabelingSurvey)) {
          if("label" %in% colnames(relabelingSurvey)){
            relabelingSurvey["labelReport"] = substr(relabelingSurvey[,"label"],1,80)
          }else{
            relabelingSurvey["labelReport"] = ""
          }
        }

        if ("hintReport" %in% colnames(relabelingSurvey)) {
          if(mean(is.na(relabelingSurvey[,"hintReport"]))==1){
            if("hint" %in% colnames(relabelingSurvey)){
              relabelingSurvey["hintReport"] = substr(relabelingSurvey[,"hint"],1,80)
            }else{
              relabelingSurvey["hintReport"] = ""
            }
          }else{
            relabelingSurvey["hintReport"] = substr(relabelingSurvey[,"hintReport"],1,80)
          }
        }

        if (!"hintReport" %in% colnames(relabelingSurvey)) {
          if("hint" %in% colnames(relabelingSurvey)){
            relabelingSurvey["hintReport"] = substr(relabelingSurvey[,"hint"],1,80)
          }else{
            relabelingSurvey["hintReport"] = ""
          }
        }
        relabelingSurvey[,"labelReport"] <- as.character(relabelingSurvey[,"labelReport"])
        relabelingSurvey[,"hintReport"] <- as.character(relabelingSurvey[,"hintReport"])
        relabelingSurvey <- relabelingSurvey[reqNames]
      }

      relabelingSurvey <- relabelingSurvey[startsWith(tolower(relabelingSurvey$type), "select_one") |
                                              startsWith(tolower(relabelingSurvey$type), "select_multiple") |
                                               startsWith(tolower(relabelingSurvey$type), "integer") |
                                               startsWith(tolower(relabelingSurvey$type), "decimal") |
                                               startsWith(tolower(relabelingSurvey$type), "geopoint") |
                                               startsWith(tolower(relabelingSurvey$type), "calculate") |
                                               startsWith(tolower(relabelingSurvey$type), "date")
                                             ,]


      relabelingSurvey <- relabelingSurvey[ order(as.numeric(row.names(relabelingSurvey))), ]
      if(nrow(relabelingSurvey)==0){
        relabelingSurvey[nrow(relabelingSurvey)+1,] <- NA
      }
      sheets[["relabelingSurvey"]] <- relabelingSurvey

      ################################################################################


      #################################relabeling Choices#######################################
      if (!is.null(input$relabelingChoicesTable)) {
        relabelingChoices = hot_to_r(input$relabelingChoicesTable)
      }
      else{
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        relabelingChoices <- as.data.frame(read_excel(form_tmp, sheet = "choices"),
                                           stringsAsFactors = FALSE)
        names(relabelingChoices)[tolower(names(relabelingChoices)) == "label::english"] <- "label"
        reqNames <- c("list_name", "name")
        if("label" %in% colnames(relabelingChoices)){
          reqNames <- c(reqNames, "label", "labelReport")
          relabelingChoices[,"label"] <- as.character(relabelingChoices[,"label"])
        }else{
          reqNames <- c(reqNames, "labelReport")
        }

        if ("labelReport" %in% colnames(relabelingChoices)) {
          if(mean(is.na(relabelingChoices[,"labelReport"]))==1){
            if("label" %in% colnames(relabelingChoices)){
              relabelingChoices["labelReport"] = substr(relabelingChoices[,"label"],1,80)
            }else{
              relabelingChoices["labelReport"] = ""
            }
          }else{
            relabelingChoices["labelReport"] = substr(relabelingChoices[,"labelReport"],1,80)
          }
        }


        if (!"labelReport" %in% colnames(relabelingChoices)) {
          if("label" %in% colnames(relabelingChoices)){
            relabelingChoices["labelReport"] = substr(relabelingChoices[,"label"],1,80)
          }else{
            relabelingChoices["labelReport"] = ""
          }
        }
        relabelingChoices[,"labelReport"] <- as.character(relabelingChoices[,"labelReport"])
        relabelingChoices <- relabelingChoices[reqNames]
      }
      relabelingChoices <- relabelingChoices[ order(as.numeric(row.names(relabelingChoices))), ]
      if(nrow(relabelingChoices)==0){
        relabelingChoices[nrow(relabelingChoices)+1,] <- NA
      }
      sheets[["relabelingChoices"]] <- relabelingChoices

      ################################################################################

      #################################selectOne Type#######################################
      if (!is.null(input$selectOneTypeTable)) {
        selectOneType = hot_to_r(input$selectOneTypeTable)
      }
      else {
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        selectOneType <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                       stringsAsFactors = FALSE)
        reqNames <- c("type",   "name" ,  "label", "variable",
                      "disaggregation", #"chapter",
                      "structuralequation.risk","structuralequation.coping","structuralequation.resilience","anonymise","correlate","clean","cluster","predict","mappoint","mappoly"

        )
        if(sum(sapply(reqNames, function(x){x %in% colnames(selectOneType)})) != length(reqNames)){
          print("gfblkmgk")
          shinyalert("Error",
                     paste("You need to make sure that all required fields are existing in survey sheet\n",
                           reqNames
                     ),
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        selectOneType <- selectOneType[reqNames]
        selectOneType <- selectOneType[startsWith(tolower(selectOneType$type), "select_one"),]


        #selectOneType$chapter <- as.character(selectOneType$chapter)
        selectOneType$variable <- as.character(ifelse(is.na(selectOneType$variable),"factor", selectOneType$variable ))

        selectOneType$disaggregation <- as.logical(selectOneType$disaggregation)
        selectOneType$structuralequation.risk <- as.logical(selectOneType$structuralequation.risk)
        selectOneType$structuralequation.coping <- as.logical(selectOneType$structuralequation.coping)
        selectOneType$structuralequation.resilience <- as.logical(selectOneType$structuralequation.resilience)
        selectOneType$anonymise <- as.character(selectOneType$anonymise)
        selectOneType$correlate <- as.logical(selectOneType$correlate)
        selectOneType$clean <- as.logical(selectOneType$clean)
        selectOneType$cluster <- as.logical(selectOneType$cluster)
        selectOneType$predict <- as.logical(selectOneType$predict)
        selectOneType$mappoint <- as.logical(selectOneType$mappoint)
        selectOneType$mappoly <- as.logical(selectOneType$mappoly)
      }
      selectOneType <- selectOneType[ order(as.numeric(row.names(selectOneType))), ]
      sheets[["selectOneType"]] <- selectOneType

      ################################################################################


      ###############################Order Ordinal Variables##########################

      form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
      survey <- sheets[["selectOneType"]]
      reqNames <- c("type",  "variable")


      if(sum(sapply(reqNames, function(x){x %in% colnames(survey)})) != length(reqNames)){
        print("kdsfj9854q43as")
        shinyalert("Error",
                   paste("You need to make sure that all required fields are existing in survey sheet\n",
                         reqNames
                   ),
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      survey <- survey[reqNames]

      survey <- survey[!is.na(survey[,"variable"]),]
      survey <- survey[survey[,"variable"]=="ordinal",]
      survey <- survey[startsWith(tolower(survey[,"type"]), "select_one"),]

      varOfOrder <- sapply(survey[,"type"], function(x) {
        strsplit(x," ")[[1]][2]
      }, simplify = TRUE, USE.NAMES = FALSE)

      choices <- as.data.frame(read_excel(form_tmp, sheet = "choices"),
                               stringsAsFactors = FALSE)
      reqNames <- c("list_name",  "name", "label", "order")
      if(sum(sapply(reqNames, function(x){x %in% colnames(choices)})) != length(reqNames)){
        print("jhaa8d77")
        shinyalert("Error",
                   paste("You need to make sure that all required fields are existing in choices sheet\n",
                         reqNames
                   ),
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }
      choices <- choices[reqNames]
      choices <- choices[choices[,"list_name"] %in% varOfOrder,]


      choices$list_name <- as.character(choices$list_name)
      choices$name <- as.character(choices$name)
      choices$label <- as.character(choices$label)
      choices$order <- as.integer(choices$order)
      orderOrdinalVariables = choices

      orderOrdinalVariables <- orderOrdinalVariables[ order(as.numeric(row.names(orderOrdinalVariables))), ]
      sheets[["orderOrdinalVariables"]] <- orderOrdinalVariables

      ################################################################################


      #################################Select_multiple Type#######################################
      if (!is.null(input$selectMultipleTypeTable)) {
        selectMultipleType = hot_to_r(input$selectMultipleTypeTable)
      }
      else {
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        selectMultipleType <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                            stringsAsFactors = FALSE)
        reqNames <- c("type",   "name" ,  "label", "variable",
                      "disaggregation", #"chapter",
                      "structuralequation.risk","structuralequation.coping","structuralequation.resilience","anonymise","correlate","clean","cluster","predict","mappoint","mappoly"

        )
        if(sum(sapply(reqNames, function(x){x %in% colnames(selectMultipleType)})) != length(reqNames)){
          print("iu3280rfiogj9")
          shinyalert("Error",
                     paste("You need to make sure that all required fields are existing in survey sheet\n",
                           reqNames
                     ),
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        selectMultipleType <- selectMultipleType[reqNames]
        selectMultipleType <- selectMultipleType[startsWith(tolower(selectMultipleType$type), "select_multiple"),]

        selectMultipleType[startsWith(selectMultipleType$type,"select_multiple") ,"variable"] = "factor"

        #selectMultipleType$chapter <- as.character(selectMultipleType$chapter)
        selectMultipleType$variable <- as.character(selectMultipleType$variable)

        selectMultipleType$disaggregation <- as.logical(selectMultipleType$disaggregation)
        selectMultipleType$structuralequation.risk <- as.logical(selectMultipleType$structuralequation.risk)
        selectMultipleType$structuralequation.coping <- as.logical(selectMultipleType$structuralequation.coping)
        selectMultipleType$structuralequation.resilience <- as.logical(selectMultipleType$structuralequation.resilience)
        selectMultipleType$anonymise <- as.character(selectMultipleType$anonymise)
        selectMultipleType$correlate <- as.logical(selectMultipleType$correlate)
        selectMultipleType$clean <- as.logical(selectMultipleType$clean)
        selectMultipleType$cluster <- as.logical(selectMultipleType$cluster)
        selectMultipleType$predict <- as.logical(selectMultipleType$predict)
        selectMultipleType$mappoint <- as.logical(selectMultipleType$mappoint)
        selectMultipleType$mappoly <- as.logical(selectMultipleType$mappoly)
      }
      selectMultipleType <- selectMultipleType[ order(as.numeric(row.names(selectMultipleType))), ]
      sheets[["selectMultipleType"]] <- selectMultipleType

      ################################################################################


      #################################Numeric type#######################################
      if (!is.null(input$numericTypeTable)) {
        numericType = hot_to_r(input$numericTypeTable)
      }
      else {
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        numericType <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                     stringsAsFactors = FALSE)
        reqNames <- c("type",   "name" ,  "label", "variable",
                      "disaggregation", #"chapter",
                      "structuralequation.risk","structuralequation.coping","structuralequation.resilience","anonymise","correlate","clean","cluster","predict","mappoint","mappoly"

        )
        if(sum(sapply(reqNames, function(x){x %in% colnames(numericType)})) != length(reqNames)){
          print("009ffdvosd")
          shinyalert("Error",
                     paste("You need to make sure that all required fields are existing in survey sheet\n",
                           reqNames
                     ),
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        numericType <- numericType[reqNames]
        numericType <- numericType[startsWith(tolower(numericType$type), "integer") |
                                     startsWith(tolower(numericType$type), "decimal") |
                                     startsWith(tolower(numericType$type), "geopoint") |
                                     startsWith(tolower(numericType$type), "calculate")
                                   ,]

        numericType[tolower(numericType$type) %in% c("integer") ,"variable"] = "integer"
        numericType[tolower(numericType$type) %in% c("decimal","geopoint", "calculate") ,"variable"] = "numeric"


        #numericType$chapter <- as.character(numericType$chapter)
        numericType$variable <- as.character(numericType$variable)

        numericType$disaggregation <- as.logical(numericType$disaggregation)
        numericType$structuralequation.risk <- as.logical(numericType$structuralequation.risk)
        numericType$structuralequation.coping <- as.logical(numericType$structuralequation.coping)
        numericType$structuralequation.resilience <- as.logical(numericType$structuralequation.resilience)
        numericType$anonymise <- as.character(numericType$anonymise)
        numericType$correlate <- as.logical(numericType$correlate)
        numericType$clean <- as.logical(numericType$clean)
        numericType$cluster <- as.logical(numericType$cluster)
        numericType$predict <- as.logical(numericType$predict)
        numericType$mappoint <- as.logical(numericType$mappoint)
        numericType$mappoly <- as.logical(numericType$mappoly)
      }
      numericType <- numericType[ order(as.numeric(row.names(numericType))), ]
      sheets[["numericType"]] <- numericType

      ################################################################################


      #################################Date type#######################################
      if (!is.null(input$dateTypeTable)) {
        dateType = hot_to_r(input$dateTypeTable)
      }
      else {
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        dateType <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                  stringsAsFactors = FALSE)
        reqNames <- c("type",   "name" ,  "label", "variable",
                      "disaggregation", #"chapter",
                      "structuralequation.risk","structuralequation.coping","structuralequation.resilience","anonymise","correlate","clean","cluster","predict","mappoint","mappoly"

        )
        if(sum(sapply(reqNames, function(x){x %in% colnames(dateType)})) != length(reqNames)){
          print("198u3rjfdk")
          shinyalert("Error",
                     paste("You need to make sure that all required fields are existing in survey sheet\n",
                           reqNames
                     ),
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        dateType <- dateType[reqNames]
        dateType <- dateType[startsWith(tolower(dateType$type), "date") |
                               startsWith(tolower(dateType$type), "time") |
                               startsWith(tolower(dateType$type), "datetime")
                             ,]

        if(nrow(dateType)!=0){
          dateType[tolower(dateType$type) %in% c("date") ,"variable"] = "date"
          dateType[tolower(dateType$type) %in% c("time") ,"variable"] = "time"
          dateType[tolower(dateType$type) %in% c("datetime") ,"variable"] = "datetime"

          #dateType$chapter <- as.character(dateType$chapter)
          dateType$variable <- as.character(dateType$variable)

          dateType$disaggregation <- as.logical(dateType$disaggregation)
          dateType$structuralequation.risk <- as.logical(dateType$structuralequation.risk)
          dateType$structuralequation.coping <- as.logical(dateType$structuralequation.coping)
          dateType$structuralequation.resilience <- as.logical(dateType$structuralequation.resilience)
          dateType$anonymise <- as.character(dateType$anonymise)
          dateType$correlate <- as.logical(dateType$correlate)
          dateType$clean <- as.logical(dateType$clean)
          dateType$cluster <- as.logical(dateType$cluster)
          dateType$predict <- as.logical(dateType$predict)
          dateType$mappoint <- as.logical(dateType$mappoint)
          dateType$mappoly <- as.logical(dateType$mappoly)
        }
      }
      dateType <- dateType[ order(as.numeric(row.names(dateType))), ]
      sheets[["dateType"]] <- dateType

      ################################################################################

      ############################# Indicators Sheet #################################
      form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
      indicator <- as.data.frame(read_excel(form_tmp, sheet = "indicator"),
                                 stringsAsFactors = FALSE)

      indicator <- indicator[!is.na(indicator$fullname),]
      indicator <- indicator %>% arrange(fullname)

      indicatorsInfo[["data"]] <- indicator
      ################################################################################

      ################################# Chapter#######################################
      form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
      survey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                              stringsAsFactors = FALSE)
      #indicator <- as.data.frame(read_excel(form_tmp, sheet = "indicator"),
      #                          stringsAsFactors = FALSE)


      survey <- survey[!is.na(survey$chapter),]
      indicator <- indicator[!is.na(indicator$chapter),]

      if(nrow(survey)!=0){
        surveyDataFrame <- data.frame(
          chapter = survey$chapter,
          sheet = "survey",
          var = survey$name,
          stringsAsFactors=FALSE
        )
      }else{
        surveyDataFrame <- data.frame(
          chapter = character(),
          sheet = character(),
          var = character(),
          stringsAsFactors=FALSE
        )
      }

      if(nrow(indicator)!=0){
        indicatorDataFrame <- data.frame(
          chapter = indicator$chapter,
          sheet = "indicator",
          var = indicator$fullname,
          stringsAsFactors=FALSE
        )
      }else{
        indicatorDataFrame <- data.frame(
          chapter = character(),
          sheet = character(),
          var = character(),
          stringsAsFactors=FALSE
        )
      }

      chaptersDF <- rbind(surveyDataFrame, indicatorDataFrame)
      chaptersDF <- chaptersDF %>% arrange(chapter)

      chaptersDataFrame[["data"]] <- chaptersDF
      ################################################################################


    }, error = function(err) {
      print("9")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  ####################################### Data Processing page ############################################
  output$dataProcessing <- renderUI({
    if(!projectConfigurationInfo$log[["isAnalysisPlanCompleted"]]){
      infoBox(
        width = 12,strong("Warning"),h4("You cannot proceed without checking the Analysis Plan section by 'Check the Plan' button in the previous step", align = "center")
        ,icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    }else{
      box(id="loadDataButtonBox",
          width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
          column(width = 2, align="left",
                 icon("arrow-right", "fa-24x")
          ),
          column(width = 2, align="left",
                 icon("arrow-right", "fa-24x")
          ),
          column(width = 3, align="center",
                 actionButton("loadDataButton", "Load Data", class = "processButtonLarge")
          ),
          column(width = 2, align="right",
                 icon("arrow-right", "fa-24x")
          ),
          column(width = 2, align="right",
                 icon("arrow-right", "fa-24x")
          )
      )
    }
  })

  observeEvent(input$loadDataButton,{
    tryCatch({
      result <- kobo_load_data(app = "shiny")
      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }else{
        projectConfigurationInfo$log[["isDataProcessingCompleted"]] <- TRUE
        shinyalert("Wooooow",
                   "You can start Reports Generation",
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
      }

    }, error = function(err) {
      print("fdhfhdflolkjllk7")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })


  ####################################### Reports Generation page ############################################
  output$reportsGeneration <- renderUI({
    if(!projectConfigurationInfo$log[["isDataProcessingCompleted"]]){
      infoBox(
        width = 12,strong("Warning"),h4("You cannot proceed without completing Data Processing section", align = "center")
        ,icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    }else{
      column(width = 12, align="center",
             column(width = 12, align="center",style="margin-bottom:15px;",
                    column(width = 8, align="center",
                           actionButton("crunchingReportButton", "Crunching Report", class = "toolButton006495", style="height: 250px; font-size: xx-large; width: 100%;")
                    ),
                    column(width = 4, align="center",
                           actionButton("anonymisationReportButton", "Anonymisation Report", class = "toolButtonE0A025", style="height: 250px; font-size: xx-large; width: 100%;")
                    )
             ),
             column(width = 12, align="center",style="margin-bottom:15px;",
                    column(width = 6, align="center",
                           actionButton("monitoringReportButton", "Monitoring Report", class = "toolButtonF4D00C", style="height: 250px; font-size: xx-large; width: 100%;")
                    ),
                    column(width = 6, align="center",
                           actionButton("clusterReportButton", "Cluster Report", class = "toolButton004C70", style="height: 250px; font-size: xx-large; width: 100%;")
                    )
             ),
             column(width = 12, align="center",style="margin-bottom:15px;",
                    column(width = 5, align="center",
                           actionButton("predictionReportButton", "Prediction Report", class = "toolButton0093D1", style="height: 250px; font-size: xx-large; width: 100%;")
                    ),
                    column(width = 7, align="center",
                           actionButton("scoreReportButton", "Scoring Report", class = "toolButtonF2635F", style="height: 250px; font-size: xx-large; width: 100%;")
                    )
             )
      )
    }
  })

  ##################crunching Report################
  observeEvent(input$crunchingReportButton,{
    tryCatch({
      result <- kobo_crunching_report(app = "shiny")
      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }else{
        shinyalert("Wooooow",
                   "Done!! Reports are in the folder OUT - Review the report- Adjust your configuration files and you will be very soon ready to start the qualitative analysis and the analysis workshops...",
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        showModal(showCrunchingReportLinks())
      }

    }, error = function(err) {
      print("jhjhgfjhjfhg")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  showCrunchingReportLinks <- function() {
    tryCatch({
      return(modalDialog(id="showCrunchingReportPopUp",
                         title = "Crunching Reports",
                         uiOutput("crunchingReportBody"),
                         size = "l",
                         footer = tagList(
                           modalButton("Exit", icon("sign-out-alt"))
                         )
      )
      )

    }, error = function(err) {
      print("dhfgdhfgdhgfgd")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  }

  output$crunchingReportBody <- renderText({
    tryCatch({
      s <- ""
      mainPath <- paste(kobo_getMainDirectory(),"/out/crunching_reports/", sep = "")
      filesNames <- list.files(mainPath)
      filesNames <- sort(filesNames)
      for (fn in filesNames) {
        s <- paste(s,
                   box(width = 6, title = fn, solidHeader = FALSE, status = "primary", collapsed = F, collapsible = F,
                       downloadLink(paste("download",fn,sep = ""), paste("Download",fn))
                   )
                   ,sep="")

      }
      lapply(1:length(filesNames), function(m) {
        output[[paste("download",filesNames[m],sep = "")]] <- downloadHandler(
          filename = filesNames[m],
          content = function(file) {
            file.copy(paste(kobo_getMainDirectory(),"/out/crunching_reports/",filesNames[m], sep = ""), file)
          }
        )
      })
      return(s)
    }, error = function(err) {
      print("gdhggfmlkfhoghkgf")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })
  ##################cluster Report################
  observeEvent(input$clusterReportButton,{
    tryCatch({
      MainDataFrame_edited <- read.csv(paste(mainDir(), "data", "/MainDataFrame_edited.csv", sep = "/", collapse = "/"), stringsAsFactors = F)
      result <- kobo_cluster_report(MainDataFrame_edited, app = "shiny")
      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }else{
        shinyalert("Wooooow",
                   "Done!! Reports are in the folder OUT - Review the report- furter review your clustering assumptions and regenerate as needed...",
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        showModal(showClusterReportLinks())
      }

    }, error = function(err) {
      print("htghfghfghythdh")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  showClusterReportLinks <- function() {
    tryCatch({
      return(modalDialog(id="showClusterReportPopUp",
                         title = "Cluster Reports",
                         uiOutput("clusterReportBody"),
                         size = "l",
                         footer = tagList(
                           modalButton("Exit", icon("sign-out-alt"))
                         )
      )
      )

    }, error = function(err) {
      print("dhrthdyhnfrhntkjk")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  }

  output$clusterReportBody <- renderText({
    tryCatch({
      s <- ""
      mainPath <- paste(kobo_getMainDirectory(),"/out/cluster_reports/", sep = "")
      filesNames <- list.files(mainPath)
      filesNames <- sort(filesNames)
      for (fn in filesNames) {
        s <- paste(s,
                   box(width = 6, title = fn, solidHeader = FALSE, status = "primary", collapsed = F, collapsible = F,
                       downloadLink(paste("download",fn,sep = ""), paste("Download",fn))
                   )
                   ,sep="")

      }
      lapply(1:length(filesNames), function(m) {
        output[[paste("download",filesNames[m],sep = "")]] <- downloadHandler(
          filename = filesNames[m],
          content = function(file) {
            file.copy(paste(kobo_getMainDirectory(),"/out/cluster_reports/",filesNames[m], sep = ""), file)
          }
        )
      })
      return(s)
    }, error = function(err) {
      print("fdjkghdfijldfjkgfos")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  ##################anonymisation Report################
  observeEvent(input$anonymisationReportButton,{
    tryCatch({
      MainDataFrame_edited <- read.csv(paste(mainDir(), "data", "/MainDataFrame_edited.csv", sep = "/", collapse = "/"), stringsAsFactors = F)
      result <- kobo_anonymisation_report(MainDataFrame_edited, app = "shiny")
      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }else{
        shinyalert("Wooooow",
                   "Done!! Reports are in the folder OUT - Review the report- furter anonymise and regenerate as needed...",
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        showModal(showAnonymisationReportLinks())
      }

    }, error = function(err) {
      print("fgjhikiliklik")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  showAnonymisationReportLinks <- function() {
    tryCatch({
      return(modalDialog(id="showAnonymisationReportPopUp",
                         title = "Anonymisation Reports",
                         uiOutput("anonymisationReportBody"),
                         size = "l",
                         footer = tagList(
                           modalButton("Exit", icon("sign-out-alt"))
                         )
      )
      )

    }, error = function(err) {
      print("kiulgikigykuj")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  }

  output$anonymisationReportBody <- renderText({
    tryCatch({
      s <- ""
      mainPath <- paste(kobo_getMainDirectory(),"/out/anonymisation_reports/", sep = "")
      filesNames <- list.files(mainPath)
      filesNames <- sort(filesNames)
      for (fn in filesNames) {
        s <- paste(s,
                   box(width = 6, title = fn, solidHeader = FALSE, status = "primary", collapsed = F, collapsible = F,
                       downloadLink(paste("download",fn,sep = ""), paste("Download",fn))
                   )
                   ,sep="")

      }
      lapply(1:length(filesNames), function(m) {
        output[[paste("download",filesNames[m],sep = "")]] <- downloadHandler(
          filename = filesNames[m],
          content = function(file) {
            file.copy(paste(kobo_getMainDirectory(),"/out/anonymisation_reports/",filesNames[m], sep = ""), file)
          }
        )
      })
      return(s)
    }, error = function(err) {
      print("fdjkghdfijldfjkgfos")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })





  ####################################### Data Dissemination page ############################################
  output$dataDissemination <- renderUI({
    if(!projectConfigurationInfo$log[["isDataProcessingCompleted"]]){
      infoBox(
        width = 12,strong("Warning"),h4("You cannot proceed without completing Data Processing section", align = "center")
        ,icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    }else{
      column(width = 12, align="center",
             column(width = 12, align="center",style="margin-bottom:15px;",
                    column(width = 6, align="center",
                           actionButton("ddiButton", "Generate a DDI file", class = "toolButton006495", style="height: 700px; font-size: xx-large; width: 100%;")
                    ),
                    column(width = 6, align="center",
                           actionButton("hdxButton", "Export to HDX", class = "toolButtonE0A025", style="height: 700px; font-size: xx-large; width: 100%;")
                    )
             )
      )
    }
  })
  ##################Generate a DDI file################
  observeEvent(input$ddiButton,{
    tryCatch({
      result <- kobo_ddi(app = "shiny")
      if(class(result) == "try-error"){
        shinyalert("Error",
                   result,
                   type = "error",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#ff4d4d",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        return(FALSE)
      }else{
        shinyalert("Wooooow",
                   "Done!! XML file in the OUT folder",
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        showModal(showDDIfilesLinks())
      }

    }, error = function(err) {
      print("lkjjhhgkjg")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })

  showDDIfilesLinks <- function() {
    tryCatch({
      return(modalDialog(id="showDDIfilesPopUp",
                         title = "DDI Files",
                         uiOutput("ddiFilesBody"),
                         size = "l",
                         footer = tagList(
                           modalButton("Exit", icon("sign-out-alt"))
                         )
      )
      )

    }, error = function(err) {
      print("ydhffhgjhgf")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  }

  output$ddiFilesBody <- renderText({
    tryCatch({
      s <- ""
      mainPath <- paste(kobo_getMainDirectory(),"/out/ddi/", sep = "")
      filesNames <- list.files(mainPath)
      filesNames <- sort(filesNames)
      for (fn in filesNames) {
        s <- paste(s,
                   box(width = 6, title = fn, solidHeader = FALSE, status = "primary", collapsed = F, collapsible = F,
                       downloadLink(paste("download",fn,sep = ""), paste("Download",fn))
                   )
                   ,sep="")

      }
      lapply(1:length(filesNames), function(m) {
        output[[paste("download",filesNames[m],sep = "")]] <- downloadHandler(
          filename = filesNames[m],
          content = function(file) {
            file.copy(paste(kobo_getMainDirectory(),"/out/ddi/",filesNames[m], sep = ""), file)
          }
        )
      })
      return(s)
    }, error = function(err) {
      print("ytfjyhjtyhrthe")
      shinyalert("Error",
                 err$message,
                 type = "error",
                 closeOnClickOutside = FALSE,
                 confirmButtonCol = "#ff4d4d",
                 animation = FALSE,
                 showConfirmButton = TRUE
      )
    })
  })


})


# Run the application
shinyApp(ui = ui, server = server)
