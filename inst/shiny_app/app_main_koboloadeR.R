#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


kobo_projectinit()


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
                                 
                                 tabPanel(value = "pc", title = div(span("1"),span("Project Configuration"), class="arrow_box"),
                                          uiOutput("projectConfiguration")
                                 ),
                                 
                                 # Analysis Plan Configuration --------------------------------------------
                                 
                                 tabPanel(value = "apc", title = div(span("2"),span("Analysis Plan Configuration"), class="arrow_box"),
                                          uiOutput("analysisPlanConfiguration")
                                          
                                 ),
                                 
                                 # Data Processing ----------------------------------------
                                 
                                 tabPanel(value = "dp", title = div(span("3"),span("Data Processing"), class="arrow_box"),
                                          uiOutput("dataProcessing")
                                          
                                 ),
                                 
                                 # Reports Generation ----------------------------------------
                                 
                                 tabPanel(value = "rg", title = div(span("4"),span("Reports Generation"), class="arrow_box"), 
                                          uiOutput("reportsGeneration")
                                          
                                 ),
                                 
                                 # Reports Generation ----------------------------------------
                                 
                                 tabPanel(value = "dd", title = div(span("5"),span("Data Dissemination"), class="arrow_box"),
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
    questionsWidth = 5,
    yesNoInputWidth = 3,
    warningBlockWidth = 4
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
        if(file.exists(paste(mainDir(), "data", "/form.xls", sep = "/", collapse = "/"))  ){
          projectConfigurationInfo$log[["xlsForm"]] <- TRUE
          result <- kobo_get_begin_repeat()
          projectConfigurationInfo$data[["beginRepeatList"]] = c("data",result$names)
          projectConfigurationInfo$log[["beginRepeatList"]] = TRUE
          projectConfigurationInfo$log[["isPrepared"]] <- FALSE
          tracker$value = tracker$value + 1
        }
        if(file.exists(paste(mainDir(), "data", "/data.csv", sep = "/", collapse = "/")) ){
          dataFile <- read.csv( paste(mainDir(), "data", "/data.csv", sep = "/", collapse = "/")
                                , header=TRUE , stringsAsFactors = FALSE)
          projectConfigurationInfo$log[["data"]] <- TRUE
          projectConfigurationInfo$data[["data"]] <- dataFile
          projectConfigurationInfo$log[["isGenerated"]] <- FALSE
          tracker$value = tracker$value + 1
        }
        tracker$value = tracker$value + 1
      }
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
    x<<-projectConfigurationInfo$log
  })
  
  ####################################### Project Configuration page ############################################
  
  
  output$projectConfiguration <- renderUI({
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
                   div(class="warningBlock",
                       span(class="warningTitle","WARNING!"),
                       span(class="warningBody","Be careful, there is already xlsform file (form.xls) in the data directory, once you upload the new file, it will be overridden.")
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
                                style="width:100%; margin-top: 25px;", class="uploadButton" )
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
                   if(file.exists(paste(mainDir(), "data", "/data.csv", sep = "/", collapse = "/"))){
                     div(class="warningBlock",
                         span(class="warningTitle","WARNING!"),
                         span(class="warningBody","Be careful, there is already data.csv file in the data directory, once you upload the new file, it will be overridden.")
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
                            actionButton("dataUploadFileButton", "Upload file", icon("upload"), class="uploadButton",
                                         style="width:100%; margin-bottom: 20px; height:45px;")
                     )
                   )
            )
        )
      ),
      conditionalPanel(
        condition = "input.doYouHaveFormSelectInput == 'Yes'",
        div(id="doYouHaveDatasetsDiv",
            box(id="doYouHaveDatasetsBox", title = "File(s) related to project",
                width=12,status="primary", solidHeader = FALSE, collapsible = TRUE,
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
                         column(width = 12,
                                actionButton("saveDataFilesButton", "Upload and Save files", icon("upload"), class="uploadButton", style="margin: 15px 0px; width:100%;")
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
                   h4("Does xlsform include settings?")
            ),
            column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                   selectInput("formIncludeSettingsSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
            )
        )
      ),
      conditionalPanel(
        condition = "input.doYouHaveDataSelectInput == 'No' || input.doYouHaveDatasetsSelectInput == 'No'",
        infoBox(
          width = 12,strong("Warning"),h4("You cannot proceed without data file",align="center")
          ,icon = icon("exclamation-triangle"),
          color = "yellow"
        )
      ),
      conditionalPanel(
        condition = "input.doYouHaveFormSelectInput == 'No' && input.doYouWantGenerateFormSelectInput == 'No'",
        infoBox(
          width = 12,strong("Warning"),h4("You cannot proceed without xlsform",align="center")
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
  })
  
  observeEvent(input$formIncludeSettingsSelectInput, {
    projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
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
                 "You can start the Analysis Plan Configrution",
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
            width = 12,strong("Perfect!"),h4("You can start the Analysis Plan Configrution",align="center")
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
      inFile <- input$xlsFormUploadedFile
      if (!is.null(inFile)){
        wb <- xlsx::loadWorkbook(inFile$datapath)
        xlsx::saveWorkbook(wb, paste(mainDir(), "data", "/form.xls", sep = "/", collapse = "/"))
        projectConfigurationInfo$log[["xlsForm"]] <- TRUE
        result <- kobo_get_begin_repeat()
        projectConfigurationInfo$data[["beginRepeatList"]] = c("data",result$names)
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
        projectConfigurationInfo$log[["isPrepared"]] <- FALSE
        projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
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
      for(i in 1:(length(projectConfigurationInfo$data[["beginRepeatList"]]))){
        inFile <- input[[paste("fileInput",projectConfigurationInfo$data[["beginRepeatList"]][i],sep = "")]]
        if (!is.null(inFile)){
          dataFile <- read.csv(inFile$datapath, header=TRUE, sep=input[[paste("separator",projectConfigurationInfo$data[["beginRepeatList"]][i],sep = "")]], stringsAsFactors = FALSE)
          
          fileName <- paste("/",projectConfigurationInfo$data[["beginRepeatList"]][i], ".csv", sep="")
          
          write.csv(dataFile,  paste(mainDir(), "data", fileName, sep = "/", collapse = "/"), row.names = FALSE)
        }
      }
      updateProgress()
      projectConfigurationInfo$log[["subAndMainfiles"]] <- TRUE
      projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
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
  
  observeEvent(input$dataUploadFileButton, {
    tryCatch({
      inFile <- input$dataUploadedFile
      if (!is.null(inFile)){
        dataFile <- read.csv(inFile$datapath, header=TRUE, sep=input$dataUploadedFileSep, stringsAsFactors = FALSE)
        write.csv(dataFile,  paste(mainDir(), "data", "/data.csv", sep = "/", collapse = "/"), row.names = FALSE)
        projectConfigurationInfo$log[["data"]] <- TRUE
        projectConfigurationInfo$data[["data"]] <- dataFile
        projectConfigurationInfo$log[["isGenerated"]] <- FALSE
        projectConfigurationInfo$log[["isRecordSettingsCompleted"]] <- FALSE
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
      shinyalert("Error",
                 "You can't run this function without uploading data.csv file",
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
        shinyalert("Done, xlsform created using 'kobo_to_xlsform' function",
                   "You can creates and save a xlsform skeleton from a data file in your data folder\nThe form.xls will be saved in the data folder of your project",
                   type = "success",
                   closeOnClickOutside = FALSE,
                   confirmButtonCol = "#28A8E2",
                   animation = FALSE,
                   showConfirmButton = TRUE
        )
        
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
      
    }
  })
  
  observeEvent(input$prepareFormButton, {
    tryCatch({
      if(!projectConfigurationInfo$log[["xlsForm"]] ||
         (sum(input$doYouHaveFormSelectInput == "No") &&  sum(input$doYouWantGenerateFormSelectInput == "Yes") && projectConfigurationInfo$log[["isGenerated"]] == FALSE)
      ){
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
        kobo_prepare_form()
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
      }
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
  
  output$dataInputsUI <- renderText({
    if(!projectConfigurationInfo$log[["beginRepeatList"]]){
      s <-""
      s <- paste(
        infoBox(
          width = 12,strong("Information"),h4("You have to upload xlsform before uploading data files",align="center"), icon = icon("exclamation-triangle"),
          color = "orange"
        )
        , s ,sep="" )
      return(s)
    }else{
      s <- ""
      for(i in 1:(length(projectConfigurationInfo$data[["beginRepeatList"]]))){
        s <- paste(s , box(class="uploadFilesBox",title = projectConfigurationInfo$data[["beginRepeatList"]][i],  status = "primary",
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
    }
  })
  
  output$recordSettingsUI <- renderText({
    s <-""
    if(
      sum(input$doYouHaveFormSelectInput == "No") &&
      sum(input$doYouWantGenerateFormSelectInput == "Yes") &&
      sum(input$doYouHaveDataSelectInput == "Yes") &&
      projectConfigurationInfo$log[["data"]] == FALSE 
    ){
      s <- paste(infoBox(
        width = 12,strong("Information"),h4("You need to upload the data file before starting configuration of Record Settings",align="center"), icon = icon("exclamation-triangle"),
        color = "orange"
      ), s ,sep="" )
      return(s)
    }else if(
      (sum(input$doYouHaveFormSelectInput == "No") &&
       sum(input$doYouWantGenerateFormSelectInput == "Yes") &&
       sum(input$doYouHaveDataSelectInput == "Yes") &&
       (
         projectConfigurationInfo$log[["isPrepared"]] == FALSE ||
         projectConfigurationInfo$log[["isGenerated"]] == FALSE )
      )
    ){
      if(projectConfigurationInfo$log[["isPrepared"]] == FALSE  && projectConfigurationInfo$log[["isGenerated"]] == FALSE ){
        s <- paste(infoBox(
          width = 12,strong("Information"),h4("You have to run 'Generate xlsform' function and 'Prepare xlsform' function before starting configuration of Record Settings",align="center"), icon = icon("exclamation-triangle"),
          color = "orange"
        ), s ,sep="" )
      }else if(projectConfigurationInfo$log[["isGenerated"]] == FALSE ){
        s <- paste(infoBox(
          width = 12,strong("Information"),h4("You have to run 'Generate xlsform' function before starting configuration of Record Settings",align="center"), icon = icon("exclamation-triangle"),
          color = "orange"
        ), s ,sep="" )
      }else if(projectConfigurationInfo$log[["isPrepared"]] == FALSE ){
        s <- paste(infoBox(
          width = 12,strong("Information"),h4("You have to run 'Prepare xlsform' function before starting configuration of Record Settings",align="center"), icon = icon("exclamation-triangle"),
          color = "orange"
        ), s ,sep="" )
      }
      return(s)
    }else if(
      sum(input$doYouHaveFormSelectInput == "Yes") &&
      sum(input$doYouHaveDatasetsSelectInput == "Yes") &&
      sum(input$formIncludeSettingsSelectInput == "No") &&
      projectConfigurationInfo$log[["subAndMainfiles"]] == FALSE 
    ){
      s <- paste(infoBox(
        width = 12,strong("Information"),h4("You have to upload all required data files before starting configuration of Record Settings",align="center"), icon = icon("exclamation-triangle"),
        color = "orange"
      ), s ,sep="" )
      return(s)
    }else if(
      sum(input$doYouHaveFormSelectInput == "Yes") &&
      sum(input$doYouHaveDatasetsSelectInput == "Yes") &&
      sum(input$formIncludeSettingsSelectInput == "No") &&
      projectConfigurationInfo$log[["isPrepared"]] == FALSE 
    ){
      s <- paste(infoBox(
        width = 12,strong("Information"),h4("You have to run 'Prepare xlsform' function before starting configuration of Record Settings",align="center"), icon = icon("exclamation-triangle"),
        color = "orange"
      ), s ,sep="" )
      return(s)
    }
    s <- paste(
      fluidRow(
        
        column(12, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px; margin-bottom: 20px; background-color: ghostwhite; padding-top: 20px;",
               column(width = projectConfigurationTheme$questionsWidth, style = "margin-bottom: 10px; border-bottom: 1px dotted lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                      h4("What sampling do you have?")
               ),
               column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                      selectInput("samplingSelectInput", label = NULL,choices = c("-- select --",
                                                                                  "No sampling(type 1)",
                                                                                  "Cluster sample (type 2)",
                                                                                  "Stratified sample (type 3)"
                      ))
                      
               ),
               conditionalPanel(
                 condition = "input.samplingSelectInput == 'Cluster sample (type 2)'",
                 column(width = 12, style="margin: 15px 0px 15px; border-top: 1px solid lightgray; padding: 20px 10px 0px;",
                        column(width = 6, 
                               selectizeInput("variableNameCluster", label = "Select the name of cluster variable",choices = projectConfigurationInfo$data[["xlsFormFields"]]
                                              ,options = list(placeholder = '-- select --', onInitialize = I('function() { this.setValue(""); }'))
                               )
                        ),
                        column(width = 6,
                               textInput("clusterIdTextInput", label = "Enter the clustre Id", placeholder = "Ex: 230948")
                        ),
                        column(width = 12,
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
                                        div(class="warningBlock",
                                            span(class="warningTitle","WARNING!"),
                                            span(class="warningBody","Be careful, there is already weightsCluster.csv file in the data directory, once you upload the new file, it will be overridden.")
                                        )
                                      }
                                      
                               )
                        )
                 )
                 
               ),
               conditionalPanel(
                 condition = "input.samplingSelectInput == 'Stratified sample (type 3)'",
                 column(width = 12, style="margin: 15px 0px 15px; border-top: 1px solid lightgray; padding: 20px 10px 0px;",
                        column(width = 6, 
                               selectizeInput("variableNameStratified", label = "Select the name of stratified variable",choices = projectConfigurationInfo$data[["xlsFormFields"]]
                                              ,options = list(placeholder = '-- select --', onInitialize = I('function() { this.setValue(""); }'))
                               )
                        ),
                        column(width = 6,
                               textInput("stratifiedIdTextInput", label = "Enter the stratified Id", placeholder = "Ex: 230948")
                        ),
                        column(width = 12,
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
                                        div(class="warningBlock",
                                            span(class="warningTitle","WARNING!"),
                                            span(class="warningBody","Be careful, there is already weightsStratified.csv file in the data directory, once you upload the new file, it will be overridden.")
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
                        div(class="warningBlock",
                            span(class="warningTitle","WARNING!"),
                            span(class="warningBody","Be careful, there is already cleaningLog.csv file in the data directory, once you upload the new file, it will be overridden.")
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
        column(12, style = "border: 1px solid lightgray; border-bottom-right-radius: 7px; margin-bottom: 20px; background-color: ghostwhite; padding-top: 0px;",
               actionButton("saveRecordSettingsConfigurationButton", "Save Settings", icon("upload"), class="uploadButton", style="margin: 15px 0px; height:45px; width:100%;")
        )
        
        
      ), s ,sep="" )
    
    return(s)
  })
  
  observeEvent(input$saveRecordSettingsConfigurationButton, {
    tryCatch({
      settingsDF <- data.frame(name = character(),
                               label = character(),
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
      if(sum(input$samplingSelectInput == "No sampling(type 1)")){
        settingsDF[lastRow,"name"] <- "sample_type"
        settingsDF[lastRow,"label"] <- "Sample type of the project"
        settingsDF[lastRow,"value"] <- "No sampling(type 1)"
      }else if(sum(input$samplingSelectInput == "Cluster sample (type 2)")){
        settingsDF[lastRow,"name"] <- "sample_type"
        settingsDF[lastRow,"label"] <- "Sample type of the project"
        settingsDF[lastRow,"value"] <- input$samplingSelectInput
        
        if(sum(input$variableNameCluster == "")){
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
        settingsDF[lastRow,"label"] <- "The name of cluster variable"
        settingsDF[lastRow,"value"] <- input$variableNameCluster
        
        if(sum(input$clusterIdTextInput == "")){
          shinyalert("Error",
                     "You have to enter the clustre Id",
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        lastRow <- lastRow+1
        settingsDF[lastRow,"name"] <- "cluster_id"
        settingsDF[lastRow,"label"] <- "Cluster Id reference for weightsCluster file"
        settingsDF[lastRow,"value"] <- input$clusterIdTextInput
        updateProgress()
        inFileWeightsCluster<- input$weightsClusterFileInput
        if(is.null(inFileWeightsCluster)){
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
          settingsDF[lastRow,"name"] <- "weights_cluster"
          settingsDF[lastRow,"label"] <- "Weights that will be used in cluster sample"
          settingsDF[lastRow,"value"] <- "weightsCluster.csv"
          settingsDF[lastRow,"path"] <-  paste(mainDir(), "data", "/weightsCluster.csv", sep = "/", collapse = "/")
        }
      }else if(sum(input$samplingSelectInput == "Stratified sample (type 3)")){
        settingsDF[lastRow,"name"] <- "sample_type"
        settingsDF[lastRow,"label"] <- "Sample type of the project"
        settingsDF[lastRow,"value"] <- input$samplingSelectInput
        updateProgress()
        if(sum(input$variableNameStratified == "")){
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
        if(sum(input$StratifiedIdTextInput == "")){
          shinyalert("Error",
                     "You have to enter the clustre Id",
                     type = "error",
                     closeOnClickOutside = FALSE,
                     confirmButtonCol = "#ff4d4d",
                     animation = FALSE,
                     showConfirmButton = TRUE
          )
          return(FALSE)
        }
        lastRow <- lastRow+1
        settingsDF[lastRow,"name"] <- "stratified_id"
        settingsDF[lastRow,"label"] <- "Stratified Id reference for weightsStratified file"
        settingsDF[lastRow,"value"] <- input$stratifiedIdTextInput
        
        inFileWeightsStratified<- input$weightsStratifiedFileInput
        if(is.null(inFileWeightsStratified)){
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
          settingsDF[lastRow,"name"] <- "weights_stratified"
          settingsDF[lastRow,"label"] <- "Weights that will be used in Stratified sample"
          settingsDF[lastRow,"value"] <- "weightsStratified.csv"
          settingsDF[lastRow,"path"] <-  paste(mainDir(), "data", "/weightsStratified.csv", sep = "/", collapse = "/")
        }
      }
      updateProgress()
      if(sum(input$cleaningLogSelectInput == "-- select --")){
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
        settingsDF[lastRow,"value"] <- "No"
      }else{
        inFilecleaningLog <- input$cleaningLogFileInput
        if(is.null(inFilecleaningLog) && sum(input$cleaningLogSelectInput == "Yes")){
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
          settingsDF[lastRow,"value"] <- "cleaningLog.csv"
          settingsDF[lastRow,"path"] <-  paste(mainDir(), "data", "/cleaningLog.csv", sep = "/", collapse = "/")
        }
      }
      x<<-settingsDF
      wb <- xlsx::loadWorkbook(paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/"))
      sheets <- xlsx::getSheets(wb)
      settingsSheet <- sheets[["settings"]]
      xlsx::addDataFrame(settingsDF, settingsSheet, col.names=TRUE, row.names=FALSE)
      xlsx::saveWorkbook(wb, paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/"))
      projectConfigurationInfo$log[["isRecordSettingsSaved"]] <- TRUE
      updateProgress()
      
      shinyalert("Done, Record Settings Configuration has been successfully saved",
                 "You can find the Settings in 'settings' sheet in xlsform file",
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
  
  #######################################           End               ############################################
  
  ####################################### Analysis Plan Configuration page ############################################
  sheets <- reactiveValues()
  lastMenuItem <- reactiveValues(v=NULL)
  
  output$analysisPlanConfiguration <- renderUI({
    #if(!projectConfigurationInfo$log[["isRecordSettingsCompleted"]]){
    if(FALSE){ 
     infoBox(
        width = 12,strong("Warning"),h4("You cannot proceed without completing Project Configuration section",align="center")
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
        conditionalPanel(
          condition = "input.doYouHaveAnalysisPlanSelectInput=='No'",
          
          column(width = 12,
          div(id="wellMenuForAPC",
            sidebarLayout(
              sidebarPanel(width = 2,
                sidebarMenu(id="sidebarMenuForAP",
                  menuItem(div(span("1",class="numberStep"),span("Re-Labeling Survey Sheet")), tabName = "relabelingSurvey"),
                  menuItem(div(span("2",class="numberStep"),span("Re-Labeling Choices Sheet")), tabName = "relabelingChoices"),
                  #menuItem(div(span("3",class="numberStep"),span("Text type")), tabName = "textType"),
                  menuItem(div(span("3",class="numberStep"),span("Select_one type")), tabName = "selectOneType"),
                  menuItem(div(span("4",class="numberStep"),span("Order Ordinal Variables")), tabName = "orderOrdinalVariables"),
                  menuItem(div(span("5",class="numberStep"),span("Select_multiple type")), tabName = "selectMultipleType"),
                  menuItem(div(span("6",class="numberStep"),span("Numeric type")), tabName = "numericType"),
                  #menuItem(div(span("8",class="numberStep"),span("Integer type")), tabName = "integerType"),
                  menuItem(div(span("7",class="numberStep"),span("Date type")), tabName = "dateType"),
                  #menuItem(div(span("10",class="numberStep"),span("Decimal type")), tabName = "decimalType"),
                  menuItem(div(span("8",class="numberStep"),span("Indicators Sheet")), tabName = "indicatorsSheet"),
                  menuItem(div(span("9",class="numberStep"),span("Chapter")), tabName = "chapter"),
                  menuItem(NULL, icon = icon("info-circle"), tabName = "infoAPC")
                ),
                div(id="styleFormDiv", style="background-color: #ecf0f5;",
                    actionButton("styleFormButton", "Add style to xlsform", style="width: 100%; margin: 10px 0% 0px; line-height: 35px;", class="uploadButton"))
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
         )
        ),
        conditionalPanel(
          condition = "input.doYouHaveAnalysisPlanSelectInput=='No' || input.doYouHaveAnalysisPlanSelectInput=='Yes'",
          box(id="decoAndCheckplanBox",
              width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
              column(width = 2, align="left",
                     icon("arrow-right", "fa-14x") 
              ),
              column(width = 3, align="center",
                     actionButton("ProduceDataDictionnaryButton", "Produce a Data Dictionnary", class = "processButton")
              ),
              column(width = 2, align="center",
                     icon("arrow-right", "fa-14x") 
              ),
              column(width = 3, align="center",
                     actionButton("checkPlanButton", "Check the Plan", class = "processButton")
              ),
              column(width = 2, align="right",
                     icon("arrow-down", "fa-14x") 
              )
          )
        )
      )
    }
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
        kobo_prepare_form()
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
  
  
  #####################relabeling Survey#############################
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
          hot_col("label::Report", width = 400, allowInvalid = FALSE,
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
      }else{
        temp <- temp %>% hot_col("label::Report", width = 400, allowInvalid = FALSE,
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
      
      if("hint" %in% colnames(sheets[["relabelingSurvey"]])){
        temp <- temp %>% hot_col("hint", readOnly = TRUE, width = 200) %>%
          hot_col("hint::Report", width = 400, allowInvalid = FALSE,
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
      }else{
        temp <- temp %>% hot_col("hint::Report", width = 400, allowInvalid = FALSE,
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
  
  #####################relabeling Choices#############################
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
          hot_col("label::Report", width = 400, allowInvalid = FALSE,
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
        temp <- temp %>% hot_col("label::Report", width = 400, allowInvalid = FALSE,
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
  
  
  #####################selectOne Type#############################
  output$selectOneTypeUI <- renderUI({
    box(id="selectOneTypeBox",
        width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        #uiOutput("selectOneTypeBody")
        rHandsontableOutput("selectOneTypeTable")
    )
  })

  output$selectOneTypeBody <- renderUI({
    
    if(is.null(sheets[["selectOneType"]])){
      return(FALSE)
    }
    if(nrow(sheets[["selectOneType"]])>0){
      rHandsontableOutput("selectOneTypeTable")
    }else{
      infoBox(
        width = 12,strong("Info"),
        h4("There is no select_one type, you can start with the next step",align="center")
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
        hot_col("anonymise", type = "dropdown", source = c("default-non-anonymised", "remove", "reference", "scramble"), width = 120) %>%
        hot_col("correlate",  width = 120, halign="htCenter") %>%
        hot_col("clean",  width = 120, halign="htCenter") %>%
        hot_col("cluster",  width = 120, halign="htCenter") %>%
        hot_col("predict",  width = 120, halign="htCenter") %>%
        hot_col("mappoint",  width = 120, halign="htCenter") %>%
        hot_col("mappoly",  width = 120, halign="htCenter") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)

      temp
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
  ###################################################################
  

  #####################Order Ordinal Variables#############################
  output$orderOrdinalVariablesUI <- renderUI({
    box(id="orderOrdinalVariablesBox",
        width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        #uiOutput("orderOrdinalVariablesBody")
        rHandsontableOutput("orderOrdinalVariablesTable")
    )
  })
  output$orderOrdinalVariablesBody <- renderUI({
    
    if(is.null(sheets[["selectOneType"]])){
      return(FALSE)
    }
    if(nrow(sheets[["selectOneType"]])>0){
      rHandsontableOutput("orderOrdinalVariablesTable")
    }else{
      infoBox(
        width = 12,strong("Info"),
        h4("There is no select_one type, you can start with the next step",align="center")
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
  
  
  #####################Select_multiple type#############################
  output$selectMultipleTypeUI <- renderUI({
    box(id="selectMultipleTypeBox",
        width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        #uiOutput("selectMultipleTypeBody")
        rHandsontableOutput("selectMultipleTypeTable")
    )
  })
  output$selectMultipleTypeBody <- renderUI({
    
    if(is.null(sheets[["selectMultipleType"]])){
      return(FALSE)
    }
    if(nrow(sheets[["selectMultipleType"]])>0){
      rHandsontableOutput("selectMultipleTypeTable")
    }else{
      infoBox(
        width = 12,strong("Info"),
        h4("There is no select_Multiple type, you can start with the next step",align="center")
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
        hot_col("anonymise", type = "dropdown", source = c("default-non-anonymised", "remove", "reference", "scramble"), width = 120) %>%
        hot_col("correlate",  width = 120, halign="htCenter") %>%
        hot_col("clean",  width = 120, halign="htCenter") %>%
        hot_col("cluster",  width = 120, halign="htCenter") %>%
        hot_col("predict",  width = 120, halign="htCenter") %>%
        hot_col("mappoint",  width = 120, halign="htCenter") %>%
        hot_col("mappoly",  width = 120, halign="htCenter") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
      temp
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
  ###################################################################
  
  
  #####################Numeric type#############################
  output$numericTypeUI <- renderUI({
    box(id="numericTypeBox",
        width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        #uiOutput("numericTypeBody")
        rHandsontableOutput("numericTypeTable")
    )
  })
  output$numericTypeBody <- renderUI({
    
    if(is.null(sheets[["numericType"]])){
      return(FALSE)
    }
    if(nrow(sheets[["numericType"]])>0){
      rHandsontableOutput("selectMultipleTypeTable")
    }else{
      infoBox(
        width = 12,strong("Info"),
        h4("There is no Numeric type, you can start with the next step",align="center")
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
        hot_col("anonymise", type = "dropdown", source = c("default-non-anonymised", "remove", "reference", "scramble"), width = 120) %>%
        hot_col("correlate",  width = 120, halign="htCenter") %>%
        hot_col("clean",  width = 120, halign="htCenter") %>%
        hot_col("cluster",  width = 120, halign="htCenter") %>%
        hot_col("predict",  width = 120, halign="htCenter") %>%
        hot_col("mappoint",  width = 120, halign="htCenter") %>%
        hot_col("mappoly",  width = 120, halign="htCenter") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
      temp
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
  ###################################################################
  
  
  #####################Date type#############################
  output$dateTypeUI <- renderUI({
    box(id="dateTypeBox",
        width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,height = 650,
        #uiOutput("dateTypeBody")
        rHandsontableOutput("dateTypeTable")
    )
  })
  output$dateTypeBody <- renderUI({
    
    if(is.null(sheets[["dateType"]])){
      return(FALSE)
    }
    if(nrow(sheets[["dateType"]])>0){
      rHandsontableOutput("dateTypeTable")
    }else{
      infoBox(
        width = 12,strong("Info"),
        h4("There is no Date type, you can start with the next step",align="center")
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
        hot_col("anonymise", type = "dropdown", source = c("default-non-anonymised", "remove", "reference", "scramble"), width = 120) %>%
        hot_col("correlate",  width = 120, halign="htCenter") %>%
        hot_col("clean",  width = 120, halign="htCenter") %>%
        hot_col("cluster",  width = 120, halign="htCenter") %>%
        hot_col("predict",  width = 120, halign="htCenter") %>%
        hot_col("mappoint",  width = 120, halign="htCenter") %>%
        hot_col("mappoly",  width = 120, halign="htCenter") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
      temp
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
  ###################################################################
  
  
  ##################### Indicators Sheet ############################
  indicatorsInfo <- reactiveValues(data=NULL, selectedIndicator=NULL, operationType=NULL)
  
  output$indicatorsSheetUI <- renderUI({
    box(id="indicatorsSheetBox",
        width=12, solidHeader = FALSE, collapsible = FALSE,height = 650,
        column(width = 12,style="border-bottom: 1px solid lightgray; margin: 10px 0px 35px;",
               actionButton("addIndicatorButton", "Add Indicator", icon = icon("plus"), class="uploadButton",
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
                         textInfoOfRow , class="divOFIndRow"
                       ),
                       column(width = 7,
                              actionButton(paste("editIndicatorButton", ind, sep = ""), "Edit Indicator", icon = icon("edit"), class="toolButton", style="height: 50px; margin-bottom:20px;", width="100%")
                       ),
                       column(width = 5,
                              actionButton(paste("deleteIndicatorButton", ind, sep = ""), "Delete Indicator", icon = icon("trash-alt"), class="deleteButton", style="height: 50px; margin-bottom:20px;", width="100%")
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
            kobo_edit_form(indicator = indicator)
            
            
            indicatorsIF <- indicatorsInfo[["data"]]
            indicatorsIF <- indicatorsIF[indicatorsIF$fullname != indicators[j],]
            updateProgress()
            indicatorsIF <- indicatorsIF %>% arrange(fullname)
            updateProgress()
            indicatorsInfo[["data"]] <- indicatorsIF
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
                           modalButton("Cancel", icon("sign-out-alt")),
                           actionButton("saveIndicatorButton", ifelse(type=="Add","Add the Indicator", "Edit the Indicator"), class="toolButton", style="height: 35px;")
                         )
      ))
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
                     column(width = 6, offset = 0,
                            textInput("indicatorLabelInput", label = NULL, value = rowInd[1,"label"], width = "100%")
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
                   column(
                     width=12,
                      uiOutput("calculationBuilderToolBody")
                   )
                 ),
                 
                 box(id="moreOptionsIndicatorBox",title = "Optional Inputs...",
                     width=12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status = "primary",
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
                            h4("Apply sensitive?")
                     ),
                     column(width = 6, offset = 0,
                            selectInput("indicatorSensitiveInput", label = NULL, selected = ifelse(rowInd[1,"sensitive"]=="TRUE","Yes",ifelse(rowInd[1,"sensitive"]=="FALSE","No","-- select --")),
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
                            h4("Apply structuralequation?")
                     ),
                     column(width = 6, offset = 0,
                            selectInput("indicatorStructuralequationInput", label = NULL, selected = ifelse(rowInd[1,"structuralequation"]=="TRUE","Yes",ifelse(rowInd[1,"structuralequation"]=="FALSE","No","-- select --")),
                                        choices = c("-- select --","Yes","No"), 
                                        width = "100%")
                     )
                   )
                   
                 )
                 ,sep = "")
      
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
      
      if(sum(input$indicatorFullnameInput %in% indicatorsIF$fullname)){
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
      
      if(sum(input$indicatorFrameInput=="-- select --")){
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
      
      if(sum(input$indicatorCaseSelectInput == "-- select --")){
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
      
      ######################Calculation Builder Tool#########################
      
      
      calculationResult <- c()
      if(sum(input$indicatorCaseSelectInput=="Discretize a value")){
        tryCatch({ 
          ############################        Validation        ############################
          if (
            sum(input$frameDVSelectInput == "-- select --") ||
            sum(input$variableDVSelectInput == "-- select --") ||
            sum(input$breaksDVTextInput == "")
          ) {
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
          calculationResult <- paste(rest,input$frameDVSelectInput,"$",input$variableDVSelectInput, " ", sep="")
          calculationResult <- paste(calculationResult, ",c(", pre,"))" , sep = ""  )
        }, error = function(err) {
          calculationResult <- structure(c, class = "try-error")
        })
      }
      else if(sum(input$indicatorCaseSelectInput=="Re categorize a categorical variable by re coding modalities")){
        tryCatch({ 
          ############################        Validation        ############################
          if (
            sum(input$frameFRSelectInput == "-- select --") ||
            sum(input$variableFRSelectInput == "-- select --") ||
            sum(input$listnameFRSelectInput == "-- select --")
          ) {
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
          calculationResult <- paste(calculationResult,input$frameFRSelectInput,"$",input$variableFRSelectInput, ", ", sep="")
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
            sum(input$frameSUSelectInput == "-- select --")
          ) {
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
                          input$frameSUSelectInput,
                          "$",
                          input$varSU1,
                          ", ",
                          input$frameSUSelectInput,
                          "$",
                          input$varSU2,
                          ifelse(length(variablesToUseSU$idOfVar)>0,
                                 ", ",""), sep="")
          
          
          
          counter <- 1
          for(i in variablesToUseSU$idOfVar){
            val <- input[[paste("varSU", i, sep = "")]]
            calculationResult <- paste(calculationResult,
                            input$frameSUSelectInput,
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
            sum(input$frameMMASelectInput == "-- select --") ||
            sum(input$variableMMASelectInput == "-- select --") ||
            sum(input$statisticalFunctionsMMASelectInput == "-- select --")
          ) {
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
          
          
          varFrame <- paste(input$frameMMASelectInput,"$",input$variableMMASelectInput, sep="")
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
            sum(input$frameD2SelectInput == "-- select --") ||
            sum(input$variableD2SelectInput1 == "-- select --") ||
            sum(input$variableD2SelectInput2 == "-- select --")
          ) {
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
          
          
          
          calculationResult <- paste(input$frameD2SelectInput,"$",input$variableD2SelectInput1, " / ", sep="")
          calculationResult <- paste(calculationResult, input$frameD2SelectInput,"$",input$variableD2SelectInput2, sep="")
          }, error = function(err) {
            calculationResult <- structure(c, class = "try-error")
          })
      }
      
      ################################################################
      
      if(class(calculationResult) == "try-error"){
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

  
  ###################################################################
  
  
  #####################    Chapter      #############################
  chaptersDataFrame <- reactiveValues(data=NULL, selectedChapter=NULL, operationType=NULL)
  
  output$chapterUI <- renderUI({
    box(id="chapterBox",
        width=12, solidHeader = FALSE, collapsible = FALSE,height = 650,
        column(width = 12,style="border-bottom: 1px solid lightgray; margin: 10px 0px 35px;",
               actionButton("addChapterButton", "Add Chapter", icon = icon("plus"), class="uploadButton",
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
                       span("Variables: ", class="titleTagVar"),div(
                           textSur , class="divVar"
                       ),
                       span("Indicators: ", class="titleTagInd"),div(
                           textInd , class="divInd"
                       ),
                       column(width = 7,
                              actionButton(paste("editChapterButton", chp, sep = ""), "Edit Chapter", icon = icon("edit"), class="toolButton", style="height: 50px; margin-bottom:20px;", width="100%")
                       ),
                       column(width = 5,
                              actionButton(paste("deleteChapterButton", chp, sep = ""), "Delete Chapter", icon = icon("trash-alt"), class="deleteButton", style="height: 50px; margin-bottom:20px;", width="100%")
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
            kobo_edit_form(survey = survey, indicator = indicator)
            chaptersDF <- chaptersDataFrame[["data"]]
            chaptersDF <- chaptersDF[chaptersDF$chapter != cahpters[j],]
            updateProgress()
            chaptersDF <- chaptersDF %>% arrange(chapter)
            updateProgress()
            chaptersDataFrame[["data"]] <- chaptersDF
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
  
  observeEvent(input$addChapterButton,{
    tryCatch({
      chaptersDataFrame$selectedChapter <- ""
      chaptersDataFrame$operationType <- "Add"
      showModal(showChapterTool("Add",NULL))
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
      }else{
        return(modalDialog(id="showChapterToolPopUp", 
                    title = ifelse(type=="Add","Add Chapter", paste("Edit",chapterName,"chapter")),
                    uiOutput("chapterToolBody"),
                    size = "l",
                    footer = tagList(
                      modalButton("Cancel", icon("sign-out-alt")),
                      actionButton("saveChapterButton", ifelse(type=="Add","Add the Chapter", "Edit the Chapter"), class="toolButton", style="height: 35px;")
                    )
        ))
      }
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
                              h4(paste("There are no Variables available."),align="center")
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
                              h4(paste("There are no Indicators available."),align="center")
                              ,icon = icon("info-circle"),
                              color = "teal"
                            )
                          }
                   )
                 ),sep = "")
      
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
      survey[!is.na(survey$chapter) & survey$chapter==selChap,"chapter"] <- NA
      indicator[!is.na(indicator$chapter) & indicator$chapter==selChap,"chapter"] <- NA
      updateProgress()
      survey[!is.na(survey$name) & survey$name %in% selectedVar,"chapter"] <- input$chapterNameInput
      indicator[!is.na(indicator$fullname) & indicator$fullname %in% selectedInd,"chapter"] <- input$chapterNameInput
      updateProgress()
      kobo_edit_form(survey = survey, indicator = indicator)
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
        userRelabelingSurvey <- sheets[["relabelingSurvey"]]
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        mainSurvey <- as.data.frame(read_excel(form_tmp, sheet = "survey"),
                                    stringsAsFactors = FALSE)
        
        if(identical(as.character(mainSurvey["label::Report"]),as.character(userRelabelingSurvey["label::Report"])) &
           identical(as.character(mainSurvey["hint::Report"]),as.character(userRelabelingSurvey["hint::Report"]))
           ){
          return(FALSE)
        }
        
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
        
        mainSurvey["label::Report"] = userRelabelingSurvey["label::Report"]
        mainSurvey["hint::Report"] = userRelabelingSurvey["hint::Report"]

        kobo_edit_form(survey = mainSurvey)
        updateProgress()
      }
      else if(las == "relabelingChoices"){
        userRelabelingChoices <- sheets[["relabelingChoices"]]
        form_tmp <- paste(mainDir(), "data", "form.xls", sep = "/", collapse = "/")
        mainChoices <- as.data.frame(read_excel(form_tmp, sheet = "choices"),
                                    stringsAsFactors = FALSE)
        
        if(identical(as.character(mainChoices["label::Report"]),as.character(userRelabelingChoices["label::Report"]))){
          return(FALSE)
        }
        
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
        
        mainChoices["label::Report"] <- userRelabelingChoices["label::Report"]
        
        kobo_edit_form(choices = mainChoices)
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
        kobo_edit_form(survey = newSurvey)
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
        kobo_edit_form(choices = newChoices)
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
        kobo_edit_form(survey = newSurvey)
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
        kobo_edit_form(survey = newSurvey)
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
        kobo_edit_form(survey = newSurvey)
      }
      
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
  #############################################################################
  
  
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
        condition = "input.variableDVSelectInput != '-- select --' && input.frameDVSelectInput != '-- select --' && input.breaksDVTextInput != '' && input.indicatorCaseSelectInput == 'Discretize a value'",
        uiOutput("resultDVUI")
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
        condition = "input.listnameFRSelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Re categorize a categorical variable by re coding modalities'",
        uiOutput("resultFRUI")
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
        condition = "input.frameSUSelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Sum up different numeric or integer variables'",
        uiOutput("resultSUUI")
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
        condition = "input.statisticalFunctionsMMASelectInput != '-- select --' && input.variableMMASelectInput != '-- select --' && input.frameMMASelectInput != '-- select --' && input.indicatorCaseSelectInput == 'Calculate min, max or avg value for multiple integer or numeric variables'",
        uiOutput("resultMMAUI")
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
        condition = "input.frameD2SelectInput != '-- select --' && input.variableD2SelectInput1 != '-- select --' && input.variableD2SelectInput2 != '-- select --' && input.indicatorCaseSelectInput == 'Calculate ratio by dividing 2 numeric or integer variables'",
        uiOutput("resultD2UI")
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
            h4(paste("You cannot proceed without",input$frameDVSelectInput,"file"),align="center")
            ,icon = icon("exclamation-triangle"),
            color = "yellow"
          )
        }
  
      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message,align="center")
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
                   div(class="help-tip",style="top: 0px;left: 25px;",
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
        h4(err$message,align="center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })
  
  output$resultDVUI <- renderText({
    tryCatch({
      s <- ""
      if(!is.null(input$breaksDVTextInput)){
        if(class(resultDVUIValue$text) == "try-error"){
          s<- paste(
            div(id="resultUIDivError",
              column(style="margin-top: 20px;",
                     width=12,
                     box(id="resultUIBoxError",
                         title="ERROR",
                         style="border: 1px solid lightgray; font-size: x-large;min-height: 300px;",
                         width=12, solidHeader = FALSE, collapsible = FALSE,
                         p(resultDVUIValue$text$message)
                     )
                     
              )
            ),s,sep = "")
        }else if(input$breaksDVTextInput != "" && resultDVUIValue$text != ""){
          s<- paste(
            div(id="resultUIDivSuccess",
              column(style="margin-top: 20px;",
              width=12,
              box(id="resultUIBoxSuccess",
                  title=paste("Copy the text and paste it into calculation column of row number:",input$rowNumberForCalculationBuilder),
                  style="border: 1px solid lightgray; font-size: x-large;min-height: 300px;",
                  width=12, solidHeader = FALSE, collapsible = FALSE,
                  p(resultDVUIValue$text)
              )
    
            )
          ),s,sep = "")
        }
      }
      return(s)
    }, error = function(err) {
      return(infoBox(
        width = 12,strong("Error"),
        h4(err$message,align="center")
        ,icon = icon("times"),
        color = "red"
      ))
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
            h4(paste("You cannot proceed without",input$frameDVSelectInput,"file"),align="center")
            ,icon = icon("exclamation-triangle"),
            color = "yellow"
          )
        }
      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message,align="center")
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
        h4(err$message,align="center")
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
                      div(class="help-tip",style="top: 0px;left: 25px;",
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
        h4(err$message,align="center")
        ,icon = icon("times"),
        color = "red"
      ))
    })
  })
  
  output$resultFRUI <- renderText({
    tryCatch({ 
      s <- ""
      if(!is.null(input$listnameFRSelectInput)){
        if(class(resultFRUIValue$text) == "try-error"){
          s<- paste(
            div(id="resultUIDivError",
                column(style="margin-top: 20px;",
                       width=12,
                       box(id="resultUIBoxError",
                           title="ERROR",
                           style="border: 1px solid lightgray; font-size: x-large;min-height: 300px;",
                           width=12, solidHeader = FALSE, collapsible = FALSE,
                           p(resultFRUIValue$text$message)
                       )
                       
                )
            ),s,sep = "")
        }else if(resultFRUIValue$text != ""){
          s<- paste(
            div(id="resultUIDivSuccess",
                column(style="margin-top: 20px;",
                       width=12,
                       box(id="resultUIBoxSuccess",
                           title=paste("Copy the text and paste it into calculation column of row number:",input$rowNumberForCalculationBuilder),
                           style="border: 1px solid lightgray; font-size: x-large;min-height: 300px;",
                           width=12, solidHeader = FALSE, collapsible = FALSE,
                           p(resultFRUIValue$text)
                       )
                       
                )
            ),s,sep = "")
        }
      }
      return(s)
    }, error = function(err) {
      return(infoBox(
        width = 12,strong("Error"),
        h4(err$message,align="center")
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
        h4(err$message,align="center")
        ,icon = icon("times"),
        color = "red"
      ))
    })
  })
  
  output$mainTwoVariablesSUUI <- renderUI({
    column(
      width=12,
      column(width = 1, offset = 0,align="center",
             icon("flag", "fa-2x")
      ),
      column(width = 3, offset = 0,align="center",
             selectInput("varSU1", label = NULL,choices = variablesToUseSU$col,width = "100%", selected = 1)
      ),
      column(width = 1, offset = 0,align="center",
             icon("plus", "fa-2x")
      ),
      column(width = 4, offset = 0,align="center",
             selectInput("varSU2", label = NULL,choices = variablesToUseSU$col,width = "100%", selected = 15)
      ),
      column(width = 3, offset = 0,align="center",
             actionButton("addVariablesSU", "Add Variable", class="toolButton", style="height: 35px;", icon = icon("plus-circle","fa-1x"))
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
          column(width = 1, offset = 0,align="center",
                 icon("plus", "fa-2x")
          ),
          column(width = 8, offset = 0,align="center",
                 selectInput(paste("varSU", i, sep = ""), label = NULL,choices = variablesToUseSU$col,width = "100%", selected = tempVal)
          ),
          column(width = 2, offset = 0,align="center",
                 actionButton(paste("deleteVariableSU", i ,sep = ""), NULL, class="toolButtonDelete", style="height: 35px;", icon = icon("times","fa-2x"))
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
                         column(width = 1, offset = 0,align="center",
                                icon("plus", "fa-2x")
                         ),
                         column(width = 8, offset = 0,align="center",
                                selectInput(paste("varSU", i, sep = ""), label = NULL,choices = variablesToUseSU$col,width = "100%", selected = tempVal)
                         ),
                         column(width = 2, offset = 0,align="center",
                                actionButton(paste("deleteVariableSU", i ,sep = ""), NULL, class="toolButtonDelete", style="height: 35px;", icon = icon("times","fa-2x"))
                         )
                       ),sep="")
          }
          
          variablesToUseSU$otherVariablesSUUI <- s
        
      }, ignoreInit = TRUE,once = TRUE)
    })
    
    
    variablesToUseSU$otherVariablesSUUI <- s
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
  
  output$resultSUUI <- renderText({
    tryCatch({
      s <- ""
      if(class(resultSUUIValue$text) == "try-error"){
        s<- paste(
          div(id="resultUIDivError",
              column(style="margin-top: 20px;",
                     width=12,
                     box(id="resultUIBoxError",
                         title="ERROR",
                         style="border: 1px solid lightgray; font-size: x-large;min-height: 300px;",
                         width=12, solidHeader = FALSE, collapsible = FALSE,
                         p(resultSUUIValue$text$message)
                     )
                     
              )
          ),s,sep = "")
      }else if(resultSUUIValue$text != ""){
        s<- paste(
          div(id="resultUIDivSuccess",
              column(style="margin-top: 20px;",
                     width=12,
                     box(id="resultUIBoxSuccess",
                         title=paste("Copy the text and paste it into calculation column of row number:",input$rowNumberForCalculationBuilder),
                         style="border: 1px solid lightgray; font-size: x-large;min-height: 300px;",
                         width=12, solidHeader = FALSE, collapsible = FALSE,
                         p(resultSUUIValue$text)
                     )
                     
              )
          ),s,sep = "")
      }
      return(s)
    }, error = function(err) {
      return(infoBox(
        width = 12,strong("Error"),
        h4(err$message,align="center")
        ,icon = icon("times"),
        color = "red"
      ))
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
            h4(paste("You cannot proceed without",input$frameMMASelectInput,"file"),align="center")
            ,icon = icon("exclamation-triangle"),
            color = "yellow"
          )
        }
        
      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message,align="center")
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
        h4(err$message,align="center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })
  
  output$resultMMAUI <- renderText({
    tryCatch({
      s <- ""
      if(class(resultMMAUIValue$text) == "try-error"){
        s<- paste(
          div(id="resultUIDivError",
              column(style="margin-top: 20px;",
                     width=12,
                     box(id="resultUIBoxError",
                         title="ERROR",
                         style="border: 1px solid lightgray; font-size: x-large;min-height: 300px;",
                         width=12, solidHeader = FALSE, collapsible = FALSE,
                         p(resultMMAUIValue$text$message)
                     )
                     
              )
          ),s,sep = "")
      }else if(input$statisticalFunctionsMMASelectInput != "-- select --" && resultMMAUIValue$text != ""){
        s<- paste(
          div(id="resultUIDivSuccess",
              column(style="margin-top: 20px;",
                     width=12,
                     box(id="resultUIBoxSuccess",
                         title=paste("Copy the text and paste it into calculation column of row number:",input$rowNumberForCalculationBuilder),
                         style="border: 1px solid lightgray; font-size: x-large;min-height: 300px;",
                         width=12, solidHeader = FALSE, collapsible = FALSE,
                         p(resultMMAUIValue$text)
                     )
                     
              )
          ),s,sep = "")
      }
        
      return(s)
    }, error = function(err) {
      return(infoBox(
        width = 12,strong("Error"),
        h4(err$message,align="center")
        ,icon = icon("times"),
        color = "red"
      ))
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
            h4(paste("You cannot proceed without",input$frameD2SelectInput,"file"),align="center")
            ,icon = icon("exclamation-triangle"),
            color = "yellow"
          )
        }
        
      }
    }, error = function(err) {
      infoBox(
        width = 12,strong("Error"),
        h4(err$message,align="center")
        ,icon = icon("times"),
        color = "red"
      )
    })
  })
  
  output$resultD2UI <- renderText({
    tryCatch({
      s <- ""
      if(class(resultD2UIValue$text) == "try-error"){
        s<- paste(
          div(id="resultUIDivError",
              column(style="margin-top: 20px;",
                     width=12,
                     box(id="resultUIBoxError",
                         title="ERROR",
                         style="border: 1px solid lightgray; font-size: x-large;min-height: 300px;",
                         width=12, solidHeader = FALSE, collapsible = FALSE,
                         p(resultD2UIValue$text$message)
                     )
                     
              )
          ),s,sep = "")
      }else if(resultD2UIValue$text != ""){
        s<- paste(
          div(id="resultUIDivSuccess",
              column(style="margin-top: 20px;",
                     width=12,
                     box(id="resultUIBoxSuccess",
                         title=paste("Copy the text and paste it into calculation column of row number:",input$rowNumberForCalculationBuilder),
                         style="border: 1px solid lightgray; font-size: x-large;min-height: 300px;",
                         width=12, solidHeader = FALSE, collapsible = FALSE,
                         p(resultD2UIValue$text)
                     )
                     
              )
          ),s,sep = "")
      }
      return(s)
    }, error = function(err) {
      return(infoBox(
        width = 12,strong("Error"),
        h4(err$message,align="center")
        ,icon = icon("times"),
        color = "red"
      ))
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
  
  observeEvent(input$queryConverterButton,{
    if(sum(input$indicatorCaseSelectInput == "-- select --")){
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
    
    if(sum(input$indicatorCaseSelectInput=="Discretize a value")){
      tryCatch({ 
        ############################        Validation        ############################
        if (
          sum(input$frameDVSelectInput == "-- select --") ||
          sum(input$variableDVSelectInput == "-- select --") ||
          sum(input$breaksDVTextInput == "")
        ) {
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
        result <- "cut("
        result <- paste(result,input$frameDVSelectInput,"$",input$variableDVSelectInput, " ", sep="")
        result <- paste(result, ",c(", pre,"))" , sep = ""  )
        resultDVUIValue$text <- result
      }, error = function(err) {
        resultDVUIValue$text <- structure(c, class = "try-error")
      })
    }
    else if(sum(input$indicatorCaseSelectInput=="Re categorize a categorical variable by re coding modalities")){
      tryCatch({ 
        ############################        Validation        ############################
        if (
          sum(input$frameFRSelectInput == "-- select --") ||
          sum(input$variableFRSelectInput == "-- select --") ||
          sum(input$listnameFRSelectInput == "-- select --")
        ) {
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
        
        result <- "fct_recode("
        result <- paste(result,input$frameFRSelectInput,"$",input$variableFRSelectInput, ", ", sep="")
        factorValues <- choicesSheetFR()[choicesSheetFR()$list_name==input$listnameFRSelectInput,c("list_name", "name", "label")]
        for(i in 1:nrow(factorValues)){
          
          temp <- input[[paste(i,factorValues[i,"name"],sep = "-")]]
          
          result <- paste(result,"\"",temp,"\""," = ","\"",factorValues[i,"name"],"\"",sep = "")
          
          if(i!=nrow(factorValues)){
            result <- paste(result,", ",sep = "")
          }
        }
        result <- paste(result,")",sep = "")
        resultFRUIValue$text <- result
      }, error = function(err) {
        resultFRUIValue$text <- structure(c, class = "try-error")
      })
    }
    else if(sum(input$indicatorCaseSelectInput=="Sum up different numeric or integer variables") ){
      tryCatch({ 
        ############################        Validation        ############################
        if (
          sum(input$frameSUSelectInput == "-- select --")
        ) {
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
        
        result <- "psum("
        result <- paste(result,
                        input$frameSUSelectInput,
                        "$",
                        input$varSU1,
                        ", ",
                        input$frameSUSelectInput,
                        "$",
                        input$varSU2,
                        ifelse(length(variablesToUseSU$idOfVar)>0,
                        ", ",""), sep="")
        
        
        
        counter <- 1
        for(i in variablesToUseSU$idOfVar){
          val <- input[[paste("varSU", i, sep = "")]]
          result <- paste(result,
                          input$frameSUSelectInput,
                          "$",
                          val
                          , sep="")
          
          if(counter != length(variablesToUseSU$idOfVar)){
            result <- paste(result,", ", sep="")
          }
          counter <- counter + 1
        }

        
        result <- paste(result,")",sep = "")
        resultSUUIValue$text <- result
      }, error = function(err) {
        resultSUUIValue$text <- structure(c, class = "try-error")
      })
    }
    else if(sum(input$indicatorCaseSelectInput=="Calculate min, max or avg value for multiple integer or numeric variables")){
      tryCatch({ 
        ############################        Validation        ############################
        if (
          sum(input$frameMMASelectInput == "-- select --") ||
          sum(input$variableMMASelectInput == "-- select --") ||
          sum(input$statisticalFunctionsMMASelectInput == "-- select --")
          ) {
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
        
        
        varFrame <- paste(input$frameMMASelectInput,"$",input$variableMMASelectInput, sep="")
        if(sum(input$statisticalFunctionsMMASelectInput=="Minimum")){
          result <- paste("min(",varFrame," ,na.rm = TRUE)",sep = "")
        }else if(sum(input$statisticalFunctionsMMASelectInput=="Maximum")){
          result <- paste("max(",varFrame," ,na.rm = TRUE)",sep = "")
        }else if(sum(input$statisticalFunctionsMMASelectInput=="Average")){
          result <- paste("mean(",varFrame," ,na.rm = TRUE)",sep = "")
        }else if(sum(input$statisticalFunctionsMMASelectInput=="Median")){
          result <- paste("median(",varFrame," ,na.rm = TRUE)",sep = "")
        }else if(sum(input$statisticalFunctionsMMASelectInput=="Mode")){
          result <- paste("uniqv[which.max(tabulate(match(",varFrame, ", unique(",varFrame,"))))",sep = "")
        }else if(sum(input$statisticalFunctionsMMASelectInput=="Standard Deviation")){
          result <- paste("sd(",varFrame," ,na.rm = TRUE)",sep = "")
        }else if(sum(input$statisticalFunctionsMMASelectInput=="Interquartile Range")){
          result <- paste("IQR(",varFrame," ,na.rm = TRUE)",sep = "")
        }
        resultMMAUIValue$text <- result
      }, error = function(err) {
        resultMMAUIValue$text <- structure(c, class = "try-error")
      })
    }
    else if(sum(input$indicatorCaseSelectInput=="Calculate ratio by dividing 2 numeric or integer variables")){
      tryCatch({ 
        ############################        Validation        ############################
        if (
          sum(input$frameD2SelectInput == "-- select --") ||
          sum(input$variableD2SelectInput1 == "-- select --") ||
          sum(input$variableD2SelectInput2 == "-- select --")
        ) {
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
        
        
      
        result <- paste(input$frameD2SelectInput,"$",input$variableD2SelectInput1, " / ", sep="")
        result <- paste(result, input$frameD2SelectInput,"$",input$variableD2SelectInput2, sep="")
        resultD2UIValue$text <- result
      }, error = function(err) {
        resultD2UIValue$text <- structure(c, class = "try-error")
      })
    }
  })
  
  observeEvent(input$indicatorCaseSelectInput,{
    resultDVUIValue$text <- ""
    resultFRUIValue$text <- ""
    resultSUUIValue$text <- ""
    resultMMAUIValue$text <- ""
    resultD2UIValue$text <- ""
  })
  
  observe({
    tryCatch({
      if(FALSE){#if(!projectConfigurationInfo$log[["isRecordSettingsCompleted"]]){
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
          reqNames <- c(reqNames, "label", "label::Report")
          relabelingSurvey[,"label"] <- as.character(relabelingSurvey[,"label"])
        }else{
          reqNames <- c(reqNames, "label::Report")
        }    
        
        if("hint" %in% colnames(relabelingSurvey)){
          reqNames <- c(reqNames, "hint", "hint::Report")
          relabelingSurvey[,"hint"] <- as.character(survey[,"hint"])
        }else{
          reqNames <- c(reqNames, "hint::Report")
        } 
        
        if ("label::Report" %in% colnames(relabelingSurvey)) {
          relabelingSurvey["label::Report"] = substr(relabelingSurvey[,"label::Report"],1,80)
        }
        if (!"label::Report" %in% colnames(relabelingSurvey)) {
          if("label" %in% colnames(relabelingSurvey)){
            relabelingSurvey["label::Report"] = substr(relabelingSurvey[,"label"],1,80)
          }else{
            relabelingSurvey["label::Report"] = ""
          }
        }
        if ("hint::Report" %in% colnames(relabelingSurvey)) {
          relabelingSurvey["hint::Report"] = substr(relabelingSurvey[,"hint::Report"],1,80)
        }
        if (!"hint::Report" %in% colnames(relabelingSurvey)) {
          if("hint" %in% colnames(relabelingSurvey)){
            relabelingSurvey["hint::Report"] = substr(relabelingSurvey[,"hint"],1,80)
          }else{
            relabelingSurvey["hint::Report"] = ""
          }
        }
        relabelingSurvey[,"label::Report"] <- as.character(relabelingSurvey[,"label::Report"])
        relabelingSurvey[,"hint::Report"] <- as.character(relabelingSurvey[,"hint::Report"])
        relabelingSurvey <- relabelingSurvey[reqNames]
      }
      
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
          reqNames <- c(reqNames, "label", "label::Report")
          relabelingChoices[,"label"] <- as.character(relabelingChoices[,"label"])
        }else{
          reqNames <- c(reqNames, "label::Report")
        }    
  
        if ("label::Report" %in% colnames(relabelingChoices)) {
          relabelingChoices["label::Report"] = substr(relabelingChoices[,"label::Report"],1,80)
        }
        if (!"label::Report" %in% colnames(relabelingChoices)) {
          if("label" %in% colnames(relabelingChoices)){
            relabelingChoices["label::Report"] = substr(relabelingChoices[,"label"],1,80)
          }else{
            relabelingChoices["label::Report"] = ""
          }
        }
        relabelingChoices[,"label::Report"] <- as.character(relabelingChoices[,"label::Report"])
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
        selectOneType$variable <- as.character(selectOneType$variable)
        
        selectOneType$disaggregation <- as.logical(selectOneType$disaggregation)
        selectOneType$structuralequation.risk <- as.logical(selectOneType$structuralequation.risk)
        selectOneType$structuralequation.coping <- as.logical(selectOneType$structuralequation.coping)
        selectOneType$structuralequation.resilience <- as.logical(selectOneType$structuralequation.resilience)
        selectOneType$anonymise <- as.logical(selectOneType$anonymise)
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
      survey <- survey[survey[,"variable"]=="ordinal factor",]
      survey <- survey[startsWith(tolower(survey[,"type"]), "select_one"),]
      
      varOfOrder <- sapply(survey[,"type"], function(x) {
        strsplit(x," ")[[1]][2]
      }, simplify = TRUE, USE.NAMES = FALSE)
  
      choices <- as.data.frame(read_excel(form_tmp, sheet = "choices"),
                               stringsAsFactors = FALSE)
      reqNames <- c("list_name",  "name", "label", "order")
      if(sum(sapply(reqNames, function(x){x %in% colnames(choices)})) != length(reqNames)){
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
        selectMultipleType$anonymise <- as.logical(selectMultipleType$anonymise)
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
        numericType$anonymise <- as.logical(numericType$anonymise)
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
        
        dateType[tolower(dateType$type) %in% c("date") ,"variable"] = "date"
        dateType[tolower(dateType$type) %in% c("time") ,"variable"] = "time"
        dateType[tolower(dateType$type) %in% c("datetime") ,"variable"] = "datetime"
        
        
        #dateType$chapter <- as.character(dateType$chapter)
        dateType$variable <- as.character(dateType$variable)
        
        dateType$disaggregation <- as.logical(dateType$disaggregation)
        dateType$structuralequation.risk <- as.logical(dateType$structuralequation.risk)
        dateType$structuralequation.coping <- as.logical(dateType$structuralequation.coping)
        dateType$structuralequation.resilience <- as.logical(dateType$structuralequation.resilience)
        dateType$anonymise <- as.logical(dateType$anonymise)
        dateType$correlate <- as.logical(dateType$correlate)
        dateType$clean <- as.logical(dateType$clean)
        dateType$cluster <- as.logical(dateType$cluster)
        dateType$predict <- as.logical(dateType$predict)
        dateType$mappoint <- as.logical(dateType$mappoint)
        dateType$mappoly <- as.logical(dateType$mappoly)
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

