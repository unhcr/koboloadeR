#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyalert)
kobo_projectinit()
header <- dashboardHeader(title = "koboloadeR Package",
                          titleWidth = 300
)

sidebar <- dashboardSidebar(width = 300,
                            sidebarMenu(
                              menuItem("Project Configuration", icon = icon("braille", class = "fa-1x"), tabName = "pc"),
                              menuItem("Analysis Plan Configuration", icon = icon("cogs", class = "fa-1x"), tabName = "apc"),
                              menuItem("Data Processing", icon = icon("table", class = "fa-1x"), tabName = "dp"),
                              menuItem("Reports Generation", icon = icon("microchip", class = "fa-1x"), tabName = "rg"),
                              menuItem("Data Dissemination", icon = icon("file", class = "fa-1x"), tabName = "dd")
                            )
)

body <- dashboardBody(useShinyalert(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),                      
  tabItems(
    tabItem(tabName = "pc",
            uiOutput("projectConfiguration")
    ),
    tabItem(tabName = "apc",
            uiOutput("analysisPlanConfiguration")
    ),
    tabItem(tabName = "dp",
            uiOutput("dataProcessing")
    ),
    tabItem(tabName = "rg",
            uiOutput("reportsGeneration")
    ),
    tabItem(tabName = "dd",
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
server <- shinyServer(function(input, output) {
  options(shiny.maxRequestSize=10000*1024^2) #make the limit up to 10GB
  mainDir <- reactive({
    gsub("/inst/shiny_app", "",  getwd())
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
        projectConfigurationInfo$log[["isPrepared"]] <- FALSE
        projectConfigurationInfo$log[["isGenerated"]] <- FALSE
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
    projectConfigurationInfo$log[["doesFormNeedToPrepareSelectInput"]] <- input$doesFormNeedToPrepareSelectInput
    
    
    if(is.null(projectConfigurationInfo$log[["data"]])){
      projectConfigurationInfo$log[["data"]] <- FALSE
    }
    if(is.null(projectConfigurationInfo$log[["xlsForm"]])){
      projectConfigurationInfo$log[["xlsForm"]] <- FALSE
    }
    
    if(is.null(projectConfigurationInfo$log[["beginRepeatList"]])){
      projectConfigurationInfo$log[["beginRepeatList"]]  <- FALSE
    }

  })
  
  output$projectConfiguration <- renderUI({
    fluidRow(
      box(id="doYouHaveFormBox",
          width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
          column(width = projectConfigurationTheme$questionsWidth, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
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
              column(width = projectConfigurationTheme$questionsWidth, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
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
            column(width = projectConfigurationTheme$questionsWidth, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
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
                               accept=c('.csv'))),
                     column(width = 3, style = "border-left: 1px solid lightgray; margin-top: 10px;",
                            radioButtons('dataUploadedFileSep', 'Separator',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t'),
                                  ',', inline =TRUE)),
                     column(width = 12,
                            actionButton("dataUploadFileButton", "Upload file", icon("upload"), class="uploadButton",
                                         style="width:100%; margin-bottom: 20px; ")
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
              column(width = projectConfigurationTheme$questionsWidth, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
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
                              actionButton("saveDataFilesButton", "Upload and Save files", icon("upload"), class="uploadButton", style="margin: 15px 0px;")
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
            column(width = projectConfigurationTheme$questionsWidth, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                   h4("Does xlsform include documented analysis plan & settings?")
            ),
            column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                   selectInput("doesFormNeedToPrepareSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
            )
        )
      ),
      conditionalPanel(
        condition = "input.doYouHaveDataSelectInput == 'No' || input.doYouWantGenerateFormSelectInput == 'No' || input.doYouHaveDatasetsSelectInput == 'No'",
        infoBox(
          width = 12,strong("Warning"),h4("You cannot proceed without data file",align="center")
          ,icon = icon("exclamation-triangle"),
          color = "yellow"
        )
      ),
      
      conditionalPanel(
        condition = "(input.doYouHaveFormSelectInput == 'No' && input.doYouWantGenerateFormSelectInput == 'Yes' && input.doYouHaveDataSelectInput == 'Yes') ||
        (input.doYouHaveFormSelectInput == 'Yes' && input.doYouHaveDatasetsSelectInput == 'Yes' && input.doesFormNeedToPrepareSelectInput == 'No')",
        
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
        (input.doYouHaveFormSelectInput == 'Yes' && input.doYouHaveDatasetsSelectInput == 'Yes' && input.doesFormNeedToPrepareSelectInput == 'No')",
        div(id="recordSettingsDiv",
          box(id="recordSettingsBox", title = "Record Settings Configuration",
              width=12,status="primary", solidHeader = FALSE, collapsible = TRUE,
              column(width = 12, align="left",
                     uiOutput("recordSettingsUI")
              )
          )
        )
      )
      
      
      
    )
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
          
          write.csv(dataFile,  paste(mainDir(), "data", fileName, sep = "/", collapse = "/"))
        }
      }
      updateProgress()
      projectConfigurationInfo$log[["subAndMainfiles"]] <- TRUE
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
        write.csv(dataFile,  paste(mainDir(), "data", "/data.csv", sep = "/", collapse = "/"))
        projectConfigurationInfo$log[["data"]] <- TRUE
        projectConfigurationInfo$data[["data"]] <- dataFile
        projectConfigurationInfo$log[["isGenerated"]] <- FALSE
        
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
         (sum(input$doYouWantGenerateFormSelectInput == "Yes") &&  projectConfigurationInfo$log[["isGenerated"]] == FALSE)
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
         sum(input$doesFormNeedToPrepareSelectInput == "No") &&
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
                                              accept=c('.csv'))
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
        sum(input$doesFormNeedToPrepareSelectInput == "No") &&
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
          column(width = projectConfigurationTheme$questionsWidth, style = "border-bottom: 1px dotted lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
               h4("What sampling do you have?")
          ),
          column(width = projectConfigurationTheme$yesNoInputWidth, offset = 0,
                 selectInput("samplingSelectInput", label = NULL,choices = c("-- select --",
                                                                             "No sampling(type 1)",
                                                                             "Cluster sample (type 2)",
                                                                             "Stratified sample (type 3)",
                                                                             "Respondent Driven Sample sample (type 4)"
                                                                             ))
          )
        ),
       
        column(12, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px; margin-bottom: 20px;",
               column(width = projectConfigurationTheme$questionsWidth, style = "border-bottom: 1px dotted lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
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
               column(width = 12,
                      conditionalPanel(
                        condition = "input.cleaningLogSelectInput == 'Yes'",
                        column(width = 9, style = "padding-left: 0px;",
                               fileInput('cleaningLogFileInput', 'Choose your Data file',
                                         accept=c('.csv'))),
                        column(width = 3, style = "border-left: 1px solid lightgray; margin-top: 10px;",
                               radioButtons('cleaningLogSep', 'Separator',
                                            c(Comma=',',
                                              Semicolon=';',
                                              Tab='\t'),
                                            ',', inline =TRUE))
                      )
               )
        )
      
      
    ), s ,sep="" )
    
    return(s)
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

