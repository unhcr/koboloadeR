#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$projectConfiguration <- renderUI({
    fluidRow(
      box(id="doYouHaveFormBox",
          width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
          column(width = 4, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                 h4("Do you have the xlsform?")
          ),
          column(width = 1, offset = 0,
                 selectInput("doYouHaveFormSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
          ),
          column(width = 12,
                 conditionalPanel(
                   condition = "input.doYouHaveFormSelectInput == 'Yes'",
                   fileInput('xlsFormUploadedFile', 'Choose you xls form',
                             accept=c('.xls'))
                 )
          )
      ),
      conditionalPanel(
        condition = "input.doYouHaveFormSelectInput == 'No'",
        box(id="doYouWantGenerateFormBox",
            width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
            conditionalPanel(
              condition = "input.doYouHaveFormSelectInput == 'No'",
              column(width = 4, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                     h4("Do you want to generate xlsform from your Data file?")
              ),
              column(width = 1, offset = 0,
                     selectInput("doYouWantGenerateFormSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
              )
            )
        )
      ),
      conditionalPanel(
        condition = "(input.doYouHaveFormSelectInput == 'Yes' || input.doYouWantGenerateFormSelectInput == 'Yes') && input.doYouHaveFormSelectInput != '-- select --'",
        box(id="doYouHaveDataBox",
            width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
            column(width = 4, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                   h4("Do you have the Data file?")
            ),
            column(width = 1, offset = 0,
                   selectInput("doYouHaveDataSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
            ),
            column(width = 12,
                   conditionalPanel(
                     condition = "input.doYouHaveDataSelectInput == 'Yes'",
                     fileInput('dataUploadedFile', 'Choose you Data file',
                               accept=c('.csv'))
                   )
            )
        )
      ),
      conditionalPanel(
        condition = "input.doYouHaveFormSelectInput == 'Yes'",
        box(id="doYouHaveDataBox",
            width=12,status="primary", solidHeader = FALSE, collapsible = FALSE,
            column(width = 4, style = "border-bottom: 1px solid lightgray; border-right: 1px dotted lightgray; border-bottom-right-radius: 7px;",
                   h4("Does xlsform include documented analysis plan & settings?")
            ),
            column(width = 1, offset = 0,
                   selectInput("doesFormNeedToPrepareSelectInput", label = NULL,choices = c("-- select --","Yes","No"))
            )
        )
      ),
      conditionalPanel(
        condition = "input.doYouHaveDataSelectInput == 'No'",
        infoBox(
          width = 12,strong("Warning"),h4("You cannot proceed without data file",align="center"),
          paste(getwd(), "data", "/", sep = "/", collapse = "/")
          ,icon = icon("exclamation-triangle"),
          color = "orange"
        )
      )
      
    )
  })
  
})
