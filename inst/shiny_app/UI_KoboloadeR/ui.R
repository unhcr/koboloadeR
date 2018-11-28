#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
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

body <- dashboardBody(
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
