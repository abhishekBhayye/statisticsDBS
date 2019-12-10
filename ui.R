library(shiny)
library(shinydashboard)
library(markdown)
library(datasets)

dashboardPage(
  
  dashboardHeader(title = "Basic dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Choose Database", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Overview", tabName = "overview", icon = icon("bible")),
      menuItem("Model", tabName = "model", icon = icon("dashboard")),
      menuItem("Hypothesis", tabName = "hypothesis", icon = icon("th")),
      menuItem("GLM", tabName = "glm", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      # First tab content
      tabItem(
        tabName = "dashboard",
        h2("Step 1: Choose Your Dataset"),
        fluidRow(
          box(
            width = 4,
            h4("You can either choose your database from the existing databases 
               in 'R' library or you can choose your own database. This database
               will be used fully in entire app for any procedure. In order to
               make any changes in databases, do it over here.")
          ),
          
          box(
            
            selectInput("dataset", "Select One",
                        choices = c("Choose .CSV File" = "csv" ,"In-build Dataset" = "inbuild")),
            
            conditionalPanel(
              condition = "input.dataset == 'csv'",
              fileInput("datafile", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              )
              
            )
            
            # conditionalPanel(
            #   condition = "input.dataset == 'inbuild'",
            #   selectInput("inBuild", "In-Build Dataset",
            #               choices = c(data(package = .packages(all.available = TRUE))$results)
            #   )
            # ),
            
            
          )
        )
      ),
      
      # Second tab content
      tabItem(tabName = "overview",
        h5("You will see all the information regarding the dataset here."),
        br(),
        fluidRow(
          
          valueBoxOutput("progressBox"),

          infoBoxOutput("progressBox2")
        ),
        fluidRow(
          column(
            dataTableOutput("value"), width = 12)
        )
        
      ),
      
      # Third tab content
      tabItem(tabName = "model",
        "3",
        source("model.R")
      ),
      
      # Fourth tab content
      tabItem(tabName = "hypothesis",
        "4",
        source("hypothesis.R")
      ),
      
      # Fifth tab content
      tabItem(tabName = "glm",
        "5",
        source("hypothesis.R")
      )
    )
  )
)
