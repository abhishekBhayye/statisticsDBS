### Application name : CA1 B9DA101 - Statistics for Data Analytics 

### Course : MSc (Data Analytics) - Sep 2019 - Group <C>  

### Developed by :
###   Abhishek Bhayye (10527126)
###   Ashwini Kadam (10527552)
###   Himaja Mineni (10529371)
###   Makarand Tawde (10527342) 

### College : Dublin Business School  

###>>> Begin >>> Abhishek Bhayye (10527126) App Structure with overview>>> 

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
        ),
        
        fluidRow(
          column(
            dataTableOutput("value"), width = 12)
        )
      ),
      
      # Second tab content
      tabItem(tabName = "overview",
        h3("You will see all the information regarding the dataset here."),
        br(),
        fluidRow(
          
          valueBoxOutput("progressBox"),

          infoBoxOutput("progressBox2")
        )
      ),
      
      ###>>> Begin >>> Ashwini Kadam(Student#10527552) >>> 
      
      # Third tab content
      tabItem(tabName = "model",
        h2("Discrete Probability Models"),
        fluidPage(
          h3(""),
          sidebarPanel( 
            selectInput("dismodel", "Select Model", 
                        choices = c(
                          "Bernoulli" = "bernoulli",
                          "Binomial" = "binomial", 
                          "Poisson" = "poisson", 
                          "Geometric" = "geometric",
                          "Hypergeometric" = "hypergeometric"), 
                        selected = "bernoulli" 
            ), 
            
            conditionalPanel(
              condition = "input.dismodel == 'bernoulli'",
              # Slider input for the probability of successful trail
              sliderInput("p", "Probability of successful trail(p)", min=0, max=1, step = 0.01, value = 0.5)
            ),
            
            conditionalPanel( 
              condition = "input.dismodel == 'binomial'", 
              numericInput("n", "parameter n in Binomial" , value = 10), 
              numericInput("p", "parameter p in Binomial" , value = 0.5) 
            ), 
            
            conditionalPanel(     
              condition = "input.dismodel == 'poisson'", 
              numericInput("lam", "parameter lambda in Poisson" , value = 1) 
            ), 
            
            conditionalPanel(     
              condition = "input.dismodel == 'geometric'", 
              numericInput("p", "parameter p in Geometric" , value = 0.5) 
            ), 
            
            numericInput("max", "upper limit for x" , value = 5),  
            sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10),  
            
            conditionalPanel( 
              condition = "input.dismodel == 'binomial'", 
              numericInput("j1", "j for Bin" , value = 1) 
            ), 
            
            conditionalPanel( 
              condition = "input.dismodel == 'poisson'", 
              numericInput("j2", "j for Poisson" , value = 1) 
            ), 
            
            conditionalPanel( 
              condition = "input.dismodel == 'geometric'", 
              numericInput("j3", "j for geometric" , value = 1) 
            ),
            
            conditionalPanel(
              condition = "input.dismodel == 'hypergeometric'",
              numericInput("m", "M" , value = 10),
              numericInput("n", "N" , value = 20),
              numericInput("k", "K" , value = 5)
            )
          ),  
          
          mainPanel(  
            plotOutput("histogram") 
            # tableOutput('tab')  
          )
        )
      ),
      
      ###>>> Begin >>> Himaja Mineni (10529371) >>>
      
      # Fourth tab content
      tabItem(tabName = "hypothesis",
        h2("Overview of Hypothesis using T-test"),
        pageWithSidebar(
          # title
          headerPanel(""),
          
          #input
          sidebarPanel
          (
            sliderInput("rangex", "Select Parameter  X:", 
                        min = 1, max = 100,  value = c(1,50), step= 1),
            sliderInput("rangey", "Select {Range Parameter for Y:", 
                        min = 1, max = 100,  value = c(1,50), step= 1),
            numericInput("nrands","NO Random Numbers to be Generated?", value = 20,min = 5,step = 1),
            h3("Alternative Attribute in t.test Function"),
            checkboxInput("greater","Greater", value = TRUE),
            checkboxInput("less","Less"),
            actionButton("do", "Test_of_mean")
          ), 
          mainPanel( width = 8,
            fluidRow(
              h3(""),
              tabsetPanel(
                tabPanel("One Variable : T-Test",
                  fluidRow(
                    column(12,
                           
                      box(
                        width = 3,
                        h5("Random Numbers for X"),
                        verbatimTextOutput("summary")  
                      ),
                      
                      box(
                        width = 9,
                        h5("T-Test for mean"),
                        verbatimTextOutput("corr") 
                      )
                    )
                  )
                ),
                tabPanel("Two variable T-Test",
                  fluidRow(
                    column(12,
                      
                      box(
                        width = 3,
                        h5("Random Numbers for X"),
                        verbatimTextOutput("tdataone")
                      ),
                      
                      box(
                        width = 3,
                        h5("Random Numbers for Y"),
                        verbatimTextOutput("tdatatwo")  
                      ),
                      
                      box(
                        width = 6,
                        h5("T-Test for (x,y) "),
                        verbatimTextOutput("trestwo")  
                      )
                    
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      ###>>> Begin >>> Makarand Tawde (10527342) >>>
      
      # Fifth tab content
      tabItem(tabName = "glm",
        h2("Prediction using GLM"),
        pageWithSidebar(
          headerPanel(""),
          
          sidebarPanel(
            
            #Selector for file upload
            fileInput('datafile', 'Choose CSV file',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain')),
            #These column selectors are dynamically created when the file is loaded
            #uiOutput("Independent"),
            #uiOutput("Dependent"),
            helpText("In case of Continuous target var choose atleast 2 independent variables"),
            selectInput("columns",label="Please select independent variables",multiple=TRUE,choices =""),
            selectInput("columns2",label="Please select dependent variable",choices =""),
            selectInput("Distribution","Please select distribution type",
                        choices = c("Discrete","Continuous"))
            
          ),
          mainPanel(
            tabsetPanel(type = 'tab',
                        tabPanel("Selected cols",DT::dataTableOutput("selData")),
                        tabPanel("Actual vs Predicted",plotOutput("plot")),
                        tabPanel("Summary",DT::dataTableOutput("Summary")),
                        tabPanel("Measures",DT::dataTableOutput("Measures")),
                        tabPanel("Prediction",DT::dataTableOutput("Predictions")))
          )
        )
      )
    )
  )
)
