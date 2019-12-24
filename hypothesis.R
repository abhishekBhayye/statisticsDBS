library(shiny) 

ui <- pageWithSidebar(
  # title
  headerPanel("T-test for Two- and  -One variable "),
  
  #input
  sidebarPanel
  (
    h5("Search Bar"),
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
               h3(textOutput('caption')),
               tabsetPanel(
                 tabPanel("One Variable : T-Test",
                          fluidRow(
                            column(12,
                                   h3("Random Numbers for X"),
                                   verbatimTextOutput("summary"),br(),hr(),
                                   
                                   h3("T-Test for mean"),
                                   verbatimTextOutput("corr"),br(),hr()
                            )
                          )
                 ) ,
                 tabPanel("Two variable T-Test",
                          fluidRow(
                            column(12,hr(),h3("Results For test"),hr(),
                                   
                                   h3("Random Numbers for X"),
                                   verbatimTextOutput("tdataone"),br(),hr(),
                                   
                                   h3("Random Numbers for Y"),
                                   verbatimTextOutput("tdatatwo"),br(),hr(),
                                   
                                   h3("T-Test for (x,y) "),
                                   verbatimTextOutput("trestwo"),br(),hr() 
                                   
                            )
                          )
                 )
               )
             )
  )
)


server<- function(input, output, session) {
  
  
  observeEvent(input$do, {   
    library(distr)
    
    lowertail <- input$rangex[1]
    uppertail <- input$rangex[2]
    total_random_variables <- input$nrands
    
    dataX <-runif(total_random_variables, min=lowertail, max= uppertail )
    
    
    dataX.df <- as.data.frame( dataX )
    
    
    output$summary<-renderPrint({
      dataX.df
    })
    
    value = ifelse(input$greater,"g","l")
    result <- t.test(dataX.df,alternative=value,mu=0.3)
    
    output$corr<-renderPrint({
      result
    })
    
    
    #__________________________________________________________________________________
    
    
    lowertail <- input$rangey[1]
    uppertail <- input$rangey[2]
    total_random_variables <- input$nrands
    
    dataY <-runif(total_random_variables, min=lowertail, max= uppertail)
    
    
    dataY.df <- as.data.frame( dataY )
    
    output$tdataone<-renderPrint({
      dataX.df
    })
    
    output$tdatatwo<-renderPrint({
      dataY.df
    })
    
    value = ifelse(input$greater,"g","l")
    result <- t.test(dataX.df, dataY.df, alternative=value,mu=0.3)
    
    output$trestwo<-renderPrint({
      result
    })
    
    #newdata <- input$files
    #confirm load
    output$caption<-renderText({
      
      if(is.null(input$files)) { "Load Data" }  else { "Data loaded" }
      
      
    })
  })
}



shinyApp(ui, server)
