library(shiny) 

ui <- fluidPage( 
  pageWithSidebar(
    headerPanel("Continous random variables"),
    sidebarPanel(
      selectInput("conmodel","Select Model",
                  choices = c("Normal" = "normal",
                              "Exponential" = "exponential",
                              "Uniform" = "uniform"),
      ),
      sliderInput("s","number of simulated data", min=1,max=1000,value=10),
      
      conditionalPanel(
        condition = "input.conmodel == 'exponential'",
        numericInput("lam","Parameter lambda in exponential", value = 1)
      ),
      
      
      
      conditionalPanel(
        condition = "input.conmodel == 'normal'",
        numericInput("mu","Parameter mu in normal",value = 0),
        numericInput("sigma","parameter sigma in normal", value = 1)
      ),
      
      numericInput("i","support", value = 2),
      
      conditionalPanel(
        condition = "input.connmodel == 'normal'",
        numericInput("j1","j in normal", value = 0)
      ),
      
      
      conditionalPanel(     
        condition = "input.conmodel == 'exponential'",
        numericInput("j2", "j in exponential" , value = 0)
      ), 
      
      
      
      conditionalPanel( 
        
        condition = "input.conmodel == 'uniform'", 
        
        numericInput("a", "parameter a in Normal" , value = -2),  
        
        numericInput("b", "parameter b in Normal" , value = 0.8) 
        
      ) 
      
    ),
    
    # mainPanel(  
    
    #   plotOutput("graphout"),  
    
    #   tableOutput('tab'), 
    
    #   tableOutput('prob')  
    
    #  ) 
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Graph",plotOutput("graphout")), #Plot
                  tabPanel("Random Number",DT:dataTableOutput("tab")),# Table
                  tabPanel("Random Number", verbatimTextOutput("prt")), #Print
                  tabPanel("Prediction",tableOutput('prob')) #Print
      )
    )
    
  )
)


server <-  function(input, output){
  output$graphout <- renderPlot({
    
    #normal
    if(input$connModel == 'normal'){
      par(mfrow = c(1,2))
      x = seq(-input$i,input$i,0.01)
      plot(x,dnorm(x,input$mu,input$sigma),type='I',col='red')
    }
    
    #exponential
    if(input$connmodel == 'exponential'){
      #exponential
      par(mfrow=c(1,2)) 
      
      x=seq(0,input$i,0.01)  
      
      plot(x,dexp(x,input$lam),type='l',col='green') 
    }
    
    if (input$conmodel == 'uniform') { 
      
      a <- input$a 
      
      b <- input$b 
      
      n1 <- input$s 
      
      
      
      rand.unif <- runif(n1, min = a, max = b) 
      
      
      
      hist(rand.unif,  
           
           freq = FALSE,  
           
           xlab = 'x',   
           
           ylim = c(0, 0.4), 
           
           xlim = c(-3,3), 
           
           density = 20, 
           
           main = "Uniform distribution") 
      
      
      
      
      
      curve(dunif(x, min = a, max = b),  
            
            from = -3, to = 3,  
            
            n = n1,  
            
            col = "darkblue",  
            
            lwd = 2,  
            
            add = TRUE,  
            
            yaxt = "n", 
            
            ylab = 'probability') 
      
      
      
      
      
    } 
    
    
    
  }) 
  
  
  
  output$prt <- renderPrint({  
    
    Normal=rnorm(input$s,input$mu, input$sigma)  
    
    Exp=rexp(input$s,input$lam)  
    
    
    
    if (input$conmodel == 'exponential') { 
      
      print(Exp) 
      
      
      
    } 
    
    else 
      
    { 
      
      print(Normal) 
      
    } 
    
    
    
  }) 
  
  
  
  
  
  
  
  
  
  
  
  output$tab <- DT::renderDataTable({  
    
    Normal=rnorm(input$s,input$mu, input$sigma)  
    
    Exp=rexp(input$s,input$lam)  
    
    
    
    if (input$conmodel == 'exponential') { 
      
      DT::datatable(data.frame(Exp), options = list(lengthChange = TRUE)) 
      
      
      
    } 
    
    else 
      
    { 
      
      DT::datatable(data.frame(Normal), options = list(lengthChange = TRUE)) 
      
    } 
    
    
    
    
    
    
    
  })  
  
  
  
  output$prob <- renderPrint({  
    
    p1=pnorm(input$j1,input$mu, input$sigma)  
    
    p2=pexp(input$j2,input$lam)  
    
    
    
    if (input$conmodel == 'exponential') { 
      
      print(p2)  
      
    } 
    
    
    
    if (input$conmodel == 'normal') { 
      
      print(p1)  
      
    } 
    
    
    
    
    
  })
}

shinyApp(ui, server)