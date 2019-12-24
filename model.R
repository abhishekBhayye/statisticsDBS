library(shiny) 

ui <- fluidPage(
  h5("Please choose the model and  to get output graphs"),
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


server <-  function(input, output){
  output$histogram <- renderPlot({ 
    
    # binomial  
    if (input$dismodel == 'binomial') { 
      par(mfrow=c(1,2))  
      d <- density(rbinom(1000,input$n,input$p))  
      plot(d, main="Kernel Density of generated data")  
      polygon(d, col="red", border="blue") 
      x=0:input$n  
      plot(x,dbinom(x,input$n,input$p))  
    } 
    
    # poisson 
    if (input$dismodel == 'poisson') { 
      par(mfrow=c(1,2))   
      D=rpois(input$s, input$lam)  
      tab=table(D)  
      barplot(tab,col='blue')  
      x1=0:input$max  
      y1=dpois(x1,input$lam)  
      plot(x1,y1,type='b')  
    } 
    
    # geometric  
    if (input$dismodel == 'geometric') { 
      par(mfrow=c(1,2)) 
      D=rgeom(input$s, input$p)  
      tab=table(D)  
      barplot(tab,col='blue')  
      x2=0:input$max  
      y2=dgeom(x2,input$p)  
      plot(x2,y2,type='b')  
    } 
    
    # bernoulli      
    if (input$dismodel == 'bernoulli') { 
      par(mfrow=c(1,2))
      Density <- density(rbinom(input$s,1,input$p))
      plot(Density, main="Kernel Density of generated data")
      polygon(Density, col="red", border="blue")
      x=0:1
      plot(x,dbinom(x,1,input$p))
    }
    
    # hypergeometric
    if (input$dismodel == 'hypergeometric') { 
      par(mfrow=c(1,2))
      D=rhyper(nn=input$s, m=input$m, n=input$n, k=rep(input$k, input$s))
      tab=table(D)
      barplot(tab,col='blue')
      x2=0:input$s
      y2=dhyper(x2, m=input$m, n=input$n, k=input$k, log=FALSE)
      plot(x2,y2,type='b')
    }
  }) 
}

shinyApp(ui, server)