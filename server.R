library(shiny)

function(input, output){
  
  output$histogram <- renderPlot({
    
    if(input$models == "bino"){
      par(mfrow=c(2,1))
      d <- density(rbinom(1000,input$n,input$p))
      plot(d, main = "Binomial Distribution of generaed Data")
      polygon(d, col = "yellow", border = "red")
      x=0:input$n
      plot(x,dbinom(x,input$n,input$p))
    }
    
  })
}
