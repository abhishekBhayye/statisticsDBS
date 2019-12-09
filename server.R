library(shiny)

function(input, output){
  dataForInput <- reactiveVal(NULL)
  
  myData <- reactive({
    if(input$dataset == "csv"){
      file1 <- input$datafile
      if (is.null(file1)) {
      return()
      }
      dataForInput = read.csv(file=file1$datapath)
      return(dataForInput)
    }
    
    if(input$dataset == "inbuild"){
      dataForInput <- input$inBuild
    }
  })
  
  
  output$value <- renderDataTable({
    myData()
  })
  
  

}
