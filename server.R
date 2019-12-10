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
  }, options = list(pageLength = 10, searching = FALSE, scrollX = TRUE)
  )
  
  output$progressBox <- renderValueBox({
    infoBox(
      "No. of Rows", paste0(nrow(myData())), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "No. of Columns", paste0(ncol(myData())), icon = icon("cog", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })

}
