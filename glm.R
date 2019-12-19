library(shiny)
library(pdfetch)
library(MASS)


server <- function(input, output,session) {
  RMSE <- 0
  values = reactiveValues()
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  
  
  observe({
    updateSelectInput(session,"columns",choices=colnames(filedata()))
    updateSelectInput(session,"columns2",choices=colnames(filedata()))
  })
  
  #GLM logic
  #x1 <- data$gre     
  #x2 <- data$gpa 
  #x3 <- data$rank 
  #y  <- data$admit 
  output$glmperf <- renderPlot({
    df <-na.omit(filedata())
    tarinddata <- cbind(df[,input$columns2],df[,input$columns])
    colnames(tarinddata) = c(input$columns2,input$columns)
    colnames(tarinddata)[1] <- "Y"
    set.seed(199) 
    
    #fit.glm <- glm(y ~.,dataset, family="binomial") # ~. shows that we include all ind. variables 
    
    #summary(fit.glm)  
    
    n = nrow(tarinddata)  
    
    indexes = sample(n,n*(80/100))  
    
    trainset = data.frame(tarinddata[indexes,] ) 
    
    testset = data.frame(tarinddata[-indexes,] )
    
    # Fit the full model  
    
    actual=testset$Y
    pred_test <- data.frame(testset)
    
    full.model <- glm(Y ~.,data=trainset, family="gaussian")
    
    values$full <- full.model
    
    pred_full <- predict(full.model,testset[,input$columns])
    
    rmse_full = sqrt(sum((pred_full -actual)^2)/nrow(testset))
    
    
    
    
    reduced.model =stepAIC(full.model) 
    values$full = full.model
    values$reduced <- reduced.model
    pred_red = predict(reduced.model,testset[,input$columns])
    rmse_red = sqrt(sum((pred_red -actual)^2)/nrow(testset))
    
    values$rmse <- data.frame('Full'=rmse_full,'Reduced'=rmse_red)
    
    par(mfrow=c(1,2))
    plot(actual,type='o',col='red',xlab = 'observations',ylab=input$columns2,main='FULL')
    
    lines(pred_full,type='o',col='blue')
    
    legend(
      "topleft",
      lty=c(1,1),
      col=c("red","blue"),
      legend=c("Real","Predicted")
    )
    
    plot(actual,type='o',col='red',xlab = 'observations',ylab=input$columns2,main='Reduced')
    
    lines(pred_red,type='o',col='blue')
    
    legend(
      "topleft",
      lty=c(1,1),
      col=c("red","blue"),
      legend=c("Real","Predicted")
    )
    
    
    
  })
  
  
  output$selData <- DT::renderDataTable({
    df <- filedata()
    tarinddata <- cbind(df[,input$columns2],df[,input$columns])
    colnames(tarinddata) = c(input$columns2,input$columns)
    
    DT::datatable(tarinddata,options = list(lengthChange = TRUE))
  })
  
  output$RMSE <- DT::renderDataTable({
    DT::datatable(values$rmse,options=list(lengthChange=TRUE))
    
  })
  
  
  
  
  
  forecast_out <- reactive({
    Var_Count <- length(input$columns)
    new_data <- as.numeric(paste(lapply(1:Var_Count,function(i){
      inputName <- paste0(input$columns[i])
      input[[inputName]]
    })))
    input_data <- data.frame(t(new_data))
    
    for (i in 1:Var_Count)
    {
      colnames(input_data)[i] <- input$columns[i]
    }
    
    new_predict_full <- predict(values$full,input_data)
    new_predict_red <- predict(values$reduced,input_data)
    
    
    pred_data_new <- data.frame(new_predict_full,new_predict_red)
    
    colnames(pred_data_new)[1] <- paste('Full Mode -',input$columns2)
    colnames(pred_data_new)[2] <- paste('Reduced Mode -',input$columns2)
    
    return(pred_data_new)
    
  })
  
  output$Prediction <- DT::renderDataTable({
    DT::datatable(forecast_out(),options=list(lengthChange = TRUE))
  })
}


##############################################################################################

ui <- pageWithSidebar(
  headerPanel("Prediction using GLM"),
  
  sidebarPanel(
    #Selector for file upload
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    #These column selectors are dynamically created when the file is loaded
    #uiOutput("Independent"),
    #uiOutput("Dependent"),
    selectInput("columns",label="Please select independent variables",multiple=TRUE,choices =""),
    selectInput("columns2",label="Please select dependent variable",choices ="")
  ),
  mainPanel(
    tabsetPanel(type = 'tab',
                tabPanel("Selected",DT::dataTableOutput("selData")),
                tabPanel("Test/Predicted",plotOutput("glmperf")),
                tabPanel("RMSE",DT::dataTableOutput("RMSE")),
                tabPanel("Prediction",DT::dataTableOutput("Prediction")))
  )
)

shinyApp(ui, server)
