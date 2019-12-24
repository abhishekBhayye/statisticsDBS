### Application name : CA1 B9DA101 - Statistics for Data Analytics 

### Course : MSc (Data Analytics) - Sep 2019 - Group <C>  

### Developed by : Makarand Tawde (10527342) 

### College : Dublin Business School  

###>>> Begin >>> Makarand Tawde (10527342) >>> 

library(shiny)
library(pdfetch)
library(MASS)


server <- function(input, output,session) {
  RMSE <- 0
  values = reactiveValues()
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    
    #validating independent variables if not selected
    validate(
      need(input$datafile != "", "Please select a data set")
    )
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  
  #columns : independent columns
  #columns2 : dependent column
  
  observe({
    
    updateSelectInput(session,"columns",choices=colnames(filedata()))
    updateSelectInput(session,"columns2",choices=colnames(filedata()))
  })
  
  
  
  
  output$plot <- renderPlot({
    if(input$Distribution=='Continuous')
    {
      
      df <-na.omit(filedata())
      tarinddata <- cbind(df[,input$columns2],df[,input$columns])
      colnames(tarinddata) = c(input$columns2,input$columns)
      colnames(tarinddata)[1] <- "Y"
      set.seed(199) 
      
      n = nrow(tarinddata) 
      
      indexes = sample(n,n*(80/100))  
      
      trainset = data.frame(tarinddata[indexes,] ) 
      
      testset = data.frame(tarinddata[-indexes,] )
      
      # Fit the full model  
      
      actual=testset$Y
      
      pred_test <- data.frame(testset)
      
      full.model <- glm(Y ~.,data=trainset, family='gaussian')
      
      
      values$summ <- data.frame("significant columns among selected"=summary(full.model)$coeff[-1,4] < 0.05)
      
      
      values$full <- full.model
      
      pred_full <- predict(full.model,testset[,input$columns])
      
      rmse_full = sqrt(sum((pred_full -actual)^2)/nrow(testset))
      
      
      
      #Calculations for reduced model
      reduced.model =stepAIC(full.model) 
      values$full = full.model
      values$reduced <- reduced.model
      pred_red = predict(reduced.model,testset[,input$columns])
      
      rmse_red = sqrt(sum((pred_red -actual)^2)/nrow(testset))
      
      values$rmse <- data.frame('Full model RMSE'=rmse_full,'Reduced model RMSE'=rmse_red)
      values$Predictions <- data.frame('Full'=pred_full,'Reduced'=pred_red)
      
      par(mfrow=c(1,2))
      
      #plot for full model
      
      plot(actual,type='o',col='black',xlab = 'observations',ylab=input$columns2,main='FULL')
      
      lines(pred_full,type='o',col='orange')
      
      legend(
        "topleft",
        lty=c(1,1),
        col=c("black","orange"),
        legend=c("Real","Predicted")
      )
      
      #Plot for reduced model
      
      plot(actual,type='o',col='black',xlab = 'observations',ylab=input$columns2,main='Reduced')
      
      lines(pred_red,type='o',col='orange')
      
      legend(
        "topleft",
        lty=c(1,1),
        col=c("black","orange"),
        legend=c("Real","Predicted")
      )
      
    }
    
    else
    {
      #Calculations and plot for Discrete target column
      #Note: Only full model approach used
      df <-na.omit(filedata())
      tarinddata <- cbind(df[,input$columns2],df[,input$columns])
      colnames(tarinddata) = c(input$columns2,input$columns)
      colnames(tarinddata)[1] <- "Y"
      mc = 1000
      acc=0
      
      for(i in 1:mc){
        
        n = nrow(tarinddata)  
        
        indexes = sample(n,n*(80/100))  
        
        trainset = data.frame(tarinddata[indexes,] ) 
        
        testset = data.frame(tarinddata[-indexes,] )
        
        # Fit the full model  
        
        
        pred_test <- data.frame(testset)
        
        model1<-glm(Y ~.,data=trainset, family='binomial')
        
        values$summ <- data.frame("significant columns among selected"=summary(model1)$coeff[-1,4] < 0.05)
        
        predy=predict(model1,testset)
        pred_hat=ifelse(predy>=0.5,1,0)
        actual=testset$Y
        #Confusion Matrix
        confusion_matrix=table(pred_hat,actual)
        confusion_matrix
        #Accuracy
        acc <- acc +sum(confusion_matrix[row(confusion_matrix)==col(confusion_matrix)])/sum(confusion_matrix)
        
      }
      accuracy2 = acc/mc
      values$binom <- data.frame('Accuracy'=accuracy2)
      values$binompredictions <- data.frame('Prediction'=pred_hat)
      
      par(mfrow=c(1,2))
      plot(actual,type='o',col='black',xlab = 'observations',ylab=input$columns2)
      
      lines(pred_hat,type='o',col='orange')
      
      legend(
        "topleft",
        lty=c(1,1),
        col=c("black","orange"),
        legend=c("Real","Predicted")
      )  
      
      
      
    }
    
  })
  
  
  
  
  #Function for displaying selected column data
  
  output$selData <- DT::renderDataTable({
    df <- filedata()
    tarinddata <- cbind(df[,input$columns2],df[,input$columns])
    colnames(tarinddata) = c(input$columns2,input$columns)
    
    DT::datatable(tarinddata,options = list(lengthChange = TRUE))
  })
  
  
  #Function for displaying the measure of Performances
  #For continuous: RMSE
  #For Discrete: Accuracy
  
  output$Measures <- DT::renderDataTable({
    if(input$Distribution=='Continuous')
    {
      DT::datatable(values$rmse,options=list(lengthChange=TRUE))
      
    }
    else
    {
      DT::datatable(values$binom,options=list(lengthChange=TRUE))  
      
    }
    
  })
  
  
  #Function for displaying Predictions 
  
  output$Predictions <- DT::renderDataTable({
    if(input$Distribution=='Continuous')
    {
      
      DT::datatable(values$Predictions,options=list(lengthChange=TRUE))
      
    }
    else
    {
      
      DT::datatable(values$binompredictions,options=list(lengthChange=TRUE))
      
    }
  })
  
  
  #Function for displaying Significant selected columns
  
  output$Summary <- DT::renderDataTable({
    DT::datatable(values$summ,options=list(lengthChange=TRUE))
    
  })
  
  
  
}

###<<< End   <<< Makarand Tawde (10527342) <<<  

##############################################################################################
### Application name : CA1 B9DA101 - Statistics for Data Analytics 

### Course : MSc (Data Analytics) - Sep 2019 - Group <C>  

### Developed by : Makarand Tawde (10527342) 

### College : Dublin Business School  

###>>> Begin >>> Makarand Tawde (10527342) >>> 


ui <- pageWithSidebar(
  headerPanel("Prediction using GLM"),
  
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
###<<< End   <<< Makarand Tawde (10527342) <<< 
shinyApp(ui, server)
