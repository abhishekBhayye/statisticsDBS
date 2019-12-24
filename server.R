library(shiny)

function(input, output, session){
  
  ###>>> Begin >>> Abhishek Bhayye (10527126) >>> 
  
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
  
  
  ###>>> Begin >>> Ashwini Kadam(Student#10527552) >>> 
  
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
  
  ###>>> Begin >>> Himaja Mineni (10529371) >>> 
  
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
  
  
  ###>>> Begin >>> Makarand Tawde (10527342) >>>
  
  RMSE <- 0
  values = reactiveValues()
  #This function is repsonsible for loading in the selected file
  
  #columns : independent columns
  #columns2 : dependent column
  
  observe({
    
    updateSelectInput(session,"columns",choices=colnames(myData()))
    updateSelectInput(session,"columns2",choices=colnames(myData()))
  })
  
  output$plot <- renderPlot({
    if(input$Distribution=='Continuous')
    {
      
      df <-na.omit(myData())
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
      df <-na.omit(myData())
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
    df <- myData()
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
