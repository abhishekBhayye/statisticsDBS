library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = "Interactive Plots"
)

body <- dashboardBody(
  fluidPage(
    column(width = 4,
           selectInput("models", "SELECT MODEL:",
                       c(
                         "BERNOULLI"= "bern",
                         "BINOMIAL" = "bino"
                       )
           ),

           sliderInput("n","parameter n in Binomial", min = 1, max = 1000, value = 10),

           conditionalPanel(
             "input.models == 'bino'",
             numericInput("p","param p in Binomial", min = 0, max = 1, value = "0.5")
           )
    ),

    column(width = 8,
           plotOutput("histogram")
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
