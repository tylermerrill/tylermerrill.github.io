library(datasets)
attach(iris)
library(ggplot2)

fluidPage(    
  
  titlePanel("Graphs"),
  
  
  sidebarLayout(      
    
    
    sidebarPanel(
      selectInput("Species", "Select:", 
                  choices=colnames(iris)),
      hr(),
      helpText("Data comes from the 'iris' dataset in base R")
    ),
    
    mainPanel(
      plotOutput("irisPlot"),
      plotOutput("irisPlot2"),
      plotOutput("irisPlot3")
    )
    
  )
)
