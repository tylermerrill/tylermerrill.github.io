library(datasets)
library(ggplot2)

function(input, output) {
  
  output$irisPlot <- renderPlot({
    
    plot(iris[,input$Species],
      Petal.Width,
      main=input$Species,
      ylab="Petal.Width",
      xlab="Petal.Length")
  })
    
  output$irisPlot2 <- renderPlot({
    
    ggplot(iris, aes(x = Sepal.Width, y = Petal.Length, size = Species)) +
      geom_point() +
      labs(subtitle="Mpg vs. Disp", 
           y="Mpg", 
           x="Disp", 
           title="Iris Bubble-Chart", 
           caption = "Source: 'iris' dataset")
  })
    
  output$irisPlot3 <- renderPlot({
      
    ggplot(iris, aes(Sepal.Width)) +
      geom_bar(aes(fill=Species), width = 0.5) + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
      labs(title="Iris Histogram")
    
  })
}