library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  plotOutput("plot"),
  
  downloadButton("download_png", "Download PNG")
  
)

server <- function(input, output, session) {
  
  plt1 <- reactiveVal()
  output$plot <- renderPlot({
    plotting()
    
  })
  plotting <- function(){
    ggplot(mtcars, aes(x = wt, y = mpg)) + 
      geom_point()
  }
  
  output$download_png <- downloadHandler(
    filename = "Shinyplot.png",
    content = function(file) {
      png(file)
      plotting()
      dev.off()
    }) 
  
}

shinyApp(ui, server)