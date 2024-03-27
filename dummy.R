app <- shinyApp(
  ui = shinyUI(
    fluidPage(
      tags$head(
        tags$style(HTML("
          .navbar .navbar-nav {align: center}
          .navbar .navbar-header {align: center}
        "))
      ),
      navbarPage("header",
                 tabPanel("tab1"),
                 tabPanel("tab2")
      )
    )
  ),
  
  server = function(input, output, session){}
  
)

runApp(app)