library(shiny)

ui <- navbarPage(
  title = "Well Panel Example",
  tags$head(
    tags$style(HTML("
      
      @media (min-width: 821px) {
        .custom-well {
          width: 100%;
          height: 460px;
          overflow: auto;
          background-color: white;
          color: black;
        }
      }
       @media (min-width: 768px; max-width: 820px) {
        .custom-well {
          width: 100%;
          height: 400px;
          overflow: auto;
          background-color: white;
          color: black;
        }
       }
      @media (max-width: 767px) {
        .custom-well {
          width: 100%;
          height: 200px;
          overflow: auto;
          background-color: white;
          color: black;
        }
      }
    "))
  ),
  tabPanel(
    "Tab 1",
    fluidRow(
      column(width = 2),
      column(
        width = 8,
        wellPanel(
          class = "custom-well",
          fluidRow(
            column(
              width = 7,
              h2("Mapping Human Nociceptors"),
              p("Nociceptors are specialized sensory neurons that detect damaging or potentially damaging stimuli and are found in the dorsal root ganglia (DRG) and trigeminal ganglia. These neurons are critical for the generation of neuronal signals that ultimately create the perception of pain. We sought to generate information for human nociceptors with the goal of identifying transcriptomic signatures of nociceptors, identifying species differences and potential drug targets."),
              p(strong("Citation:"),"Diana Tavares-Ferreira, Stephanie Shiers, Pradipta R Ray, Andi Wangzhou, Vivekanand Jeevakumar, Ishwarya Sankaranarayanan, Anna M Cervantes, Jeffrey C Reese, Alexander Chamessian, Bryan A Copits, Patrick M Dougherty, Robert W Gereau 4th, Michael D Burton, Gregory Dussor, Theodore J Price, Spatial transcriptomics of dorsal root ganglia identifies molecular signatures of human nociceptors.Sci. Transl. Med.14,eabj8186(2022).DOI:10.1126/scitranslmed.abj8186")
            ),
            column(
              width = 5,
              img(src = "MHN_1.webp", width = "100%")
            )
          )
        )
      ),
      column(width = 2),
    )
  )
)

server <- function(input, output) {
  # Server logic goes here
}

shinyApp(ui, server)
