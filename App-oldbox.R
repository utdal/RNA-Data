# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(profvis)
library(shinythemes)
library(DT)
library(Seurat)
library(ggplot2)
library(rpivotTable)
library(tidyverse)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(dplyr)
library(shinyalert)
library(aws.s3)
library(Matrix)
library(readxl)
library(data.table)
library(plotly)
library(shinyvalidate)
library(orca)
library(shinyLP)
library(future)
library(promises)
options(warn = -1) # help in suppressing the warnings in the console

## Google sheets authentication
#gs4_auth(cache=".secrets", email="emailid@gmail.com")
#ss <- gs4_get("spreadsheet link")

# Import the data
#drg <- drg.combined
# read rds
drg <- readRDS("drg.combined.rds") #As already exists in the work space, no need to read a
DefaultAssay(drg) <- "RNA"
AE <- AverageExpression(drg)
AE_rna <- AE$RNA


updated_data <- read_excel("cleaned_data2.xlsx") #As already exists in the work space, no need to read again
#print(class(updated_data))
updated_data_main <- as.data.frame(updated_data)
# clean the data
updated_data <- updated_data[1:20000,]
col_names <- transpose(updated_data[1])
# Load the cleaned data object as cleaned_data2
cleaned_data2 <- readRDS("cleaned_data2.rds")
columnames <- colnames(cleaned_data2[,4:20000])
#Check if there is equal number of the Gene types in the both the data sets or not?
#columnames <- colnames(cleaned_data2[,4:20000])

gene_names <- rownames(AE_rna)
dataset_names <- c("Spatial Transcriptomics (DRG)","Neuropathic Pain (DRG)")
#ds_selected <- "DRG_Human_Noiceptor"

plot_list <- c("Dot Plot", "Violin Plot", "Ridge Plot", "Feature Plot", "Dim Plot")

# Adding background color for filter and search in tables
callback <- c(
  "$('#DataTables_Table_0_length select').css('background-color', 'white');",
  "$('#DataTables_Table_0_filter input').css('background-color', 'white');"
)
callback1 <- c(
  "$('#DataTables_Table_1_length select').css('background-color', 'white');",
  "$('#DataTables_Table_1_filter input').css('background-color', 'white');"
)


# UI
ui <- navbarPage(
  useShinyjs(),
  title = div(HTML("<img src='MAIN-red-white-1line.png' width='280px' height='40px' style='vertical-align: middle;padding-top: 0px;'>")),
  windowTitle = "Sensoryomics",
  theme = "style/style.css",
  tags$head(includeHTML(("google_analytics.html"))),
  id = "navbar",
  selected = "home",
  tabPanel("Tab1",
           h1("This is tab1")),
  tabPanel(value = "home","Home",
           icon = icon("home"),includeHTML("home_updated3.html")),
  tabPanel(value="gene_search","Gene Search",icon = icon("magnifying-glass"),
           # conditionalPanel(
           #   condition = "input.navbar == 'gene_search'",
           sidebarLayout(
             sidebarPanel(id="genesearch_sidebar",
                          style = "position:fixed;width:22%;top:90px;",
                          h3(strong("Multiple Gene Search")),
                          br(),
                          selectizeInput("dataset", "Select Dataset", choices = c("Spatial Transcriptomics (DRG)","Neuropathic Pain (DRG)"), multiple = FALSE),
                          selectizeInput("geneName", "Select Genes", choices = NULL, multiple = TRUE),
                          p(strong("Note:"),"The low expressed Genes are not available!"),
                          selectizeInput("gender_select", "Select Biological Sex", choices = c("Both","Female","Male"), multiple = FALSE),
                          selectizeInput("plt", "Select Plot", choices = plot_list, multiple = FALSE),
                          conditionalPanel(
                            condition = "input.dataset == 'Spatial Transcriptomics (DRG)'",  
                            downloadButton("download_plots", "Download Plots"),),
                          conditionalPanel(
                            condition = "input.dataset == 'Neuropathic Pain (DRG)'",
                            downloadButton("download_boxplots", "Download Plots"),),
                          width = 3),
             
             mainPanel(
               # adding title
               h4(strong("Selected Dataset:")),
               # Newly added code to table appearance change
               tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled ,.dataTables_wrapper thead th {
                                          color: #000000 !important;background-color: #FFFFFF}")),
               br(),
               conditionalPanel(
                 condition = "input.dataset == 'Spatial Transcriptomics (DRG)'",
                 fluidRow(column(dataTableOutput ("datatable"), width = 12)),
               ),
               conditionalPanel(
                 condition = "input.dataset == 'Neuropathic Pain (DRG)'",
                 fluidRow(column(dataTableOutput ("datatable_npp"), width = 12)),
               ),
               br(),
               h4(strong("Plots:")),
               # Table value color
               tags$head(tags$style("#datatable table {color:#000000 }", media="screen", type="text/css")),
               conditionalPanel(
                 condition = "input.dataset == 'Spatial Transcriptomics (DRG)'",
                 fluidRow(id = "firstp1",box(plotOutput("plot1"), width = 12)),
                 br(),
                 fluidRow(box(plotOutput("plot2"), width = 12)),
                 br(),
                 fluidRow(box(plotOutput("plot3"), width = 12)),
                 br(),
                 fluidRow(box(plotOutput("plot4"), width = 12)),
                 br(),
                 fluidRow(box(plotOutput("plot5"), width = 12)),
                 br(),
                 fluidRow(box(plotOutput("plot6"), width = 12)),
                 br()
               ),
               conditionalPanel(
                 condition = "input.dataset == 'Neuropathic Pain (DRG)'",
                 fluidRow(box(plotOutput("p1",width = "100%", height = "400px"),width = 12)),
                 br(),
                 fluidRow(box(plotOutput("p2",width = "100%", height = "400px"),width = 12)),
                 br(),
                 fluidRow(box(plotOutput("p3",width = "100%", height = "400px"),width = 12)),
                 br(),
                 fluidRow(box(plotOutput("p4",width = "100%", height = "400px"),width = 12)),
                 br(),
                 fluidRow(box(plotOutput("p5",width = "100%", height = "400px"),width = 12)),
                 br(),
                 fluidRow(box(plotOutput("p6",width = "100%", height = "400px"),width = 12)),
                 br()
               ),width = 9
             ),
             fluid = FALSE
           )
           
  ),
  tabPanel("Data",icon = icon("database"),
           fluidRow(
             br(),
             column(1),
             column(2,align="left",selectizeInput("Data_Set","Select Dataset",choices = c("Spatial Transcriptomics (DRG)","Neuropathic Pain (DRG)"),multiple = FALSE,selected = "Spatial Transcriptomics (DRG)")),
             column(6),
             column(2,align="right",downloadButton("download_dataset", "Download Dataset")),
             column(1)
           ),
           br(),
           #h6("Dataset Description:"),
           fluidRow(
             column(1),
             column(10,box( width = 100, uiOutput("description_text"))),
             column(1)
           ),
           br(),
           #h5("Dataset Selected:"),
           fluidRow(
             column(1),
             column(10,dataTableOutput ("dataset_table"), width = 10),
             column(1)
           ),
  ),
  tabPanel("About",icon = icon("info"),includeHTML("home_updated.html")),
  tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://paincenter.utdallas.edu/\"><img src=\"social-in-red-circle.png\" alt=\"alt\" style=\"float:right;width:65px;height:65px;padding-top:5px;\"> </a></div>');
    console.log(header)")
  ),
  
)

# Server
server <- function(input, output, session) {
  profvis({
    beginning <- Sys.time()
    observeEvent(input$d, {
      runjs("$('a[data-value=gene_search]').click();")
    })
    observeEvent(input$d1, {
      runjs("$('a[data-value=gene_search]').click();")
    })
    # to check the no of times submitted a mail in the session
    attempts <- reactiveVal(0)
    observe({
      if (attempts() >= 4) {
        # Disable input elements or display a message
        # For example, you can disable a textInput element:
        disable("submitemail")
        hide("label_correct")
        runjs("document.getElementById(`label`).innerHTML=`You have exceeded the limit!`;")
      }
    })
    # Disabling the plot type in gene search tab when NPP dataset is selected
    observe({
      # When a value is selected in dataset, disable plot type filter
      if (input$dataset== 'Neuropathic Pain (DRG)') {
        shinyjs::hide("plt")
        shinyjs::disable("gender_select")
      } else {
        shinyjs::show("plt")
        shinyjs::disable("gender_select")
      }
    })
    onclick("submitemail", {
      if(input$text == "") {
        runjs("document.getElementById(`label`).innerHTML=`Please enter a valid email`;")
      }
      else if(!grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(input$text), ignore.case = TRUE)) {
        runjs("document.getElementById(`label`).innerHTML=`Please enter a valid email`;")
      } else {
        if(attempts() < 3) {
          runjs("document.getElementById(`label`).innerHTML=``;")
          runjs("document.getElementById(`label_correct`).innerHTML=`Thank you for signing up!`;")
          runjs("document.getElementById(`text`).value = '';")
          print(input$text)
          print(length(input$text))
          #if(length(reactive(as.character(input$text())))<64)
          #{
          #  sheet_append(ss, data.frame(input$text))
          #}
        }
        attempts(attempts() + 1)
        # Commented the below section of code that saves the emial data in aws path
        # write.table(input$text, "emails.csv", append = TRUE, row.names = FALSE, col.names = FALSE)
        # put_object(
        #   file = "./emails.csv",
        #   object = "emails.csv",
        #   bucket = "sensoryomics-emails1",
        #   region = "us-east-2",
        #   key = "AKIA6I57NTAM2H7MHLTT",
        #   secret = "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"
        # )
      }
    })
    
    fig <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
    
    
    updateSelectizeInput(session, 'geneName', choices = gene_names, server = TRUE, selected = gene_names[1], options = list(maxItems = 6, maxOptions = 40))
    
    updateSelectizeInput(session, 'selected_gene', choices = colnames(cleaned_data2[,4:20000]), server = TRUE, selected = col_names[4], options = list(maxItems = 6, maxOptions = 40))
    updateSelectizeInput(session, 'Data_Set', choices = dataset_names, server = TRUE, selected = dataset_names[1], options = list(maxItems = 1, maxOptions = 10))
    updateSelectizeInput(session, 'dataset', choices = dataset_names, server = TRUE, selected = dataset_names[1], options = list(maxItems = 1, maxOptions = 10))
    
    # Create subsetted reactive list
    sub_list <- reactive(subset(gene_names, (gene_names %in% input$geneName)))
    debounce(sub_list, 5000)
    sub_list_NPP <- reactive(subset(columnames, (columnames %in% input$geneName)))
    #sub_list_NPP <- reactive(subset(columnames, (columnames %in% input$selected_gene)))
    debounce(sub_list_NPP, 5000)
    
    observe({
      #shinyjs::toggle("p1", condition = isTRUE(length(sub_list_NPP())>0))
      shinyjs::toggle("p2", condition = isTRUE(length(sub_list_NPP())>1))
      shinyjs::toggle("p3", condition = isTRUE(length(sub_list_NPP())>2))
      shinyjs::toggle("p4", condition = isTRUE(length(sub_list_NPP())>3))
      shinyjs::toggle("p5", condition = isTRUE(length(sub_list_NPP())>4))
      shinyjs::toggle("p6", condition = isTRUE(length(sub_list_NPP())>5))
    })
    # Validation message display code
    # output$p1 <- renderPlotly({
    #   box_plots()
    #   })
    box_vals <- reactiveValues()
    box_vals2 <- reactiveValues()
    box_vals3 <- reactiveValues()
    box_vals4 <- reactiveValues()
    box_vals5 <- reactiveValues()
    box_vals6 <- reactiveValues()
    plot_list <- reactiveVal(list())
    # Create a list to store the generated plotly plots
    plotly_plot_list <- reactiveVal(list())
    # Create a list to store ggplot2 plots
    ggplot_list <- reactiveVal(list())
    
    output$p1 <- renderPlot({
      validate(
        need(input$geneName,HTML("Please Select the Gene!"))
      )
      selection <- which(input$geneName[1]==colnames(cleaned_data2), arr.ind = TRUE)
      bp1 <- graph_gene(selection)
      #plotly_plot_list(c(plotly_plot_list(), list(bp1)))
      #plot_list <- plot_list(list(bp1))
      box_vals$plt <- bp1
      return(bp1)
    })
    output$p2 <- renderPlot({
      if (length(input$geneName)<2){
        return(NULL)
      }else{
        selection <- which(input$geneName[2]==colnames(cleaned_data2), arr.ind = TRUE)
        bp2 <- graph_gene(selection)
        #plotly_plot_list(c(plotly_plot_list(), list(bp2)))
        box_vals2$plt <- bp2
        return(bp2)
      }
    })
    output$p3 <- renderPlot({
      if (length(input$geneName)<3){
        return(NULL)
      }else{
        selection <- which(input$geneName[3]==colnames(cleaned_data2), arr.ind = TRUE)
        bp3 <- graph_gene(selection)
        #plotly_plot_list(append(plotly_plot_list(), list(bp3)))
        box_vals3$plt <- bp3
        return(bp3)
      }
    })
    output$p4 <- renderPlot({
      if (length(input$geneName)<4){
        return(NULL)
      }else{
        selection <- which(input$geneName[4]==colnames(cleaned_data2), arr.ind = TRUE)
        bp4 <-  graph_gene(selection)
        #plotly_plot_list(append(plotly_plot_list(), list(bp4)))
        box_vals4$plt <- bp4
        return(bp4)
      }
    })
    output$p5 <- renderPlot({
      if (length(input$geneName)<5){
        return(NULL)
      }else{
        selection <- which(input$geneName[5]==colnames(cleaned_data2), arr.ind = TRUE)
        bp5 <- graph_gene(selection)
        #plotly_plot_list(append(plotly_plot_list(), list(bp5)))
        box_vals5$plt <- bp5
        return(bp5)
      }
    })
    output$p6 <- renderPlot({
      if (length(input$geneName)<6){
        return(NULL)
      }else{
        selection <- which(input$geneName[6]==colnames(cleaned_data2), arr.ind = TRUE)
        bp6 <- graph_gene(selection)
        #plotly_plot_list(append(plotly_plot_list(), list(bp6)))
        box_vals6$plt <- bp6
        return(bp6)
      }
    })
    
    ######### rendering the table based on dataset choosen #######
    ## creating reactive table for data set selected
    select_dataset_main <- reactive({
      debounce(sub_list, 5000)
      tbl <- reactive(subset(AE_rna, (gene_names %in% sub_list())))
      debounce(tbl, 5000)
      tbl()
      return(tbl())
    })
    
    # Output for the table on the all gene data tab
    output$datatable <- renderDataTable(select_dataset_main(),
                                        options = list(scrollX = TRUE, scrollY = TRUE, autoWidth = TRUE)
    )
    ##
    select_dataset_npp <- reactive({
      debounce(sub_list_NPP, 5000)
      tbl1 <- reactive(subset(updated_data_main, (GeneName %in% c("pain state","Age","sex",sub_list_NPP()))))
      debounce(tbl1, 5000)
      tbl1()
      return(tbl1())
    })
    
    # Output for the table on the all gene data tab
    output$datatable_npp <- renderDataTable(select_dataset_npp(),
                                            options = list(scrollX = TRUE, scrollY = TRUE, autoWidth = TRUE)
    )
    ##############################################################
    
    # This is used to store the plots that will be downloaded
    vals <- reactiveValues()
    
    # Create a reactive plot list that will be used to display the plots on the page
    plts <- reactive({
      
      if (input$plt == "Dot Plot") {
        plt <- DotPlot(drg, features = sub_list())
        vals$plt <- plt
        
        return(plt)
        
      } else if (input$plt == "Dim Plot") {
        plt <- DimPlot(drg)
        vals$plt <- plt
        
        return(plt)
        
      } else if (input$plt == "Violin Plot") {
        L = length(sub_list())
        plt <- VlnPlot(drg, features = sub_list(), combine = FALSE)
        
        nc = 2
        if (L <= 2) {
          nc = 1
        }
        
        plt1 <- VlnPlot(drg, features = sub_list(), ncol = nc, combine = TRUE)
        vals$plt <- plt1
        
        return(plt)
        
      } else if (input$plt == "Ridge Plot") {
        L = length(sub_list())
        plt <- RidgePlot(drg, features = sub_list(), combine = FALSE)
        
        nc = 2
        if (L <= 2) {
          nc = 1
        }
        
        plt1 <- RidgePlot(drg, features = sub_list(), ncol = nc, combine = TRUE)
        vals$plt <- plt1
        
        return(plt)
        
      } else if (input$plt == "Feature Plot") {
        L = length(sub_list())
        plt <- FeaturePlot(drg, features = sub_list(), combine = FALSE)
        
        nc = 2
        if (L <= 2) {
          nc = 1
        }
        
        plt1 <- FeaturePlot(drg, features = sub_list(), ncol = nc, combine = TRUE)
        vals$plt <- plt1
        
        return(plt)
        
      }
      
    })
    
    #trying to make the page length increase with no of selections increase
    observe({
      shinyjs::toggle("plot2", condition = isTRUE(length(sub_list())>1 && input$plt != "Dot Plot" && input$plt != "Dim Plot"))
      shinyjs::toggle("plot3", condition = isTRUE(length(sub_list())>2 && input$plt != "Dot Plot" && input$plt != "Dim Plot"))
      shinyjs::toggle("plot4", condition = isTRUE(length(sub_list())>3 && input$plt != "Dot Plot" && input$plt != "Dim Plot"))
      shinyjs::toggle("plot5", condition = isTRUE(length(sub_list())>4 && input$plt != "Dot Plot" && input$plt != "Dim Plot"))
      shinyjs::toggle("plot6", condition = isTRUE(length(sub_list())>5 && input$plt != "Dot Plot" && input$plt != "Dim Plot"))
    })
    
    # Outputs for the first-sixth plot positions
    output$plot1 <- renderPlot({
      validate(
        need(input$geneName,HTML("Please Select the Gene!"))
      )
      if (input$plt == "Dot Plot" | input$plt == "Dim Plot") {
        plts()
      } else if (input$plt == "Violin Plot" | input$plt == "Ridge Plot" | input$plt == "Feature Plot") {
        plts()[1]
      }
    })
    
    output$plot2 <- renderPlot({
      if (input$plt == "Violin Plot" | input$plt == "Ridge Plot" | input$plt == "Feature Plot") {
        plts()[2]
      }
    })
    
    output$plot3 <- renderPlot({
      if (input$plt == "Violin Plot" | input$plt == "Ridge Plot" | input$plt == "Feature Plot") {
        plts()[3]
      }
    })
    
    output$plot4 <- renderPlot({
      if (input$plt == "Violin Plot" | input$plt == "Ridge Plot" | input$plt == "Feature Plot") {
        plts()[4]
      }
    })
    
    output$plot5 <- renderPlot({
      if (input$plt == "Violin Plot" | input$plt == "Ridge Plot" | input$plt == "Feature Plot") {
        plts()[5]
      }
    })
    
    output$plot6 <- renderPlot({
      if (input$plt == "Violin Plot" | input$plt == "Ridge Plot" | input$plt == "Feature Plot") {
        plts()[6]
      }
    })
    
    ## creating reactive table for data set selected
    select_dataset <- reactive({
      if(input$Data_Set == "Spatial Transcriptomics (DRG)"){
        #ds_selected <- "DRG_Human_Noiceptor"
        return(AE_rna)
      }
      else if(input$Data_Set=="Neuropathic Pain (DRG)"){
        #ds_selected <- "Neuropathic_Pain"
        return(updated_data)
      }
    })
    
    # Output for the table on the all gene data tab
    output$dataset_table <- renderDataTable(select_dataset(), options = list(
      scrollX = TRUE
    )
    )
    
    # Output for the download button
    output$download_plots <- downloadHandler(
      filename = function() {
        paste(input$plt,".pdf",sep = "")
      },
      content = function(file) {
        pdf(file, width = 15, height = 15)
        print(vals$plt)
        dev.off()
      }
    )
    # Output for the download button
    output$download_boxplots <- downloadHandler(
      filename = function() {
        paste("Neuropathic Pain Plots",".pdf",sep = "")
      },
      content = function(file) {
        # Create a PDF file to save all the plots
        #save_image(fig,file=file, height=500, width=700)
        # pdf(NULL)
        pdf(file, width = 8, height = 6)
        #print(box_vals$plt)
        if(length(sub_list())==1){
          print(box_vals$plt)
        }
        if (length(sub_list())==2) {
          print(box_vals$plt)
          print(box_vals2$plt)
        }
        if (length(sub_list())==3) {
          print(box_vals$plt)
          print(box_vals2$plt)
          print(box_vals3$plt)
        }
        if (length(sub_list())==4) {
          print(box_vals$plt)
          print(box_vals2$plt)
          print(box_vals3$plt)
          print(box_vals4$plt)
        }
        if (length(sub_list())==5) {
          print(box_vals$plt)
          print(box_vals2$plt)
          print(box_vals3$plt)
          print(box_vals4$plt)
          print(box_vals5$plt)
        }
        if (length(sub_list())==6) {
          print(box_vals$plt)
          print(box_vals2$plt)
          print(box_vals3$plt)
          print(box_vals4$plt)
          print(box_vals5$plt)
          print(box_vals6$plt)
        }
        dev.off()
      }
    )
    # Creating a reactive expression to generate dynamic text description
    dynamic_text <- reactive({
      selected_option <- input$Data_Set
      # You can use if-else or switch statements to define the dynamic text based on the selection
      if (selected_option == "Spatial Transcriptomics (DRG)") {
        return(HTML("<p><b>Description:</b> You have selected <i><b><span style='color:#4D0202;'>Spatial Transcriptomics (DRG)</span></b></i> dataset. This data was generated using single-neuron resolution approach, more detais visit- <a href='https://pubmed.ncbi.nlm.nih.gov/35171654/'>Article Link</a>. Here you can find the average gene expression for each of the 12 neuronal subtypes.</p>"))
        #return (dataset1_desc)
        #return("Description: You selected spatial transcriptomics (DRG) dataset. This data was generated using single-neuron resolution approach (more detais - https://pubmed.ncbi.nlm.nih.gov/35171654/). Here you can find the average gene expression for each of the 12 neuronal subtypes.</p>")
      } else if (selected_option == "Neuropathic Pain (DRG)") {
        return(HTML("<p><b>Description:</b> You have selected <i><b><span style='color:#4D0202;'>Neuropathic Pain (DRG)</span></b></i> dataset.This data was generated using sequenced human dorsal root ganglia, more details visit- <a href='https://academic.oup.com/brain/article/146/2/749/6648727?login=true'>Journal Link</a>. Here you can find the quantile normalized TPMs of neuron-enriched samples.</p>"))
        #return (dataset2_desc)
        #return("You have selected Neuropathic Pain (DRG) dataset.")
      } else {
        return("Selet the Dataset you want to view.")
      }
    })
    # creating a reactive expression to change the dataset for download handler
    output$download_dataset <- downloadHandler(
      filename = function(){
        paste(input$Data_Set,"csv",sep = ".")
      },
      content = function(file){
        write.csv(select_dataset(),file,sep = ",")
      }
    )
    # Display the dynamic text in the textOutput element
    output$description_text <- renderUI({
      dynamic_text()
    })
    end <- Sys.time()
    print(end - beginning)
  })
}

graph_gene <- function(gene){
  
  # turning Pain States in categories
  cleaned_data2$painState <- as.factor(cleaned_data2$painState)
  
  # get all the column names and the gene name
  col_names <- colnames(cleaned_data2)
  
  gene_name <- col_names[gene] # change this value to change which gene shows up
  
  # load the name of the gene into a variable
  gene_data <- as.numeric(cleaned_data2[[gene_name]])
  
  # create the graph object
  g <- ggplot(data = cleaned_data2,
              mapping = aes(x = painState,
                            y = gene_data,
                            fill=sex))
  
  # plot the graph
  g + geom_boxplot() + scale_fill_manual(values = c("#cc0029","#3232a8")) + ggtitle(gene_name) + ylab('TPM') + xlab('Pain State') +
    theme(
      panel.background = element_rect(fill = 'white', color = 'black'),
      panel.grid.major.y = element_line(color = 'darkgray', linetype = 'dashed')
    )
  
}

# graph_gene <- function(gene){
#   
#   # turning Pain States in categories
#   cleaned_data2$painState <- as.factor(cleaned_data2$painState)
#   
#   # get all the column names and the gene name
#   col_names <- colnames(cleaned_data2)
#   
#   gene_name <- col_names[gene] # change this value to change which gene shows up
#   
#   # load the name of the gene into a variable
#   gene_data <- as.numeric(cleaned_data2[[gene_name]])
#   
#   ## plotly graph
#   mrg <- list(l = 55, r = 50,
#               b = 50, t = 50,
#               pad = 20)
#   p <- plot_ly(
#     data = cleaned_data2,
#     y = ~gene_data,
#     x = ~painState,
#     type = "box",
#     color = ~sex,
#     showlegend = TRUE,boxpoints = "all", jitter = 0.3, pointpos = 0, marker = list(color = 'black'), colors = c("#ff5A1D","#366676")
#   ) %>% layout(boxmode = "group",
#                title = list(text = paste0("<b>",gene_name, "</b>")),
#                xaxis = list(title = "<b>Pain State</b>",
#                             zeroline = FALSE),
#                yaxis = list(title = paste0(c(rep("&nbsp;", 20),
#                                              "<b>TPM</b>",
#                                              rep("&nbsp;", 20),
#                                              rep("\n&nbsp;", 1)),
#                                            collapse = ""),
#                             zeroline = FALSE,font = list(size = 15), standoff = 50L), margin = mrg,showlegend = TRUE, legend = list(font = list(size = 15)))
#   config(p, displayModeBar = TRUE, toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
#                                                                filename= paste0(gene_name,' - Average TPM Boxplot'),
#                                                                height= 500,
#                                                                width= 1000,
#                                                                scale= 1 ))
#   # Create a variable to store the image
#   #image <- plotly_IMAGE(p, height = 500, width = 1000, scale = 1)
#   
#   # Return the image and the plot
#   #return(list(plot = p, image = image))
#   
# }

# Run the application
shinyApp(ui = ui, server = server)
