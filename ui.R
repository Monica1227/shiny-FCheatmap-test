
shinyUI(pageWithSidebar(
  
  headerPanel("FCheatmap: a web-tool for generation of heatmaps among different comparison groups with significant labels",
              tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
                        tags$style(type="text/css", "select { max-width: 200px; }"),
                        tags$style(type="text/css", "textarea { max-width: 185px; }"),
                        tags$style(type="text/css", ".jslider { max-width: 200px; }"),
                        tags$style(type='text/css', ".well { max-width: 330px; }"),
                        tags$style(type='text/css', ".span4 { max-width: 330px; }")) 
  ),
  
  # App title ----
  titlePanel("Uploading Files"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      tags$h1("Differential analysis results"),
      fileInput("file1", "Choose the first File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      fileInput("file2", "Choose the second File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      fileInput("file3", "Choose the third File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      fileInput("file4", "Choose the fourth File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line ----
      tags$hr(),
      # submitButton("Submit", icon("sync")),
      # actionButton("goButton", "Go!", class = "btn-success"),
      width=3
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput('heatmapP'),
      width=8
    )
  )
)
)
      
      
   
      
      
      

