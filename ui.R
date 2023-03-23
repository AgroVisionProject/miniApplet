# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("AgroIBIS Decision Support System"),
  hr(),
  
  fluidRow(
    column(6,
           tags$h4("To view site specific corn yield and nitrate leaching responses to fertilizer N:"),
           tags$li("first click on your state"),
           tags$li("and then your county"),
           tags$li("and then the site marker"),
           leafletOutput("map",height = 600)
    ),
    column(6,
           plotlyOutput(outputId = "plot1"),
           br(),
           uiOutput("range"),
           gt_output("values")
           
    )
    
  ), 
  # ),
  br(),
  actionButton("reset", "Clear locations")
)
