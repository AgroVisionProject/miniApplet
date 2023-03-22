# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("AgroIBIS Decision Support System"),
  
  fluidRow(
    column(6,
           leafletOutput("map",height = 600)
    ),
    column(6,
           h4("Choose location by clicking on map marker"),
           #actionButton("plot", "Show data"),
           br(), 
           br(),
           plotlyOutput(outputId = "plot1")
    )
    
  ), 
  # ),
  br(),
  actionButton("reset", "Clear locations")
)
