# Define UI for application that draws a histogram

ui <- fluidPage(
  
  titlePanel("AgroIBIS Decision Support System"),
  hr(),
  
  sidebarLayout(
    sidebarPanel(tags$h4("To view site specific corn yield and nitrate leaching responses to fertilizer N:"),
                 tags$li("Click on your state"),
                 tags$li("Then, click on your county"),
                 tags$li("Then, click on the site marker"),
                 tags$li("Then, view results"),
                 br(),
                 actionButton("reset", "Clear locations"),
                 hr(),
                 uiOutput("simSelectionUI"),
                 uiOutput("pricesUI"),
                 uiOutput("sliderUI")),
    mainPanel(leafletOutput("map"),
              br(),
              br(),
              uiOutput("plotUI")
              
  )
  )
)
