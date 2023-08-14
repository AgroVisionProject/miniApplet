# Define UI for application that draws a histogram

ui <- fluidPage(
  
  titlePanel("AgroIBIS Decision Support System"),
  hr(),
  
  sidebarLayout(
    sidebarPanel(tags$h4("To view site specific corn yield and nitrate leaching responses to fertilizer N:"),
                 tags$li("Click on your state"),
                 tags$li("then your county"),
                 tags$li("then the site marker"),
                 br(),
                 actionButton("reset", "Clear locations"),
                 hr(),
                 uiOutput("simSelectionUI"),
                 uiOutput("sliderUI")),
    mainPanel(leafletOutput("map"),
              br(),
              fluidRow(column(6,
                              numericInput("cornPrice", "Price of corn ($/bu)", value = 5)
                              ),
                       column(6, 
                              numericInput("fertPrice", "Price of N fertilizer ($/lb)", value = 1))),
              br(),
              #uiOutput("econPlotUI"),
              uiOutput("plotUI")
              
              
  )
  )
)
