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
                              numericInput("fertPrice", "Price of N fertilizer ($/lb)", value = 1)
                              )),
              p("Assumptions"),
              fluidRow(column(4,
                              numericInput("cornImp", "Corn Yield Improvements", value = 1)
                              ),
                       column(4, 
                              numericInput("fertImp", "Nitrogen improvements", value = 1)
                              ),
                       column(4,
                              numericInput("NUE", "Nitrogen Use Efficiency", value = 0.5)
                              )),
              br(),
              #uiOutput("econPlotUI"),
              uiOutput("plotUI")
              
              
  )
  )
)
