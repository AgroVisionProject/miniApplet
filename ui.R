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
                 #tags$h4("Next, select up to 2 land management scenario and draw plots."),
                 # disabled(selectizeInput(inputId = "simName",
                 #                         label = "Cropping systems",
                 #                         multiple = TRUE,
                 #                         options = list(maxItems = 2),
                 #                         choices = simNames)),
                 # disabled(radioButtons(inputId = "simName1",
                 #              label = "Choose cropping system",
                 #              choices = simNames)),
                 uiOutput("simSelectionUI"),
                 #shinyjs::useShinyjs(),
                 # disabled(
                 #   actionButton("plot", "Draw plots")
                 # ),
                 uiOutput("sliderUI")),
    mainPanel(leafletOutput("map"),
              br(),
              #plotlyOutput("plot1")
              #plotlyOutput("plot2")
              uiOutput("plotUI")
              
  )
  )
)
