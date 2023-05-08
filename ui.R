# Define UI for application that draws a histogram
# ui <- fluidPage(
#   
#   titlePanel("AgroIBIS Decision Support System"),
#   hr(),
#   
#   fluidRow(
#            tags$h4("To view site specific corn yield and nitrate leaching responses to fertilizer N:"),
#            tags$li("first click on your state"),
#            tags$li("and then your county"),
#            tags$li("and then the site marker"),
#            br(),
#            actionButton("reset", "Clear locations"),
#            hr(),
#            leafletOutput("map",height = 600)
#     ),
#   hr(),
#   fluidRow(
#     #col(6, 
#         selectInput(inputId = "simName",
#                     label = "Choose cropping system",
#                     choices = simNames),
#         #)#,
#    # col(6, 
#    actionButton("plot", "Draw plots")
#     ),
#   hr(),
#   fluidRow(plotlyOutput(outputId = "plot1"),
#            conditionalPanel(
#              condition = "output.plot1",
#              #uiOutput('plotDone'),
#              column(#width = 10, offset = 1,
#                10,
#                align = "center",
#                tags$h4("Move the slider to see the response values"),
#                uiOutput("range"),
#                gt_output("values")))
#  )
# )


ui <- fluidPage(
  
  titlePanel("AgroIBIS Decision Support System"),
  hr(),
  
  sidebarLayout(
    sidebarPanel(tags$h4("To view site specific corn yield and nitrate leaching responses to fertilizer N:"),
                 tags$li("first click on your state"),
                 tags$li("and then your county"),
                 tags$li("and then the site marker"),
                 br(),
                 actionButton("reset", "Clear locations"),
                 br(),
                 selectInput(inputId = "simName",
                             label = "Choose cropping system",
                             choices = simNames),
                 shinyjs::useShinyjs(),
                 disabled(
                   actionButton("plot", "Draw plots")
                 )
                 ),
    mainPanel(leafletOutput("map",height = 600),
              br(),
              plotlyOutput(outputId = "plot1"),
              conditionalPanel(
                condition = "output.plot1",
                tags$h4("Move the slider to see the response values"),
                uiOutput("range"),
                gt_output("values")
              )
  )
  )
)
