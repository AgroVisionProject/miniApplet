# Define UI for application that draws a histogram

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
  
  titlePanel(fluidRow(
    column(2, #offset = 1,
           img(src = "uw-crest.png", height = 80)),
    column(8, align = "center",
           "Nitrogen Decision Support Tool for Corn Management"),
    column(2, #offset = -1,
           img(src = "grasslandColorCenter.png", height = 80)))
    ),
  
  bsCollapse(id = "methods",
             bsCollapsePanel("Methods", style = "success", # default, info, warning
              "Text here"
             )),
    
  sidebarLayout(
    sidebarPanel(h5(em("Zoom in to a location by either clicking on the map or zooming in using the zoom feature on the map,
                    to view corn yield and nitrate leaching responses to fertilizer N.")),
                 #h3("To view site specific corn yield and nitrate leaching responses to fertilizer N."),
                 br(),
                 actionButton("reset", "Clear locations"),
                 shinyjs::useShinyjs(),
                 downloadButton("download", "Download Data"),
                 hr(),
                 shinyjs::useShinyjs(),
                 uiOutput("simSelectionUI"),
                 uiOutput("pricesUI"),
                 uiOutput("sliderUI")),
    mainPanel(leafletOutput("map", height = 350) %>%
                withSpinner(type = 3,
                            color.background = "white"),
              br(),
              br(),
              uiOutput("plotUI")
              
  )
  )
)
