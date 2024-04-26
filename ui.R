# Define UI for application that draws a histogram

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
  
  titlePanel(fluidRow(
    column(9, align = "center",
           h2("Nitrogen Decision Support Tool for Corn Management"), 
           h4("Assess the impact of land use and N fertilizer applications on nitrogen losses to groundwater")),
    column(3, align = "center",
           img(src = "uw-crest.png", height = 80)))
  ),
  # fluidRow(column(12, align = "center", 
  #                 h3("Assess the impact of land use and N fertilizer applications on nitrogen losses to groundwater"))),
  # 
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
                 uiOutput("slider1UI"),
                 uiOutput("slider2UI")),
    mainPanel(leafletOutput("map", height = 350) %>%
                withSpinner(type = 3,
                            color.background = "white"),
              br(),
              br(),
              uiOutput("plotUI")
              
    )
  )
)