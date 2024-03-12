## MAP ##

# UI ----


mapUI <- function() {
  ns <- NS("map")
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h4("To view site specific corn yield and nitrate leaching responses to fertilizer N:"),
        tags$li("Click on your state"),
        tags$li("Then, click on your county"),
        tags$li("Then, click on the site marker"),
        tags$li("Then, view results"),
        br(),
        actionButton(ns("reset"), "Refresh")
      ),
      mainPanel(
        leafletOutput(ns("map")) %>%
          withSpinner(type = 8)
        )
    )
  )
  
}


# Server ----


mapServer <- function() {
  moduleServer(
    id = "map",
    function(input, output, session) {
      ns <- session$ns
      
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          # # the display order of map data
          addMapPane("states", 451) %>%
          addMapPane("counties", 452) %>%
          addMapPane("sites", 453) %>%
          addPolygons(data = states,
                      group = "state",
                      col = "blue",
                      layerId = ~state,
                      options = pathOptions(pane = "states")
                      ) %>% 
          addPolygons(data = counties,
                      group = "county",
                      col = "darkgreen",
                      layerId = ~id,
                      options = pathOptions(pane = "counties")
          ) %>%
          # addLayersControl(overlayGroups = c("states", "counties", "sites")) %>%
          # # addCircleMarkers(data = stateSites$df,
          # #                  lat = ~lat, lng = ~lon,
          # #                  group = "sites") %>%
          
          ##TODO should I make it so the zoomlevels don't overlap
          groupOptions("state", zoomLevels = 1:8) %>%
          groupOptions("county", zoomLevels = 8:14) %>%
          groupOptions("sites", zoomLevels = 9:14) %>%
          setView(lat = 41.5, lng = -93.5, zoom = 4) %>% # fitbounds?
          addProviderTiles("Esri.WorldTopoMap")
      })
      
      observeEvent(input$map_shape_click, {
        
        #print(input$map_shape_click)
        click <- input$map_shape_click
        
        
        if (click$group == "state") {
          
          state <- states %>% filter(state == click$id)
          lat <- as.numeric(state$stateLat)
          lon <- as.numeric(state$stateLin)
            
          leafletProxy(ns("map")) %>%
            setView(lat = lat, lng = lon, zoom = 8)
          return()
        } 
        
        if (click$group == "county") {
          
          county = county_centroids %>% filter(id == click$id)
          #print(county)
          
          leafletProxy(ns("map")) %>%
            setView(
              lat = county$lat,
              lng = county$lon,
              zoom = 10
            )
          return()
        } 
        
      })
      
      # render counties for state
      observeEvent(TRUE, {
          
        leafletProxy(ns("map")) %>%
          addCircleMarkers(
            data = sites,
            lat = ~lat, lng = ~lon,
            layerId = ~id,
            group = "sites", 
            options = pathOptions(pane = "sites")
          )
          
      })
      
      selectedSite <- reactiveVal()
      
      observeEvent(input$map_marker_click, {
          print("selected point")

          click <- input$map_marker_click
          print(click)
          site <- sites %>% filter(id == click$id)
          selectedSite(site)
          
      })
      
      observe({
        
        req(selectedSite())
        
        leafletProxy(ns("map")) %>%
          clearGroup("cur_site") %>%
          addMarkers(
            data = selectedSite(), 
            group = "cur_site"
          )
        
      })
      
      observeEvent(input$reset, {
        # reset the map
        selectedSite(NULL)
        ##TODO add sim1(NULL)??
        leafletProxy(ns("map")) %>%
          clearGroup("cur_site") %>%
          setView(lat = 41.5, lng = -93.5, zoom = 4) # fitbounds?
        
      })
      
      return(reactive(selectedSite()))
      # end
    }
  )
}

# old server

# base_map <- function() {
#   leaflet() %>%
#     addTiles() %>%
#     addPolygons(data = states,
#                 group = "state",
#                 col = "blue",
#                 layerId = ~state) %>%
#     addPolygons(data = counties,
#                 group = "county",
#                 col = "darkgreen") %>%
#     groupOptions("state", zoomLevels = 1:9) %>%
#     groupOptions("county", zoomLevels = 7:10) %>%
#     addProviderTiles("Esri.WorldTopoMap")
# }


# # reactiveVal for the map object, and corresponding output object.
# react_map <- reactiveVal(base_map())
# 
# # reactiveVals for map---------------------------
# 
# # map-------------------------
# output$map <- renderLeaflet({
#   react_map()
# })
# 

# stateSites <- reactiveValues()
# stateSites$df <- data.frame()
# #countySites <- reactiveVal()
# 
# observeEvent(input$map_shape_click, {
#   
#   print("map click")
#   # open map to country view 
#   p <- input$map_shape_click # get input value
#   if (is.null(p))  
#     return()
#   
#   # zoom to state view
#   if(p$group == "state") {
#    
#     leafletProxy("map") %>%
#       setView(lat = p$lat, lng = p$lng, zoom = 7)
#     
#     # subset sites to state
#     stateSites$df <- sites %>%
#       filter(state == p$id)
#     
#   }
#   
#   #zoom to county view with marker points
#   if(p$group == "county") {
#     
#     # county map with observation sites
#     leafletProxy("map") %>%
#       addCircleMarkers(data = stateSites$df,
#                        lat = ~lat, lng = ~lon,
#                        group = "sites") %>%
#       setView(lat = p$lat, lng = p$lng, zoom = 10) 
#     
#   }
#   
# })
# 
# # selected point----------------
##selectedPoint <- reactiveValues(lat = NULL, lon = NULL)
# #point <- reactiveVal()
# selectedPoint <- reactiveVal()
#vals <- reactiveValues(count = 0)
# 
# observeEvent(input$map_marker_click, {
#   
#   print("selected point")
#   
#   click <- input$map_marker_click
#   lat <- click$lat
#   # print("lat")
#   # print(lat)
#   lon <- click$lng
#   # print("lon")
#   # print(lon)
#   
#   selectedPoint(
#   list(lat = lat, lon = lon)
#   )
#   # selectedPoint$lat <- lat
#   # selectedPoint$lon <- lon
#   
#   vals$count <- vals$count + 1
# 
#   # zoom to site
#   leafletProxy("map") %>%
#     setView(lng = lon, lat = lat, zoom = 12) %>%
#     clearGroup("sampleSite") %>%
#     addMarkers(lng = lon, lat = lat, group = "sampleSite")
#   
# })
# 