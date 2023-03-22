# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #print(head(map_df))
  base_map <- function() {
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = states, 
                  layerId = ~NAME)
  }
  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  output$map <- renderLeaflet({
    react_map()
  }) 
  
  # get lat lon for state zoom reset
  stateCenter <- reactive({
    
    p <- input$map_shape_click
    state_df <- states %>%
      filter(NAME == p$id)
    stateLat <- as.numeric(state_df$INTPTLAT)
    stateLon <- as.numeric(state_df$INTPTLON)
    data.frame(lat = stateLat, lon = stateLon)
    
  })
  
  # filter county shapefiles to just the state of interest
  selected_state <- reactive({
    
    p <- input$map_shape_click
    filter(counties, STATE_NAME == p$id)
    
  })
  
  
  # filter agroIbis output to county of interest
  selected_county <- reactive({
    p <- input$map_shape_click
    map_df %>% filter(STATE_NAME == stateName) %>% filter(NAME == p$id)
    
  })
  
  #ID <- reactiveValues() ##TODO maybe store p$ID in here?
  
  
  output$map <- renderLeaflet({
    react_map()
  })
  
  clicks <- reactiveValues(count = 0)
  
  observeEvent(input$map_shape_click, {
    
    # open map to country view 
    p <- input$map_shape_click # get input value
    if (is.null(p))  
      return()
    
    #print(p)
    clicks$count <- clicks$count + 1
    #print(clicks$count)
    
    # zoom to state view
    if(clicks$count == 1) {
      latLon_df <- stateCenter()
      #print(latLon_df)
      county_df <- selected_state()
      #print(head(county_df))
      stateName <<- unique(county_df$STATE_NAME)
      #print(stateName)
      
      leafletProxy("map") %>%
        setView(lat = latLon_df$lat, lng = latLon_df$lon, zoom = 7) %>%
        addPolygons(data = county_df,
                    layerId = ~NAME)
    }
    
    #zoom to county view with marker points
    if(clicks$count == 2) {
      
      data_df <- selected_county()
      #head(data_df)
      #print(p$lat)
      #print(p$lng)
      
      leafletProxy("map") %>%
        setView(lat = p$lat, lng = p$lng, zoom = 9) %>%
        addCircleMarkers(data = data_df,
                         lat = ~lat, lng = ~lon #,
                         #layerId = ~ID
                        )
    }
    
    
  })
  
  observeEvent(input$map_marker_click, {
    
    validate(
      need(df, "select new location")
    )
    
    
    map_click_info <- input$map_marker_click
    #print("marker click info")
    #print(map_click_info)
    
    leafletProxy("map") %>%
      setView(lng = map_click_info$lng, lat = map_click_info$lat, zoom = 11) %>%
      addMarkers(lng = map_click_info$lng, lat = map_click_info$lat)

    #ID = map_click_info$id
    #print(ID)
    
    yield_df_sub <- yield_df %>%
      filter(lat == map_click_info$lat,
             lon == map_click_info$lng) 
    #print('yield df')
    #print(yield_df_sub)
    
    leach_df_sub <- leach_df %>%
      filter(lat == map_click_info$lat,
             lon == map_click_info$lng)
    #print('leach df')
    #print(leach_df_sub)
    
    yield_y <- c()
    if(yield_df_sub$fun == 0) {
      yield_y = rep(yield_df_sub$a, times = length(fert))
    }
    if(yield_df_sub$fun == 3) {
      yield_y = yield_df_sub$a * exp((-yield_df_sub$b)*(yield_df_sub$c^fert))
    }
    if(yield_df_sub$fun == 4) {
      yield_y = ifelse(fert < yield_df_sub$c, yield_df_sub$a + (yield_df_sub$b*fert), 
                       yield_df_sub$a + (yield_df_sub$b*yield_df_sub$c))
    }
    if(yield_df_sub$fun == 5) {
      yield_y = yield_df_sub$a + (yield_df_sub$b*fert)
    }
    #print(yield_y)
    
    leach_y <- c()
    if(leach_df_sub$fun == 0) {
      leach_y = rep(leach_df_sub$a, times = length(fert))
    }
    if(leach_df_sub$fun == 1) {
      leach_y = leach_df_sub$a + (leach_df_sub$b*fert) + (leach_df_sub$c*fert^2)
    }
    #print(leach_y)
    
    df = data.frame(fert = fert, yield = yield_y, leaching = leach_y)
    print(df)

    #observeEvent(input$plot, {


      output$plot1 <- renderPlotly({

       

        plot_ly(df, x = ~fert, y = ~ yield, name = "Yield (bu/ac)", 
                type = 'scatter', mode = 'lines',
                line = list(color = "#5dbb63", width = 4),
                hovertext = ~ round(yield, 2),
                hoverinfo = "text") %>% 
          add_trace(y = ~ leaching, name = "NO3 leaching (kg/ha)",
                    line = list(color = "#c99f6e", width = 4),
                    hovertext = ~ round(leaching, 2),
                    hoverinfo = "text") %>%
          layout(xaxis = list(title = "N fertilizer (N kg/ha)"),
                 yaxis = list (title = " "),
                 hovermode = "x unified")

      })

    #})
    
  })
  
  
  observeEvent(input$reset, {
    # reset the map
    react_map(base_map()) 
    # reset the click counts
    clicks$count <- 0
    df <- NULL
    print('cleared df')
    print(head(df))
    
  })
  
}