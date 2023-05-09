# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # functions-----------------------
  base_map <- function() {
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = states, 
                  group = "state", 
                  col = "blue",
                  layerId = ~state) %>%
      addPolygons(data = counties, 
                  group = "county",
                  col = "darkgreen") %>%
      addCircleMarkers(data = sites, 
                       lat = ~lat, lng = ~lon,
                       group = "sites") %>%
      groupOptions("state", zoomLevels = 1:10) %>%
      groupOptions("county", zoomLevels = 7:10) %>%
      groupOptions("sites", zoomLevels = 9:15) %>%
      addProviderTiles("Esri.WorldTopoMap")
  }
  
  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  
  # reactiveVals for map---------------------------
  
  # map-------------------------
  output$map <- renderLeaflet({
    react_map()
  })
  
  
  observeEvent(input$map_shape_click, {
    
    # open map to country view 
    p <- input$map_shape_click # get input value
    if (is.null(p))  
      return()
    print(p)
    
    # zoom to state view
    if(p$group == "state") {
     
      leafletProxy("map") %>%
        setView(lat = p$lat, lng = p$lng, zoom = 7)
    }
    
    #zoom to county view with marker points
    if(p$group == "county") {
      
      # county map with observation sites
      leafletProxy("map") %>%
        setView(lat = p$lat, lng = p$lng, zoom = 10) 
      
    }
    
  })
  
  selectedPoint <- reactiveVal()
  
  observeEvent(input$map_marker_click, {
    
    
    click <- input$map_marker_click
    lat <- click$lat
    lon <- click$lng
    
    selectedPoint(
      list(lat = lat, lon = lon)
    )

    # zoom to site
    leafletProxy("map") %>%
      setView(lng = lon, lat = lat, zoom = 12) %>%
      clearGroup("sampleSite") %>%
      addMarkers(lng = lon, lat = lat, group = "sampleSite")
    
    # show land management selection and plot button
    shinyjs::enable("simName")
    shinyjs::enable("plot")
    
  })
  
  observeEvent(selectedPoint(), {

    print(selectedPoint())

  })
  
  # make df---------------------------
  df <- eventReactive({input$simName
                      input$plot}, {

    req(selectedPoint())
    #eq(input$simName)

    simNumber <- filter(sims, cropSystem == input$simName)
    selected_sim <- simNumber$simulation
    #print(paste("sim:", selected_sim))
    site_lat <- selectedPoint()$lat
    #print(paste("lat:", site_lat))
    site_lon <- selectedPoint()$lon
    #print(paste("lon:", site_lon))

    yield_df_sum <- yield_df %>%
      filter(sim == selected_sim,
             lat == site_lat,
             lon == site_lon)
    #print(yield_df_sum)

    leach_df_sum <- leach_df %>%
      filter(sim == selected_sim,
             lat == site_lat,
             lon == site_lon)
    #print(leach_df_sum)

    yieldFun <- yield_df_sum$fun
    leachFun <- leach_df_sum$fun

    yield_y <- responseCurve(dataframe = yield_df_sum, fun = yieldFun)
    leach_y <- responseCurve(dataframe = leach_df_sum, fun = leachFun)

    data.frame(fert = round(kgha_to_lbac(fert)), yield = yield_y, leaching = kgha_to_lbac(leach_y))

  })

  # make plot------------------
  observeEvent(input$plot, {

    req(df())
    dat <- df()
    print(head(dat))

    output$plot1 <- renderPlotly({


      plot_ly(dat, x = ~fert, y = ~ yield, name = "Yield (bu/ac)",
              type = 'scatter', mode = 'lines',
              line = list(color = "#5dbb63", width = 4),
              hovertext = ~ paste("Yield:", round(yield, 1), "bu/ac"),
              hoverinfo = "text") %>%
        add_trace(y = ~ leaching, name = "Nitrate leaching (lb/ac)",
                  line = list(color = "#c99f6e", width = 4),
                  hovertext = ~ paste("Nitrate leaching:", round(leaching, 1), "lbs/ac"),
                  hoverinfo = "text") %>%
        add_trace(y = 0,
                  opacity = 0,
                  hovertext = ~ paste("N fert rate:",fert, "lbs N/ac"),
                  hoverinfo = "text",
                  showlegend = F) %>%
        layout(title = "Corn yield and nitrate leaching response \n to N fertilizer \n",
               xaxis = list(title = "N fertilizer (N lb/ac)"),
               yaxis = list (title = " "),
               hovermode = "x unified")

    })

    output$range <- renderUI({

      sliderInput(
        inputId = "range",
        label = "N fertilizer (lb/ac)",
        min = 0, max = 270, value = 100, step = 10
      )
    })
    

    output$values <- render_gt({
      
      req(input$range)

      dat %>%
        filter(fert == input$range) %>%
        mutate(leaching = round(leaching, 1),
               yield = round(yield, 1)) %>%
        gt() %>%
        cols_label(
          fert = "N fertilizer (lb/ac)",
          yield = "Yield (bu/ac)",
          leaching = "Nitrate leaching (lb/ac)"
        )
     })

   })

  # reset vals---------------
  observeEvent(input$reset, {
    # reset the map
    react_map(base_map())
    output$plot1 <- NULL
    shinyjs::disable("plot")

  })

 }