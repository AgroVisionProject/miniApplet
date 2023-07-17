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
      # addCircleMarkers(data = sites,
      #                  lat = ~lat, lng = ~lon,
      #                  group = "sites") %>%
      groupOptions("state", zoomLevels = 1:9) %>%
      groupOptions("county", zoomLevels = 7:10) %>%
      #groupOptions("sites", zoomLevels = 11:15) %>%
      # addLayersControl(
      #   overlayGroups = c("state", "county", "site"),
      #   options = layersControlOptions(collapsed = FALSE)
      # ) %>%
      addProviderTiles("Esri.WorldTopoMap")
  }

  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  
  # reactiveVals for map---------------------------
  
  # map-------------------------
  output$map <- renderLeaflet({
    react_map()
  })
  
  # observeEvent(react_map(), {
  #   
  #   #map <- leafletProxy("map")
  # 
  #   delay(200,
  #         leafletProxy("map") %>%
  #           addPolygons(data = counties,
  #                       group = "county",
  #                       col = "darkgreen") %>%
  #           groupOptions("county", zoomLevels = 7:11)
  #         )
  #   
  #   delay(400,
  #         leafletProxy("map") %>%
  #           addCircleMarkers(data = sites,
  #                            lat = ~lat, lng = ~lon,
  #                            group = "sites") %>%
  #           groupOptions("sites", zoomLevels = 10:15)
  #         )
  # })
  
  stateSites <- reactiveValues()
  stateSites$df <- data.frame()
  #countySites <- reactiveVal()
  
  observeEvent(input$map_shape_click, {
    
    print("map click")
    # open map to country view 
    p <- input$map_shape_click # get input value
    if (is.null(p))  
      return()
    #print(p)
    
    # zoom to state view
    if(p$group == "state") {
      
      print(paste("state", p$id))
     
      leafletProxy("map") %>%
        setView(lat = p$lat, lng = p$lng, zoom = 7)
      
      # subset sites to state
      stateSites$df <- sites %>%
        filter(state == p$id)
      
    }
    
    #zoom to county view with marker points
    if(p$group == "county") {
      
      print(p)
      
      # county map with observation sites
      leafletProxy("map") %>%
        addCircleMarkers(data = stateSites$df,
                         lat = ~lat, lng = ~lon,
                         group = "sites") %>%
        setView(lat = p$lat, lng = p$lng, zoom = 10) 
      
    }
    
  })
  
  selectedPoint <- reactiveVal()
  
  observeEvent(input$map_marker_click, {
    
    #print("point selection")
    
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
    #shinyjs::enable("plot")
    
  })
  
  # observeEvent(selectedPoint(), {
  # 
  #   print(selectedPoint())
  # 
  # })
  
  # make df---------------------------
  # df <- eventReactive({input$simName
  #                     input$plot}, {
  observe({

    req(selectedPoint())
    req(input$simName)
    
     site_lat <- selectedPoint()$lat
     print(paste("lat:", site_lat))
     site_lon <- selectedPoint()$lon
     print(paste("lon:", site_lon))
    

    #eq(input$simName)
    print("first sim")
    print(length(input$simName))

    simNumber1 <- filter(sims, cropSystem == input$simName[1])
    selected_sim1 <- simNumber1$simulation

    dat1 <- makeDF(sim = selected_sim1, site_lat = site_lat, site_lon = site_lon)
    dat1 <- dat1 %>%
      rename(yield1 = yield,
             leach1 = leaching)
    print(head(dat1))
    
    if(length(input$simName) == 2) {
      print("sim is length 2")
      simNumber2 <- filter(sims, cropSystem == input$simName[2])
      selected_sim2 <- simNumber2$simulation
      dat2 <- makeDF(sim = selected_sim2, site_lat = site_lat, site_lon = site_lon)
      dat2 <- dat2 %>%
        rename(yield2 = yield,
               leach2 = leaching)
      print(head(dat2))

      compareDat <- left_join(dat1, dat2, by = "fert")
      print(head(compareDat))
      # make compareDat based on a sequence of fert at every 10 increments
      fert10 <- seq(from = 0, to = 300, by = 10)
      fert10 <- data.frame(fert10 = fert10)
      compareDat <- left_join(fert10, compareDat, by = c("fert10" = "fert"))
      print(head(compareDat))
    }

    
    output$plot1 <- renderPlotly({

      if(length(input$simName) == 1)
      { print("test")
        plot_ly(dat1, x = ~fert, y = ~ yield1, name = "Yield (bu/ac)",
                type = 'scatter', mode = 'lines+markers',
                line = list(color = "#5dbb63", width = 1),
                hovertext = ~ paste("Yield:", round(yield1, 1), "bu/ac"),
                hoverinfo = "text") %>%
          add_trace(y = ~ leach1, name = "Nitrate leaching (lb/ac)",
                    line = list(color = "#c99f6e", width = 1),
                    hovertext = ~ paste("Nitrate leaching:", round(leach1, 1), "lbs/ac"),
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
      } 
      else 
        {print("test2")
        plot_ly(compareDat, x = ~fert10, y = ~ yield1, name = "sim 1: Yield (bu/ac)",
                type = 'scatter', mode = 'lines+markers',
                line = list(color = "#5dbb63", width = 2),
                marker = list(symbol = "x", size = 10, color = "#5dbb63"),
                hovertext = ~ paste("Yield 1:",round(yield1, 1), "bu/ac"),
                hoverinfo = "text") %>%
          add_trace(y = ~ leach1, name = "sim 1: NO3 leaching (lb/ac)",
                    line = list(color = "#5dbb63", width = 2),
                    hovertext = ~paste("Nitrate leaching 1:",round(leach1, 1), "lbs/ac"),
                    marker = list(symbol = "circle", color = "#5dbb63"),
                    #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
                    hoverinfo = "text") %>%
          add_trace(y = ~ yield2, name = "sim 2: Yield (bu/ac)",
                    line = list(color = "#c99f6e", width = 2),
                    hovertext = ~paste("Yield 2:",round(yield2, 1), "bu/ac"),
                    marker = list(symbol = "x", color = "#c99f6e"),
                    #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
                    hoverinfo = "text") %>%
          add_trace(y = ~ leach2, name = "sim 2: NO3 leaching (lb/ac)",
                    line = list(color = "#c99f6e", width = 2),
                    hovertext = ~paste("Nitrate leaching 2:",round(leach2, 1), "lbs/ac"),
                    #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
                    marker = list(symbol = "x", color = "#c99f6e"),
                    hoverinfo = "text") %>%
          add_trace(y = 0,
                    opacity = 0,
                    hovertext = ~ paste("N fert rate:",fert10, "lbs N/ac"),
                    hoverinfo = "text",
                    showlegend = F) %>%
          layout(title = "Corn yield and nitrate leaching response \n to N fertilizer \n",
                 xaxis = list(title = "N fertilizer (N lb/ac)"),
                 yaxis = list (title = " "),
                 hovermode = "x unified")
      }

    })

   
    # 
    #print(paste("sim:", selected_sim))
    
    # simNumber <- filter(sims, cropSystem == input$simName[1])
    # selected_sim <- simNumber$simulation

    # yield_df_sum <- yield_df %>%
    #   filter(sim == selected_sim,
    #          lat == site_lat,
    #          lon == site_lon)
    # print(yield_df_sum)

    # leach_df_sum1 <- leach_df %>%
    #   filter(sim == selected_sim1,
    #          lat == site_lat,
    #          lon == site_lon)
    # #print(leach_df_sum)
    # 
    # yieldFun <- yield_df_sum$fun
    # leachFun <- leach_df_sum$fun
    # 
    # yield_y <- responseCurve(dataframe = yield_df_sum, fun = yieldFun)
    # leach_y <- responseCurve(dataframe = leach_df_sum, fun = leachFun)
    # 
    # dat1 <- data.frame(fert = round(kgha_to_lbac(fert)), yield = yield_y, leaching = kgha_to_lbac(leach_y))
    #dat <- data.frame(fert = round(kgha_to_lbac(fert)), yield = yield_y, leaching = kgha_to_lbac(leach_y))
    #dat <- df()
    

    
    # output$plot2 <- renderPlotly({
    # 
    # 
    #   plot_ly(compareDat, x = ~fert10, y = ~ yield1, name = "sim 1: Yield (bu/ac)",
    #           type = 'scatter', mode = 'lines+markers',
    #           line = list(color = "#5dbb63", width = 2),
    #           marker = list(symbol = "x", size = 10, color = "#5dbb63"),
    #           hovertext = ~ paste("Yield 1:",round(yield1, 1), "bu/ac"),
    #           hoverinfo = "text") %>%
    #     add_trace(y = ~ leach1, name = "sim 1: NO3 leaching (lb/ac)",
    #               line = list(color = "#5dbb63", width = 2),
    #               hovertext = ~paste("Nitrate leaching 1:",round(leach1, 1), "lbs/ac"),
    #               marker = list(symbol = "circle", color = "#5dbb63"),
    #               #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
    #               hoverinfo = "text") %>%
    #     add_trace(y = ~ yield2, name = "sim 2: Yield (bu/ac)",
    #               line = list(color = "#c99f6e", width = 2),
    #               hovertext = ~paste("Yield 2:",round(yield2, 1), "bu/ac"),
    #               marker = list(symbol = "x", color = "#c99f6e"),
    #               #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
    #               hoverinfo = "text") %>%
    #     add_trace(y = ~ leach2, name = "sim 2: NO3 leaching (lb/ac)",
    #               line = list(color = "#c99f6e", width = 2),
    #               hovertext = ~paste("Nitrate leaching 2:",round(leach2, 1), "lbs/ac"),
    #               #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
    #               marker = list(symbol = "x", color = "#c99f6e"),
    #               hoverinfo = "text") %>%
    #     add_trace(y = 0,
    #               opacity = 0,
    #               hovertext = ~ paste("N fert rate:",fert10, "lbs N/ac"),
    #               hoverinfo = "text",
    #               showlegend = F) %>%
    #     layout(title = "Corn yield and nitrate leaching response \n to N fertilizer \n",
    #            xaxis = list(title = "N fertilizer (N lb/ac)"),
    #            yaxis = list (title = " "),
    #            hovermode = "x unified")
    # })

    output$range <- renderUI({

      sliderInput(
        inputId = "range",
        label = "N fertilizer (lb/ac)",
        min = 0, max = 268, value = 100, step = 1
      )
    })


    output$values <- render_gt({

      req(input$range)

      newdat <- dat1 %>%
        filter(fert == input$range) %>%
        mutate(leaching = round(leach1, 1),
               yield = round(yield1, 1))

      # remove duplicates
      newdat <- newdat[1,]

      newdat %>%
        gt() %>%
        cols_label(
          fert = "N fertilizer (lb/ac)",
          yield = "Yield (bu/ac)",
          leaching = "Nitrate leaching (lb/ac)"
        )
     })

  })

  # make plot------------------
  # observeEvent(input$plot, {
  # 
  #   req(df())
    # dat <- df()
    # print(head(dat))
    # 
    # output$plot1 <- renderPlotly({
    # 
    # 
    #   plot_ly(dat, x = ~fert, y = ~ yield, name = "Yield (bu/ac)",
    #           type = 'scatter', mode = 'lines+markers',
    #           line = list(color = "#5dbb63", width = 1),
    #           hovertext = ~ paste("Yield:", round(yield, 1), "bu/ac"),
    #           hoverinfo = "text") %>%
    #     add_trace(y = ~ leaching, name = "Nitrate leaching (lb/ac)",
    #               line = list(color = "#c99f6e", width = 1),
    #               hovertext = ~ paste("Nitrate leaching:", round(leaching, 1), "lbs/ac"),
    #               hoverinfo = "text") %>%
    #     add_trace(y = 0,
    #               opacity = 0,
    #               hovertext = ~ paste("N fert rate:",fert, "lbs N/ac"),
    #               hoverinfo = "text",
    #               showlegend = F) %>%
    #     layout(title = "Corn yield and nitrate leaching response \n to N fertilizer \n",
    #            xaxis = list(title = "N fertilizer (N lb/ac)"),
    #            yaxis = list (title = " "),
    #            hovermode = "x unified")
    # 
    # })
    # 
    # output$range <- renderUI({
    # 
    #   sliderInput(
    #     inputId = "range",
    #     label = "N fertilizer (lb/ac)",
    #     min = 0, max = 268, value = 100, step = 1
    #   )
    # })
    # 
    # 
    # output$values <- render_gt({
    # 
    #   req(input$range)
    # 
    #   newdat <- dat %>%
    #     filter(fert == input$range) %>%
    #     mutate(leaching = round(leaching, 1),
    #            yield = round(yield, 1))
    # 
    #   # remove duplicates
    #   newdat <- newdat[1,]
    # 
    #   newdat %>%
    #     gt() %>%
    #     cols_label(
    #       fert = "N fertilizer (lb/ac)",
    #       yield = "Yield (bu/ac)",
    #       leaching = "Nitrate leaching (lb/ac)"
    #     )
    #  })
  # 
  #  })

  # reset vals---------------
  observeEvent(input$reset, {
    # reset the map
    react_map(base_map())
    output$plot1 <- NULL
    shinyjs::disable("plot")

  })

 }