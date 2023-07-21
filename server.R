# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # map -----------------------
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

  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  
  # reactiveVals for map---------------------------
  
  # map-------------------------
  output$map <- renderLeaflet({
    react_map()
  })
  
 
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
      
      #print(paste("state", p$id))
     
      leafletProxy("map") %>%
        setView(lat = p$lat, lng = p$lng, zoom = 7)
      
      # subset sites to state
      stateSites$df <- sites %>%
        filter(state == p$id)
      
    }
    
    #zoom to county view with marker points
    if(p$group == "county") {
      
      #print(p)
      
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

  
  # data creation ---------------------------
  
  ## data1----------------
  dat1 <- reactive({
    
    req(selectedPoint())
    req(input$simName)
    
    site_lat <- selectedPoint()$lat
    site_lon <- selectedPoint()$lon
    simulation1 <- filter(sims, cropSystem == input$simName[1]) 
    print('simulation1')
    print(simulation1$simulation)

    
    df <- makeDF(sim = simulation1$simulation, site_lat = site_lat, site_lon = site_lon) %>%
      rename(yield1 = yield,
             leach1 = leaching)
    
    print("dat1")
    print(head(df))
    df
 
  })
  
  ## data2---------------------------
  dat2 <- reactive({
    
    req(length(input$simName) == 2)
    
    site_lat <- selectedPoint()$lat
    site_lon <- selectedPoint()$lon
    simulation2 <- filter(sims, cropSystem == input$simName[2])
    
    df <- makeDF(sim = simulation2$simulation, site_lat = site_lat, site_lon = site_lon) %>%
      rename(yield2 = yield,
             leach2 = leaching)
    
    print("dat2")
    print(head(df))
    df
    
  })
  
  ## compareDat--------------------
  compareDat <- reactive({
    
    req(length(input$simName) == 2)
    print("inside compare")
    df1 <- dat1()
    df2 <- dat2()
    
    compareDat <- left_join(df1, df2, by = "fert")
    fert10 <- seq(from = 0, to = 300, by = 10)
    fert10 <- data.frame(fert10 = fert10)
    compareDat_fert10 <- left_join(fert10, compareDat, by = c("fert10" = "fert"))
    
    print(head(compareDat_fert10))
    compareDat_fert10
    
  })
  
  # plots--------------------------
  
  output$plotUI <- renderUI({

    req(input$simName)
    print("inside render UI")

    plot <- c()
    if(length(input$simName) >= 1) {
      print(length(input$simName))
      print(input$simName)
      print("plot1")
      plot <- plotlyOutput('plot1')
      }
    if(length(input$simName) == 2) {
      print("plot2")

      plot <- plotlyOutput('plot2')

      }

    plot

    })


 output$plot1 <- renderPlotly({

   req(dat1())
   print('plot1')
   print(head(dat1()))

   plot_ly(dat1(), x = ~fert, y = ~ yield1, name = "Yield (bu/ac)",
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
            hovermode = "x unified",
            legend = list(orientation = 'h', y = -0.2))
   })

 output$plot2 <- renderPlotly({

   req(compareDat())
   sim1 <- input$simName[1]
   sim2 <- input$simName[2]

   plot_ly(compareDat(), x = ~fert10, y = ~ yield1, name = paste0(sim1, ": Yield (bu/ac)"),
           type = 'scatter', mode = 'lines+markers',
           line = list(color = "#5dbb63", width = 2),
           marker = list(symbol = "x", size = 10, color = "#5dbb63"),
           hovertext = ~ paste("Yield 1:",round(yield1, 1), "bu/ac"),
           hoverinfo = "text") %>%
     add_trace(y = ~ leach1, name = paste0(sim1, ": NO3 leaching (lb/ac)"),
               line = list(color = "#5dbb63", width = 2),
               hovertext = ~paste("Nitrate leaching 1:",round(leach1, 1), "lbs/ac"),
               marker = list(symbol = "circle", color = "#5dbb63"),
               #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
               hoverinfo = "text") %>%
     add_trace(y = ~ yield2, name = paste0(sim2, ": Yield (bu/ac)"),
               line = list(color = "#c99f6e", width = 2),
               hovertext = ~paste("Yield 2:",round(yield2, 1), "bu/ac"),
               marker = list(symbol = "x", color = "#c99f6e"),
               #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
               hoverinfo = "text") %>%
     add_trace(y = ~ leach2, name = paste0(sim2, ": NO3 leaching (lb/ac)"),
               line = list(color = "#c99f6e", width = 2),
               hovertext = ~paste("Nitrate leaching 2:",round(leach2, 1), "lbs/ac"),
               #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
               marker = list(symbol = "circle", color = "#c99f6e"),
               hoverinfo = "text") %>%
     add_trace(y = 0,
               opacity = 0,
               hovertext = ~ paste("N fert rate:",fert10, "lbs N/ac"),
               hoverinfo = "text",
               showlegend = F) %>%
     layout(title = "Corn yield and nitrate leaching response \n to N fertilizer \n",
            xaxis = list(title = "N fertilizer (N lb/ac)"),
            yaxis = list (title = " "),
            hovermode = "x unified",
            legend = list(orientation = 'h',
                          y = -0.3))

 })

    
output$sliderUI <- renderUI({
      
  req(input$simName)
  ui <- list()
  if(length(input$simName) >= 1) {
    # print(length(input$simName))
    # print(input$simName)
    ui <- list(
      gt_output("values1"),
      br(),
      uiOutput("range")
    )
    if(length(input$simName) == 2)  {
      print(length(input$simName))
      print(input$simName)
      ui <- list(gt_output("values1"),
                 br(),
                 gt_output("values2"),
                 br(),
                 uiOutput("range")
      )
    }
  }
  
  ui
      
})

output$range <- renderUI({
    sliderInput(
    inputId = "range_dat",
    label = "N fertilizer (lb/ac)",
    min = 0, max = 268, value = 100, step = 1
  )
})


output$values1 <- render_gt({
    req(input$range_dat)
  
    newdat1 <- dat1() %>%
    filter(fert == input$range_dat) %>%
    mutate(leaching = round(leach1, 1),
           yield = round(yield1, 1))
  
    print('dat1gt')
    print(head(newdat1))

      # remove duplicates
    newdat1 <- newdat1[1,]

    newdat1 %>%
      select(c(fert, yield, leaching)) %>%
      gt() %>%
      cols_label(
        fert = "N fertilizer (lb/ac)",
        yield = "Yield (bu/ac)",
        leaching = "Nitrate leaching (lb/ac)"
      ) %>%
      tab_header(title = paste(input$simName[1], "output"))
   })
    
  # output$range2 <- renderUI({
  #   
  #     #req(dat2$df)
  # 
  #   sliderInput(
  #     inputId = "range_dat2",
  #     label = "N fertilizer (lb/ac)",
  #     min = 0, max = 268, value = 100, step = 1
  #   )
  # })


output$values2 <- render_gt({
    req(input$range_dat)
    #req(dat2$df)
    newdat2 <- dat2() %>%
      filter(fert == input$range_dat) %>%
      mutate(leaching = round(leach2, 1),
             yield = round(yield2, 1))
    print('dat2gt')
    print(head(newdat2))

    # remove duplicates
    newdat2 <- newdat2[1,]

    newdat2 %>%
      select(c(fert, yield, leaching)) %>%
      gt() %>%
      cols_label(
        fert = "N fertilizer (lb/ac)",
        yield = "Yield (bu/ac)",
        leaching = "Nitrate leaching (lb/ac)"
      ) %>%
      tab_header(title = paste(input$simName[2], "output"))
})


  # reset vals---------------
  observeEvent(input$reset, {
    # reset the map
    react_map(base_map())
    output$plot2 <- NULL
    output$sliderUI <- NULL
    updateSelectizeInput(session = getDefaultReactiveDomain(), "simName", selected = character(0))
    #shinyjs::disable("plot2")
    #shinyjs::disable("sliderUI")
    dat1 <- NULL
    dat2 <- NULL
    

  })

 }