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
    
    # zoom to state view
    if(p$group == "state") {
     
      leafletProxy("map") %>%
        setView(lat = p$lat, lng = p$lng, zoom = 7)
      
      # subset sites to state
      stateSites$df <- sites %>%
        filter(state == p$id)
      
    }
    
    #zoom to county view with marker points
    if(p$group == "county") {
      
      # county map with observation sites
      leafletProxy("map") %>%
        addCircleMarkers(data = stateSites$df,
                         lat = ~lat, lng = ~lon,
                         group = "sites") %>%
        setView(lat = p$lat, lng = p$lng, zoom = 10) 
      
    }
    
  })
  
  # selected point----------------
  #selectedPoint <- reactiveValues(lat = NULL, lon = NULL)
  #point <- reactiveVal()
  selectedPoint <- reactiveVal()
  vals <- reactiveValues(count = 0)
  
  observeEvent(input$map_marker_click, {
    
    print("selected point")
    
    click <- input$map_marker_click
    lat <- click$lat
    print(lat)
    lon <- click$lng
    print(lon)
    
    selectedPoint(
    list(lat = lat, lon = lon)
    )
    # selectedPoint$lat <- lat
    # selectedPoint$lon <- lon
    
    vals$count <- vals$count + 1

    # zoom to site
    leafletProxy("map") %>%
      setView(lng = lon, lat = lat, zoom = 12) %>%
      clearGroup("sampleSite") %>%
      addMarkers(lng = lon, lat = lat, group = "sampleSite")
    
  })
  
  # sim selection UI-------------------
  
  output$simSelectionUI <- renderUI({

    #req(selectedPoint()$lat)
    #req(selectedPoint())
    #req(input$map_marker_click)
    req(vals$count >= 1)

    print("inside render selection")

    tagList(
      uiOutput("select1"),
      br(),
      uiOutput("select2")
    )

  })
  
  output$select1 <- renderUI({
    
    radioButtons(inputId = "simSelect1",
                 label = "Choose Cropping System",
                 choices = simNames,
                 selected = character(0))
    
  })
  
  output$select2 <- renderUI({
    
    req(input$simSelect1)
    
    sims2 <- sims %>%
      filter(cropSystem != input$simSelect1)
    
    simNames2 <- sims2$cropSystem
    
    radioButtons(inputId = "simSelect2",
                 label = "Choose Cropping System to Compare",
                 choices = simNames2,
                 selected = character(0))
    
  })
  
  
  
  # data creation ---------------------------
  
  ## data1----------------
  dat1 <- reactive({
    
    #req(selectedPoint())
    req(input$simSelect1)
    print("inside dat 1")
    
    site_lat <- selectedPoint()$lat
    site_lon <- selectedPoint()$lon
    cornPrice <- input$cornPrice
    fertPrice <- input$fertPrice
    print(site_lat)
    simulation1 <- filter(sims, cropSystem == input$simSelect1) 

    dat <- makeDF(sim = simulation1$simulation, site_lat = site_lat, site_lon = site_lon,
                  cornPrice = cornPrice, fertPrice = fertPrice) %>%
      rename(yield1 = yield,
             leach1 = leaching,
             net1 = net)
    
    print("dat1")
    print(head(dat))
    
    dat
    
  })
  
  ## data2---------------------------
  dat2 <- reactive({
    
    #req(selectedPoint())
    req(input$simSelect2)
    
    site_lat <- selectedPoint()$lat
    site_lon <- selectedPoint()$lon
    cornPrice <- input$cornPrice
    fertPrice <- input$fertPrice
    simulation2 <- filter(sims, cropSystem == input$simSelect2)
    
    makeDF(sim = simulation2$simulation, site_lat = site_lat, site_lon = site_lon,
           cornPrice = cornPrice, fertPrice = fertPrice) %>%
      rename(yield2 = yield,
             leach2 = leaching,
             net2 = net)
    
  })
  
  ## compareDat--------------------
  compareDat <- reactive({
    
    req(input$simSelect2)
    df1 <- dat1()
    df2 <- dat2()
    
    compareDat <- left_join(df1, df2, by = "fert")
    fert10 <- seq(from = 0, to = 300, by = 10)
    fert10 <- data.frame(fert10 = fert10)
    compareDat_fert10 <- left_join(fert10, compareDat, by = c("fert10" = "fert"))
    
    print(head(compareDat_fert10))
    compareDat_fert10
    
  })
  
  # econ plot UI --------------------
  
  # output$econPlotUI <- renderUI({
  #   
  #   req(vals$count >= 1)
  #   req(dat1())
  #   
  #   econDF <- dat1()
  #   econDF$cornVal <- econDF$yield1 * input$cornPrice
  #   econDF$fertCost <- econDF$fert * input$fertPrice
  #   econDF$net <- econDF$cornVal - econDF$fertCost
  #   print("econDF")
  #   print(head(econDF))
  #   
  #   renderPlot({
  #     
  #   ggplot(data = econDF) +
  #       geom_point(aes(x = fert, y = yield1), color = "#5dbb63") +
  #       geom_point(aes(x = fert, y = net), color = "darkgreen") +
  #       geom_point(aes(x = fert, y = leach1), color = "#c99f6e")
  #     
  #   })
  #   
  # })
  
  
  
  # plot UI-------------------------
  
  output$plotUI <- renderUI({

    req(input$simSelect1)
    #req(dat1())
    req(vals$count >= 1)

    plot <- c()
    if(is.null(input$simSelect1) == FALSE) {
      
      plot <- plotlyOutput('plot1')
      
      }
    if(is.null(input$simSelect2) == FALSE) {

      plot <- plotlyOutput('plot2')

      }

    plot

    })

## plot 1--------------------
 output$plot1 <- renderPlotly({
   
   #req(input$simSelect1)

   plot_ly(dat1(), x = ~fert, y = ~ yield1, name = "Yield (bu/ac)",
           type = 'scatter', mode = 'lines+markers',
           line = list(color = "#5dbb63", width = 1),
           marker = list(size = 10, color = "#5dbb63"),
           hovertext = ~ paste("Yield:", round(yield1, 1), "bu/ac"),
           hoverinfo = "text") %>%
     add_trace(y = ~ leach1, name = "Nitrate leaching (lb/ac)",
               line = list(color = "#c99f6e", width = 1),
               marker = list(color = "#c99f6e"),
               hovertext = ~ paste("Nitrate leaching:", round(leach1, 1), "lbs/ac"),
               hoverinfo = "text") %>%
     add_trace(y = ~ net1, name = "Net profits ($/ac)",
               line = list(color = "black", width = 1),
               marker = list(color = "black"),
               hovertext = ~ paste("Return to N:", round(net1, 1), "$/ac"),
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

  ## plot 2--------------------------
 output$plot2 <- renderPlotly({

   #req(input$simSelect2)
   sim1 <- input$simSelect1
   sim2 <- input$simSelect2

   plot_ly(compareDat(), x = ~fert10, y = ~ yield1, name = paste(sim1, "yield (bu/ac)"),
           type = 'scatter', mode = 'lines+markers',
           line = list(color = "#5dbb63", width = 2),
           marker = list(symbol = "x", size = 10, color = "#5dbb63"),
           hovertext = ~ paste(sim1, "yield:",round(yield1, 1), "bu/ac"),
           hoverinfo = "text") %>%
     add_trace(y = ~ leach1, name = paste(sim1, "NO3 leaching (lb/ac)"),
               line = list(color = "#5dbb63", width = 2),
               hovertext = ~paste(sim1, "nitrate leaching:",round(leach1, 1), "lbs/ac"),
               marker = list(symbol = "circle", color = "#5dbb63"),
               #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
               hoverinfo = "text") %>%
     add_trace(y = ~ net1, name = paste(sim1, "return to N ($/ac)"),
               line = list(color = "#5dbb63", width = 1),
               marker = list(symbol = "circle-open", color = "#5dbb63"),
               hovertext = ~ paste(sim1, "return to N:", round(net1, 1), "$/ac"),
               hoverinfo = "text") %>%
     add_trace(y = ~ yield2, name = paste(sim2, "yield (bu/ac)"),
               line = list(color = "#c99f6e", width = 2),
               hovertext = ~paste(sim2, "yield:",round(yield2, 1), "bu/ac"),
               marker = list(symbol = "x", color = "#c99f6e"),
               #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
               hoverinfo = "text") %>%
     add_trace(y = ~ leach2, name = paste(sim2, "NO3 leaching (lb/ac)"),
               line = list(color = "#c99f6e", width = 2),
               hovertext = ~paste(sim2, "nitrate leaching:",round(leach2, 1), "lbs/ac"),
               #hovertemplate = "<i>Surveys: %{text}</i><extra></extra>",
               marker = list(symbol = "circle", color = "#c99f6e"),
               hoverinfo = "text") %>%
     add_trace(y = ~ net2, name = paste(sim2, "return to N ($/ac)"),
               line = list(color = "#c99f6e", width = 1),
               marker = list(symbol = "circle-open", color = "#c99f6e"),
               hovertext = ~ paste(sim2, "return to N:", round(net2, 1), "$/ac"),
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


# slider UI--------------
 
 output$range <- renderUI({
   
   sliderInput(
     inputId = "range_dat",
     label = "N fertilizer (lb/ac)",
     min = 0, max = 268, value = 100, step = 1
   )
   
 })
 
 
  output$sliderUI <- renderUI({
    
    req(vals$count >= 1)
    req(input$simSelect1)
  
    tagList(
      uiOutput("values1"),
      br(),
      uiOutput("values2"),
      br(),
      gt_output("range")
    )
      
  })


output$values1 <- render_gt({
  
  #req(length(input$simSelect1) == 1)
  req(input$range_dat)
  #req(dat1())
  
  newdat1 <- dat1() %>%
    filter(fert == input$range_dat) %>% 
    mutate(leaching = round(leach1, 1),
           yield = round(yield1, 1))
  
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
    tab_header(title = paste(input$simSelect1, "output"))
  
  })
    

  output$values2 <- render_gt({
    
    #req(length(input$simSelect2) == 1)
    req(input$range_dat)
    #req(dat2())
     
    newdat2 <- dat2() %>%
      filter(fert == input$range_dat) %>%
      mutate(leaching = round(leach2, 1),
            yield = round(yield2, 1))
  
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
      tab_header(title = paste(input$simSelect2, "output"))
  })


  # reset vals---------------
  observeEvent(input$reset, {
    # reset the map
    react_map(base_map())
    vals$count = 0
    # resets the points
    selectedPoint()
    dat1()
    dat2()
    compareDat()
    
  })

 }