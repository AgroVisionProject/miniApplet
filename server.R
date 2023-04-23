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
      addCircleMarkers(data = map_df, 
                       lat = ~lat, lng = ~lon,
                       group = "sites") %>%
      groupOptions("state", zoomLevels = 1:10) %>%
      groupOptions("county", zoomLevels = 7:10) %>%
      groupOptions("sites", zoomLevels = 9:15) %>%
      addProviderTiles("Esri.WorldTopoMap")
  }
  
  # make_df <- function(dat) {
  #   dat %>%
  #     filter(sim == input$simName) %>%
  #     filter(lat == sampleSite$lat,
  #            lon == sampleSite$lng)
  # }
  # 
  ##TODO is sampleSite a function? It needs to be reactive to input$map_marker_click
  
  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  
  # reactiveVals for map---------------------------
  
  ## filter agroIbis output to county of interest---------------------
  # selected_county <- reactive({
  #   p <- input$map_shape_click
  #   map_df %>% filter(STATE_NAME == stateName) %>% filter(NAME == p$id)
  #   
  # })
  
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
  
  observeEvent(input$map_marker_click, {
    
    
    map_click_info <- input$map_marker_click
    print("marker click info")
    print(map_click_info)
    print(sim())
    
    # does this zoom into site?
    leafletProxy("map") %>%
      setView(lng = map_click_info$lng, lat = map_click_info$lat, zoom = 11) %>%
      addMarkers(lng = map_click_info$lng, lat = map_click_info$lat)
    
    # 
    # ##TODO this needs to be reactive to the input$simName, as well as the mapclickinfo
    # # does this df need to be a reactive df?
    # # yield_df_sub <- eventReactive(button, { })
    # # yield_df_sub <- yield_df1 %>%
    # #   filter(lat == map_click_info$lat,
    # #          lon == map_click_info$lng)
    # yielddf <- yield_df_sub()
    # #print('yield df')
    # # print(yield_df_sub)
    # 
    # ##TODO this needs to be reactive to the input$simName, as well as the mapclickinfo
    # # leach_df_sub <- eventReactive(button, { })
    # # leach_df_sub <- leach_df1 %>%
    # #   filter(lat == map_click_info$lat,
    # #          lon == map_click_info$lng)
    # # print('leach df')
    # # print(leach_df_sub)
    # leach_df <- leach_df_sub()
    # 
    # yield_y <- c()
    # if(yield_df$fun == 0) {
    #   yield_y = rep(yield_df$a, times = length(fert))
    # }
    # if(yield_df$fun == 3) {
    #   yield_y = yield_df$a * exp((-yield_df$b)*(yield_df$c^fert))
    # }
    # if(yield_df$fun == 4) {
    #   yield_y = ifelse(fert < yield_df$c, yield_df$a + (yield_df$b*fert),
    #                    yield_df$a + (yield_df$b*yield_df$c))
    # }
    # if(yield_df$fun == 5) {
    #   yield_y = yield_df$a + (yield_df$b*fert)
    # }
    # #print(yield_y)
    # 
    # leach_y <- c()
    # if(leach_df$fun == 0) {
    #   leach_y = rep(leach_df$a, times = length(fert))
    # }
    # if(leach_df$fun == 1) {
    #   leach_y = leach_df$a + (leach_df$b*fert) + (leach_df$c*fert^2)
    # }
    # #print(leach_y)
    # 
    # ##TODO should this dataframe be reactive?
    # df = data.frame(fert = fert, yield = yield_y, leaching = leach_y)
    # print(df)
    # 
    # 
    # output$plot1 <- renderPlotly({
    #   
    #   # validate(
    #   #   need(df, "select new location")
    #   # )
    #   #
    #   # if(is.null(df)) {
    #   #   output$plot1 <- NULL
    #   # }
    #   
    #   #output$plotDone <- renderUI({tags$input(type="hidden", value="TRUE")})
    #   
    #   plot_ly(df, x = ~fert, y = ~ yield, name = "Yield (bu/ac)",
    #           type = 'scatter', mode = 'lines',
    #           line = list(color = "#5dbb63", width = 4),
    #           hovertext = ~ round(yield, 2),
    #           hoverinfo = "text") %>%
    #     add_trace(y = ~ leaching, name = "Nitrate leaching (lb/ac)",
    #               line = list(color = "#c99f6e", width = 4),
    #               hovertext = ~ round(leaching, 2),
    #               hoverinfo = "text") %>%
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
    #     inputId = "fertRange",
    #     label = "N fertilizer (kg/ha)",
    #     min = 0, max = 300, value = 100, step = 10
    #   )
    # })
    # 
    # output$values <- render_gt({
    #   
    #   Nval <- unique(input$fertRange)
    #   
    #   df %>%
    #     filter(fert == Nval) %>%
    #     mutate(leaching = round(leaching, 2),
    #            yield = round(yield, 2)) %>%
    #     gt() %>%
    #     cols_label(
    #       fert = "N fertilizer (kg/ha)",
    #       yield = "Yield (bu/ac)",
    #       leaching = "Nitrate leaching (kg/ha)"
    #     )
    # })
  })
  
  
  # determine which sim number
  sim <- reactive({

    filter(sims, cropSystem == input$simName)$simulation

  })
  
  #print(sim())
  
  # plot_lat <- eventReactive(input$map_marker_click, {
  #   
  #   map_click_info <- input$map_marker_click
  #   map_click_info$lat
  #   
  # })
  # 
  # print(plot_lat())
  # 
  # plot_lon <- eventReactive(input$map_marker_click, {
  #   
  #   map_click_info <- input$map_marker_click
  #   map_click_info$lng
  #   
  # })
  # 
  # print(plot_lon())
  
#   yield_df_sub <- eventReactive(input$plot, {
#     
#     #print(sim())
#     #print(plot_lat())
#     
#     yield_df %>% filter(sim == sim()) %>% 
#       filter(lat == plot_lat(),
#              lon == plot_lon())
# 
#   })
#   
#   print(yield_df_sub())
#   
#   leach_df_sub <- eventReactive(input$plot, {
#     
#     leach_df %>% filter(sim == sim()) %>% 
#       filter(lat == plot_lat(),
#              lon == plot_lon())
#     
#   })
#   
#   print(leach_df_sub())
#   
# }
#   
  # observeEvent(input$map_marker_click, {
  # 
  # 
  #   map_click_info <- input$map_marker_click
  #   #print("marker click info")
  #   #print(map_click_info)
  # 
  #   # does this zoom into site?
  #   leafletProxy("map") %>%
  #     setView(lng = map_click_info$lng, lat = map_click_info$lat, zoom = 11) %>%
  #     addMarkers(lng = map_click_info$lng, lat = map_click_info$lat)
  # 
  #   ID = map_click_info$id
  #   print(paste("line 119", ID))
  # 
  #   ##TODO this needs to be reactive to the input$simName, as well as the mapclickinfo
  #   # does this df need to be a reactive df?
  #   # yield_df_sub <- eventReactive(button, { })
  #   # yield_df_sub <- yield_df1 %>%
  #   #   filter(lat == map_click_info$lat,
  #   #          lon == map_click_info$lng)
  #   yielddf <- yield_df_sub()
  #   #print('yield df')
  #   # print(yield_df_sub)
  # 
  #   ##TODO this needs to be reactive to the input$simName, as well as the mapclickinfo
  #   # leach_df_sub <- eventReactive(button, { })
  #   # leach_df_sub <- leach_df1 %>%
  #   #   filter(lat == map_click_info$lat,
  #   #          lon == map_click_info$lng)
  #   # print('leach df')
  #   # print(leach_df_sub)
  #   leach_df <- leach_df_sub()
  # 
  #   yield_y <- c()
  #   if(yield_df$fun == 0) {
  #     yield_y = rep(yield_df$a, times = length(fert))
  #   }
  #   if(yield_df$fun == 3) {
  #     yield_y = yield_df$a * exp((-yield_df$b)*(yield_df$c^fert))
  #   }
  #   if(yield_df$fun == 4) {
  #     yield_y = ifelse(fert < yield_df$c, yield_df$a + (yield_df$b*fert),
  #                      yield_df$a + (yield_df$b*yield_df$c))
  #   }
  #   if(yield_df$fun == 5) {
  #     yield_y = yield_df$a + (yield_df$b*fert)
  #   }
  #   #print(yield_y)
  # 
  #   leach_y <- c()
  #   if(leach_df$fun == 0) {
  #     leach_y = rep(leach_df$a, times = length(fert))
  #   }
  #   if(leach_df$fun == 1) {
  #     leach_y = leach_df$a + (leach_df$b*fert) + (leach_df$c*fert^2)
  #   }
  #   #print(leach_y)
  # 
  #   ##TODO should this dataframe be reactive?
  #   df = data.frame(fert = fert, yield = yield_y, leaching = leach_y)
  #   print(df)
  # 
  # 
  #     output$plot1 <- renderPlotly({
  # 
  #       # validate(
  #       #   need(df, "select new location")
  #       # )
  #       #
  #       # if(is.null(df)) {
  #       #   output$plot1 <- NULL
  #       # }
  # 
  #       #output$plotDone <- renderUI({tags$input(type="hidden", value="TRUE")})
  # 
  #       plot_ly(df, x = ~fert, y = ~ yield, name = "Yield (bu/ac)",
  #               type = 'scatter', mode = 'lines',
  #               line = list(color = "#5dbb63", width = 4),
  #               hovertext = ~ round(yield, 2),
  #               hoverinfo = "text") %>%
  #         add_trace(y = ~ leaching, name = "Nitrate leaching (lb/ac)",
  #                   line = list(color = "#c99f6e", width = 4),
  #                   hovertext = ~ round(leaching, 2),
  #                   hoverinfo = "text") %>%
  #         layout(title = "Corn yield and nitrate leaching response \n to N fertilizer \n",
  #                xaxis = list(title = "N fertilizer (N lb/ac)"),
  #                yaxis = list (title = " "),
  #                hovermode = "x unified")
  # 
  #     })
  # 
  #     output$range <- renderUI({
  # 
  #       sliderInput(
  #         inputId = "fertRange",
  #         label = "N fertilizer (kg/ha)",
  #         min = 0, max = 300, value = 100, step = 10
  #       )
  #     })
  # 
  #     output$values <- render_gt({
  # 
  #       Nval <- unique(input$fertRange)
  # 
  #       df %>%
  #         filter(fert == Nval) %>%
  #         mutate(leaching = round(leaching, 2),
  #                yield = round(yield, 2)) %>%
  #         gt() %>%
  #         cols_label(
  #           fert = "N fertilizer (kg/ha)",
  #           yield = "Yield (bu/ac)",
  #           leaching = "Nitrate leaching (kg/ha)"
  #         )
  #     })
  # })
#   
#   
#   
#   
  observeEvent(input$reset, {
    # reset the map
    react_map(base_map())
    # reset the click counts
    #clicks$count <- 0
    #output$plot1 <- NULL
    #df <- NULL

  })

 }