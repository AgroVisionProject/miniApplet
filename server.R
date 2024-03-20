# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  
  # reactiveVals for map---------------------------
  
  # map-------------------------
  output$map <- renderLeaflet({
    react_map()
  })
  
  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    #print(click)
    
    if (click$group == "state") {
      
      state <- states %>% filter(state == click$id)
      lat <- as.numeric(state$stateLat)
      lon <- as.numeric(state$stateLin)
      leafletProxy("map") %>%
        setView(lat = lat, lng = lon, zoom = 8)
      #return()
      
    } 
    
    if (click$group == "county") {
      
      county = county_centroids %>% filter(id == click$id)
      #print(county)
      
      leafletProxy("map") %>%
        setView(
          lat = county$lat,
          lng = county$lon,
          zoom = 10
        )
    } 
    
  })
  
  
  # selected site----------------
  selectedSite <- reactiveVal()
  
  observeEvent(input$map_marker_click, {
    
    click <- input$map_marker_click
    print(click)
    site <- sites %>% filter(id == click$id)
    selectedSite(site)
    
  })
  
  observeEvent(input$map_marker_click, {
    
    req(selectedSite())
    
    leafletProxy("map") %>%
      clearGroup("cur_site") %>%
      addMarkers(
        data = selectedSite(), 
        lat = ~selectedSite()$lat, lng = ~selectedSite()$lon,
        group = "cur_site"
      )
    
  })
  
  # sim selection UI-------------------
  
  output$simSelectionUI <- renderUI({

    req(selectedSite())

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
                 selected = "Rainfed continuous corn")
    
  })
  
  output$select2 <- renderUI({
    
    req(input$simSelect1)
    
    sims2 <- sims %>%
      filter(cropSystem != input$simSelect1)
    
    simNames2 <- sims2$cropSystem
    
    radioButtons(inputId = "simSelect2",
                 label = "Choose Cropping System to Compare",
                 choices = c("None", simNames2),
                 selected = "None")
    
  })
  
  
  # prices UI--------------------
  
  output$pricesUI <- renderUI({
    
    req(selectedSite())
    #req(vals$count >= 1)
    
    fluidRow(column(6,
                    numericInput("cornPrice", "Price of corn ($/bu)", value = 5, min = 1, max = 20)
                    ),
             column(6,
                    numericInput("fertPrice", "Price of N fertilizer ($/lb)", value = 1, min = 0, max = 20)
                    ))
    
  })
  
  
  # data creation ---------------------------
  
  ## data1----------------
  dat1 <- reactive({
    
    req(selectedSite())
    req(input$simSelect1)
    req(input$cornPrice)
    req(input$fertPrice)
    #print("inside dat 1")
    
    site_lat <- selectedSite()$lat
    site_lon <- selectedSite()$lon
    cornPrice <- input$cornPrice
    fertPrice <- input$fertPrice
    # NUE <- input$NUE
    # cornTech <- input$cornImp
    # fertEff <- input$fertImp
    simulation1 <- filter(sims, cropSystem == input$simSelect1) 
    #simulation1 <- filter(sims, simulation == 1)
    
    dataList <- makeDF(sim = simulation1$simulation, site_lat = site_lat, site_lon = site_lon,
           cornPrice = cornPrice, fertPrice = fertPrice) 
    #print(dataList)
    
    data1 <- dataList[[1]] %>%
      rename(yield1 = yield,
             leach1 = leaching,
             conc1 = concentration,
             net1 = net)
    #print(data1)
    
    stdev1 <- dataList[[2]] %>%
      rename(yield1 = yield,
             leach1 = leaching,
             conc1 = concentration,
             yld_stdev1 = stdev_cropyld,
             leach_stdev1 = stdev_no3leach,
             conc_stdev1 = stdev_no3conc)
    #print(stdev1)
    
    return(list(data1, stdev1))
    
  })
  
  ## data2---------------------------
  dat2 <- reactive({
    
    req(selectedSite())
    req(input$simSelect2)
    
    site_lat <- selectedSite()$lat
    site_lon <- selectedSite()$lon
    cornPrice <- input$cornPrice
    fertPrice <- input$fertPrice
    #NUE <- input$NUE
    #cornTech <- input$cornImp
    #fertEff <- input$fertImp
    simulation2 <- filter(sims, cropSystem == input$simSelect2)
    
    dataList <- makeDF(sim = simulation2$simulation, site_lat = site_lat, site_lon = site_lon,
                       cornPrice = cornPrice, fertPrice = fertPrice)
    #print(dataList)
    
    data1 <- dataList[[1]] %>%
      rename(yield2 = yield,
             leach2 = leaching,
             conc2 = concentration,
             net2 = net)
    #print(data1)
    
    stdev1 <- dataList[[2]] %>%
      rename(yield2 = yield,
             leach2 = leaching,
             conc2 = concentration,
             yld_stdev2 = stdev_cropyld,
             leach_stdev2 = stdev_no3leach,
             conc_stdev2 = stdev_no3conc)
    #print(stdev1)
    
    return(list(data1, stdev1))
    
  })
  
  
  # plot UI-------------------------
  
  output$plotUI <- renderUI({

    req(input$simSelect1)
    #req(dat1())
    #req(dat2())
    #req(vals$count >= 1)
    req(input$simSelect2)
    req(selectedSite())
    
    sim1 <- str_to_title(input$simSelect1)
    sim2 <- str_to_title(input$simSelect2)
    if(input$simSelect2 == "None") {
      title <- paste("Responses to Fertilizer N (30 year average) in", sim1)
      plotYldAndRtN <- plotlyOutput('plotYieldReturnSim1')
      plotYldAndLeach <- plotlyOutput('plotYieldLeachSim1')
      plotYldAndConc <- plotlyOutput('plotYieldConcSim1')
    } else {
      title <- paste("Responses to Fertilizer N (30 year average) in", sim1, "and", sim2)
      plotYldAndRtN <- plotlyOutput('plotYieldReturnSim2')
      plotYldAndLeach <- plotlyOutput('plotYieldLeachSim2')
      plotYldAndConc <- plotlyOutput('plotYieldConcSim2')
    }

    tagList(
      tags$h4(title),
      tabsetPanel(
        tabPanel("Yield and Return to N",
                 plotYldAndRtN),
        tabPanel("Yield and Nitrate Leaching",
                 plotYldAndLeach),
        tabPanel("Yield and Nitrate Concentration",
                 plotYldAndConc),
        footer = "Click on legend items to add or remove variables from plot",
        )
    )

    })

## yield & return to N plot sim1-----------------------------
  
  output$plotYieldReturnSim1 <- renderPlotly({
    
    req(dat1())
    
    data1 <- dat1()[[1]]
    stdev1 <- dat1()[[2]]
    
    plot_ly(data = data1, x = ~fert, hoverinfo = "text") %>%
      add_lines(y = ~ yield1, name = "Yield (bu/ac)",
                yaxis = "y2",
                line = list(color = "#ff9843", width = 4, dash = "solid"),
                hovertext = ~ paste("Yield:",round(yield1, 1), "bu/ac"),
                legendgroup = "yield1") %>%
      add_lines(y = ~ net1, name = "Return to N ($/ac)",
                line = list(color = "#ff9843", width = 4, dash = "dot"),
                hovertext = ~ paste("Return to N:", round(net1, 1), "$/ac"),
                legendgroup = "net1") %>%
      add_ribbons(data = stdev1, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
                  line = list(
                    color = "#ff9843",
                    width = 1,
                    opacity = 0.5),
                  fillcolor = "#ff9843",
                  yaxis = "y2",
                  hovertext = ~paste("±", round(yld_stdev1)),
                  opacity = 0.5,
                  legendgroup = "yield1", showlegend = FALSE) %>%
      layout(
        xaxis = list(title = list(text =  "N fertilizer (N lb/ac)",
                                  font = list(size = 15))),
        yaxis = list(title = list(text = "Return to N ($/ac)",
                                  font = list(size = 15))),
        yaxis2 = yield_y,
        hovermode = "x unified",
        margin = list(r = 50, b = 10, t = 50),
        legend = list(orientation = 'h', y = -0.5, 
                      font = list(size = 14))
        ) 
    
  })
  
## yield & return to N plot sim2-----------------------------
  
  output$plotYieldReturnSim2 <- renderPlotly({
    
    sim1 <- input$simSelect1
    sim2 <- input$simSelect2
    
    data1 <- dat1()[[1]]
    stdev1 <- dat1()[[2]]
    data2 <- dat2()[[1]]
    stdev2 <- dat2()[[2]]
    
    plot_ly(data = data1, x = ~fert, hoverinfo = "text") %>%
      add_lines(y = ~ yield1, name = paste(sim1, "yield (bu/ac)"),
                yaxis = "y2",
                line = list(color = "#ff9843", width = 4, dash = "solid"),
                hovertext = ~ paste(sim1, "yield:",round(yield1, 1), "bu/ac"),
                legendgroup = "yield1") %>%
      add_lines(y = ~ net1, name = paste(sim1, "return to N ($/ac)"),
                line = list(color = "#ff9843", width = 4, dash = "dot"),
                hovertext = ~ paste(sim1, "return to N:", round(net1, 1), "$/ac"),
                legendgroup = "net1") %>%
      add_ribbons(data = stdev1, x = ~fert, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
                  line = list(
                    color = "#ff9843",
                    width = 1,
                    opacity = 0.5),
                  fillcolor = "#ff9843",
                  yaxis = "y2",
                  hovertext = ~paste("±", round(yld_stdev1)),
                  opacity = 0.5,
                  legendgroup = "yield1", showlegend = FALSE) %>%
      add_lines(data = data2, y = ~ yield2, name = paste(sim2, "yield (bu/ac)"),
                line = list(color = "#3468c0", width = 4, dash = "solid"),
                hovertext = ~ paste(sim2, "yield:",round(yield2, 1), "bu/ac"),
                yaxis = "y2",
                hoverinfo = "text",
                legendgroup = "yield2") %>%
      add_lines(y = ~ net2, name = paste(sim2, "return to N ($/ac)"),
                line = list(color = "#3468c0", width = 4, dash = "dot"),
                hovertext = ~ paste(sim2, "return to N:", round(net2, 1), "$/ac"),
                legendgroup = "net2") %>%
      add_ribbons(data = stdev2, ymin = ~ yield2 - yld_stdev2, ymax = ~ yield2 + yld_stdev2,
                  line = list(
                    color = "#3468c0",
                    width = 5,
                    opacity = 0.5),
                  fillcolor = "#3468c0",
                  opacity = 0.75,
                  yaxis = "y2",
                  hovertext = ~paste("±", round(yld_stdev2)),
                  legendgroup = "yield2", showlegend = FALSE) %>%
      layout(
        xaxis = list(title = list(text =  "N fertilizer (N lb/ac)",
                                  font = list(size = 15))),
        yaxis = list(title = list(text = "Return to N ($/ac)",
                                  font = list(size = 15))),
        yaxis2 = yield_y,
        hovermode = "x unified",
        margin = list(r = 50, b = 10, t = 50),
        legend = list(orientation = 'h', y = -0.5, 
                      font = list(size = 14))) 
  
  })  
  
  ## yield and leaching plot sim1----------------------
  
  output$plotYieldLeachSim1 <- renderPlotly({
    
    req(dat1())
    
    data1 <- dat1()[[1]]
    stdev1 <- dat1()[[2]]
    
    plot_ly(data = data1, x = ~fert, hoverinfo = "text") %>%
      add_lines(y = ~ yield1, name = "Yield (bu/ac)",
                yaxis = "y2",
                line = list(color = "#ff9843", width = 4, dash = "solid"),
                hovertext = ~ paste("Yield:",round(yield1, 1), "bu/ac"),
                legendgroup = "yield1") %>%
      add_lines(y = ~ leach1, name = "NO<sub>3</sub> leaching (lb/ac)",
                line = list(color = "#ff9843", width = 4, dash = "dot"),
                hovertext = ~paste("NO<sub>3</sub> leaching:",round(leach1, 1), "lbs/ac"),
                legendgroup = "leach1") %>%
      add_ribbons(data = stdev1, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
                  line = list(
                    color = "#ff9843",
                    width = 0.5,
                    opacity = 0),
                  fillcolor = "#ff9843",
                  yaxis = "y2",
                  hovertext = ~paste("Yield: ±", round(yld_stdev1)),
                  opacity = 0.5,
                  legendgroup = "yield1", showlegend = FALSE) %>%
      add_ribbons(ymin = ~ leach1 - leach_stdev1, ymax = ~ leach1 + leach_stdev1,
                  line = list(
                    color = "#ff9843",
                    width = 0.5,
                    opacity = 0),
                  hovertext = ~paste("NO<sub>3</sub> leaching: ±", round(leach_stdev1)),
                  fillcolor = "#ff9843",
                  opacity = 0.5,
                  legendgroup = "leach1", showlegend = FALSE) %>%
      layout(xaxis = list(title = list(text =  "N fertilizer (N lb/ac)",
                                       font = list(size = 15))),
             yaxis = list(title = list(text = "NO<sub>3</sub> leaching (lb/ac)",
                                       font = list(size = 15))),
             yaxis2 = yield_y,
             hovermode = "x unified",
             margin = list(r = 50, b = 10, t = 50),
             legend = list(orientation = 'h', y = -0.5, 
                           font = list(size = 14))) 
    
    
  })
  
  ## yield and leaching plot sim2------------
  
  output$plotYieldLeachSim2 <- renderPlotly({
    
    sim1 <- input$simSelect1
    sim2 <- input$simSelect2
    
    data1 <- dat1()[[1]]
    stdev1 <- dat1()[[2]]
    data2 <- dat2()[[1]]
    stdev2 <- dat2()[[2]]
    
    plot_ly(data = data1, x = ~fert, hoverinfo = "text") %>%
      add_lines(y = ~ yield1, name = paste(sim1, "yield (bu/ac)"),
                yaxis = "y2",
                line = list(color = "#ff9843", width = 4, dash = "solid"),
                hovertext = ~ paste(sim1, "yield:",round(yield1, 1), "bu/ac"),
                legendgroup = "yield1") %>%
      add_lines(y = ~ leach1, name = paste(sim1, "NO<sub>3</sub> leaching (lb/ac)"),
                line = list(color = "#ff9843", width = 4, dash = "dot"),
                hovertext = ~paste(sim1, "NO<sub>3</sub> leaching:",round(leach1, 1), "lbs/ac"),
                legendgroup = "leach1") %>%
      add_ribbons(data = stdev1, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
                  line = list(
                    color = "#ff9843",
                    width = 0.5,
                    opacity = 0),
                  fillcolor = "#ff9843",
                  yaxis = "y2",
                  hovertext = ~paste(sim1, "yield: ±", round(yld_stdev1)),
                  opacity = 0.5,
                  legendgroup = "yield1", showlegend = FALSE) %>%
      add_ribbons(ymin = ~ leach1 - leach_stdev1, ymax = ~ leach1 + leach_stdev1,
                  line = list(
                    color = "#ff9843",
                    width = 0.5,
                    opacity = 0),
                  hovertext = ~paste(sim1, "NO<sub>3</sub> leaching: ±", round(leach_stdev1)),
                  fillcolor = "#ff9843",
                  opacity = 0.5,
                  legendgroup = "leach1", showlegend = FALSE) %>%
      add_lines(data = data2, y = ~ yield2, name = paste(sim2, "yield (bu/ac)"),
                line = list(color = "#5dbb63", width = 4, dash = "solid"),
                hovertext = ~ paste(sim2, "yield:",round(yield2, 1), "bu/ac"),
                hoverinfo = "text",
                yaxis = "y2",
                legendgroup = "yield2") %>%
      add_lines(y = ~ leach2, name = paste(sim2, "NO<sub>3</sub> leaching (lb/ac)"),
                line = list(color = "#5dbb63", width = 4, dash = "dot"),
                hovertext = ~ paste(sim2, "NO<sub>3</sub> leaching:", round(leach2, 1), "$/ac"),
                legendgroup = "net2") %>%
      add_ribbons(data = stdev2,  ymin = ~ yield2 - yld_stdev2, ymax = ~ yield2 + yld_stdev2,
                  line = list(
                    color = "#5dbb63",
                    width = 0.5,
                    opacity = 0),
                  fillcolor = "#5dbb63",
                  yaxis = "y2",
                  opacity = 0.5,
                  hovertext = ~paste(sim2, "yield:", "±", round(yld_stdev2)),
                  legendgroup = "yield2", showlegend = FALSE) %>%
      add_ribbons(ymin = ~ leach2 - leach_stdev2, ymax = ~ leach2 + leach_stdev2,
                  line = list(
                    color = "#5dbb63",
                    width = 0.5,
                    opacity = 0),
                  fillcolor = "#5dbb63",
                  opacity = 0.5,
                  hovertext = ~paste(sim2, "NO<sub>3</sub> leaching: ±", round(leach_stdev2)),
                  legendgroup = "leach2", showlegend = FALSE) %>%
      layout(xaxis = list(title = list(text =  "N fertilizer (N lb/ac)",
                                       font = list(size = 15))),
             yaxis = list(title = list(text = "NO<sub>3</sub> leaching (lb/ac)",
                                       font = list(size = 15))),
             yaxis2 = yield_y,
             hovermode = "x unified",
             margin = list(r = 50, b = 10, t = 50),
             legend = list(orientation = 'h', y = -0.5, 
                           font = list(size = 14))) 
    
  })
  
  ## yield and concentration plot sim1----------------------
  
  output$plotYieldConcSim1 <- renderPlotly({
    
    req(dat1())
    
    data1 <- dat1()[[1]]
    stdev1 <- dat1()[[2]]
    
    plot_ly(data = data1, x = ~fert, hoverinfo = "text") %>%
      add_lines(y = ~ yield1, name = "Yield (bu/ac)",
                yaxis = "y2",
                line = list(color = "#ff9843", width = 4, dash = "solid"),
                hovertext = ~ paste("Yield:",round(yield1, 1), "bu/ac"),
                legendgroup = "yield1") %>%
      add_lines(y = ~ conc1, name = "NO<sub>3</sub> Concentration (ppm)",
                line = list(color = "#ff9843", width = 4, dash = "dot"),
                hovertext = ~paste("NO<sub>3</sub> concentration:",round(conc1, 1), "(ppm)"),
                legendgroup = "conc1") %>%
      add_lines(y = 10, name = "Max safe NO<sub>3</sub> (10 ppm)",
                line = list(color = "black", width = 4, dash = "solid"),
                hovertext='Max safe NO<sub>3</sub>') %>%
      add_ribbons(data = stdev1, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
                  line = list(
                    color = "#ff9843",
                    width = 0.5,
                    opacity = 0),
                  fillcolor = "#ff9843",
                  yaxis = "y2",
                  hovertext = ~paste("Yield: ±", round(yld_stdev1)),
                  opacity = 0.5,
                  legendgroup = "yield1", showlegend = FALSE) %>%
      add_ribbons(ymin = ~ conc1 - conc_stdev1, ymax = ~ conc1 + conc_stdev1,
                  line = list(
                    color = "#ff9843",
                    width = 0.5,
                    opacity = 0),
                  fillcolor = "#ff9843",
                  hovertext = ~paste("NO<sub>3</sub> concentration: ±", round(conc_stdev1)),
                  opacity = 0.5,
                  legendgroup = "conc1", showlegend = FALSE) %>%
      layout(xaxis = list(title = list(text =  "N fertilizer (N lb/ac)",
                                       font = list(size = 15))),
             yaxis = list(title = list(text = "NO<sub>3</sub> concentration (ppm)",
                                       font = list(size = 15))),
             yaxis2 = yield_y,
             hovermode = "x unified",
             margin = list(r = 50, b = 10, t = 50),
             legend = list(orientation = 'h', y = -0.5, 
                           font = list(size = 14))) 
    
  })
  
  ## yield and concentration plot sim2----------------------
  
  output$plotYieldConcSim2 <- renderPlotly({
    
    sim1 <- input$simSelect1
    sim2 <- input$simSelect2
    
    data1 <- dat1()[[1]]
    stdev1 <- dat1()[[2]]
    data2 <- dat2()[[1]]
    stdev2 <- dat2()[[2]]
    
    plot_ly(data = data1, x = ~fert, hoverinfo = "text") %>%
      add_lines(y = ~ yield1, name = paste(sim1, "yield (bu/ac)"),
                yaxis = "y2",
                line = list(color = "#ff9843", width = 4, dash = "solid"),
                hovertext = ~ paste(sim1, "yield:",round(yield1, 1), "bu/ac"),
                legendgroup = "yield1") %>%
      add_lines(y = ~ conc1, name = paste(sim1, "NO<sub>3</sub> Concentration (ppm)"),
                line = list(color = "#ff9843", width = 4, dash = "dot"),
                hovertext = ~paste(sim1, "NO<sub>3</sub> concentration:",round(conc1, 1), "(ppm)"),
                legendgroup = "conc1") %>%
      add_ribbons(data = stdev1, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
                  line = list(
                    color = "#ff9843",
                    width = 0.5,
                    opacity = 0),
                  fillcolor = "#ff9843",
                  yaxis = "y2",
                  hovertext = ~paste(sim1, "yield: ±", round(yld_stdev1)),
                  opacity = 0.5,
                  legendgroup = "yield1", showlegend = FALSE) %>%
      add_ribbons(ymin = ~ conc1 - conc_stdev1, ymax = ~ conc1 + conc_stdev1,
                  line = list(
                    color = "#ff9843",
                    width = 0.5,
                    opacity = 0),
                  fillcolor = "#ff9843",
                  hovertext = ~paste(sim1, "NO<sub>3</sub> concentration: ±", round(conc_stdev1)),
                  opacity = 0.5,
                  legendgroup = "conc1", showlegend = FALSE) %>%
      add_lines(data = data2, y = ~ yield2, name = paste(sim2, "yield (bu/ac)"),
                line = list(color = "#593587", width = 3, dash = "solid"),
                hovertext = ~ paste(sim2, "yield:",round(yield2, 1), "bu/ac"),
                hoverinfo = "text",
                yaxis = "y2",
                legendgroup = "yield2") %>%
      add_lines(y = ~ conc2, name = paste(sim2, "NO<sub>3</sub> concentration (ppm)"),
                line = list(color = "#593587", width = 3, dash = "dot"),
                #yaxis = "y2",
                hovertext = ~ paste(sim2, "NO<sub>3</sub> concentration:", round(conc2, 1), "ppm"),
                legendgroup = "conc2") %>%
      add_ribbons(data = stdev2, ymin = ~ yield2 - yld_stdev2, ymax = ~ yield2 + yld_stdev2,
                  line = list(
                    color = "#593587",
                    width = 0.5,
                    opacity = 0),
                  fillcolor = "#593587",
                  yaxis = "y2",
                  opacity = 0.5,
                  hovertext = ~paste(sim2, "yield:", "±", round(yld_stdev2)),
                  legendgroup = "yield2", showlegend = FALSE) %>%
      add_ribbons(ymin = ~ conc2 - conc_stdev2, ymax = ~ conc2 + conc_stdev2,
                  line = list(
                    color = "#593587",
                    width = 0.5,
                    opacity = 0),
                  fillcolor = "#593587",
                  opacity = 0.5,
                  hovertext = ~paste(sim2, "NO<sub>3</sub> concentration:", "±", round(conc_stdev2)),
                  legendgroup = "conc2", showlegend = FALSE) %>%
      add_lines(y = 10, name = "Max safe NO<sub>3</sub> (10 ppm)",
                line = list(color = "black", width = 4, dash = "solid"),
                hovertext='Max safe NO<sub>3</sub>') %>%
      layout(xaxis = list(title = list(text =  "N fertilizer (N lb/ac)",
                                       font = list(size = 15))),
             yaxis = list(title = list(text = "NO<sub>3</sub> concentration (ppm)",
                                       font = list(size = 15))),
             yaxis2 = yield_y,
             hovermode = "x unified",
             margin = list(r = 50, b = 10, t = 50),
             legend = list(orientation = 'h', y = -0.5, 
                           font = list(size = 14))) 
    
  })
  
  # slider UI--------------
 
 output$range <- renderUI({
   
   maxFert <- max(dat1()[[1]]$fert)
   print(maxFert)
   
   sliderInput(
     inputId = "range_dat",
     label = "N fertilizer (lb/ac)",
     min = 0, max = maxFert, value = 100, step = 1
   )
   
 })
 
 
  output$sliderUI <- renderUI({
    
    #req(vals$count >= 1)
    req(input$simSelect1)
    req(selectedSite())
  
    tagList(
      uiOutput("values1"),
      br(),
      uiOutput("values2"),
      br(),
      p("Use the slider to view responses at specific N rates in the table"),
      gt_output("range")
    )
      
  })


output$values1 <- render_gt({
  
  #req(length(input$simSelect1) == 1)
  req(input$range_dat)
  
  newdat1 <- dat1()[[1]] %>%
    filter(fert == input$range_dat) %>% 
    mutate(leaching = round(leach1, 1),
           yield = round(yield1, 1),
           concentration = round(conc1, 1),
           net = round(net1, 1))
  
  # remove duplicates
  newdat1 <- newdat1[1,]

  newdat1 %>%
    select(c(fert, yield, leaching, concentration, net)) %>%
    gt() %>%
    cols_label(
      fert = "N fertilizer (lb/ac)",
      yield = "Yield (bu/ac)",
      leaching = "{{NO_3}} leaching (lb/ac)",
      concentration = "{{NO_3}} concentration (ppm)",
      net = "RTN ($/ac)",
      .fn = md
    ) %>%
    tab_header(title = paste(input$simSelect1, "output"))
  
  })
    

  output$values2 <- render_gt({
    
    #req(length(input$simSelect2) == 1)
    req(input$range_dat)
    #req(dat2())
    
    newdat2 <- dat2()[[1]] %>%
      filter(fert == input$range_dat) %>%
      mutate(leaching = round(leach2, 1),
            yield = round(yield2, 1),
            concentration = round(conc2, 1),
            net = round(net2, 1))
  
    # remove duplicates
    newdat2 <- newdat2[1,]

    newdat2 %>%
      select(c(fert, yield, leaching, concentration, net)) %>%
      gt() %>%
      cols_label(
        fert = "N fertilizer (lb/ac)",
        yield = "Yield (bu/ac)",
        leaching = "{{NO_3}} leaching (lb/ac)",
        concentration = "{{NO_3}} concentration (ppm)",
        net = "RTN ($/ac)",
        .fn = md
      ) %>%
      tab_header(title = paste(input$simSelect2, "output"))
  })


  # reset vals---------------
  observeEvent(input$reset, {
    # reset the map
    leafletProxy("map") %>%
      clearGroup("cur_site") %>%
      setView(lat = 41.5, lng = -93.5, zoom = 4) 
    selectedSite(NULL)
    
  })

}



## compareDat--------------------
# compareDat <- reactive({
#   
#   req(input$simSelect2)
#   df1 <- dat1()[[1]]
#   df2 <- dat2()[[1]]
#   
#   compareDat <- left_join(df1, df2) 
#   print("compareDat")
#   print(head(compareDat))
#   
#   stdev1 <- dat1()[[2]]
#   stdev2 <- dat2()[[2]]
#   
#   compareStdev <- left_join(stdev1, stdev2) 
#   print("compareDat")
#   print(head(compareDat))
#   
#   stdevCompare 
#   
#   
#   compareDat
#   
# })

# ## plot 1--------------------
# output$plot1 <- renderPlotly({
#   
#   req(dat1())
#   
#   data1 <- dat1()[[1]]
#   stdev1 <- dat1()[[2]]
#   
#   plot_ly(data1, x = ~fert, hoverinfo = "text") %>%
#     add_lines(y = ~ yield1, name = "Yield (bu/ac)",
#               line = list(color = "#5dbb63", width = 2),
#               hovertext = ~ paste("Yield:", round(yield1, 1), "bu/ac"),
#               legendgroup = "yield") %>%
#     add_lines(y = ~ leach1, name = "NO<sub>3</sub> leaching (lb/ac)",
#               line = list(color = "#c99f6e", width = 2),
#               hovertext = ~ paste("NO<sub>3</sub> leaching:", round(leach1, 1), "lbs/ac"),
#               legendgroup = "leach") %>%
#     add_lines(y = ~ conc1, name = "NO<sub>3</sub> concentration (ppm)",
#               line = list(color = "#6e8fc9", width = 2),
#               hovertext = ~ paste("NO<sub>3</sub> concentration:", round(conc1, 1), "ppm"),
#               legendgroup = "conc") %>%
#     add_lines(y = ~ net1, name = "Return to N ($/ac)",
#               line = list(color = "black", width = 2.5),
#               hovertext = ~ paste("Return to N:", round(net1, 1), "$/ac"),
#               legendgroup = "net") %>%
#     add_ribbons(data = stdev1, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
#                 line = list(
#                   color = "#5dbb63",
#                   width = 0.5,
#                   opacity = 0),
#                 hovertext = ~paste("yield: ±", round(yld_stdev1)),
#                 fillcolor = "#5dbb63",
#                 opacity = 0.5,
#                 legendgroup = "yield", showlegend = FALSE) %>%
#     add_ribbons(ymin = ~ leach1 - leach_stdev1, ymax = ~ leach1 + leach_stdev1,
#                 line = list(
#                   color = "#c99f6e",
#                   width = 0.5,
#                   opacity = 0),
#                 hovertext = ~paste("NO<sub>3</sub> leaching: ±", round(leach_stdev1, 1)),
#                 fillcolor = "#c99f6e",
#                 opacity = 0.5,
#                 legendgroup = "leach", showlegend = FALSE) %>%
#     add_ribbons(ymin = ~ conc1 - conc_stdev1, ymax = ~ conc1 + conc_stdev1,
#                 line = list(
#                   color = "#6e8fc9",
#                   width = 0.5,
#                   opacity = 0),
#                 hovertext = ~paste("NO<sub>3</sub> concentration: ±", round(conc_stdev1, 1)),
#                 fillcolor = "#6e8fc9",
#                 opacity = 0.5,
#                 legendgroup = "conc", showlegend = FALSE) %>%
#     add_trace(y = 0,
#               type = 'scatter', mode = 'lines',
#               opacity = 0,
#               hovertext = ~ paste("N fert rate:",fert, "lbs N/ac"),
#               hoverinfo = "text",
#               showlegend = F) %>%
#     layout(title = "Responses to fertilizer N",
#            xaxis = list(title = "N fertilizer (N lb/ac)"),
#            yaxis = list (title = " "),
#            hovermode = "x unified",
#            legend = list(orientation = 'h', y = -0.2))
#   
#   
# })
# 
# 
# ## plot 2--------------------------
# output$plot2 <- renderPlotly({
#   
#   #req(input$simSelect2)
#   sim1 <- input$simSelect1
#   sim2 <- input$simSelect2
#   
#   data1 <- dat1()[[1]]
#   stdev1 <- dat1()[[2]]
#   data2 <- dat2()[[1]]
#   stdev2 <- dat2()[[2]]
#   
#   #print(stdev2)
#   
#   ##TODO FIGURE OUT HOW TO REORDER THE HOVER OVERS SO THAT FERT IS ON THE BOTTOM
#   plot_ly(data = data1, x = ~fert, hoverinfo = "text") %>%
#     add_lines(y = ~ yield1, name = paste(sim1, "yield (bu/ac)"),
#               line = list(color = "#5dbb63", width = 2, dash = "solid"),
#               hovertext = ~ paste(sim1, "yield:",round(yield1, 1), "bu/ac"),
#               legendgroup = "yield1") %>%
#     add_lines(y = ~ leach1, name = paste(sim1, "NO3 leaching (lb/ac)"),
#               line = list(color = "#5dbb63", width = 2, dash = "dash"),
#               hovertext = ~paste(sim1, "NO<sub>3</sub> leaching:",round(leach1, 1), "lbs/ac"),
#               legendgroup = "leach1") %>%
#     add_lines(y = ~ conc1, name = paste(sim1, "NO3 concentration (ppm)"),
#               line = list(color = "#5dbb63", width = 2, dash = "dot"),
#               hovertext = ~ paste(sim1, "NO<sub>3</sub> concentration:", round(conc1, 1), "ppm"),
#               legendgroup = "conc1") %>%
#     add_lines(y = ~ net1, name = paste(sim1, "return to N ($/ac)"),
#               line = list(color = "#5dbb63", width = 3, dash = "dashdot"),
#               hovertext = ~ paste(sim1, "return to N:", round(net1, 1), "$/ac"),
#               legendgroup = "net1") %>%
#     add_ribbons(data = stdev1, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
#                 line = list(
#                   color = "#5dbb63"),
#                 fillcolor = "#5dbb63",
#                 hovertext = ~paste(sim1, "yield:", "±", round(yld_stdev1)),
#                 opacity = 0.5,
#                 legendgroup = "yield1", showlegend = FALSE) %>%
#     add_ribbons(ymin = ~ leach1 - leach_stdev1, ymax = ~ leach1 + leach_stdev1,
#                 line = list(
#                   color = "#5dbb63",
#                   width = 0.5,
#                   opacity = 0),
#                 fillcolor = "#5dbb63",
#                 hovertext = ~paste(sim1, "NO3 leach:", "±", round(leach_stdev1)),
#                 opacity = 0.5,
#                 legendgroup = "leach1", showlegend = FALSE) %>%
#     add_ribbons(ymin = ~ conc1 - conc_stdev1, ymax = ~ conc1 + conc_stdev1,
#                 line = list(
#                   color = "#5dbb63",
#                   width = 0.5,
#                   opacity = 0),
#                 hovertext = ~paste(sim1, "NO3 concentration:", "±", round(conc_stdev1)),
#                 fillcolor = "#5dbb63",
#                 opacity = 0.5,
#                 legendgroup = "conc1", showlegend = FALSE)  %>%
#     add_lines(data = data2, y = ~ yield2, name = paste(sim2, "yield (bu/ac)"),
#               line = list(color = "#c99f6e", width = 1, dash = "solid"),
#               hovertext = ~paste(sim2, "yield:",round(yield2, 1), "bu/ac"),
#               hoverinfo = "text",
#               legendgroup = "yield2") %>%
#     add_lines(y = ~ leach2, name = paste(sim2, "NO3 leaching (lb/ac)"),
#               line = list(color = "#c99f6e", width = 1, dash = "dash"),
#               hovertext = ~paste(sim2, "NO<sub>3</sub> leaching:",round(leach2, 1), "lbs/ac"),
#               hoverinfo = "text",
#               legendgroup = "leach2") %>%
#     add_lines(y = ~ conc2, name = paste(sim2, "NO3 concentration (ppm)"),
#               line = list(color = "#c99f6e", width = 1, dasy = "dot"),
#               hovertext = ~ paste(sim2, "NO<sub>3</sub> concentration:", round(conc2, 1), "ppm"),
#               hoverinfo = "text",
#               legendgroup = "conc2") %>%
#     add_lines(y = ~ net2, name = paste(sim2, "return to N ($/ac)"),
#               line = list(color = "#c99f6e", width = 3, dash = "dashdot"),
#               hovertext = ~ paste(sim2, "return to N:", round(net2, 1), "$/ac"),
#               hoverinfo = "text",
#               legendgroup = "net2") %>%
#     add_ribbons(data = stdev2, ymin = ~ yield2 - yld_stdev2, ymax = ~ yield2 + yld_stdev2,
#                 line = list(
#                   color = "#c99f6e",
#                   width = 0.5,
#                   opacity = 0),
#                 hovertext = ~paste(sim2, "yield:", "±", round(yld_stdev2)),
#                 fillcolor = "#c99f6e",
#                 opacity = 0.5,
#                 legendgroup = "yield2", showlegend = FALSE) %>%
#     add_ribbons(ymin = ~ leach2 - leach_stdev2, ymax = ~ leach2 + leach_stdev2,
#                 line = list(
#                   color = "#c99f6e",
#                   width = 0.5,
#                   opacity = 0),
#                 fillcolor = "#c99f6e",
#                 hovertext = ~paste(sim2, "NO3 leaching:", "±", round(leach_stdev2)),
#                 opacity = 0.5,
#                 legendgroup = "leach2", showlegend = FALSE) %>%
#     add_ribbons(ymin = ~ conc2 - conc_stdev2, ymax = ~ conc2 + conc_stdev2,
#                 line = list(
#                   color = "#c99f6e",
#                   width = 0.5,
#                   opacity = 0),
#                 hovertext = ~paste(sim2, "NO3 concentration:", "±", round(conc_stdev2)),
#                 fillcolor = "#c99f6e",
#                 opacity = 0.5,
#                 legendgroup = "conc2", showlegend = FALSE) %>%
#     add_trace(y = 0,
#               type = 'scatter', mode = 'lines',
#               opacity = 0,
#               hovertext = ~ paste("N fert rate:",fert, "lbs N/ac"),
#               hoverinfo = "text",
#               showlegend = F) %>%
#     layout(title = "Responses to fertilizer N",
#            xaxis = list(title = "N fertilizer (N lb/ac)"),
#            yaxis = list (title = " "),
#            hovermode = "x unified",
#            legend = list(orientation = 'h',
#                          y = -0.3))
#   
# })

