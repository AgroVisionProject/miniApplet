# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  
  # map-------------------------
  output$map <- renderLeaflet({
    react_map()
  })
  
  # selected site----------------
  selectedSite <- reactiveVal()
  
  # zooming in at clicks----------
  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    #print(click)
    
    if (click$group == "state") {
      
      #state <- states %>% filter(state == click$id)
      lat <- as.numeric(click$lat)
      lon <- as.numeric(click$lng)
      leafletProxy("map") %>%
        setView(lat = lat, lng = lon, zoom = 7)
      #return()
      
    } 
    
    if (click$group == "county") {
      
      #county = county_centroids %>% filter(id == click$id)
      #print(county)
      lat <- as.numeric(click$lat)
      lon <- as.numeric(click$lng)
      
      leafletProxy("map") %>%
        setView(
          lat = lat,
          lng = lon,
          zoom = 10
        )
    } 
    
    if(click$group == "sites") {
      
      click <- input$map_shape_click
      lat <- as.numeric(click$lat)
      lon <- as.numeric(click$lng)
      # print("click group sites")
      #print(click)
      site <- sites %>% filter(id == click$id)
      selectedSite(site)
      
      text = paste0(site$county, " County, ", site$state, "<br/>",
                    "Soil texture: ", str_to_sentence(site$texture),  "<br/>",
                    "Average annual precipitation: ", round(site$avgPrecip), " mm (", round(site$avgPrecip*0.0393701), " in)", "<br/>",
                    "Average annual growing degree days: ", round(site$avgGDD), " C (", round(site$avgGDD_F), " F)")
      
      
      leafletProxy("map") %>%
        clearGroup("cur_site") %>%
        addPopups(
          data = site, 
          lat = ~site$lat, lng = ~site$lon, text,
          #options = popupOptions(closeButton = FALSE),
          group = "cur_site"
        ) %>%
        setView(
          lat = lat,
          lng = lon,
          zoom = 12
        )
      
    }
    
  })
  
  observe({
    req(selectedSite())
    zoom <- input$map_zoom
    # print("zoom")
    # print(zoom)
    if(zoom < 10) {
      leafletProxy("map") %>%
        clearGroup("cur_site")
    }
    
  })
  
  # sim selection UI-------------------
  
  output$simSelectionUI <- renderUI({
    
    req(selectedSite())
    
    tagList(
      uiOutput("select1"),
      br(),
      uiOutput("select2"),
      br(),
      uiOutput("wetDry")
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
  
  output$wetDry <- renderUI({
    
    #req(input$simSelect2 == "None")
    checkboxGroupInput(inputId = "wetDry",
                       label = "Display wet or dry years",
                       choices = c("Wettest 5 years", "Driest 5 years"),
                       selected = NULL)
    
  })
  
  # observeEvent(input$simSelect2, {
  #   if(input$simSelect2 != "None") {
  #     shinyjs::disable('wetDry')
  #   } else {
  #     shinyjs::enable('wetDry')
  #   }
  # })
  
  
  
  observeEvent(input$simSelect1, {
    
    updateCheckboxGroupInput(session, "wetDry",
                             choices = c("Wettest 5 years", "Driest 5 years"),
                             selected = NULL)
    
  }, ignoreInit = TRUE)
  
  
  
  # prices UI--------------------
  
  output$pricesUI <- renderUI({
    
    req(selectedSite())
    #req(vals$count >= 1)
    
    fluidRow(column(6,
                    numericInput("cornPrice", "Price of corn ($/bu)", value = 4, min = 1, max = 20, step = 0.5)
    ),
    column(6,
           numericInput("fertPrice", "Price of N fertilizer ($/lb)", value = 0.4, min = 0, max = 20, step = 0.1)
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
    #print(selectedSite())
    
    siteID <- selectedSite()$id
    cornPrice <- input$cornPrice
    fertPrice <- input$fertPrice
    simulation1 <- filter(sims, cropSystem == input$simSelect1) 
    
    dataList <- makeDF(simulation = simulation1$simulation, site_ID = siteID,
                       cornPrice = cornPrice, fertPrice = fertPrice)  
    
    data1 <- dataList$modelDF 
    #print(data1)
    
    stdev1 <- dataList$stdevDF %>%
      rename(yld_stdev = stdev_cropyld,
             leach_stdev = stdev_no3leach,
             conc_stdev = stdev_no3conc)
    #print(stdev1)
    
    return(list(data1 = data1, stdev1 = stdev1))
    
  })
  
  ## data2---------------------------
  dat2 <- reactive({
    
    req(selectedSite())
    req(input$simSelect2)
    
    cornPrice <- input$cornPrice
    fertPrice <- input$fertPrice
    siteID <- selectedSite()$id
    simulation2 <- filter(sims, cropSystem == input$simSelect2)
    
    dataList <- makeDF(sim = simulation2$simulation, site_ID = siteID,
                       cornPrice = cornPrice, fertPrice = fertPrice)
    #print(dataList)
    
    data2 <- dataList$modelDF
    #print(data1)
    
    stdev2 <- dataList$stdevDF %>%
      rename(yld_stdev = stdev_cropyld,
             leach_stdev = stdev_no3leach,
             conc_stdev = stdev_no3conc)
    #print(stdev1)
    
    return(list(data2 = data2, stdev2 = stdev2))
    
  })
  
  ## wet dry data----------
  wetDryData1 <- reactive({
    
    req(selectedSite())
    
    site_lat <- selectedSite()$lat
    site_lon <- selectedSite()$lon
    siteID <- selectedSite()$id
    simulation1 <- filter(sims, cropSystem == input$simSelect1) 
    
    wetDryDF <- makeWetDryDF(sim = simulation1$simulation, site_ID = siteID)
    print(wetDryDF)
    
    return(wetDryDF)
    
  }) 
  
  wetDryData2 <- reactive({
    
    req(selectedSite())
    
    site_lat <- selectedSite()$lat
    site_lon <- selectedSite()$lon
    siteID <- selectedSite()$id
    simulation2 <- filter(sims, cropSystem == input$simSelect2) 
    
    wetDryDF <- makeWetDryDF(sim = simulation2$simulation, site_ID = siteID)
    #print(wetDryDF)
    
    return(wetDryDF)
    
  }) 
  
  ## fertilizerRec----------
  fertRec1 <- reactive({
    
    site <- selectedSite()
    fertPrice <- input$fertPrice
    cornPrice <- input$cornPrice
    sim <- filter(sims, cropSystem == input$simSelect1)
    
    nRec <- determineFertRec(simulation = sim$simulation, site = site, cornPrice = cornPrice, fertPrice = fertPrice)
    
  })
  
  fertRec2 <- reactive({
    
    site <- selectedSite()
    fertPrice <- input$fertPrice
    cornPrice <- input$cornPrice
    sim <- filter(sims, cropSystem == input$simSelect2)
    
    nRec <- determineFertRec(simulation = sim$simulation, site = site, cornPrice = cornPrice, fertPrice = fertPrice)
    
  })
  
  
  # plot UI-------------------------
  
  output$plotUI <- renderUI({
    
    req(input$simSelect1)
    req(input$simSelect2)
    req(selectedSite())
    
    sim1 <- str_to_title(input$simSelect1)
    sim2 <- str_to_title(input$simSelect2)
    if(input$simSelect2 == "None") {
      title <- paste("Responses to Fertilizer N (30 year average) in", sim1)
      plotYldAndRtN <- plotlyOutput('plotYield', height = "600px")
      plotYldAndLeach <- plotlyOutput('plotYieldLeachSim1', height = "600px")
      plotYldAndConc <- plotlyOutput('plotYieldConcSim1', height = "600px")
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
      
    } else {
      title <- paste("Responses to Fertilizer N (30 year average) in", sim1, "and", sim2)
      plotYldAndRtN <- plotlyOutput('plotYield', height = "600px")
      plotYldAndRtN2 <- plotlyOutput('plotYieldReturnSim2', height = "600px")
      plotYldAndLeach <- plotlyOutput('plotYieldLeachSim1', height = "600px")
      plotYldAndLeach2 <- plotlyOutput('plotYieldLeachSim2', height = "600px")
      plotYldAndConc <- plotlyOutput('plotYieldConcSim1', height = "600px")
      plotYldAndConc2 <- plotlyOutput('plotYieldConcSim2', height = "600px")
      tagList(
        tags$h4(title),
        tabsetPanel(
          tabPanel("Yield and Return to N",
                   fluidRow(column(6, plotYldAndRtN),
                            column(6, plotYldAndRtN2))),
          tabPanel("Yield and Nitrate Leaching",
                   fluidRow(column(6, plotYldAndLeach),
                            column(6, plotYldAndLeach2))),
          tabPanel("Yield and Nitrate Concentration",
                   fluidRow(column(6, plotYldAndConc),
                            column(6, plotYldAndConc2))),
          footer = "Click on legend items to add or remove variables from plot",
        )
      )
      
    }
    
  })
  
  ## yield & return to N plot sim1-----------------------------
  
  output$plotYield <- renderPlotly({
    
    req(dat1())
    nRec <- fertRec1()
    #print("nrec")
    #print(nRec)
    
    data1 <- dat1()$data1
    stdev1 <- dat1()$stdev1
    #print(stdev1)
    wetDryData <- wetDryData1() %>%
      filter(fertilizerLbsAc <= max(data1$fert))
    
    wet <- "none"
    dry <- "none"
    check <- input$wetDry
    #print(check)
    if(length(check) <=1) {
      ifelse(grepl("Wet", check), wet <- "wet", wet <- "none");
      ifelse(grepl("Dri", check), dry <- "dry", dry <- "none")
    } else {
      wet <- "wet";
      dry <- "dry"
    }
    #print(paste("wet", wet))
    #print(paste("dry", dry))
    #makeSim1plot(simDat = modelDF1, wetDryDat = wetDryData, stdevDF = stdevDF, variable = "leach", wet = "wet", dry = "dry")
    makeSim1plot(simDat = data1, stdevDF = stdev1, wetDryDat = wetDryData, variable = "rtn", wet = wet, dry = dry, nRec = nRec)
    
  })
  
  
  ## yield & return to N plot sim2-----------------------------
  
  output$plotYieldReturnSim2 <- renderPlotly({
    
    data2 <- dat2()$data2
    stdev2 <- dat2()$stdev2
    nRec <- fertRec2()
    #print(stdev1)
    wetDryData <- wetDryData2() %>%
      filter(fertilizerLbsAc <= max(data2$fert))
    
    wet <- "none"
    dry <- "none"
    check <- input$wetDry
    #print(check)
    if(length(check) <=1) {
      ifelse(grepl("Wet", check), wet <- "wet", wet <- "none");
      ifelse(grepl("Dri", check), dry <- "dry", dry <- "none")
    } else {
      wet <- "wet";
      dry <- "dry"
    }
    #print(paste("wet", wet))
    #print(paste("dry", dry))
    #makeSim1plot(simDat = modelDF1, wetDryDat = wetDryData, stdevDF = stdevDF, variable = "leach", wet = "wet", dry = "dry")
    makeSim1plot(simDat = data2, stdevDF = stdev2, wetDryDat = wetDryData, variable = "rtn", wet = wet, dry = dry, nRec = nRec)
    
    
    
  })
  
  ## yield and leaching plot sim1----------------------
  
  output$plotYieldLeachSim1 <- renderPlotly({
    
    req(dat1())
    nRec <- fertRec1()
    
    data1 <- dat1()$data1
    # print("plotyieldleach1")
    # print(nRec)
    # print(head(data1))
    stdev1 <- dat1()$stdev1
    #print(head(stdev1))
    wetDryData <- wetDryData1() %>%
      filter(fertilizerLbsAc <= max(data1$fert))
    #print(head(wetDryData))
    
    wet <- "none"
    dry <- "none"
    check <- input$wetDry
    #print(check)
    if(length(check) <=1) {
      ifelse(grepl("Wet", check), wet <- "wet", wet <- "none");
      ifelse(grepl("Dri", check), dry <- "dry", dry <- "none")
    } else {
      wet <- "wet";
      dry <- "dry"
    }
    #print(paste("wet", wet))
    #print(paste("dry", dry))
    
    makeSim1plot(simDat = data1, stdevDF = stdev1, wetDryDat = wetDryData, variable = "leach", wet = wet, dry = dry, nRec = nRec)
    
  })
  
  ## yield and leaching plot sim2------------
  
  output$plotYieldLeachSim2 <- renderPlotly({
    
    data2 <- dat2()$data2
    stdev2 <- dat2()$stdev2
    nRec <- fertRec2()
    
    # print("plotyieldleach2")
    # print(nRec)
    # print(head(data2))
    # print(head(stdev2))
    
    #print(stdev1)
    wetDryData <- wetDryData2() %>%
      filter(fertilizerLbsAc <= max(data2$fert))
    #print(head(wetDryData))
    
    wet <- "none"
    dry <- "none"
    check <- input$wetDry
    #print(check)
    if(length(check) <=1) {
      ifelse(grepl("Wet", check), wet <- "wet", wet <- "none");
      ifelse(grepl("Dri", check), dry <- "dry", dry <- "none")
    } else {
      wet <- "wet";
      dry <- "dry"
    }
    #print(paste("wet", wet))
    #print(paste("dry", dry))
    #makeSim1plot(simDat = modelDF1, wetDryDat = wetDryData, stdevDF = stdevDF, variable = "leach", wet = "wet", dry = "dry")
    makeSim1plot(simDat = data2, stdevDF = stdev2, wetDryDat = wetDryData, variable = "leach", wet = wet, dry = dry, nRec = nRec)
    
    
    
    
  })
  
  ## yield and concentration plot sim1----------------------
  
  output$plotYieldConcSim1 <- renderPlotly({
    
    req(dat1())
    nRec <- fertRec1()
    
    data1 <- dat1()$data1
    stdev1 <- dat1()$stdev1
    #print(stdev1)
    wetDryData <- wetDryData1() %>%
      filter(fertilizerLbsAc <= max(data1$fert))
    
    wet <- "none"
    dry <- "none"
    check <- input$wetDry
    #print(check)
    if(length(check) <=1) {
      ifelse(grepl("Wet", check), wet <- "wet", wet <- "none");
      ifelse(grepl("Dri", check), dry <- "dry", dry <- "none")
    } else {
      wet <- "wet";
      dry <- "dry"
    }
    #print(paste("wet", wet))
    #print(paste("dry", dry))
    
    makeSim1plot(simDat = data1, stdevDF = stdev1, wetDryDat = wetDryData, variable = "conc", wet = wet, dry = dry, nRec = nRec)
    
  })
  
  ## yield and concentration plot sim2----------------------
  
  output$plotYieldConcSim2 <- renderPlotly({
    
    data2 <- dat2()$data2
    stdev2 <- dat2()$stdev2
    nRec <- fertRec2()
    #print(stdev1)
    wetDryData <- wetDryData2() %>%
      filter(fertilizerLbsAc <= max(data2$fert))
    
    wet <- "none"
    dry <- "none"
    check <- input$wetDry
    #print(check)
    if(length(check) <=1) {
      ifelse(grepl("Wet", check), wet <- "wet", wet <- "none");
      ifelse(grepl("Dri", check), dry <- "dry", dry <- "none")
    } else {
      wet <- "wet";
      dry <- "dry"
    }
    #print(paste("wet", wet))
    #print(paste("dry", dry))
    #makeSim1plot(simDat = modelDF1, wetDryDat = wetDryData, stdevDF = stdevDF, variable = "leach", wet = "wet", dry = "dry")
    makeSim1plot(simDat = data2, stdevDF = stdev2, wetDryDat = wetDryData, variable = "conc", wet = wet, dry = dry, nRec = nRec)
    
    
  })
  
  
  # slider UI--------------
  
  output$slider1UI <- renderUI({
    
    #req(vals$count >= 1)
    req(input$simSelect1)
    req(selectedSite())
    print(input$simSelect2)
    
    tagList(
      uiOutput("values1"),
      br(),
      helpText("Use the slider below to view responses at specific N rates in the table above. Default value is fertilizer N recommendation."),
      uiOutput("range1")
    )
    
  })
  
  output$slider2UI <- renderUI({
    
    req(input$simSelect2 != "None")
    
    tagList(
      uiOutput("values2"),
      br(),
      helpText("Use the slider below to view responses at specific N rates in the table above"),
      uiOutput("range2")
    )
    
  })
  
  output$range1 <- renderUI({
    
    maxFert <- max(dat1()$data1$fert)
    nRec <- fertRec1()
    #print(maxFert)
    
    sliderInput(
      inputId = "range_dat1",
      label = "N fertilizer (lb/ac)",
      min = 0, max = maxFert, value = nRec, step = 1
    )
    
  })
  
  output$values1 <- render_gt({
    
    #req(length(input$simSelect1) == 1)
    req(input$range_dat1)
    #print(dat1()$data1)
    
    newdat1 <- dat1()$data1 %>%
      filter(fert == input$range_dat1) %>%
      mutate(leaching = round(leaching, 1),
             yield = round(yield, 1),
             concentration = round(concentration, 1),
             net = round(net, 1))
    
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
  
  output$range2 <- renderUI({
    
    maxFert <- max(dat2()$data2$fert)
    nRec <- fertRec2()
    #print(maxFert)
    
    sliderInput(
      inputId = "range_dat2",
      label = "N fertilizer (lb/ac)",
      min = 0, max = maxFert, value = nRec, step = 1
    )
    
  })
  
  output$values2 <- render_gt({
    
    #req(length(input$simSelect2) == 1)
    req(input$range_dat2)
    #req(dat2())
    
    newdat2 <- dat2()$data2 %>%
      #filter(fert == 150) %>%
      filter(fert == input$range_dat2) %>%
      mutate(leaching = round(leaching, 1),
             yield = round(yield, 1),
             concentration = round(concentration, 1),
             net = round(net, 1))
    
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
  
  # download data--------------
  
  shinyjs::disable("download")
  observeEvent(dat1(), {
    shinyjs::enable("download")
  })
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0(selectedSite()$county, "_", selectedSite()$state, "_", input$simSelect1, ".csv")
    },
    content = function(file) {
      df <- dat1()$data1 %>%
        rename(fertLbsAc = fert,
               yield = yield1,
               no3_leach = leach1,
               no3_conc = conc1,
               returnToN = net1)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # reset vals---------------
  observeEvent(input$reset, {
    # reset the map
    leafletProxy("map") %>%
      clearGroup("cur_site") %>%
      setView(lat = 43.0, lng = -92.5, zoom = 5) 
    selectedSite(NULL)
    
  })
  
}





