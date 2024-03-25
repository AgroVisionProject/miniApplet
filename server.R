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
      uiOutput("wetDry")
    )

  })
  
  output$select1 <- renderUI({
    
    radioButtons(inputId = "simSelect1",
                 label = "Choose Cropping System",
                 choices = simNames,
                 selected = "Rainfed continuous corn")
    
  })
  
  ## output$wetDry------------
  output$wetDry <- renderUI({
    
    checkboxGroupInput(inputId = "wetDry",
                label = "Display wettest or driest years",
                choices = c("Wettest 5 years", "Driest 5 years"),
                selected = NULL)
    
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
    simulation1 <- filter(sims, cropSystem == input$simSelect1) 
    
    data <- makeDF(sim = simulation1$simulation, site_lat = site_lat, site_lon = site_lon) %>%
      rename(yield1 = yield)
    #print(data)
    
    return(data)
    
  })
  
  ## wet dry data----------
  wetDryData <- reactive({
    
    req(selectedSite())
    
    site_lat <- selectedSite()$lat
    site_lon <- selectedSite()$lon
    simulation1 <- filter(sims, cropSystem == input$simSelect1) 
    
    wetDryDF <- makeWetDryDF(sim = simulation1$simulation, site_lat = site_lat, site_lon = site_lon)
    #print(wetDryDF)
    
    return(wetDryDF)
    
  }) 
  
  
  # plot UI-------------------------
  
  output$plotUI <- renderUI({

    req(input$simSelect1)
    req(selectedSite())
    
    sim1 <- str_to_title(input$simSelect1)
    title <- paste("Responses to Fertilizer N (30 year average) in", sim1)
    plotYld<- plotlyOutput('plotYield')

    tagList(
      tags$h4(title),
      plotYld
    )

    })

## yield & return to N plot sim1-----------------------------
  
  output$plotYield <- renderPlotly({
    
    req(dat1())
    
    data1 <- dat1()
    wetDryData <- wetDryData()
    
    wet <- "none"
    dry <- "none"
    check <- input$wetDry
    print(check)
    if(length(check) <=1) {
      ifelse(grepl("Wet", check), wet <- "wet", wet <- "none");
      ifelse(grepl("Dri", check), dry <- "dry", dry <- "none")
    } else {
      wet <- "wet";
      dry <- "dry"
    }
    print(paste("wet", wet))
    print(paste("dry", dry))
    
    makeYldplot(simDat = data1, wetDryDat = wetDryData, wet = wet, dry = dry)
    
    # plot_ly(data = data1, x = ~fert, hoverinfo = "text") %>%
    #   add_lines(y = ~ yield1, name = "Yield (bu/ac)",
    #             line = list(color = "#ff9843", width = 4, dash = "solid"),
    #             hovertext = ~ paste("Yield:",round(yield1, 1), "bu/ac")) %>%
    #   layout(
    #     xaxis = list(title = list(text =  "N fertilizer (N lb/ac)",
    #                               font = list(size = 15))),
    #     yaxis = list(title = list(text = "Yield (bu/ac)",
    #                               font = list(size = 15))),
    #     hovermode = "x unified",
    #     margin = list(r = 50, b = 10, t = 50),
    #     legend = list(orientation = 'h', y = -0.5,
    #                   font = list(size = 14))
    #     )
    
  })
  

  
  
  # observe wet dry------------
  # observeEvent(input$wetDry, {
  #   
  #   plot <- plotlyProxy("plotYield", session)
  #   req(selectedSite())
  #   #req(input$simSelect1)
  #   #print(input$wetDry)
  #   
  #   site_lat <- selectedSite()$lat
  #   site_lon <- selectedSite()$lon
  #   simulation1 <- filter(sims, cropSystem == input$simSelect1) 
  #   
  #   wetdrydf <- makeWetDryDF(sim = simulation1$simulation, site_lat = site_lat, site_lon = site_lon)
  #   #print(length(input$wetDry))
  #   vars = input$wetDry
  #   print(length(vars))
  #   
  #   ifelse(length(vars) == 1,
  #          # if(input$wetDry == "Wettest 5 years") {
  #          #   plotlyProxy("plotYield", session) %>%
  #          #     plotlyProxyInvoke("addTraces",
  #          #                       list(x=c(wetdrydf$fertilizerLbsAc),y=c(wetdrydf$wetYield),
  #          #                            type = 'scatter',
  #          #                            mode = 'lines'))
  #          # } else {
  #          #   plotlyProxy("plotYield", session) %>%
  #          #     plotlyProxyInvoke("addTraces",
  #          #                       list(x=c(wetdrydf$fertilizerLbsAc),y=c(wetdrydf$dryYield),
  #          #                            type = 'scatter',
  #          #                            mode = 'lines'))
  #          # },
  #          ifelse(length(vars) == 2,
  #                 
  #                 # {plotlyProxy("plotYield", session) %>%
  #                 #   plotlyProxyInvoke("deleteTraces", list(as.integer(1)));
  #                 # 
  #                 #   plotlyProxy("plotYield", session) %>%
  #                 #     plotlyProxyInvoke("addTraces",
  #                 #                       list(x=c(wetdrydf$fertilizerLbsAc),y=c(wetdrydf$dryYield),
  #                 #                            type = 'scatter',
  #                 #                            mode = 'lines')) %>%
  #                 #     plotlyProxyInvoke("addTraces",
  #                 #                       list(x=c(wetdrydf$fertilizerLbsAc),y=c(wetdrydf$wetYield),
  #                 #                            type = 'scatter',
  #                 #                            mode = 'lines'))},
  #                 
  #                 plotlyProxy("plotYield", session) %>%
  #                   plotlyProxyInvoke("deleteTraces", list(as.integer(4)))
  #          )
  #   )
  #   
  # }, ignoreNULL=FALSE )
  
  observeEvent(input$simSelect1, {
    
    updateCheckboxGroupInput(session, "wetDry",
                             choices = c("Wettest 5 years", "Driest 5 years"),
                             selected = NULL)
    
  }, ignoreInit = TRUE)
  
  # slider UI--------------
 
 output$range <- renderUI({
   
   maxFert <- max(dat1()$fert)
   #print(maxFert)
   
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
      p("Use the slider to view responses at specific N rates in the table"),
      gt_output("range")
    )
      
  })


output$values1 <- render_gt({
  
  #req(length(input$simSelect1) == 1)
  req(input$range_dat)
  
  newdat1 <- dat1() %>%
    filter(fert == input$range_dat) %>% 
    mutate(yield = round(yield1, 1))
  
  # remove duplicates
  newdat1 <- newdat1[1,]

  newdat1 %>%
    select(c(fert, yield)) %>%
    gt() %>%
    cols_label(
      fert = "N fertilizer (lb/ac)",
      yield = "Yield (bu/ac)",
      .fn = md
    ) %>%
    tab_header(title = paste(input$simSelect1, "output"))
  
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





