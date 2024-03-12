## plots ##

# UI ----


plotUI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("plots"))
  
  
}


# Server ----


plotServer <- function(id, sim1, sim2, dat1, dat2) { 
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$plots <- renderUI({
        
        req(sim1())
        req(dat1())
        
        sim1Name <- str_to_title(sim1())
        sim2Name <- str_to_title(sim2())
        
        if(sim2Name == "None") {
          title <- paste("Responses to Fertilizer N (30 year average) in", sim1Name)
          plotYldAndRtN <- plotlyOutput(ns('plotYieldReturnSim1'))
          plotYldAndLeach <- plotlyOutput(ns('plotYieldLeachSim1'))
          plotYldAndConc <- plotlyOutput(ns('plotYieldConcSim1'))
        } 
        
        if(sim2Name != "None") {
          title <- paste("Responses to Fertilizer N (30 year average) in", sim1Name, "and", sim2Name)
          plotYldAndRtN <- plotlyOutput(ns('plotYieldReturnSim2'))
          plotYldAndLeach <- plotlyOutput(ns('plotYieldLeachSim2'))
          plotYldAndConc <- plotlyOutput(ns('plotYieldConcSim2'))
        }

        # if(sim2Name == "None") {
        # 
        #   plotYldAndRtN <- plotlyOutput(ns('plotYieldReturnSim1'))
        #   plotYldAndLeach <- plotlyOutput(ns('plotYieldLeachSim1'))
        #   plotYldAndConc <- plotlyOutput(ns('plotYieldConcSim1'))
        # 
        # }
        # if(sim2Name != "None") {
        # 
        #   plotYldAndRtN <- plotlyOutput(ns('plotYieldReturnSim2'))
        #   plotYldAndLeach <- plotlyOutput(ns('plotYieldLeachSim2'))
        #   plotYldAndConc <- plotlyOutput(ns('plotYieldConcSim2'))
        # 
        # }
        
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
        
        data1 <- dat1()$df
        stdev1 <- dat1()$sd
        #wetDry <- dat1()$wetDry

        yaxislabel <- list(title = list(text = "Return to N ($/ac)",
                                        font = list(size = 15)))

        p <- base_plot(data1, stdev1, yaxislabel) %>%
          add_lines(data = data1, y = ~ net1, name = "Return to N ($/ac)",
                    line = list(color = "#ff9843", width = 4, dash = "dot"),
                    hovertext = ~ paste("Return to N:", round(net1, 1), "$/ac"),
                    legendgroup = "net1")# %>%
          # add_lines(data = wetDry, x = ~ fertilizerLbsAc, y = ~wetYield,
          #           name = "Yield from wettest 5 years",
          #           line = list(color = "#3468c0", width = 4, dash = "dash"),
          #           hoverText = ~paste("Wet yield:", round(wetYield,1))) %>%
          # add_lines(data = wetDry, x = ~ fertilizerLbsAc, y = ~dryYield,
          #           name = "Yield from driest 5 years",
          #           line = list(color = "#3468c0", width = 4, dash = "dot"),
          #           hoverText = ~paste("Dry yield:", round(dryYield,1)))

      })

      ## yield & return to N plot sim2-----------------------------

      output$plotYieldReturnSim2 <- renderPlotly({

        data1 <- dat1()$df
        stdev1 <- dat1()$sd
        data2 <- dat2()$df
        stdev2 <- dat2()$sd
        
        plot_ly(data = data1, x = ~fert, hoverinfo = "text") %>%
          add_lines(y = ~ yield1, name = paste(sim1(), "yield (bu/ac)"),
                    yaxis = "y2",
                    line = list(color = "#ff9843", width = 4, dash = "solid"),
                    hovertext = ~ paste(sim1(), "yield:",round(yield1, 1), "bu/ac"),
                    legendgroup = "yield1") %>%
          add_lines(y = ~ net1, name = paste(sim1(), "return to N ($/ac)"),
                    line = list(color = "#ff9843", width = 4, dash = "dot"),
                    hovertext = ~ paste(sim1(), "return to N:", round(net1, 1), "$/ac"),
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
          add_lines(data = data2, y = ~ yield2, name = paste(sim2(), "yield (bu/ac)"),
                    line = list(color = "#3468c0", width = 4, dash = "solid"),
                    hovertext = ~ paste(sim2(), "yield:",round(yield2, 1), "bu/ac"),
                    yaxis = "y2",
                    hoverinfo = "text",
                    legendgroup = "yield2") %>%
          add_lines(y = ~ net2, name = paste(sim2(), "return to N ($/ac)"),
                    line = list(color = "#3468c0", width = 4, dash = "dot"),
                    hovertext = ~ paste(sim2(), "return to N:", round(net2, 1), "$/ac"),
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

        data1 <- dat1()$df
        stdev1 <- dat1()$sd
        
        yaxislabel <- list(title = list(text = "NO<sub>3</sub> leaching (lb/ac)",
                                        font = list(size = 15)))
        
        p <- base_plot(data1, stdev1, yaxislabel) %>%
          add_lines(data = data1, y = ~ leach1, name = "NO<sub>3</sub> leaching (lb/ac)",
                    line = list(color = "#ff9843", width = 4, dash = "dot"),
                    hovertext = ~ paste("NO<sub>3</sub> leaching:", round(leach1, 1), "lb/ac"),
                    legendgroup = "leach1") %>%
          add_ribbons(data = stdev1, ymin = ~ leach1 - leach_stdev1, ymax = ~ leach1 + leach_stdev1,
                      line = list(
                        color = "#ff9843",
                        width = 0.5,
                        opacity = 0),
                      hovertext = ~paste("NO<sub>3</sub> leaching: ±", round(leach_stdev1)),
                      fillcolor = "#ff9843",
                      opacity = 0.5,
                      legendgroup = "leach1", showlegend = FALSE)

      })

      ## yield and leaching plot sim2------------

      output$plotYieldLeachSim2 <- renderPlotly({

        data1 <- dat1()[[1]]
        stdev1 <- dat1()[[2]]
        data2 <- dat2()[[1]]
        stdev2 <- dat2()[[2]]

        plot_ly(data = data1, x = ~fert, hoverinfo = "text") %>%
          add_lines(y = ~ yield1, name = paste(sim1(), "yield (bu/ac)"),
                    yaxis = "y2",
                    line = list(color = "#ff9843", width = 4, dash = "solid"),
                    hovertext = ~ paste(sim1(), "yield:",round(yield1, 1), "bu/ac"),
                    legendgroup = "yield1") %>%
          add_lines(y = ~ leach1, name = paste(sim1(), "NO<sub>3</sub> leaching (lb/ac)"),
                    line = list(color = "#ff9843", width = 4, dash = "dot"),
                    hovertext = ~paste(sim1(), "NO<sub>3</sub> leaching:",round(leach1, 1), "lbs/ac"),
                    legendgroup = "leach1") %>%
          add_ribbons(data = stdev1, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
                      line = list(
                        color = "#ff9843",
                        width = 0.5,
                        opacity = 0),
                      fillcolor = "#ff9843",
                      yaxis = "y2",
                      hovertext = ~paste(sim1(), "yield: ±", round(yld_stdev1)),
                      opacity = 0.5,
                      legendgroup = "yield1", showlegend = FALSE) %>%
          add_ribbons(ymin = ~ leach1 - leach_stdev1, ymax = ~ leach1 + leach_stdev1,
                      line = list(
                        color = "#ff9843",
                        width = 0.5,
                        opacity = 0),
                      hovertext = ~paste(sim1(), "NO<sub>3</sub> leaching: ±", round(leach_stdev1)),
                      fillcolor = "#ff9843",
                      opacity = 0.5,
                      legendgroup = "leach1", showlegend = FALSE) %>%
          add_lines(data = data2, y = ~ yield2, name = paste(sim2(), "yield (bu/ac)"),
                    line = list(color = "#5dbb63", width = 4, dash = "solid"),
                    hovertext = ~ paste(sim2(), "yield:",round(yield2, 1), "bu/ac"),
                    hoverinfo = "text",
                    yaxis = "y2",
                    legendgroup = "yield2") %>%
          add_lines(y = ~ leach2, name = paste(sim2(), "NO<sub>3</sub> leaching (lb/ac)"),
                    line = list(color = "#5dbb63", width = 4, dash = "dot"),
                    hovertext = ~ paste(sim2(), "NO<sub>3</sub> leaching:", round(leach2, 1), "$/ac"),
                    legendgroup = "net2") %>%
          add_ribbons(data = stdev2,  ymin = ~ yield2 - yld_stdev2, ymax = ~ yield2 + yld_stdev2,
                      line = list(
                        color = "#5dbb63",
                        width = 0.5,
                        opacity = 0),
                      fillcolor = "#5dbb63",
                      yaxis = "y2",
                      opacity = 0.5,
                      hovertext = ~paste(sim2(), "yield:", "±", round(yld_stdev2)),
                      legendgroup = "yield2", showlegend = FALSE) %>%
          add_ribbons(ymin = ~ leach2 - leach_stdev2, ymax = ~ leach2 + leach_stdev2,
                      line = list(
                        color = "#5dbb63",
                        width = 0.5,
                        opacity = 0),
                      fillcolor = "#5dbb63",
                      opacity = 0.5,
                      hovertext = ~paste(sim2(), "NO<sub>3</sub> leaching: ±", round(leach_stdev2)),
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

        data1 <- dat1()$df
        stdev1 <- dat1()$sd
        
        yaxislabel <- list(title = list(text = "NO<sub>3</sub> concentration (ppm)",
                                        font = list(size = 15)))
        
        p <- base_plot(data1, stdev1, yaxislabel) %>%
          add_lines(data = data1, y = ~ conc1, name = "NO<sub>3</sub> concentration (ppm)",
                    line = list(color = "#ff9843", width = 4, dash = "dot"),
                    hovertext = ~ paste("NO<sub>3</sub> concentration:", round(conc1, 1), "ppm"),
                    legendgroup = "conc1") %>%
          add_ribbons(data = stdev1, ymin = ~ conc1 - conc_stdev1, ymax = ~ conc1 + conc_stdev1,
                      line = list(
                        color = "#ff9843",
                        width = 0.5,
                        opacity = 0),
                      hovertext = ~paste("NO<sub>3</sub> concentration: ±", round(conc_stdev1)),
                      fillcolor = "#ff9843",
                      opacity = 0.5,
                      legendgroup = "conc1", showlegend = FALSE) %>%
          add_lines(y = 10, name = "EPA NO<sub>3</sub> drinking water standard",
                    line = list(color = "black", width = 3, dash = "dash"),
                    hovertext='EPA NO<sub>3</sub> drinking water standard: 10 ppm')

        
      })

      ## yield and concentration plot sim2----------------------

      output$plotYieldConcSim2 <- renderPlotly({

        data1 <- dat1()[[1]]
        stdev1 <- dat1()[[2]]
        data2 <- dat2()[[1]]
        stdev2 <- dat2()[[2]]

        plot_ly(data = data1, x = ~fert, hoverinfo = "text") %>%
          add_lines(y = ~ yield1, name = paste(sim1(), "yield (bu/ac)"),
                    yaxis = "y2",
                    line = list(color = "#ff9843", width = 4, dash = "solid"),
                    hovertext = ~ paste(sim1(), "yield:",round(yield1, 1), "bu/ac"),
                    legendgroup = "yield1") %>%
          add_lines(y = ~ conc1, name = paste(sim1(), "NO<sub>3</sub> Concentration (ppm)"),
                    line = list(color = "#ff9843", width = 4, dash = "dot"),
                    hovertext = ~paste(sim1(), "NO<sub>3</sub> concentration:",round(conc1, 1), "(ppm)"),
                    legendgroup = "conc1") %>%
          add_ribbons(data = stdev1, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
                      line = list(
                        color = "#ff9843",
                        width = 0.5,
                        opacity = 0),
                      fillcolor = "#ff9843",
                      yaxis = "y2",
                      hovertext = ~paste(sim1(), "yield: ±", round(yld_stdev1)),
                      opacity = 0.5,
                      legendgroup = "yield1", showlegend = FALSE) %>%
          add_ribbons(ymin = ~ conc1 - conc_stdev1, ymax = ~ conc1 + conc_stdev1,
                      line = list(
                        color = "#ff9843",
                        width = 0.5,
                        opacity = 0),
                      fillcolor = "#ff9843",
                      hovertext = ~paste(sim1(), "NO<sub>3</sub> concentration: ±", round(conc_stdev1)),
                      opacity = 0.5,
                      legendgroup = "conc1", showlegend = FALSE) %>%
          add_lines(data = data2, y = ~ yield2, name = paste(sim2(), "yield (bu/ac)"),
                    line = list(color = "#593587", width = 3, dash = "solid"),
                    hovertext = ~ paste(sim2(), "yield:",round(yield2, 1), "bu/ac"),
                    hoverinfo = "text",
                    yaxis = "y2",
                    legendgroup = "yield2") %>%
          add_lines(y = ~ conc2, name = paste(sim2(), "NO<sub>3</sub> concentration (ppm)"),
                    line = list(color = "#593587", width = 3, dash = "dot"),
                    #yaxis = "y2",
                    hovertext = ~ paste(sim2(), "NO<sub>3</sub> concentration:", round(conc2, 1), "ppm"),
                    legendgroup = "conc2") %>%
          add_ribbons(data = stdev2, ymin = ~ yield2 - yld_stdev2, ymax = ~ yield2 + yld_stdev2,
                      line = list(
                        color = "#593587",
                        width = 0.5,
                        opacity = 0),
                      fillcolor = "#593587",
                      yaxis = "y2",
                      opacity = 0.5,
                      hovertext = ~paste(sim2(), "yield:", "±", round(yld_stdev2)),
                      legendgroup = "yield2", showlegend = FALSE) %>%
          add_ribbons(ymin = ~ conc2 - conc_stdev2, ymax = ~ conc2 + conc_stdev2,
                      line = list(
                        color = "#593587",
                        width = 0.5,
                        opacity = 0),
                      fillcolor = "#593587",
                      opacity = 0.5,
                      hovertext = ~paste(sim2(), "NO<sub>3</sub> concentration:", "±", round(conc_stdev2)),
                      legendgroup = "conc2", showlegend = FALSE) %>%
          add_lines(y = 10, name = "EPA NO<sub>3</sub> drinking water standard",
                    line = list(color = "black", width = 3, dash = "dash"),
                    hovertext='EPA NO<sub>3</sub> drinking water standard: 10 ppm') %>%
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
      
      #return(reactive(selectedSite()))
      # end
    }
  )
}