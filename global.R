library(shiny)
library(leaflet)
#library(leaflet.extras)
library(tidyverse)
library(sf)
library(plotly)
library(gt)
library(reactlog)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(htmltools)

# load shapefiles
states = st_read("data/ncr_states_simple.shp")
counties = st_read("data/ncr_counties_simple.shp") %>%
  mutate(id = paste0(STATEFP, COUNTYFP))

county_centroids = counties %>% 
  select(id, geometry) %>% 
  st_centroid() %>%
  mutate(
    lat = st_coordinates(.)[,2],
    lon = st_coordinates(.)[,1]
  ) %>%
  st_drop_geometry() %>%
  arrange(id)

# load data-----------------
leach_df <- read_csv("data/leachDataSiteUpdate.csv.gz")
yield_df <- read_csv("data/yieldDataSiteUpdate.csv.gz")
conc_df <- read_csv("data/concDataSiteUpdate.csv.gz")

sim1Var <- read_csv("data/sim1_SDUpdateSite.csv.gz")
sim2Var <- read_csv("data/sim2_SDUpdateSite.csv.gz")
sim3Var <- read_csv("data/sim3_SDUpdateSite.csv.gz")
sim4Var <- read_csv("data/sim4_SDUpdateSite.csv.gz")

sim1wetdry <- read_csv("data/sim1wetdryUpdateSite.csv.gz")
sim2wetdry <- read_csv("data/sim2wetdryUpdateSite.csv.gz")
sim3wetdry <- read_csv("data/sim3wetdryUpdateSite.csv.gz")
sim4wetdry <- read_csv("data/sim4wetdryUpdateSite.csv.gz")

sites <- read_csv("data/sampleSitesBioGDD.csv.gz") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>%
  mutate(id = row_number(), .before = 1)

textures <- levels(as.factor(sites$texture))
sandyTextures <- c("sand", "loamy sand")


sims <-  readxl::read_xlsx("data/simulationNames.xlsx")
simNames = sims$cropSystem

# fert recs----------
fertRecs <- readxl::read_xlsx("data/fertRecs.xlsx")

# define fertilizer/x axis----------
fert = seq(from = 0, to = 350, by = 1) #kg/ha
# convert kg/ha to lb/ac
kgha_to_lbac <- function(x) {
  lbac <- x*(2.20462/2.47105)
  return(lbac)
}

# map function------------

base_map <- function() {
  leaflet() %>%
    addTiles() %>%
    addMapPane("states", 451) %>%
    addMapPane("counties", 452) %>%
    addMapPane("sites", 453) %>%
    addPolygons(data = states,
                group = "state",
                col = "white",
                opacity = 0.5,
                #fill = FALSE,
                fillOpacity = 0,
                weight = 2,
                layerId = ~state,
                highlightOptions = highlightOptions(color = "#1AA7EC", weight = 3,
                                                    bringToFront = TRUE),
                options = pathOptions(pane = "states")) %>%
    addPolygons(data = counties,
                group = "county",
                col = "darkgrey",
                opacity = 0.5,
                #fill = FALSE,
                fillOpacity = 0,
                weight = 3,
                highlightOptions = highlightOptions(color = "#1AA7EC", weight = 3,
                                                    bringToFront = TRUE),
                layerId = ~id,
                options = pathOptions(pane = "counties")) %>%
    addRectangles(data = sites,
                  lng1 = ~ lon - .04, lng2 = ~ lon + .04,
                  lat1 = ~ lat - .04, lat2 = ~ lat + .04,
                  layerId = ~id,
                  col = "darkgrey",
                  weight = 2,
                  #opacity = 0.5,
                  fillOpacity = 0,
                  opacity = 3,
                  group = "sites",
                  highlightOptions = highlightOptions(color = "#1AA7EC", weight = 2,
                                                      bringToFront = TRUE),
                  options = pathOptions(pane = "sites")) %>%
    groupOptions("state", zoomLevels = 1:8) %>%
    groupOptions("county", zoomLevels = 7:14) %>%
    groupOptions("sites", zoomLevels = 9:14) %>%
    setView(lat = 43.0, lng = -92.5, zoom = 5) %>%
    #addProviderTiles("USGS.USImageryTopo") #%>%
    addProviderTiles("Esri.WorldImagery") %>%
    addProviderTiles("Stadia.StamenTerrainLabels") %>%
    addProviderTiles("Stadia.StamenTerrainLines")
}

# determine response curve--------
responseCurve <- function(dataframe, fun) {
  
  response <- c()
  #constant
  if(fun == 0) {
    response = rep(dataframe$a, times = length(fert))
  }
  #non-negative quadratic
  if(fun == 1) {
    response = dataframe$a + (dataframe$b*fert) + (dataframe$c*fert^2)
  }
  # gompertz
  if(fun == 3) {
    response = dataframe$a * exp((-dataframe$b)*(dataframe$c^fert))
  }
  # piecewise linear
  if(fun == 4) {
    response = ifelse(fert < dataframe$c, dataframe$a + (dataframe$b*fert),
                      dataframe$a + (dataframe$b*dataframe$c))
  }
  #linear
  if(fun == 5) {
    response = dataframe$a + (dataframe$b*fert)
  }
  
  return(response)
  
}

# make data frame----------------------

makeDF <- function(simulation, site_ID, cornPrice, fertPrice) {
  
  req(is.na(simulation) == FALSE)
  #print(paste("simulation", simulation))
  
  # create stdev data---------------
  if(simulation == 1) {var = sim1Var}
  if(simulation == 2) {var = sim2Var}
  if(simulation == 3) {var = sim3Var}
  if(simulation == 4) {var = sim4Var}
  
  var <- var %>%
    filter(id == site_ID)
  
  #print(head(var))
  
  # filter by lat, lon, sim---------------
  yield_df_sum <- yield_df %>%
    filter(sim == simulation,
           id == site_ID)
  #print(yield_df_sum)
  
  leach_df_sum <- leach_df %>%
    filter(sim == simulation,
           id == site_ID)
  
  conc_df_sum <- conc_df %>%
    filter(sim == simulation,
           id == site_ID)
  
  # create dependent vars---------------
  yieldFun <- yield_df_sum$fun
  leachFun <- leach_df_sum$fun
  concFun <- conc_df_sum$fun
  
  yield_y <- responseCurve(dataframe = yield_df_sum, fun = yieldFun)
  leach_y <- responseCurve(dataframe = leach_df_sum, fun = leachFun)
  conc_y <- responseCurve(dataframe = conc_df_sum, fun = concFun)
  
  cornVal <- (yield_y - yield_y[1]) * cornPrice
  fertCost <- round(kgha_to_lbac(fert)) * fertPrice
  NUE <- 0.5
  nloss <- NUE * leach_y * fertPrice
  net <- cornVal - fertCost - nloss
  
  # full modeled df---------------
  modelDF <- data.frame(fert = round(kgha_to_lbac(fert)), yield = yield_y, leach = kgha_to_lbac(leach_y), conc = conc_y,
                        net = net) %>%
    arrange(fert)
  
  var <- var %>%
    select(c(stdev, variable, fertilizerLbsAc)) %>%
    mutate(fert = round(fertilizerLbsAc))
  
  # join modeled DF and variance------------
  model_var <- left_join(modelDF, var, relationship = "many-to-many") 
  #print(tail(modelDF))
  
  # create stdev column for each variable------------
  stdev_wide <- model_var %>%
    drop_na(stdev) %>%
    pivot_wider(values_from = stdev, names_from = "variable", names_prefix = "stdev_")
  
  maxFert <- max(stdev_wide$fert)  # modelDF max fert should be var max fert
  #print(maxFert)
  modelDF <- filter(modelDF, fert < maxFert)
  #print(tail(stdev_wide))
  
  return(list(modelDF = modelDF, stdevDF = stdev_wide))
  
}

makeWetDryDF <- function(simulation, site_ID) {
  
  req(is.na(simulation) == FALSE)
  
  if(simulation == 1) {wetDry = sim1wetdry}
  if(simulation == 2) {wetDry = sim2wetdry}
  if(simulation == 3) {wetDry = sim3wetdry}
  if(simulation == 4) {wetDry = sim4wetdry}
  
  wetDry <- wetDry %>%
    #mutate(meanFert = round(meanFert)) %>%
    filter(id == site_ID) 
  
  return(wetDry)
  
}


# plot bones-----------------------

makeBasePlot <- function(simDat, simName, variable, y1axis, y1axisLabel, yaxisUnit, y2, stdevDF, stdevVar = NULL, nRec, var2color, yLmax, yRmax) {
  
  net_axis <- list(
    #tickfont = list(color = "black"),
    overlaying = "y1",
    range = c(0, yRmax),
    #anchor = "free",
    #position = 0,
    color = "#013229",
    side = "right",
    title = list(text = "Return to N ($/ac)",
                 font = list(size = 15, color = "#013229"),
                 standoff = 10L)
  )
  
  leach_axis <- list(
    #tickfont = list(color = "black"),
    overlaying = "y",
    range = c(0, yRmax),
    side = "right",
    color = "#593587",
    title = list(text = "NO<sub>3</sub> leaching (lb/ac) (± 1 SD)",
                 font = list(size = 15, color = "#593587"),
                 standoff = 10L)
  )
  
  conc_axis <- list(
    #tickfont = list(color = "black"),
    overlaying = "y",
    range = c(0, yRmax),
    side = "right",
    color = "#1F5E96",
    title = list(text = "NO<sub>3</sub> concentration (ppm) (± 1 SD)",
                 font = list(size = 15, color = "#1F5E96"),
                 standoff = 10L)
  )
  
  #print(yLmax)
  yvar = simDat[[variable]]
  #print(y1axis)
  #print(yvar)
  stdevYvar = stdevDF[[variable]]
  #print(stdevYvar)
  # print(max(yvar))
  # print(max(simDat$yield))
  # print(max(stdevDF$yield + stdevDF$yld_stdev))
  if(!is.null(stdevVar)) {
    stdev = stdevDF[[stdevVar]]
    #yLmax = max(yvar, simDat$yield, (stdevDF$yield + stdevDF$yld_stdev))
  }
  # if(is.null(stdevVar)) {
  #   yLmax = max(yvar, simDat$yield, (simDat$yield + simDat$yld_stdev))
  # }
  #print(yLmax)
  if(variable == "net") (
    y2 = net_axis
    #yLmax = max(simDat$yield, c(simDat$yield + stdevDF$yld_stdev), simDat$net)
  )
  if(variable == "leach") {
    y2 = leach_axis
  }
  if(variable == "conc") {
    y2 = conc_axis
  }
  
  base_plot <- plot_ly(data = simDat, x = ~fert) %>%
    add_lines(y = ~ yield, name = "Yield (bu/ac)",
              yaxis = "y1",
              hoverinfo = "text",
              line = list(color = "#3B9422", width = 4, dash = "solid"),
              hovertext = ~ paste("Yield:",round(yield, 1)),
              legendgroup = "yield1") %>%
    add_lines(y = ~ yvar, name = paste0(y1axisLabel, " (", yaxisUnit, ")"),
              yaxis = "y2",
              hoverinfo = "text",
              line = list(color = var2color, width = 4, dash = "solid"),
              hovertext = ~ paste0(y1axisLabel, ": ", round(yvar, 1)),
              legendgroup = "net1") %>% ##TODO check legend group
    add_ribbons(data = stdevDF, ymin = ~ yield - yld_stdev, ymax = ~ yield + yld_stdev,
                line = list(
                  color = "#3B9422",
                  width = 1,
                  opacity = 0.3),
                fillcolor = "#3B9422",
                yaxis = "y1",
                hoverinfo="none",
                opacity = 0.3,
                legendgroup = "yield1", showlegend = FALSE) %>%
    add_segments(x = ~nRec, y = 0, xend = ~ nRec, yend = yLmax,
                 name = "N fertilizer recommendation",
                 hoverinfo="none",
                 #yaxis = "y2",
                 line = list(color = "black", dash="dot")) %>%
    layout(
      title = simName,
      xaxis = list(dtick = 25,
                   title = list(text =  "N fertilizer (N lb/ac)",
                                font = list(size = 15))),
      yaxis = list(title = list(text = "Yield (bu/ac) (± 1 SD)",
                                font = list(size = 15, color = "#2A7315")),
                   color = "#2A7315", range = c(0, yLmax)),
      yaxis2 = y2,
      shapes = list(list(type = "rect", fillcolor = "#222222", line = list(color = "#222222"),
                         opacity = 0.2, y0 = 0, y1 = yLmax, x0 = nRec-(.1*nRec), x1 = nRec+(.1*nRec))
      ),
      hovermode = "x unified",
      margin = list(r = 50, b = 10, t = 50),
      legend = list(orientation = 'h', y = -0.2,
                    font = list(size = 14))
    ) 
  
  if(variable == "net") {
    base_plot <- base_plot
  } else {
    base_plot <- base_plot %>%
      add_ribbons(data = stdevDF, ymin = ~ stdevYvar - stdev, ymax = ~ stdevYvar + stdev,
                  line = list(
                    color = var2color,
                    width = 0.5,
                    opacity = 0),
                  fillcolor = var2color,
                  yaxis = "y2",
                  hoverinfo = "none",
                  opacity = 0.3,
                  legendgroup = "conc1", showlegend = FALSE) ## TODO check legendgroup
  }

  if(variable == "conc") {
    base_plot <- base_plot %>%
      add_lines(y = 10, name = "EPA safe drinking water standard (10 NO<sub>3</sub> ppm)",
                line = list(color = "#d40000", width = 2, dash = "solid"),
                hoverinfo = "none",
                yaxis = "y2",
                hovertext='EPA safe drinking water standard')
  }
  
  return(base_plot)
  
}

# base_plot_net <- makeBasePlot(simName = "cc", simDat = modelDF1,variable = "net", y1axis = "net", y1axisLabel = "Return to N", yaxisUnit = "$/ac",
#                            stdevDF = stdevDF1, var2color = "#013229", nRec = 140, yLmax = yLmax, yRmax = yRmax)
# #
# base_plot_conc <- makeBasePlot(simDat = modelDF1, variable = "conc", y1axis = "conc", y1axisLabel = "Nitrate concentration", yaxisUnit = "ppm",
#              stdevDF = stdevDF1, stdevVar = "conc_stdev", var2color = "#1F5E96",nRec = 140, yLmax = yLmax, yRmax = yRmax)
# 
# base_plot_leach <- makeBasePlot(simDat = modelDF1, variable = "leach", y1axis = "leach", y1axisLabel = "Nitrate concentration", yaxisUnit = "lb/ac",
#              stdevDF = stdevDF1, stdevVar = "leach_stdev", var2color = "#593587",nRec = 140, yLmax = yLmax, yRmax = yRmax)
# 

addWetDryLines <- function(wetDryDat, wetY, dryY, y_side, precName, precUnits, variable, wet = "none", dry = "none", base_plot = base_plot) {
  
  dryVar = wetDryDat[[dryY]]
  wetVar = wetDryDat[[wetY]]
  if(variable == "net") {
    color = "#3c8f41"
  }
  if(variable == "leach") {
    color = "#351f50"
  }
  if(variable == "conc") {
    color = "#244784"
  }
  
  if(wet == "none" & dry == "none") { # return base plot if wet and dry are null
    #print("base")
    plt <- base_plot
  } else if (wet == "wet" & dry == "none") { # return base plot plus wet if wet is checked
    #print(paste("wet", is.null(wet)))
    plt <- base_plot %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~wetVar, name = paste("Wet", precName),
                line = list(color = color, width = 2, dash = "dash"), yaxis = y_side,
                hoverinfo = "text",
                hovertext = ~ paste0("Wet ", precName, ": ",round(wetVar, 1)))
  } else if (wet == "none" & dry == "dry") { # return base plot plus dry if dry is checked
    #print("dry")
    plt <- base_plot %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~dryVar, name = paste("Dry", precName),
                line = list(color = color, width = 2, dash = "dot"), yaxis = y_side,
                hoverinfo = "text",
                hovertext = ~ paste0("Dry ", precName, ": ",round(dryVar, 1)))
  } else { # both are checked
    #print("both")
    plt <- base_plot %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~dryVar, name = paste("Dry", precName),
                line = list(color = color, width = 2, dash = "dot"), yaxis = y_side,
                hoverinfo = "text",
                hovertext = ~ paste0("Dry ", precName, ": ",round(dryVar, 1))) %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~wetVar, name = paste("Wet", precName),
                line = list(color = color, width = 2, dash = "dash"),yaxis = y_side,
                hoverinfo = "text",
                hovertext = ~ paste0("Wet ", precName, ": ",round(wetVar, 1)))
  }
  
  plt
}

# addWetDryLines(wetDryDat = wetDryData, wetY = "wetYield", dryY = "dryYield", y_side = "y1", precName = "yield", precUnits = "bu/ac", wet = "wet", dry = "dry",
#                base_plot = base_plot_net, variable = "net")
# 
# addWetDryLines(wetDryDat = wetDryData, wetY = "wetLeach", dryY = "dryLeach", y_side = "y2", precName = "leach", precUnits = "lb/ac", wet = "wet", dry = "dry",
#                base_plot = base_plot_leach, variable = "leach")
# 
# addWetDryLines(wetDryDat = wetDryData, wetY = "wetConc", dryY = "dryConc", y_side = "y2", precName = "concentration", precUnits = "ppm", wet = "wet", dry = "dry",
#                base_plot = base_plot_conc, variable = "conc")

makeSim1plot <- function(simName, simDat, stdevDF, variable, nRec, yLmax, yRmax, #y1axis, y1axisLabel, yaxisUnit, dryY, wetY, precName,
                         wetDryDat,  wet = "none", dry = "none") {
  
  # TODO in each if, make the plot within
  # TODO make else if 
  if(variable == "net") {
    #y2 = net_axis;
    y1axis = "net";
    y1axisLabel = "Return to N";
    yaxisUnit = "$/ac";
    var2color = "#013229";
    stdevVar = NULL;
    dryY = "dryYield";
    wetY = "wetYield";
    y_side = "y1";
    precName = "yield";
    precUnits = "bu/ac"
  } else if(variable == "leach") {
    #y2 = leach_axis;
    y1axis = "leaching";
    y1axisLabel = "NO<sub>3</sub> leaching";
    yaxisUnit = "lb/ac";
    var2color = "#593587";
    stdevVar = "leach_stdev";
    dryY = "dryLeach";
    wetY = "wetLeach";
    y_side = "y2";
    precName = "leach";
    precUnits = "lb/ac"
  } else if(variable == "conc") {
    #y2 = conc_axis;
    y1axis = "concentration";
    y1axisLabel = "NO<sub>3</sub> concentration";
    yaxisUnit = "ppm";
    var2color = "#1F5E96";
    stdevVar = "conc_stdev";
    dryY = "dryConc";
    wetY = "wetConc";
    y_side = "y2";
    precName = "concentration";
    precUnits = "ppm"
  }
  
  base_plot <- makeBasePlot(simName = simName, simDat = simDat, variable = variable, yLmax = yLmax, yRmax = yRmax,
                            y1axis = y1axis, y1axisLabel = y1axisLabel, yaxisUnit = yaxisUnit,
                            stdevDF = stdevDF, stdevVar = stdevVar, nRec = nRec, var2color = var2color)
  
  
  plt <- addWetDryLines(wetDryDat = wetDryDat, wetY = wetY, dryY = dryY, y_side = y_side, precName = precName, precUnits = precUnits,
                        wet = wet, dry = dry, base_plot = base_plot, variable = variable)
  
  plt
  
}

determineFertRec <- function(simulation, site, cornPrice, fertPrice) {
  
  nPriceRatios <- c(0.05, 0.1, 0.15, 0.2)
  nToCornRatio <- fertPrice/cornPrice
  #print(nToCornRatio)
  x <- which.min(abs(nPriceRatios-nToCornRatio))
  Nratio <- nPriceRatios[x]
  #print(Nratio)
  
  previousCropping <- if_else(simulation == 1 | simulation == 2,  "corn", "soy") 
  irrigation <- if_else(simulation == 1 | simulation == 3, "no", "yes")
  
  #print(previousCropping)
  #print(irrigation)
  
  if(site$texture %in% sandyTextures) {
    nRec <- fertRecs %>%
      filter(soil == "sandy",
             irrigated == irrigation,
             NtoCorn == Nratio)
  } else {
    nRec <- fertRecs %>%
      filter(soil == "loamy",
             previousCrop == previousCropping,
             NtoCorn == Nratio)
  }
  
  return(nRec$Nrec)
  
}



# makeSim1plot(simDat = modelDF1, wetDryDat = wetDryData, stdevDF = stdevDF1, variable = "conc", wet = "wet", nRec = 140, yLmax = yLmax, yRmax = yRmax)
# makeSim1plot(simDat = modelDF1, wetDryDat = wetDryData, stdevDF = stdevDF1, variable = "net", wet = 'wet', dry = 'dry', nRec = 140, yLmax = yLmax, yRmax = yRmax)
#makeSim1plot(simDat = modelDF1, wetDryDat = wetDryData, y1axis = "net1", y1axisLabel = "Return to N", yaxisUnit = "$/ac")
# makeYldplot(simDat = simData, wetDryDat = wetDryData, dry = "dry")
# makeYldplot(simDat = simData, wetDryDat = wetDryData, wet = "wet")
# makeYldplot(simDat = simData, wetDryDat = wetDryData, wet = "wet", dry = "dry")

# test data set----------------

# yield_df_sum <- yield_df %>%
#   filter(sim == 1,
#          STATE_NAME == "Wisconsin",
#          NAME == "Dane") %>%
#   slice(1)
# 
# leach_df_sum <- leach_df %>%
#   filter(sim == 1,
#          STATE_NAME == "Wisconsin",
#          NAME == "Dane") %>%
#   slice(1)
# 
# conc_df_sum <- conc_df %>%
#   filter(sim == 1,
#          STATE_NAME == "Wisconsin",
#          NAME == "Dane") %>%
#   slice(1)
# 
# yieldFun <- yield_df_sum$fun
# leachFun <- leach_df_sum$fun
# concFun <- conc_df_sum$fun
# 
# var <- sim1Var %>%
#   filter(id == yield_df_sum$id)
# 
# 
# yield_y <- responseCurve(dataframe = yield_df_sum, fun = yieldFun)
# leach_y <- responseCurve(dataframe = leach_df_sum, fun = leachFun)
# conc_y <- responseCurve(dataframe = conc_df_sum, fun = concFun)
# 
# cornPrice = 5
# fertPrice = 1
# cornVal <- (yield_y - yield_y[1]) * cornPrice
# fertCost <- kgha_to_lbac(fert) * fertPrice
# NUE <- 0.5
# nloss <- NUE * leach_y * fertPrice
# net <- cornVal - fertCost - nloss
# 
# modelDF1 <- data.frame(fert = round(kgha_to_lbac(fert)), yield = yield_y,
#                       leach = kgha_to_lbac(leach_y), conc = conc_y,
#                       net = net)
# 
#  wetDryData <- sim1wetdry %>%
#    filter(id == yield_df_sum$id)
# # # modelDF
# #
# var1 <- var %>%
#   select(c(stdev, variable, fertilizerLbsAc, meanFert)) %>%
#   mutate(fert = round(fertilizerLbsAc))
# 
# model_var1 <- left_join(modelDF1, var1)
# 
# # modelDF max fert should be var max fert
# maxFert <- max(var1$fert)
# modelDF1 <- filter(modelDF1, fert < maxFert)
# print(maxFert)
# 
# 
# # create stdev column for each variable
# stdevDF1 <- model_var1 %>%
#   drop_na(stdev) %>%
#   pivot_wider(values_from = stdev, names_from = "variable", names_prefix = "stdev_") %>%
#   rename(yld_stdev = stdev_cropyld, leach_stdev = stdev_no3leach, conc_stdev = stdev_no3conc)

# # 
# # # test data set plot 2---------------------
# # 
# # ##TODO make 2nd data set and test plot
# # 
# yield_df_sum2 <- yield_df %>%
#   filter(sim == 2,
#          STATE_NAME == "Wisconsin",
#          NAME == "Dane") %>%
#   slice(1)
# 
# leach_df_sum2 <- leach_df %>%
#   filter(sim == 2,
#          STATE_NAME == "Wisconsin",
#          NAME == "Dane") %>%
#   slice(1)
# 
# conc_df_sum2 <- conc_df %>%
#   filter(sim == 2,
#          STATE_NAME == "Wisconsin",
#          NAME == "Dane") %>%
#   slice(1)
# 
# yieldFun2 <- yield_df_sum2$fun
# leachFun2 <- leach_df_sum2$fun
# concFun2 <- conc_df_sum2$fun
# 
# var <- sim2Var %>%
#   filter(id == yield_df_sum2$id)
# 
# yield_y2 <- responseCurve(dataframe = yield_df_sum2, fun = yieldFun2)
# leach_y2 <- responseCurve(dataframe = leach_df_sum2, fun = leachFun2)
# conc_y2 <- responseCurve(dataframe = conc_df_sum2, fun = concFun2)
# 
# cornVal2 <- (yield_y2 - yield_y2[1]) * cornPrice
# fertCost2 <- kgha_to_lbac(fert) * fertPrice
# NUE <- 0.5
# nloss2 <- NUE * leach_y2 * fertPrice
# net2 <- cornVal2 - fertCost2 - nloss2
# 
# 
# modelDF2 <- data.frame(fert = round(kgha_to_lbac(fert)), yield = yield_y2, leach = kgha_to_lbac(leach_y2), conc = conc_y2,
#                       net = net2)
# 
# var2 <- var %>%
#   select(c(stdev, variable, fertilizerLbsAc, meanFert)) %>%
#   mutate(fert = round(fertilizerLbsAc))
# 
# model_var2 <- left_join(modelDF2, var2)
# 
# # modelDF max fert should be var max fert
# maxFert <- max(var2$fert)
# modelDF2 <- filter(modelDF2, fert < maxFert)
# print(maxFert)
# print(nrow(modelDF))
# 
# # create stdev column for each variable
# stdevDF2 <- model_var2 %>%
#   drop_na(stdev) %>%
#   pivot_wider(values_from = stdev, names_from = "variable", names_prefix = "stdev_") %>%
#   rename(yld_stdev = stdev_cropyld, leach_stdev = stdev_no3leach, conc_stdev = stdev_no3conc)
# 
# # side by side plots--------
# 
# net_axis <- list(
#   #tickfont = list(color = "black"),
#   overlaying = "y",
#   #anchor = "free",
#   #position = 0,
#   color = "#013229",
#   side = "right",
#   title = list(text = "Return to N ($/ac)",
#                font = list(size = 15, color = "#013229"),
#                standoff = 10L)
# )
# 
# net_axis2 <- list(
#   #tickfont = list(color = "black"),
#   overlaying = "y3",
#   #anchor = "free",
#   #position = 0,
#   color = "#013229",
#   side = "right",
#   title = list(text = "Return to N ($/ac)",
#                font = list(size = 15, color = "#013229"),
#                standoff = 10L)
# )
# 
# 
# 
# fig1 <- plot_ly(data = modelDF1, x = ~fert) %>%
#   add_lines(y = ~ yield, name = "Yield (bu/ac)",
#             yaxis = "y1",
#             hoverinfo = "text",
#             line = list(color = "#3B9422", width = 4, dash = "solid"),
#             hovertext = ~ paste("Yield:",round(yield, 1)),
#             legendgroup = "yield1") %>%
#   add_lines(y = ~ net, name = "Return to N",
#             yaxis = "y2",
#             hoverinfo = "text",
#             line = list(color = "darkgreen", width = 4, dash = "solid"),
#             hovertext = ~ paste0("Return to N: ", round(yvar, 1)),
#             legendgroup = "net1") %>%
#   layout(
#     xaxis = list(dtick = 25,
#                  title = list(text =  "N fertilizer (N lb/ac)",
#                               font = list(size = 15))),
#     yaxis = list(title = list(text = "Yield (bu/ac) (± 1 SD)",
#                               font = list(size = 15, color = "#2A7315")),
#                  color = "#2A7315"),
#     yaxis2 = net_axis,
#     hovermode = "x unified",
#     margin = list(r = 50, b = 10, t = 50),
#     legend = list(orientation = 'h', y = -0.2,
#                   font = list(size = 14))) 
#   
# 
# fig2 <- plot_ly(data = modelDF2, x = ~fert) %>%
#   add_lines(y = ~ yield, name = "Yield (bu/ac)",
#             yaxis = "y1",
#             hoverinfo = "text",
#             line = list(color = "#3B9422", width = 4, dash = "solid"),
#             hovertext = ~ paste("Yield:",round(yield, 1)),
#             legendgroup = "yield1") %>%
#   add_lines(y = ~ net, name = "Return to N",
#             yaxis = "y2",
#             hoverinfo = "text",
#             line = list(color = "darkgreen", width = 4, dash = "solid"),
#             hovertext = ~ paste0("Return to N: ", round(yvar, 1)),
#             legendgroup = "net1") %>%
#   layout(
#     xaxis = list(dtick = 25,
#                  title = list(text =  "N fertilizer (N lb/ac)",
#                               font = list(size = 15))),
#     yaxis = list(title = list(text = "Yield (bu/ac) (± 1 SD)",
#                               font = list(size = 15, color = "#2A7315")),
#                  color = "#2A7315"),
#     yaxis2 = net_axis2,
#     hovermode = "x unified",
#     margin = list(r = 50, b = 10, t = 50),
#     showlegend = FALSE) 
# 
# subplot(fig1, fig2, shareY = TRUE, titleY =  TRUE)
# 
# plotly::schema()