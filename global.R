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
leach_df <- read_csv("data/leachDataSite.csv.gz")
yield_df <- read_csv("data/yieldDataSite.csv.gz")
conc_df <- read_csv("data/concDataSite.csv.gz")

sim1Var <- read_csv("data/sim1_SDSite.csv.gz")
sim2Var <- read_csv("data/sim2_SDSite.csv.gz")
sim3Var <- read_csv("data/sim3_SDSite.csv.gz")
sim4Var <- read_csv("data/sim4_SDSite.csv.gz")

sim1wetdry <- read_csv("data/sim1wetdrySite.csv.gz")
sim2wetdry <- read_csv("data/sim2wetdrySite.csv.gz")
sim3wetdry <- read_csv("data/sim3wetdrySite.csv.gz")
sim4wetdry <- read_csv("data/sim4wetdrySite.csv.gz")

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
  modelDF <- data.frame(fert = round(kgha_to_lbac(fert)), yield = yield_y, leaching = kgha_to_lbac(leach_y), concentration = conc_y,
             net = net) %>%
    arrange(fert)

  var <- var %>%
    select(c(stdev, variable, fertilizerLbsAc)) %>%
    mutate(fert = round(fertilizerLbsAc))

  # join modeled DF and variance------------
  model_var <- left_join(modelDF, var, relationship = "many-to-many") 
  maxFert <- max(var$fert)  # modelDF max fert should be var max fert
  modelDF <- filter(modelDF, fert < maxFert)

  # create stdev column for each variable------------
  stdev_wide <- model_var %>%
    drop_na(stdev) %>%
    pivot_wider(values_from = stdev, names_from = "variable", names_prefix = "stdev_")

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

yield_y <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = list(text = "Yield (bu/ac, ± 1 SD) ",
               font = list(size = 15),
               standoff = 10L)
)

makeBasePlot <- function(simDat, variable, y1axis, y1axisLabel, yaxisUnit, stdevDF, stdevVar = NULL, nRec, var2color) {
  
  yvar = simDat[[y1axis]]
  stdevYvar = stdevDF[[y1axis]]
  if(!is.null(stdevVar)) {
    stdev = stdevDF[[stdevVar]]
    ymax = max(yvar, simDat$yield, (stdevYvar + stdev))
  }
  if(is.null(stdevVar)) {
    ymax = max(yvar, simDat$yield)
  }
  
  base_plot <- plot_ly(data = simDat, x = ~fert) %>%
    add_lines(y = ~ yield, name = "Yield (bu/ac)",
              yaxis = "y2",
              hoverinfo = "text",
              line = list(color = "#5DBB63", width = 4, dash = "solid"),
              hovertext = ~ paste("Yield:",round(yield, 1), "bu/ac"),
              legendgroup = "yield1") %>%
    add_lines(y = ~ yvar, name = paste(y1axisLabel, yaxisUnit),
              hoverinfo = "text",
              line = list(color = var2color, width = 4, dash = "dot"),
              hovertext = ~ paste0(y1axisLabel, ": ", round(yvar, 1)),
              legendgroup = "net1") %>% ##TODO check legend group
    add_ribbons(data = stdevDF, ymin = ~ yield - yld_stdev, ymax = ~ yield + yld_stdev,
                line = list(
                  color = "#5DBB63",
                  width = 1,
                  opacity = 0.5),
                fillcolor = "#5DBB63",
                yaxis = "y2",
                hoverinfo="none",
                opacity = 0.5,
                legendgroup = "yield1", showlegend = FALSE) %>%
    add_segments(x = ~nRec, y = 0, xend = ~ nRec, yend = ymax,
                 name = "N fertilizer recommendation",
                 hoverinfo="none",
                 line = list(color = "black", dash="dot")) %>%
    layout(
      xaxis = list(dtick = 25,
                   title = list(text =  "N fertilizer (N lb/ac)",
                                font = list(size = 15))),
      yaxis = list(title = list(text = y1axisLabel,
                                font = list(size = 15))),
      yaxis2 = yield_y,
      shapes = list(list(type = "rect", fillcolor = "#222222", line = list(color = "#222222"),
                       opacity = 0.2, y0 = 0, y1 = ymax, x0 = nRec-(.1*nRec), x1 = nRec+(.1*nRec))
                    ),
      hovermode = "x unified",
      margin = list(r = 50, b = 10, t = 50),
      legend = list(orientation = 'h', y = -0.5,
                    font = list(size = 14))
    ) 
  
  if(variable == "rtn") {
    base_plot <- base_plot
  } else {
    base_plot <- base_plot %>%
      add_ribbons(data = stdevDF, ymin = ~ stdevYvar - stdev, ymax = ~ stdevYvar + stdev,
                  line = list(
                    color = var2color,
                    width = 0.5,
                    opacity = 0),
                  fillcolor = var2color,
                  hoverinfo = "none",
                  opacity = 0.5,
                  legendgroup = "conc1", showlegend = FALSE) ## TODO check legendgroup
  }

  if(variable == "conc") {
    base_plot <- base_plot %>%
        add_lines(y = 10, name = "EPA safe drinking water standard (10 NO<sub>3</sub> ppm)",
                  line = list(color = "#d40000", width = 2, dash = "solid"),
                  hoverinfo = "none",
                  hovertext='EPA safe drinking water standard')
  }

  return(base_plot)
  
}

#base_plot <- makeBasePlot(simDat = modelDF1,variable = "rtn", y1axis = "net1", y1axisLabel = "Return to N", yaxisUnit = "$/ac",stdevDF = stdevDF) 
# makeBasePlot(simDat = modelDF1, variable = "conc", y1axis = "conc1", y1axisLabel = "Nitrate concentration", yaxisUnit = "ppm",
#              stdevDF = stdevDF, stdevVar = "conc_stdev1")
# makeBasePlot(simDat = modelDF1, variable = "leach", y1axis = "leach1", y1axisLabel = "Nitrate concentration", yaxisUnit = "lb/ac",
#              stdevDF = stdevDF, stdevVar = "leach_stdev1")


addWetDryLines <- function(wetDryDat, wetY, dryY, y_side, precName, precUnits, wet = "none", dry = "none", base_plot = base_plot) {
  
  dryVar = wetDryDat[[dryY]]
  wetVar = wetDryDat[[wetY]]
  
  if(wet == "none" & dry == "none") { # return base plot if wet and dry are null
    #print("base")
    plt <- base_plot
  } else if (wet == "wet" & dry == "none") { # return base plot plus wet if wet is checked
    #print(paste("wet", is.null(wet)))
    plt <- base_plot %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~wetVar, name = paste("Wettest", precName),
                line = list(color = "blue", width = 4, dash = "solid"), yaxis = y_side,
                hovertext = ~ paste0("Wet ", precName, ": ",round(wetVar, 1), " ", precUnits))
  } else if (wet == "none" & dry == "dry") { # return base plot plus dry if dry is checked
    #print("dry")
    plt <- base_plot %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~dryVar, name = paste("Driest", precName),
                line = list(color = "blue", width = 4, dash = "dash"), yaxis = y_side,
                hovertext = ~ paste0("Dry ", precName, ": ",round(dryVar, 1), " ", precUnits))
  } else { # both are checked
    #print("both")
    plt <- base_plot %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~dryVar, name = paste("Driest", precName),
                line = list(color = "blue", width = 4, dash = "dash"), yaxis = y_side,
                hovertext = ~ paste0("Dry ", precName, ": ",round(dryVar, 1), " ", precUnits)) %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~wetVar, name = paste("Wettest", precName),
                line = list(color = "blue", width = 4, dash = "solid"),yaxis = y_side,
                hovertext = ~ paste0("Wet ", precName, ": ",round(wetVar, 1), " ", precUnits))
  }
  
  plt
}

# addWetDryLines(wetDryDat = wetDryData,
#                wetY = "wetYield", dryY = "dryYield", y_side = "y2", precName = "yield", precUnits = "bu/ac", wet = "wet", dry = "dry",
#                base_plot = base_plot)

makeSim1plot <- function(simDat, stdevDF, variable, nRec,#y1axis, y1axisLabel, yaxisUnit, dryY, wetY, precName,
                         wetDryDat,  wet = "none", dry = "none") {
  
  if(variable == "rtn") {
    y1axis = "net";
    y1axisLabel = "Return to N ($/ac)";
    yaxisUnit = "$/ac";
    var2color = "#013229"
    stdevVar = NULL;
    dryY = "dryYield";
    wetY = "wetYield";
    y_side = "y2";
    precName = "yield";
    precUnits = "bu/ac"
  }
  if(variable == "leach") {
    y1axis = "leaching";
    y1axisLabel = "NO<sub>3</sub> leaching (lb/ac, ± 1 SD)";
    yaxisUnit = "lb/ac";
    var2color = "#593587"
    stdevVar = "leach_stdev";
    dryY = "dryLeach";
    wetY = "wetLeach";
    y_side = "y1";
    precName = "leach";
    precUnits = "lb/ac"
  }
  if(variable == "conc") {
    y1axis = "concentration";
    y1axisLabel = "NO<sub>3</sub> concentration (ppm, ± 1 SD)";
    yaxisUnit = "ppm";
    var2color = "#3468c0"
    stdevVar = "conc_stdev";
    dryY = "dryConc";
    wetY = "wetConc";
    y_side = "y1";
    precName = "concentration";
    precUnits = "ppm"
  }
  
  base_plot <- makeBasePlot(simDat = simDat, variable = variable,
                            y1axis = y1axis, y1axisLabel = y1axisLabel, yaxisUnit = yaxisUnit,
                            stdevDF = stdevDF, stdevVar = stdevVar, nRec = nRec, var2color = var2color)
  
  
  plt <- addWetDryLines(wetDryDat = wetDryDat, wetY = wetY, dryY = dryY, y_side = y_side, precName = precName, precUnits = precUnits,
                        wet = wet, dry = dry, base_plot = base_plot)
  
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



#makeSim1plot(simDat = modelDF1, wetDryDat = wetDryData, stdevDF = stdevDF, variable = "conc", wet = "wet")
#makeSim1plot(simDat = modelDF1, wetDryDat = wetDryData, stdevDF = stdevDF, variable = "rtn", wet = 'wet', dry = 'dry')
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
#   filter(lat == yield_df_sum$lat,
#          lon == yield_df_sum$lon)
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
# modelDF1 <- data.frame(fert = round(kgha_to_lbac(fert)), yield1 = yield_y,
#                       leach1 = kgha_to_lbac(leach_y), conc1 = conc_y,
#                       net1 = net)
# 
#  wetDryData <- sim1wetdry %>%
#    right_join(yield_df_sum, by = c("lon.sims" = "lon", "lat.sims" = "lat"))
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
# print(nrow(modelDF))
# 
# # create stdev column for each variable
# stdevDF <- model_var1 %>%
#   drop_na(stdev) %>%
#   pivot_wider(values_from = stdev, names_from = "variable", names_prefix = "stdev_") %>%
#   rename(yld_stdev1 = stdev_cropyld, leach_stdev1 = stdev_no3leach, conc_stdev1 = stdev_no3conc)
#print("stdev_wide")

# head(stdev_wide1)
# ##TODO how do I align the ribbons and lines?
# plot_ly(modelDF1, x = ~fert, y = ~ yield1, name = "Yield (bu/ac)",
#         type = 'scatter', mode = 'lines',
#         line = list(color = "#5dbb63", width = 2),
#         #marker = list(size = 1, color = "#5dbb63"),
#         hovertext = ~ paste("Yield:", round(yield1, 1), "bu/ac"),
#         hoverinfo = "text",
#         legendgroup = "yield") %>%
#   add_trace(y = ~ leach1, name = "Nitrate leaching (lb/ac)",
#             line = list(color = "#c99f6e", width = 2),
#             #marker = list(color = "#c99f6e"),
#             hovertext = ~ paste("Nitrate leaching:", round(leach1, 1), "lbs/ac"),
#             hoverinfo = "text",
#             legendgroup = "leach") %>%
#   add_trace(y = ~ conc1, name = "Nitrate concentration (ppm)",
#             line = list(color = "#6e8fc9", width = 2),
#             #marker = list(color = "#6e8fc9"),
#             hovertext = ~ paste("Nitrate concentration:", round(conc1, 1), "ppm"),
#             hoverinfo = "text",
#             legendgroup = "conc") %>%
#   add_trace(y = ~ net1, name = "Return to N ($/ac)",
#             line = list(color = "black", width = 2.5),
#             #marker = list(color = "black"),
#             hovertext = ~ paste("Return to N:", round(net1, 1), "$/ac"),
#             hoverinfo = "text",
#             legendgroup = "net") %>%
#   # add_trace(y = 0,
#   #           opacity = 0,
#   #           hovertext = ~ paste("N fert rate:",fert, "lbs N/ac"),
#   #           hoverinfo = "text",
#   #           showlegend = F) %>%
#   add_ribbons(data = stdev_wide1, x = ~ fert, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
#               line = list(
#                 color = "#5dbb63",
#                 width = 0.5,
#                 opacity = 0),
#               hovertext = ~paste("yield: ±", round(yld_stdev1)),
#               fillcolor = "#5dbb63",
#               opacity = 0.5,
#               legendgroup = "yield", showlegend = FALSE) %>%
#   add_ribbons(ymin = ~ leach1 - leach_stdev1, ymax = ~ leach1 + leach_stdev1,
#               line = list(
#                 color = "#c99f6e",
#                 width = 0.5,
#                 opacity = 0),
#               hovertext = ~paste("nitrate leaching: ±", round(leach_stdev1, 1)),
#               fillcolor = "#c99f6e",
#               opacity = 0.5,
#               legendgroup = "leach", showlegend = FALSE) %>%
#   add_ribbons(ymin = ~ conc1 - conc_stdev1, ymax = ~ conc1 + conc_stdev1,
#               line = list(
#                 color = "#6e8fc9",
#                 width = 0.5,
#                 opacity = 0),
#               hovertext = ~paste("nitrate concentration: ±", round(conc_stdev1, 1)),
#               fillcolor = "#6e8fc9",
#               opacity = 0.5,
#               legendgroup = "conc", showlegend = FALSE) %>%
#   layout(title = "Responses to fertilizer N",
#          xaxis = list(title = "N fertilizer (N lb/ac)"),
#          yaxis = list (title = " "),
#          hovermode = "x unified",
#          legend = list(orientation = 'h', y = -0.2))
# 
# # test data set plot 2---------------------
# 
# ##TODO make 2nd data set and test plot
# 
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
#   filter(lat == yield_df_sum$lat,
#          lon == yield_df_sum$lon)
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
# modelDF2 <- data.frame(fert = round(kgha_to_lbac(fert)), yield2 = yield_y2, leach2 = kgha_to_lbac(leach_y2), conc2 = conc_y2,
#                       net2 = net2)
# 
# var2 <- var %>%
#   select(c(stdev, variable, fertilizerLbsAc, meanFert)) %>%
#   mutate(fert = round(fertilizerLbsAc))
# 
# model_var2 <- left_join(modelDF2, var2)
# 
# # modelDF max fert should be var max fert
# maxFert <- max(var$fert)
# modelDF <- filter(modelDF, fert < maxFert)
# print(maxFert)
# print(nrow(modelDF))
# 
# # create stdev column for each variable
# stdev_wide2 <- model_var2 %>%
#   drop_na(stdev) %>%
#   pivot_wider(values_from = stdev, names_from = "variable", names_prefix = "stdev_") %>%
#   rename(yld_stdev2 = stdev_cropyld, leach_stdev2 = stdev_no3leach, conc_stdev2 = stdev_no3conc)
# 
# 
# ##TODO start here
# p <- plot_ly(data = modelDF1, x = ~fert, hoverinfo = "text") %>%
#   add_lines(y = ~ yield1, name = paste("sim1", "yield (bu/ac)"),
#             line = list(color = "#5dbb63", width = 2, dash = "solid"),
#             hovertext = ~ paste("sim1", "yield:",round(yield1, 1), "bu/ac"),
#             #hoverinfo = "text",
#             legendgroup = "yield1") %>%
#   add_lines(y = ~ leach1, name = paste("sim1", "NO3 leaching (lb/ac)"),
#             line = list(color = "#5dbb63", width = 2, dash = "dash"),
#             hovertext = ~paste("sim1", "nitrate leaching:",round(leach1, 1), "lbs/ac"),
#             hoverinfo = "text",
#             legendgroup = "leach1") %>%
#   add_lines(y = ~ conc1, name = paste("sim1", "NO3 concentration (ppm)"),
#             line = list(color = "#5dbb63", width = 2, dash = "dot"),
#             hovertext = ~ paste("sim1", "nitrate concentration:", round(conc1, 1), "ppm"),
#             hoverinfo = "text",
#             legendgroup = "conc1") %>%
#   add_lines(y = ~ net1, name = paste("sim1", "return to N ($/ac)"),
#             line = list(color = "#5dbb63", width = 3, dash = "dashdot"),
#             hovertext = ~ paste("sim1", "return to N:", round(net1, 1), "$/ac"),
#             hoverinfo = "text",
#             legendgroup = "net1") %>%
#   add_ribbons(data = stdev_wide1, ymin = ~ yield1 - yld_stdev1, ymax = ~ yield1 + yld_stdev1,
#               line = list(
#                 color = "#5dbb63"),
#               fillcolor = "#5dbb63",
#               hovertext = ~paste("sim1", "yield:", "±", round(yld_stdev1)),
#               opacity = 0.5,
#               legendgroup = "yield1", showlegend = FALSE) %>%
#   add_ribbons(ymin = ~ leach1 - leach_stdev1, ymax = ~ leach1 + leach_stdev1,
#               line = list(
#                 color = "#5dbb63",
#                 width = 0.5,
#                 opacity = 0),
#               fillcolor = "#5dbb63",
#               hovertext = ~paste("nitrate leach: ±", round(leach_stdev1, 1)),
#               opacity = 0.5,
#               legendgroup = "leach1", showlegend = FALSE) %>%
#   add_ribbons(ymin = ~ conc1 - conc_stdev1, ymax = ~ conc1 + conc_stdev1,
#               line = list(
#                 color = "#5dbb63",
#                 width = 0.5,
#                 opacity = 0),
#               fillcolor = "#5dbb63",
#               hovertext = ~paste("nitrate concentration: ±", round(conc_stdev1, 1)),
#               opacity = 0.5,
#               legendgroup = "conc1", showlegend = FALSE)  %>%
#   add_lines(data = modelDF2, y = ~ yield2, name = paste("sim2", "yield (bu/ac)"),
#             line = list(color = "#c99f6e", width = 1, dash = "solid"),
#             hovertext = ~paste("sim2", "yield:",round(yield2, 1), "bu/ac"),
#             hoverinfo = "text",
#             legendgroup = "yield2") %>%
#   add_lines(y = ~ leach2, name = paste("sim2", "NO3 leaching (lb/ac)"),
#             line = list(color = "#c99f6e", width = 1, dash = "dash"),
#             hovertext = ~paste("sim2", "nitrate leaching:",round(leach2, 1), "lbs/ac"),
#             hoverinfo = "text",
#             legendgroup = "leach2") %>%
#   add_lines(y = ~ conc2, name = paste("sim2", "NO3 concentration (ppm)"),
#             line = list(color = "#c99f6e", width = 1, dasy = "dot"),
#             hovertext = ~ paste("sim2", "nitrate concentration:", round(conc2, 1), "ppm"),
#             hoverinfo = "text",
#             legendgroup = "conc2") %>%
#   add_lines(y = ~ net2, name = paste("sim2", "return to N ($/ac)"),
#             line = list(color = "#c99f6e", width = 3, dash = "dashdot"),
#             hovertext = ~ paste("sim2", "return to N:", round(net2, 1), "$/ac"),
#             hoverinfo = "text",
#             legendgroup = "net2") %>%
#   add_ribbons(data = stdev_wide2, ymin = ~ yield2 - yld_stdev2, ymax = ~ yield2 + yld_stdev2,
#               line = list(
#                 color = "#c99f6e",
#                 width = 0.5,
#                 opacity = 0),
#               fillcolor = "#c99f6e",
#               opacity = 0.5,
#               legendgroup = "yield2", showlegend = FALSE) %>%
#   add_ribbons(ymin = ~ leach2 - leach_stdev2, ymax = ~ leach2 + leach_stdev2,
#               line = list(
#                 color = "#c99f6e",
#                 width = 0.5,
#                 opacity = 0),
#               fillcolor = "#c99f6e",
#               opacity = 0.5,
#               legendgroup = "leach2", showlegend = FALSE) %>%
#   add_ribbons(ymin = ~ conc2 - conc_stdev2, ymax = ~ conc2 + conc_stdev2,
#               line = list(
#                 color = "#c99f6e",
#                 width = 0.5,
#                 opacity = 0),
#               fillcolor = "#c99f6e",
#               opacity = 0.5,
#               legendgroup = "conc2", showlegend = FALSE) %>%
#   add_trace(y = 0,
#             type = 'scatter', mode = 'lines',
#             opacity = 0,
#             hovertext = ~ paste("N fert rate:",fert, "lbs N/ac"),
#             hoverinfo = "text",
#             showlegend = F) %>%
#   layout(title = "Responses to fertilizer N",
#          xaxis = list(title = "N fertilizer (N lb/ac)"),
#          yaxis = list (title = " "),
#          hovermode = "x unified",
#          legend = list(orientation = 'h',
#                        y = -0.3))
# 
# 
# p
# 
# plotly::schema()
