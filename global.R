library(shiny)
library(leaflet)
#library(leaflet.extras)
library(tidyverse)
library(sf)
library(plotly)
library(gt)
library(reactlog)
library(shinyjs)
library(shinycssloaders)


reactlog_enable()

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


##TODO save as csv.gz
# load data-----------------
leach_df <- read_csv("data/leachData.csv.gz")
yield_df <- read_csv("data/yieldData.csv.gz")
conc_df <- read_csv("data/concData.csv.gz")

#sim1Var <- read_csv("data/sim1_SD.csv.gz")
#sim2Var <- read_csv("data/sim2_SD.csv.gz")
#sim3Var <- read_csv("data/sim3_SD.csv.gz")
#sim4Var <- read_csv("data/sim4_SD.csv.gz")

sim1wetdry <- read_csv("data/sim1wetdry.csv.gz")
sim2wetdry <- read_csv("data/sim2wetdry.csv.gz")
sim3wetdry <- read_csv("data/sim3wetdry.csv.gz")
sim4wetdry <- read_csv("data/sim4wetdry.csv.gz")

sites <- read_csv("data/sampleSites.csv.gz") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>%
  mutate(id = row_number(), .before = 1)

sims <-  readxl::read_xlsx("data/simulationNames.xlsx")
simNames = sims$cropSystem

soil <- read_csv("data/soilText.csv.gz") 
soil_csv <- read_csv("data/soilClasses.csv")

gdd <- read_csv("data/GDD.csv_gz")

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
                col = "blue",
                layerId = ~state,
                options = pathOptions(pane = "states")) %>%
    addPolygons(data = counties,
                group = "county",
                col = "darkgreen",
                layerId = ~id,
                options = pathOptions(pane = "counties")) %>%
    addCircleMarkers(
      data = sites,
      lat = ~lat, lng = ~lon,
      layerId = ~id,
      group = "sites", 
      options = pathOptions(pane = "sites")
    ) %>%
    groupOptions("state", zoomLevels = 1:8) %>%
    groupOptions("county", zoomLevels = 8:14) %>%
    groupOptions("sites", zoomLevels = 9:14) %>%
    setView(lat = 41.5, lng = -93.5, zoom = 4) %>%
    addProviderTiles("Esri.WorldTopoMap") 
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

makeDF <- function(simulation, site_lat, site_lon) {
  #makeDF <- function(simulation, site_lat, site_lon, cornPrice, fertPrice, NUE, cornTech, fertEff) {
  req(is.na(simulation) == FALSE)
  
  # if(simulation == 1) {var = sim1Var}
  # if(simulation == 2) {var = sim2Var}
  # if(simulation == 3) {var = sim3Var}
  # if(simulation == 4) {var = sim4Var}
  
  if(simulation == 1) {wetDry = sim1wetdry}
  if(simulation == 2) {wetDry = sim2wetdry}
  if(simulation == 3) {wetDry = sim3wetdry}
  if(simulation == 4) {wetDry = sim4wetdry}
  
  # var <- var %>%
  #   #mutate(meanFert = round(meanFert)) %>%
  #   filter(lat == site_lat,
  #          lon == site_lon)
  
  wetDry <- wetDry %>%
    #mutate(meanFert = round(meanFert)) %>%
    filter(lat.sims == site_lat,
           lon.sims == site_lon)
  
  
  #print("head var")
  #print(head(var))
  
  yield_df_sum <- yield_df %>%
    filter(sim == simulation,
           lat == site_lat,
           lon == site_lon)

  # leach_df_sum <- leach_df %>%
  #   filter(sim == simulation,
  #          lat == site_lat,
  #          lon == site_lon)
  # 
  # conc_df_sum <- conc_df %>%
  #   filter(sim == simulation,
  #          lat == site_lat,
  #          lon == site_lon)

  yieldFun <- yield_df_sum$fun
  # leachFun <- leach_df_sum$fun
  # concFun <- conc_df_sum$fun

  #fertOrder <- unique(sort(var$meanFert))
  #print("fert")
  #print(fertOrder)

  yield_y <- responseCurve(dataframe = yield_df_sum, fun = yieldFun)
  #print(yield_y)
  # corn yield improvements
  #yield_new <- yield_y * cornTech
  #leach_y <- responseCurve(dataframe = leach_df_sum, fun = leachFun)
  #print(leach_y)
  # nitrogen use improvements
  #leach_new <- leach_y * fertEff
  #conc_y <- responseCurve(dataframe = conc_df_sum, fun = concFun)
  #print(conc_y)

  # cornVal <- (yield_y - yield_y[1]) * cornPrice
  # fertCost <- round(kgha_to_lbac(fert)) * fertPrice
  # NUE <- 0.5
  # nloss <- NUE * leach_y * fertPrice
  # net <- cornVal - fertCost - nloss

  data.frame(fert = round(kgha_to_lbac(fert)), yield = yield_y) %>%
    arrange(fert)
  #print(nrow(modelDF))

  #print(modelDF)

  # var <- var %>%
  #   select(c(stdev, variable, fertilizerLbsAc)) %>%
  #   mutate(fert = round(fertilizerLbsAc))
   
  # join modeled DF and variance 
  #model_var <- left_join(modelDF, var, relationship = "many-to-many") 
  
  # # modelDF max fert should be var max fert
  # maxFert <- max(var$fert)
  # modelDF <- filter(modelDF, fert < maxFert)
  # #print(maxFert)
  # #print(nrow(modelDF))
  # 
  # # create stdev column for each variable
  # stdev_wide <- model_var %>%
  #   drop_na(stdev) %>%
  #   pivot_wider(values_from = stdev, names_from = "variable", names_prefix = "stdev_")
  # #print("stdev_wide")
  # #print(stdev_wide)

  #return(modelDF)
  
}

makeWetDryDF <- function(simulation, site_lat, site_lon) {
  #makeDF <- function(simulation, site_lat, site_lon, cornPrice, fertPrice, NUE, cornTech, fertEff) {
  req(is.na(simulation) == FALSE)
  
  if(simulation == 1) {wetDry = sim1wetdry}
  if(simulation == 2) {wetDry = sim2wetdry}
  if(simulation == 3) {wetDry = sim3wetdry}
  if(simulation == 4) {wetDry = sim4wetdry}
  
  wetDry <- wetDry %>%
    #mutate(meanFert = round(meanFert)) %>%
    filter(lat.sims == site_lat,
           lon.sims == site_lon)
  
 
  return(wetDry)
  
}


# plot bones-----------------------

yield_y <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = list(text = "Yield (bu/ac)",
               font = list(size = 15),
               standoff = 10L)
)

makeYldplot <- function(simDat, wetDryDat, wet = "none", dry = "none") {
  
  base_plot <- plot_ly(data = simDat, x = ~fert, hoverinfo = "text") %>%
    add_lines(y = ~ yield1, name = "Yield (bu/ac)",
              line = list(color = "#ff9843", width = 4, dash = "solid"),
              hovertext = ~ paste("Yield:",round(yield1, 1), "bu/ac")) %>%
    layout(
      xaxis = list(title = list(text =  "N fertilizer (N lb/ac)",
                                font = list(size = 15))),
      yaxis = list(title = list(text = "Yield (bu/ac)",
                                font = list(size = 15))),
      hovermode = "x unified",
      margin = list(r = 50, b = 10, t = 50),
      legend = list(orientation = 'h', y = -0.5,
                    font = list(size = 14))
    )  
  
  # return base plot if wet and dry are null
  #if(is.null(wet) & is.null(dry)) {
  if(wet == "none" & dry == "none") {
    print("base")
    plt <- base_plot
  } else if (wet == "wet" & dry == "none") { # return base plot plus wet if wet is checked
    print(paste("wet", is.null(wet)))
    plt <- base_plot %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~wetYield,
                line = list(color = "blue", width = 4, dash = "solid"),
                hovertext = ~ paste("Wet yield:",round(wetYield, 1), "bu/ac"))
  } else if (wet == "none" & dry == "dry") { # return base plot plus dry if dry is checked
    print("dry")
    plt <- base_plot %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~dryYield,
                line = list(color = "yellow", width = 4, dash = "solid"),
                hovertext = ~ paste("Dry yield:",round(wetYield, 1), "bu/ac"))
  } else { # both are checked
    print("both")
    plt <- base_plot %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~dryYield,
                line = list(color = "yellow", width = 4, dash = "solid"),
                hovertext = ~ paste("Dry yield:",round(wetYield, 1), "bu/ac")) %>%
      add_lines(data = wetDryDat, x = ~fertilizerLbsAc, y = ~wetYield,
                line = list(color = "blue", width = 4, dash = "solid"),
                hovertext = ~ paste("Wet yield:",round(wetYield, 1), "bu/ac"))
  }
 
  plt
  
}


# makeYldplot(simDat = simData, wetDryDat = wetDryData)
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
# 
# simData <- data.frame(fert = round(kgha_to_lbac(fert)), yield1 = yield_y)
# modelDF1 <- data.frame(fert = round(kgha_to_lbac(fert)), yield1 = yield_y,
#                       leach1 = kgha_to_lbac(leach_y), conc1 = conc_y,
#                       net1 = net)

 # wetDryData <- sim1wetdry %>%
 #   right_join(yield_df_sum, by = c("lon.sims" = "lon", "lat.sims" = "lat"))
# # modelDF
# 
# var1 <- var %>%
#   select(c(stdev, variable, fertilizerLbsAc, meanFert)) %>%
#   mutate(fert = round(fertilizerLbsAc))
# 
# model_var1 <- left_join(modelDF1, var1)
# 
# # modelDF max fert should be var max fert
# maxFert <- max(var$fert)
# modelDF <- filter(modelDF, fert < maxFert)
# print(maxFert)
# print(nrow(modelDF))
# 
# # create stdev column for each variable
# stdev_wide1 <- model_var1 %>%
#   drop_na(stdev) %>%
#   pivot_wider(values_from = stdev, names_from = "variable", names_prefix = "stdev_") %>%
#   rename(yld_stdev1 = stdev_cropyld, leach_stdev1 = stdev_no3leach, conc_stdev1 = stdev_no3conc)
# #print("stdev_wide")
# 
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
