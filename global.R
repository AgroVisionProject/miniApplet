library(shiny)
library(leaflet)
#library(leaflet.extras)
library(tidyverse)
library(sf)
library(plotly)
library(gt)
library(reactlog)
library(shinyjs)



reactlog_enable()

# load shapefiles
states = st_read("data/ncr_states_simple.shp")
counties = st_read("data/ncr_counties_simple.shp")


# load data-----------------
leach_df <- read_csv("data/leachData.csv")
yield_df <- read_csv("data/yieldData.csv")
conc_df <- read_csv("data/concData.csv")

sim1Var <- read_csv("data/sim1_SD.csv")
sim2Var <- read_csv("data/sim2_SD.csv")
sim3Var <- read_csv("data/sim3_SD.csv")
sim4Var <- read_csv("data/sim4_SD.csv")

sites <- read_csv("data/sampleSites.csv")
sims <-  readxl::read_xlsx("data/simulationNames.xlsx")
simNames = sims$cropSystem

# define fertilizer/x axis----------
#fert = seq(from = 0, to = 300, by = 1) #kg/ha
# convert kg/ha to lb/ac
kgha_to_lbac <- function(x) {
  lbac <- x*(2.20462/2.47105)
  return(lbac)
}

# map function------------

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
    groupOptions("state", zoomLevels = 1:9) %>%
    groupOptions("county", zoomLevels = 7:10) %>%
    addProviderTiles("Esri.WorldTopoMap")
}


# determine response curve--------
responseCurve <- function(dataframe, fun, fert) {
  
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

makeDF <- function(simulation, site_lat, site_lon, cornPrice, fertPrice) {
  #makeDF <- function(simulation, site_lat, site_lon, cornPrice, fertPrice, NUE, cornTech, fertEff) {
  
  req(is.na(simulation) == FALSE)
  
  if(simulation == 1) {var = sim1Var}
  if(simulation == 2) {var = sim2Var}
  if(simulation == 3) {var = sim3Var}
  if(simulation == 4) {var = sim4Var}
  
  var <- var %>%
    #mutate(meanFert = round(meanFert)) %>%
    filter(lat == site_lat,
           lon == site_lon)
  
  print("head var")
  print(head(var))
  
  yield_df_sum <- yield_df %>%
    filter(sim == simulation,
           lat == site_lat,
           lon == site_lon)

  leach_df_sum <- leach_df %>%
    filter(sim == simulation,
           lat == site_lat,
           lon == site_lon)

  conc_df_sum <- conc_df %>%
    filter(sim == simulation,
           lat == site_lat,
           lon == site_lon)

  yieldFun <- yield_df_sum$fun
  leachFun <- leach_df_sum$fun
  concFun <- conc_df_sum$fun

  fertOrder <- unique(sort(var$meanFert))
  print("fert")
  print(fertOrder)

  yield_y <- responseCurve(dataframe = yield_df_sum, fun = yieldFun, fert = fertOrder)
  print(yield_y)
  # corn yield improvements
  #yield_new <- yield_y * cornTech
  leach_y <- responseCurve(dataframe = leach_df_sum, fun = leachFun, fert = fertOrder)
  print(leach_y)
  # nitrogen use improvements
  #leach_new <- leach_y * fertEff
  conc_y <- responseCurve(dataframe = conc_df_sum, fun = concFun, fert = fertOrder)
  print(conc_y)

  cornVal <- (yield_y - yield_y[1]) * cornPrice
  fertCost <- round(kgha_to_lbac(fertOrder)) * fertPrice
  NUE <- 0.5
  nloss <- NUE * leach_y * fertPrice
  net <- cornVal - fertCost - nloss

  fertAttach = plyr::round_any(kgha_to_lbac(fertOrder), 5)
  print("fertAttach")
  print(fertAttach)

  modelDF <- data.frame(fert = round(kgha_to_lbac(fertOrder)), yield = yield_y, leaching = kgha_to_lbac(leach_y), concentration = conc_y,
             net = net, fertAttach = fertAttach) %>%
    arrange(fert)

  #print(modelDF)

  var <- var %>%
    select(c(stdev, variable, fertilizerLbsAc, meanFert)) %>%
    mutate(fertAttach = plyr::round_any(kgha_to_lbac(meanFert), 5)) %>%
    arrange(fertAttach)

  #print(var)
  print(levels(as.factor(var$fertAttach)))
   
  dfVar <- left_join(var, modelDF)
   
  print(dfVar)
  # 
  sd_wide <- dfVar %>% pivot_wider(values_from = stdev, names_from = "variable")
  print(sd_wide)
  return(sd_wide)
  
}

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
# # 
#  fertOrder <- unique(sort(var$meanFert))
# # 
#  yield_y <- responseCurve(dataframe = yield_df_sum, fun = yieldFun, fert = fertOrder)
# # 
#  leach_y <- responseCurve(dataframe = leach_df_sum, fun = leachFun, fert = fertOrder)
# # 
#  conc_y <- responseCurve(dataframe = conc_df_sum, fun = concFun, fert = fertOrder)
# # 
# # cornPrice = 5
# # fertPrice = 1
#  cornVal <- (yield_y - yield_y[1]) * cornPrice
#  fertCost <- kgha_to_lbac(fertOrder) * fertPrice
# # NUE <- 0.5
#  nloss <- NUE * leach_y * fertPrice
#  net <- cornVal - fertCost - nloss
# # 
#  fertAttach = plyr::round_any(kgha_to_lbac(fertOrder), 5)
# 
#  # #fertOrder <- unique(sort(var$meanFert))
# modelDF <- data.frame(fert = round(kgha_to_lbac(fertOrder)), yield = yield_y, leaching = kgha_to_lbac(leach_y), concentration = conc_y,
#                       net = net, fertAttach = fertAttach)
# 
# # modelDF
# # var <- var %>%
# #   select(c(stdev, variable, fertilizerLbsAc, meanFert)) %>%
# #   mutate(fertilizerLbsAc = round(fertilizerLbsAc))
# var <- var %>%
#   select(c(stdev, variable, fertilizerLbsAc, meanFert)) %>%
#   mutate(fertAttach = plyr::round_any(kgha_to_lbac(meanFert), 5))
# 
# #sd_wide <- var %>% pivot_wider(values_from = stdev, names_from = "variable")
# 
# dfVar <- left_join(var, modelDF)
# # 
# # dfVar <- left_join(var, modelDF, by = c("fertilizerLbsAc" = "fert"))
# # 
# # print(dfVar[1:60,])
# # # 
#  dfVar2 <- dfVar %>% pivot_wider(values_from = stdev, names_from = "variable") %>%
#    arrange(fertilizerLbsAc)
# # # print(head(dfVar2))
# # return(modelDF)
#  p <- plot_ly(dfVar2, x = ~fert, y = ~ yield, name = "Yield (bu/ac)",
#          type = 'scatter', mode = 'lines+markers',
#          line = list(color = "#5dbb63", width = 1),
#          marker = list(size = 10, color = "#5dbb63"),
#          hovertext = ~ paste("Yield:", round(yield, 1), "bu/ac"),
#          hoverinfo = "text"
#  ) %>%
#    add_trace(y = ~ leaching, name = "Nitrate leaching (lb/ac)",
#              line = list(color = "#c99f6e", width = 1),
#              marker = list(color = "#c99f6e"),
#              hovertext = ~ paste("Nitrate leaching:", round(leaching, 1), "lbs/ac"),
#              hoverinfo = "text") %>%
#    add_trace(y = ~ concentration, name = "Nitrate concentration (ppm)",
#              line = list(color = "#6e8fc9", width = 1),
#              marker = list(color = "#6e8fc9"),
#              hovertext = ~ paste("Nitrate concentration:", round(concentration, 1), "ppm"),
#              hoverinfo = "text") %>%
#    add_trace(y = ~ net, name = "Return to N ($/ac)",
#              line = list(color = "black", width = 1),
#              marker = list(color = "black"),
#              hovertext = ~ paste("Return to N:", round(net, 1), "$/ac"),
#              hoverinfo = "text") %>%
#    add_trace(y = 0,
#              opacity = 0,
#              hovertext = ~ paste("N fert rate:",fert, "lbs N/ac"),
#              hoverinfo = "text",
#              showlegend = F)
#  
#  p %>%
#    add_ribbons(ymin = ~ yield - cropyld, ymax = ~ yield + cropyld,
#                line = list(
#                  color = "#5dbb63",
#                  width = 0.5,
#                  opacity = 0),
#                fillcolor = "#5dbb63",
#                opacity = 0.5) %>%
#    add_ribbons(ymin = ~ leaching - no3leach, ymax = ~ leaching + no3leach,
#                line = list(
#                  color = "#c99f6e",
#                  width = 0.5,
#                  opacity = 0),
#                fillcolor = "#c99f6e",
#                opacity = 0.5) %>%
#    add_ribbons(data = stdev_conc, x = ~fert, ymin = ~ conc - stdev_no3conc, ymax = ~ conc + stdev_no3conc,
#                line = list(
#                  color = "black",
#                  width = 0.5,
#                  opacity = 0),
#                fillcolor = "black",
#                opacity = 0.5)
#  layout(title = "Responses to fertilizer N",
#         xaxis = list(title = "N fertilizer (N lb/ac)"),
#         yaxis = list (title = " "),
#         hovermode = "x unified",
#         legend = list(orientation = 'h', y = -0.2))
#  
# 
# yield_y <- responseCurve(dataframe = yield_df_sum, fun = yieldFun)
# leach_y <- responseCurve(dataframe = leach_df_sum, fun = leachFun)
# 
# cornPrice = 5
# fertPrice = 1.1
# cornVal <- (yield_y - yield_y[1]) * cornPrice
# fertCost <- round(kgha_to_lbac(fert)) * fertPrice
# NUE <- 0.5
# nloss <- NUE * leach_y * fertPrice
# net <- cornVal - fertCost - nloss
# 
# testdf <- data.frame(fert = round(kgha_to_lbac(fert)), yield = yield_y, leaching = kgha_to_lbac(leach_y),
#            net = net)
# 
# 
# p <- plot_ly(testdf, x = ~fert, y = ~ yield, name = "Yield (bu/ac)",
#              type = 'scatter', mode = 'lines+markers',
#              line = list(color = "#5dbb63", width = 1),
#              marker = list(size = 10, color = "#5dbb63"),
#              hovertext = ~ paste("Yield:", round(yield, 1), "bu/ac"),
#              hoverinfo = "text"
# ) %>%
#   add_trace(y = ~ leaching, name = "Nitrate leaching (lb/ac)",
#             line = list(color = "#c99f6e", width = 1),
#             marker = list(color = "#c99f6e"),
#             hovertext = ~ paste("Nitrate leaching:", round(leaching, 1), "lbs/ac"),
#             hoverinfo = "text") %>%
#   add_trace(y = ~ net, name = "Return to N ($/ac)",
#             line = list(color = "black", width = 1),
#             marker = list(color = "black"),
#             hovertext = ~ paste("Return to N:", round(net, 1), "$/ac"),
#             hoverinfo = "text") %>%
#   add_trace(y = 0,
#             opacity = 0,
#             hovertext = ~ paste("N fert rate:",fert, "lbs N/ac"),
#             hoverinfo = "text",
#             showlegend = F) %>%
#   layout(title = "Responses to fertilizer N",
#          xaxis = list(title = "N fertilizer (N lb/ac)"),
#          yaxis = list (title = " "),
#          hovermode = "x unified",
#          legend = list(orientation = 'h', y = -0.2))
# 
# p
# p %>% onRender(js)
