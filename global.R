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
sites <- read_csv("data/sampleSites.csv")
sims <-  readxl::read_xlsx("data/simulationNames.xlsx")
simNames = sims$cropSystem

# define fertilizer/x axis----------
fert = seq(from = 0, to = 300, by = 1) #kg/ha
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

makeDF <- function(simulation, site_lat, site_lon, cornPrice, fertPrice, NUE, cornTech, fertEff) {
  
  req(is.na(simulation) == FALSE)
  
  yield_df_sum <- yield_df %>%
    filter(sim == simulation,
           lat == site_lat,
           lon == site_lon)

  leach_df_sum <- leach_df %>%
    filter(sim == simulation,
           lat == site_lat,
           lon == site_lon)

  yieldFun <- yield_df_sum$fun
  leachFun <- leach_df_sum$fun

  yield_y <- responseCurve(dataframe = yield_df_sum, fun = yieldFun)
  # corn yield improvements
  yield_new <- yield_y * cornTech
  leach_y <- responseCurve(dataframe = leach_df_sum, fun = leachFun)
  # nitrogen use improvements
  leach_new <- leach_y * fertEff

  cornVal <- (yield_y - yield_y[1]) * cornPrice
  fertCost <- round(kgha_to_lbac(fert)) * fertPrice
  NUE <- NUE
  nloss <- NUE * leach_y * fertPrice
  net <- cornVal - fertCost - nloss

  data.frame(fert = round(kgha_to_lbac(fert)), yield = yield_new, leaching = kgha_to_lbac(leach_new),
             net = net)
  
}


