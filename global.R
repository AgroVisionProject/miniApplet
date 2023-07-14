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
  
  req(is.na(simulation) == FALSE)
  #print("inside function")
  
  yield_df_sum <- yield_df %>%
    filter(sim == simulation,
           lat == site_lat,
           lon == site_lon)

  # print("yield df")
  # print(nrow(yield_df_sum))
  # print(head(yield_df_sum))

  leach_df_sum <- leach_df %>%
    filter(sim == simulation,
           lat == site_lat,
           lon == site_lon)

  # print("leach df")
  # print(head(leach_df_sum))

  yieldFun <- yield_df_sum$fun
  leachFun <- leach_df_sum$fun

  yield_y <- responseCurve(dataframe = yield_df_sum, fun = yieldFun)
  leach_y <- responseCurve(dataframe = leach_df_sum, fun = leachFun)

  data.frame(fert = round(kgha_to_lbac(fert)), yield = yield_y, leaching = kgha_to_lbac(leach_y))
  
}


