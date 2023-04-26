library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(plotly)
library(gt)
library(reactlog)

reactlog_enable()

# load shapefiles
states = st_read("data/ncrStates.shp")
counties = st_read("data/ncrCounties.shp")

# load data
leach_df <- read.csv("data/leachData.csv")
yield_df <- read.csv("data/yieldData.csv")
sites <- read.csv("data/sampleSites.csv")

# define fertilizer/x axis
fert = seq(from = 0, to = 300, by = 10)

sims <-  readxl::read_xlsx("data/simulationNames.xlsx")
sims  <- sims %>%
  filter(simulation == 1 
         | simulation == 2)
simNames = sims$cropSystem

# determine respose curve
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


