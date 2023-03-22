library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(plotly)

# load shapefiles
states = st_read("data/ncrStates.shp")
counties = st_read("data/ncrCounties.shp")

# load data
leach_df <- read.csv("data/leachDataSim1.csv")
yield_df <- read.csv("data/yieldDataSim1.csv")

map_df <- yield_df %>%
  distinct(across(c(lon, lat, NAME, STATE_NAME)))

# define fertilizer/x axis
fert = seq(from = 0, to = 300, by = 10)

# define equations
# const = rep(cons_ex1$a, times = length(fert))
# gomp = gomp1$a * exp((-gomp1$b)*(gomp1$c^fert))
# pwlin = ifelse(fert < pl1$c, pl1$a + (pl1$b*fert), pl1$a + (pl1$b*pl1$c))
# lin = lin1$a + (lin1$b*fert)
# nnq = nnq1$a + (nnq1$b*fert) + (nnq1$c*fert^2)
