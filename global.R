library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(plotly)
library(gt)
library(reactlog)

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
simNames = sims$cropSystem

# define equations
# const = rep(cons_ex1$a, times = length(fert))
# gomp = gomp1$a * exp((-gomp1$b)*(gomp1$c^fert))
# pwlin = ifelse(fert < pl1$c, pl1$a + (pl1$b*fert), pl1$a + (pl1$b*pl1$c))
# lin = lin1$a + (lin1$b*fert)
# nnq = nnq1$a + (nnq1$b*fert) + (nnq1$c*fert^2)
