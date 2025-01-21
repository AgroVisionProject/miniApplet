# Define UI for application that draws a histogram

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
  
  titlePanel(fluidRow(
    column(9, align = "center",
           h2("Nitrogen Decision Support Tool for Corn Management"), 
           h4("Assess the impact of land use and N fertilizer applications on nitrogen losses to groundwater")),
    column(3, align = "center",
           img(src = "uw-crest.png", height = 80)))
  ),
  # fluidRow(column(12, align = "center", 
  #                 h3("Assess the impact of land use and N fertilizer applications on nitrogen losses to groundwater"))),
  # 
  # methods-----------------
  bsCollapse(id = "methods",
             bsCollapsePanel("Methods", style = "success", # default, info, warning
                             h3("The Agro-IBIS Nitrogen Decision Support Tool for Corn Management in the Midwest US"),
                             h4("Overview"),
                             p("The purpose of this R Shiny application is to use output data generated from Agro-IBIS, 
                               a process-based agroecosystem model, to better understand the tradeoffs between corn production and 
                               water quality in the Midwest US that are impacted by management decision-making including rotations with 
                               soybean, irrigation, and the amount of nitrogen applied to the landscape to support higher yield potential."),
                             p("The Agro-IBIS agroecosystem model (Kucharik 2003) has been developed at UW-Madison over the past 25 years. 
                               It simulates coupled carbon (C), water, energy and nitrogen (N) cycling of natural vegetation and corn, soybean,
                               and several other agroecosystems. Agro-IBIS accounts for agricultural management (e.g., fertilizer and manure 
                               application timing/amounts, planting and harvest date, irrigation) and the effects of environmental stressors 
                               on crop development and water balance using a 1 hour timestep. At a continuum of horizontal grid resolutions 
                               from 1km to 250 km, Agro-IBIS simulates optimal planting date, crop yields, water balance, and nitrate leaching, 
                               crop yield and nitrate leaching response to varied N-fertilizer applications, and yield response to weather 
                               variability. It has been previously validated at the field scale in Wisconsin and Nebraska (Kucharik and Brye 
                               2003; Kucharik and Twine 2007), at several AmeriFlux sites for coupled C and water cycling (Kucharik et al. 2006,
                               Kucharik and Twine 2007), and the Mississippi basin-wide scale (Kucharik 2003, Donner and Kucharik, 2003, 
                               Twine and Kucharik 2008) for crop yields, NO3 leaching, soil moisture, evapotranspiration, phenology and net 
                               primary production. A more recent evaluation of nitrate leaching compared to observations (Shrestha et al. 2023)
                               can be found in Ferin and Kucharik (in press)."),
                             p(HTML("<b>Note:</b> Other publications that have used the same set of scenarios used to create this N decision 
                                    support tool can be found in Zuidema et al. (2024), Liu et al. (2023), Zuidema et al. (2023), and Sun et 
                                    al. (2020).")),
                             h4("Agro-IBIS simulations and scenarios"),
                             p("Simulations were performed across a 5min x 5min (~8km x 8km) spatial grid.  A land mask was created to only 
                               simulate grid cells that had >1% cropland area within them based on datasets available for download from 
                               EarthStat.org (Ramankutty et al. 2008).  All models runs or scenarios are a “restart run” of 60 years in length 
                               using a transient timeseries of climate/weather for the 1948-2007 time period based on the ZedX daily climate 
                               dataset developed for UW-Madison researchers.  Crop management scenarios build upon a spin-up simulation that 
                               spanned 357 years; this spin up was necessary to bring soil biogeochemistry (e.g., coupled C and N cycling) to 
                               an equilibrium state.  From 1650-1849 natural/potential vegetation was simulated in every grid cell.  From 
                               1850-1924, unfertilized continuous winter wheat was simulated everywhere.  From 1925-2007 continuous corn was 
                               simulated, with fertilizer inputs consistent with Donner and Kucharik (2008) from 1945-1989, and a switch to 
                               EarthStat fertilizer/manure 5min gridded data for N commencing in 1990 (http://www.earthstat.org/).  During 
                               spin-up years from 1650-1947, the 60-year daily ZedX climate dataset was “recycled” so that the year 1650 was 
                               actual climate/weather year 1948, sequentially stepping through all years in sequence, and then restarting 
                               from the beginning when reaching the end of the timeseries."),
                             p("While the last 60 years of the restart runs were actually 2008-2067 in model years, they actually denote 
                               climate years of 1948-2007.  The first 10 years of the output data are discarded due to a new equilibrium 
                               needing to be reached after restart runs commence.  Thus, the last 50 years of each simulation/scenario can 
                               be used for assessment purposes and denote the actual timeseries of climate/weather spanning from 1958-2007. 
                               Our analysis focused on output data for the last 30 years from 1978-2007.  NASA GISS estimates of changing 
                               atmospheric CO2 from 1650-2007 were used to parameterize Agro-IBIS. During the simulations, atmospheric CO2 
                               concentration was held fixed at 391 ppm.  The simulations were conducted for continuous corn and corn-soy 
                               rotations, both irrigated and non-irrigated, though we only show the data for the corn years in the corn-soy 
                               rotations."),
                             p("The following logic was used when applying N to cropping systems:"),
                             tags$li("For continuous corn, N from mineral fertilizer and manure was applied at the planting date."),
                             tags$li("For corn/soybean rotations, N connected to soybeans in the EarthStat.org datasets was applied after 
                                     fall harvest of soybeans, with N for corn applied at springtime planting date."),
                             tags$li("The fertilizer values used in the scenarios of varied N fertilizer applications represent a spread 
                                     of 10 values ranging from no fertilizer application to an 80% increase over the baseline amounts 
                                     (EarthStat: http://www.earthstat.org/)."),
                             p("For scenarios that represented varied N applications coupled with irrigation, a threshold of 50% plant 
                               available water [(actual VWC – PWP VWC)/(FC VWC – PWP VWC)] in the top 60cm of soil was used as the trigger 
                               in every grid cell.  VWC denotes “volumetric water content”; PWP = permanent wilting point; FC = Field capacity. 
                               Irrigation was applied during a nominal 6-hour event to a grid cell to increase soil moisture to field capacity, 
                               but assumed that a maximum daily amount applied is 50mm, or approximately 2 inches.  Irrigation was only applied 
                               to corn phase of rotations (corn/soy or continuous corn).   Lastly, irrigation was only turned on when corn was 
                               actively growing."),
                             p("Soil textural data were extracted from the data developed by Miller and White (1998). The extracted grids 
                               contain the dominant soil texture class for each of 11 standard soil layers derived from State Soil Geographic 
                               (STASGO) soil data compiled by the Natural Resources Conservation Service (NRCS) of the U.S. Department of 
                               Agriculture. The original spatial resolution of the grids is 30 arc-seconds (~ 1 km), and they were aggregated 
                               to 5min spatial resolution by taking the dominant texture in each 5min x 5min pixel. The 11 standard layers 
                               defined by Miller and White (1998) are: 0-5 cm, 5-10 cm, 10-20 cm, 20-30 cm, 30-40 cm, 40-60 cm, 60-80 cm, 
                               80-100 cm, 100-150 cm, 150-200 cm, 200-250 cm.  These are the textural categories that were used in the 
                               Agro-IBIS lookup table (params.soi) and assigned for each soil layer."),
                             h4("Curve fitting"),
                             p("For each N fertilizer scenario, average yield and nitrate leaching values at each grid cell for 30 years 
                               (1978-2007) were used. The model was recently re-calibrated to reflect contemporary yield and nitrate leaching
                               values based on USDA county level yield data and a meta-analysis of nitrate leaching loss in Midwest 
                               agroecosystems (Shrestha et al. 2023).  By running Agro-IBIS many times for each grid cell, a Gompertz curve 
                               was fit to the relationship between N applications and crop yield, and a quadratic leaching curve was fit to 
                               characterize the relationship between N applications under a given practice in a given location and nitrate 
                               leaching below the root zone. Separate curves were fit for the varied scenarios including rainfed/irrigated 
                               continuous corn and corn/soy rotation. These transfer functions allow for a spatially resolved 
                               characterization of the tradeoffs between agricultural land management, yield, and potential nitrate losses 
                               to groundwater and are the main purpose for creation of the app."),
                             h4("Comparing extreme weather: driest vs. wettest years and responses"),
                             p("To display how weather variability might alter the effect of N fertilizer rates on the output variables, we 
                               determined the wettest and driest years that occurred during 1978-2007 and the corresponding model run outputs 
                               in order to show the average response in wet or dry years. Displayed data represents an average of those 5 
                               wettest or driest years. For data displayed of corn grown in corn-soy rotation, wettest and driest years 
                               correspond only the years in which corn was grown."),
                             h4("Average annual growing degree day accumulation"),
                             p("For each location, we determined the average annual growing degree days using historical maximum and minimum 
                               temperatures from the gridMET dataset. Growing degree days were accumulated for an entire calendar year, 
                               rather than limiting to the growing season. Growing degree days were calculated as GDD = [(Tmax + Tmin)/2] – 
                               Tbase, where Tbase is the base temperature threshold of 50 ºF (10 ºC) for corn and soybeans, Tmin is the 
                               minimum daily temperature, and Tmax is the max temperature. Tmin and Tmax are set equal to Tbase if they 
                               are below Tbase, and Tmin and Tmax are set equal to an upper temperature threshold of 86 ºF (30 ºC) if 
                               they go above that value."),
                             h4("Fertilizer recommendations"),
                             p("The displayed nitrogen fertilizer recommendations are determined for each grid point based on the soil texture, 
                               irrigation status, crop rotation and fertilizer N price to corn price ratio. The recommendations are based off 
                               UW Extension Publication A2809, table 6.1. Soils were defined as sandy or sandy loamy at a depth of 0-5 cm by 
                               the USDA State Soil Geographic Database (STATSGO). Table 6.1 also differentiates fertilizer recommendations for 
                               high and medium yield potential soils, but because these soils are not defined across the region included in 
                               the app, fertilizer recommendations are averaged between the two types of soil. The shaded area on the figures 
                               in the app represent +/- 10%."),
                             tags$img(src = "fertTable.png", height = "175px"),
                             h4("References"),
                             p("Donner S.D., and C.J. Kucharik, 2008. Corn-based ethanol production compromises goal of 
                               reducing nitrogen export by the Mississippi River. Proceedings of the National Academy of 
                               Sciences 105: 4513-4518.  DOI: 10.1073/pnas.0708300105."),
                             p("Donner, S.D. and C.J. Kucharik, 2003. Evaluating the impacts of land management and climate 
                               variability on crop production and nitrate export across the Upper Mississippi Basin. Global 
                               Biogeochemical Cycles, 17(3): 1085, doi:10.1029/2001GB001808."),
                             p("Ferin, K.M. and C.J. Kucharik (in press). Irrigation expansion shows potential for increased 
                               corn yield and reduced nitrogen leaching in the Midwest US. Agricultural Systems (in press)."),
                             p("Kucharik, C.J. 2003. Evaluation of a process-based agro-ecosystem model (Agro-IBIS) across 
                               the U.S. cornbelt: simulations of the inter-annual variability in maize yield. Earth 
                               Interactions, 7:  1-33."),
                             p("Kucharik, C.J. and K.R. Brye, 2003. Integrated BIosphere Simulator (IBIS) yield and nitrate 
                               loss predictions for Wisconsin maize receiving varied amounts of nitrogen fertilizer. 
                               Journal of Environmental Quality, 32: 247-268."),
                             p("Kucharik, C.J. and Twine, T.E., 2007. Residue, Respiration, and Residuals: Evaluation of a 
                               Dynamic Agroecosystem Model Using Eddy Flux Measurements and Biometric Data. Agricultural 
                               and Forest Meteorology, 146, 134-158, doi:10.1016/j.agrformet.2007.05.011."),
                             p("Kucharik, C.J. 2006.  A multidecadal trend of earlier corn planting in the central USA.  
                               Agronomy Journal, 98: 1544-1550."),
                             p("Kucharik, C.J., C. Barford, M. El Maayar, S.C. Wofsy, R.K. Monson, D.D. Baldocchi, 2006. 
                               A multiyear evaluation of a Dynamic Global Vegetation Model (DGVM) at three AmeriFlux 
                               forest sites: vegetation structure, phenology, soil temperature, and CO2 and H2O vapor 
                               exchange. Ecological Modelling, 196: 1-31."),
                             p("Liu, J., L. Bowling, C. Kucharik, S. Jame, U. Baldos, L. Jarvis, N. Ramankutty, and T.
                               Hertel. 2023. Tackling Policy Leakage and Targeting Hotspots Could Be Key to Addressing 
                               the 'Wicked' Challenge of Nutrient Pollution from Corn Production in the U.S. Environmental 
                               Research Letters 18 105002, DOI: 10.1088/1748-9326/acf727."),
                             p("Miller, D.A. and R.A. White, 1998: A Conterminous United States Multi-Layer Soil 
                               Characteristics Data Set for Regional Climate and Hydrology Modeling. Earth Interactions, 2."),
                             p("Ramankutty, N., A. T. Evan, C. Monfreda, J. A. Foley, Farming the planet: 1. Geographic 
                               distribution of global agricultural lands in the year 2000. Glob. Biogeochem. Cycles 22 (2008)."),
                             p("Shrestha, D., K. Masarik, and C.J. Kucharik. 2023. Nitrate losses from Midwest US agroecosystems: 
                               Impacts of varied management and precipitation. Journal of Soil and Water Conservation, 
                               doi:10.2489/jswc.2023.00048."),
                             p("Sun, S., B. Ordonez, M. Webster, J. Liu, C.J. Kucharik, and T. Hertel. 2020.  Fine-scale 
                               analysis of the energy-land-water nexus: nitrate leaching implications of biomass co-firing 
                               in the Midwestern U.S.  Environmental Science and Technology 54(4): 2122-2132."),
                             p("Twine, T.E., and C.J. Kucharik, 2008. Evaluating a terrestrial ecosystem model with satellite 
                               information of greenness. Journal of Geophysical Research-Biogeosciences, 
                               doi:10.1029/2007JG000599."),
                             p("Zuidema S., J. Liu, M. Chepeliev, D. Johnson, U. Lantz Baldos, S. Frolking, C.J. Kucharik, 
                               W. Wollheim, and T. Hertel. 2023. US climate policy yields water quality cobenefits in the 
                               Mississippi Basin and Gulf of Mexico. Proc. Natl. Acad. Sci., 120(43) e2302087120, 
                               https://doi.org/10.1073/pnas.2302087120."),
                             p("Zuidema, S., W. Wollheim, C.J. Kucharik, and R. Lammers. 2024. Existing wetland conservation 
                               programs miss nutrient reduction targets. Proc. Natl. Acad. Sci Nexus, 
                               https://doi.org/10.1093/pnasnexus/pgae129."),
                             br(),
                             br(),
                             hr(style = "margin-top:0px"),
                             p(em("Source code is available ", tags$a(href="https://github.com/AgroVisionProject/miniApplet", "here")))
             )),
  
  # side bar ------------------
  sidebarLayout(
    sidebarPanel(h5("Select a location to view corn yield and nitrate leaching responses to fertilizer N."),
                 h5("Zoom to location either by clicking on the map or using the", strong("Plus +"), "or", strong("Minus -"), "buttons."),
                 #h3("To view site specific corn yield and nitrate leaching responses to fertilizer N."),
                 br(),
                 actionButton("reset", "Clear locations"),
                 shinyjs::useShinyjs(),
                 downloadButton("downloadSim", "Download Data"),
                 hr(),
                 shinyjs::useShinyjs(),
                 uiOutput("simSelectionUI"),
                 uiOutput("pricesUI"),
                 uiOutput("slider1UI"),
                 uiOutput("slider2UI")),
    mainPanel(leafletOutput("map", height = 350) %>%
                withSpinner(type = 3,
                            color.background = "white"),
              br(),
              br(),
              uiOutput("plotUI")
              
    )
  )
)