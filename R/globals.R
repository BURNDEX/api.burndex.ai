library(tidymodels)
library(timetk)
library(earth)
library(R6)
library(AOI)
library(climateR)
library(sf)
library(foreach)

bagged_mars <- get_mars_model()
fire_perim  <- get_fire_data("perim")
fire_ts     <- get_fire_data("ts")
#> opendata.arcgis.com/datasets/5da472c6d27b4b67970acc7b5044c862_0.geojson