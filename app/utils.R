# --- Shiny utils ---
basemap <- function() {
  # pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
  # pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)

  leaflet() %>%
    addProviderTiles(providers$OpenStreetMap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldShadedRelief, group = "Relief") %>%
    addLayersControl(options = layersControlOptions(collapsed = FALSE),
                     baseGroups = c('Topographic', "Relief")) %>%
    addScaleBar("bottomleft") %>%
    setView(-95,40,4) %>%
    # addCircleMarkers(
    #   color = 'transparent',
    #   weight = 0.2,
    #   fillOpacity = 0.5,
    #   radius = 0.01,
    #   layerId = ~fips) %>%
    # addLegend("bottomright", pal = pal, values = ~gdp_md_est,
    #           title = "Est. GDP (2010)",
    #           labFormat = labelFormat(prefix = "$"),
    #           opacity = 1
    # )
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "feet",
      primaryAreaUnit = "sqmiles",
      activeColor = "red",
      completedColor = "green" ) %>%
    leafem::addMouseCoordinates()
}

second_map <- function() {
  # pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
  # pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
  leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addScaleBar("bottomleft") %>%
    setView(-95,40,4) %>%
    addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
    # addCircleMarkers(
    #   color = 'transparent',
    #   weight = 0.2,
    #   fillOpacity = 0.5,
    #   radius = 0.01,
    #   layerId = ~fips) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "feet",
      primaryAreaUnit = "sqmiles",
      activeColor = "red",
      completedColor = "green" ) %>%
    leafem::addMouseCoordinates()
}

third_map <- function() {
  # pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
  # pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
  leaflet() %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addScaleBar("bottomleft") %>%
    setView(-95,40,4) %>%
    addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
    # addCircleMarkers(
    #   color = 'transparent',
    #   weight = 0.2,
    #   fillOpacity = 0.5,
    #   radius = 0.01,
    #   layerId = ~fips) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "feet",
      primaryAreaUnit = "sqmiles",
      activeColor = "red",
      completedColor = "green" ) %>%
    leafem::addMouseCoordinates()
}
pop_map <- function() {
  pop <- tidycensus::get_acs(geography = "county",
                             variables = "B01003_001",
                             geometry = TRUE)
  pop$area <- st_area(pop)
  pop$area <- units::set_units(pop$area, "mi^2")
  pop <- rename(pop, population = estimate)
  pop <- pop %>%
    mutate(pop_density = population/area)
  pop <- pop %>% filter(!grepl("^02", GEOID))
  pop <- pop %>% filter(!grepl("^15", GEOID))
  pop <- st_transform(pop, 4326)
  pal <- colorQuantile(palette = "viridis", domain = pop$pop_density, n = 10)

  leaflet(data = pop) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(stroke = TRUE,
                  weight = 0.3,
                  smoothFactor = 0,
                  fillOpacity = 0.6,
                  color = ~pal(pop_density)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~ pop_density,
                title = "Population percentiles",
                opacity = 1) %>%
      leafem::addMouseCoordinates()
}

# zoom to selected county
zoom_to_county = function(map, counties, FIP){
  # Filter the counties to the input FIP code
  shp = filter(counties, fips == FIP)
  # Build a buffered bounding box to center the map on:
  bounds = shp %>%
    # make bounding box
    st_bbox() %>%
    # Make spatial
    st_as_sfc() %>%
    # Buffer to .1 degree
    st_buffer(.1) %>%
    # make new bounding box
    st_bbox() %>%
    # extract coordinates as vector
    as.vector()
  # Clear all current shapes (remember county centroids are currently markers!)
  clearShapes(map) %>%
    # Add the county shape making the outline color red and the fill an opaque white
    addPolygons(data = shp,
                color = "red",
                fillColor  = "grey",
                fillOpacity = .3) %>%
    # Fly the leaflet map to the buffered boundary
    flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}
#-- COUNTY CENTROIDS --
# Join County Data with
county_centroids = function(counties){
  cent <- st_centroid(counties) %>%
    st_as_sf()
}

zoom_to_catch = function(map, df, catchment){
  # Filter the counties to the input FIP code
  shp = filter(df, comid == catchment)
  # Build a buffered bounding box to center the map on:
  bounds = shp %>%
    # make bounding box
    st_bbox() %>%
    # Make spatial
    st_as_sfc() %>%
    # Buffer to .1 degree
    st_buffer(.1) %>%
    # make new bounding box
    st_bbox() %>%
    # extract coordinates as vector
    as.vector()
  # Clear all current shapes (remember county centroids are currently markers!)
  clearShapes(map) %>%
    # Add the county shape making the outline color red and the fill an opaque white
    addPolygons(data = shp,
                color = "red",
                fillColor  = "grey",
                fillOpacity = .3) %>%
    # Fly the leaflet map to the buffered boundary
    flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}

timeseries_data <- function(agg_df) {
  df <- agg_df %>%
    group_by(date) %>%
    mutate(tmax = mean(tmax),
           tmin = mean(tmin),
           prcp = mean(prcp),
           rhmin = mean(rhmin),
           rhmax = mean(rhmax),
           srad = mean(srad)) %>%
    slice(n = 1)
  df$date <- as.Date(df$date)
  df <- data.frame(df)
  rownames(df) <- df$date
  df
}




make_timeseries <- function(xts_df, param) {
  dygraph(data = dplyr::select(xts_df, param)) %>%
    dyHighlight(highlightCircleSize = 4,
                highlightSeriesBackgroundAlpha = .4) %>%
    dyOptions(colors = c("darkred"),
              fillGraph = TRUE)
}

# Takes map click --> creates extent polygon --> gets climate raster for prediction --> outputs point raster
get_AOI <- function(pt) {
  buffer <- pt %>%
    st_transform(5070) %>%
    st_buffer(10000) %>%
    st_transform(4326)

  bb = buffer %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_transform(4326) %>%
    st_as_sf()
}

# Takes map click --> creates extent polygon --> gets climate raster for prediction --> outputs point raster
get_county <- function(pt) {
  conus <- USAboundaries::us_counties()
  aoi <- st_filter(conus, pt)
}

# map click --> get AOI climate raster
get_raster <- function(bb, start_date = NULL, end_date = NULL) {
  r <- aggregate_maca(bb,
                 startDate = as.character(start_date),
                 endDate = as.character(end_date))


  #tidy_stack(clim, as_sf = TRUE)

  # missing step for final:
  # 1. aggregate_gridmet() replaces getGridMET
  # 2. Machine learn on aggregated climate rasters
}

# map click --> get AOI climate raster
get_prediction <- function(aoi, start = NULL, end = NULL, model) {
  agg <- aggregate_maca(aoi,
                        start_date = as.character(start),
                        end_date = as.character(end))
  cbi <- augment(model, agg) %>%
    tidy_to_raster(x = lon, y = lat, z = .pred)
}

get_classes <- function(rast, lvls){
  int <- seq(
    cellStats(rast, min),
    cellStats(rast, max),
    length.out = lvls
  )
  from    <- int[1:(lvls - 1)]
  to      <- int[2:lvls]
  becomes <- 1:(lvls -1)
  reclass_m <- matrix(c(from, to, becomes),
                      ncol = 3,
                      byrow = FALSE)
  categories <- reclassify(rast, reclass_m, include.lowest = TRUE)
}

get_terrain <- function(bb) {
  elev <- get_elev_raster(bb, z = 11)
  asp <- terrain(elev, opt = "aspect", unit = "degrees")
  slope <- terrain(elev, opt = "slope", unit = "degrees")
  st <- stack(slope, asp)
}

cbi_classes <- function(r) {
  reclass_m <- matrix(c(0, 50, 1,
                        50, 75, 2,
                        75, 90, 3,
                        90, 97.5, 4,
                        97.5, 9999, 5),
                      ncol = 3,
                      byrow = TRUE)
  categories <- reclassify(r, rcl = reclass_m, include.lowest = TRUE)
  ratify(categories)
}
get_population <- function(pt) {
  conus <- us_counties() %>%
    st_as_sf()

  county <- st_intersection(conus, pt) %>%
    st_drop_geometry()
  pop <- tidycensus::get_acs(geography = "county",
                             # county = county$name,
                             state = county$state_abbr,
                             variables = "B01003_001",
                             geometry = TRUE)
  pop$area <- st_area(pop)
  pop$area <- units::set_units(pop$area, "mi^2") %>%
    units::drop_units() %>%
    round()
  pop <- pop %>%
    rename(population = estimate) %>%
    mutate(pop_density = population/area)
  pop$pop_density <- round(pop$pop_density)
  pop
}

make_pop_map <- function(pop_data) {
  pop <- st_transform(pop_data, 4326)
  pal <- colorQuantile(palette = "viridis", domain = pop$pop_density, n = 10)

  leaflet(data = pop) %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(stroke = TRUE,
                fillColor = ~pal(pop_density),
                weight = 0.3,
                smoothFactor = 0,
                fillOpacity = 0.6) %>%
    addLegend("bottomright",
              pal = pal,
              values = ~ pop_density,
              title = "Population percentiles",
              opacity = 1) %>%
    leafem::addMouseCoordinates()
}
make_table <- function(pt) {
  conus <- us_counties() %>%
    st_as_sf()

  county <- st_intersection(conus, pt) %>%
    st_drop_geometry()
  pop <- tidycensus::get_acs(geography = "county",
                             county = county$name,
                             state = county$state_abbr,
                             variables = "B01003_001",
                             geometry = TRUE)
  pop$area <- st_area(pop)
  pop$area <- units::set_units(pop$area, "mi^2") %>%
    units::drop_units() %>%
    round()
  pop <- pop %>%
    rename(population = estimate) %>%
    mutate(pop_density = population/area)
  pop$pop_density <- round(pop$pop_density)
  tbl <- pop %>%
    filter(GEOID == county$geoid) %>%
    st_drop_geometry() %>%
    rename("Area (sq. miles)" = area,
           "Population" = population,
           "Population density" = pop_density) %>%
    pivot_longer(c(4, 6, 7), names_to = 'stat', values_to = "vals")

  mmtable(data = tbl, table_data = vals) +
    header_left(stat) +
    header_top_left(NAME) +
    cells_format(cell_predicate = T, style = list(cell_text(align = "right"))) +
    header_format("all_cols", style = list(cell_text(weight = "bolder"))) +
    header_format("all_rows", style = list(cell_text(weight = "bolder")))
}


make_table2 <- function(pop_data, pt) {
  conus <- us_counties() %>%
    st_as_sf()

  county <- st_intersection(conus, pt) %>%
    st_drop_geometry()

  tbl <- pop_data %>%
    filter(GEOID == county$geoid) %>%
    st_drop_geometry() %>%
    rename("Area (sq. miles)" = area,
           "Population" = population,
           "Population density" = pop_density) %>%
    pivot_longer(c(4, 6, 7), names_to = 'stat', values_to = "vals")

  mmtable(data = tbl, table_data = vals) +
    header_left(stat) +
    header_top_left(NAME) +
    cells_format(cell_predicate = T, style = list(cell_text(align = "right"))) +
    header_format("all_cols", style = list(cell_text(weight = "bolder"))) +
    header_format("all_rows", style = list(cell_text(weight = "bolder")))
}

custom_tile <- function(fun = "comma", digits = 0, palette = 'OrRd', n = 9) {
  fun <- match.fun(fun)

  # stopifnot(n >= 5)

  return_cut <- function(y)
    cut(y, breaks = quantile(y, probs = 0:n/n, na.rm = T),
        labels = 1:n, ordered_result = T, include.lowest = T)

  return_col <- function(y)
    RColorBrewer::brewer.pal(n, palette)[as.integer(return_cut(y))]

  formatter("span", x ~ fun(x, digits = digits),
            style = function(y) style(
              display = "block",
              padding = "0 4px",
              "border-radius" = "4px",
              # "color" = ifelse( return_cut(y) %in% c(1, 2, n-1, n),
              #                   csscolor("white"), csscolor("black")),
              "background-color" = return_col(y)
            )
  )
}
get_fires <- function(aoi, path) {
  fire <- st_read(path) %>%
    st_transform(4326) %>%
    st_filter(aoi) %>%
    # st_union() %>%
    st_transform(5070) %>%
    rmapshaper::ms_simplify(keep = 0.01) %>%
    st_transform(4326)

}
fire_table <- function(df) {
  tbl <- df %>%
    select(3, 1:2, 6, 8) %>%
    mutate(Area = st_area(df$shape)) %>%
    st_drop_geometry()

  tbl$acres <- round(tbl$acres, 2)
  tbl$Area <- round(tbl$Area)
  tbl <- tbl %>%
    janitor::clean_names(case = "title") %>%
    select(1:3, 6, 4) %>%
    arrange(-Acres)
  tbl <- rename(tbl,
                "Fire name" = "Fire Name",
                "Acres burned" = Acres,
                "Area (m\u00b2)" = Area,
                "Fire number" = "Fire Num")

  formattable(tbl, align = c("l", rep("r", NCOL(tbl) - 1)),
              list(`Fire name` = formatter("span", style = ~ style(color = "azure1",font.weight = "bold")),
                   `Acres burned` = custom_tile(digits = 1, n =8),
                   # `Area (m\u00b2)` = formatter("span", style = ~ style(color = "azure1")),
                   # `Year` = formatter("span", style = ~ style(color = "azure1",font.weight = "bold")),
                   `Fire number` = formatter("span", style = ~ style(color = "azure1",font.weight = "bold"))))
}

# --- Data utils ---
get_conus <- function() {
  dplyr::filter(
    USAboundaries::us_states(),
    !name %in% c("Hawaii", "Puerto Rico", "Alaska")
  ) %>%
    sf::st_as_sf() %>%
    sf::st_transform(5070)
}

get_north_america <- function() {
  rnaturalearth::ne_countries() %>%
    sf::st_as_sf() %>%
    dplyr::filter(continent == "North America") %>%
    sf::st_transform(
      paste(
        "+proj=aea",
        "+lat_1=20", "+lat_2=60",
        "+lat_0=40", "+lon_0=-96",
        "+x_0=0", "+y_0=0",
        "+ellps=GRS80",
        "+datum=NAD83",
        "+units=m",
        "+no_defs"
      )
    )
}

get_sb <- function() {
  AOI::aoi_get(county = "Santa Barbara", state = "CA")
}

get_global <- function() {
  rnaturalearth::ne_countries() %>%
    sf::st_as_sf() %>%
    sf::st_transform("+proj=robin")
}


# get mean temperature of all cells on a day (raster layer) --- we need a different time series method but this is a generic outline for testing
# timeseries_data <- function(pts) {
#   agg <- pts %>%
#     st_drop_geometry() %>%
#     group_by(date) %>%
#     mutate(mean = mean(tmax)) %>%
#     slice(n = 1)
#
#   agg$date <- as.Date(agg$date)
#   agg <- data.frame(agg)
#   rownames(agg) <- agg$date
#   agg
# }
chandler_bi <- function(rh, t) {
  rh_eq  <- (110 - 1.373 * rh)
  t_eq   <- (10.20 - t)
  rh_exp <- 10 ^ (-0.0142 * rh)
  main   <- ((rh_eq - 0.54 * t_eq) * (124 * rh_exp))
  main / 60
}
kelvin_to_fahrenheit <- function(t) ((t - 273.15) * (9 / 5) + 32)

tidy_raster <- function(raster) {
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        stringr::str_sub(colnames(.)[-(1:2)], start = 2L))
    ) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
  rtable
}
tidy_stack <- function(raster_list, as_sf = FALSE) {
  param_names <- names(raster_list)
  tidy_stacks <- lapply(X = raster_list, FUN = tidy_raster)
  p <- progressr::progressor(along = param_names)
  tidy_data <-
    lapply(X = param_names,
           FUN = function(rname) {
             p(paste0("Transforming ", rname, "..."))
             setNames(
               tidy_stacks[[rname]],
               c("lon", "lat", rname, "date")
             )
           }
    ) %>%
    purrr::reduce(dplyr::left_join, by = c("date", "lon", "lat")) %>%
    dplyr::relocate(lon, lat, date)
  if (as_sf) {
    tidy_data <-
      tidy_data %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(4326)
  }
  tidy_data
}

tidy_to_raster <- function(data, x, y, z) {
  xyz <- data %>%
    dplyr::select({{ x }}, {{ y }}, {{ z }}) %>%
    dplyr::rename(
      x = {{ x }},
      y = {{ y }},
      z = {{ z }}
    )
  raster::rasterFromXYZ(
    xyz = xyz,
    crs = sf::st_crs(4326)$proj4string
  )
}

common_params <- function() {
  grid   <- climateR::param_meta$gridmet$common.name
  maca   <- climateR::param_meta$maca$common.name
  common <- which(grid %in% maca)
  grid[common]
}
aggregate_gridmet <- function(aoi, start_date, end_date = NULL, as_sf = FALSE) {
  p <- progressr::progressor(steps = 3L)
  p("Getting GridMET data...")
  climate_data <- climateR::getGridMET(
    AOI       = sf::st_transform(aoi, 4326),
    param     = common_params(),
    startDate = start_date,
    endDate   = end_date
  )
  p("Tidying GridMET data...")
  tidy_clim <-
    tidy_stack(
      c(climate_data),
      as_sf = as_sf
    ) %>%
    dplyr::rename(
      prcp       = tidyselect::contains("prcp"),
      rhmax      = tidyselect::contains("rhmax"),
      rhmin      = tidyselect::contains("rhmin"),
      shum       = tidyselect::contains("shum"),
      srad       = tidyselect::contains("srad"),
      tmin       = tidyselect::contains("tmin"),
      tmax       = tidyselect::contains("tmax"),
    ) %>%
    dplyr::mutate(
      rhavg = (rhmax + rhmin) / 2,
      tavg  = (tmax + tmin) / 2,
      cbi_rhmax_tmax = chandler_bi(rhmax, tmax),
      cbi_rhmin_tmax = chandler_bi(rhmin, tmax),
      cbi_rhavg_tmax = chandler_bi(rhavg, tmax),
      cbi_rhmax_tmin = chandler_bi(rhmax, tmin),
      cbi_rhmin_tmin = chandler_bi(rhmin, tmin),
      cbi_rhavg_tmin = chandler_bi(rhavg, tmin),
      cbi_rhmax_tavg = chandler_bi(rhmax, tavg),
      cbi_rhmin_tavg = chandler_bi(rhmin, tavg),
      cbi_rhavg_tavg = chandler_bi(rhavg, tavg),
      burn_index = (
        cbi_rhmax_tmax + cbi_rhmin_tmax + cbi_rhavg_tmax +
          cbi_rhmax_tmin + cbi_rhmin_tmin + cbi_rhavg_tmin +
          cbi_rhmax_tavg + cbi_rhmin_tavg + cbi_rhavg_tavg
      ) / 9
    ) %>%
    dplyr::select(lat, lon, date, prcp, rhmax, rhmin, shum,
                  srad, tmin, tmax, burn_index)
  p("Tidied!")
  tidy_clim
}
aggregate_maca <- function(aoi, start_date, end_date = NULL, as_sf = FALSE) {
  p <- progressr::progressor(steps = 3L)
  p("Getting MACA data...")
  climate_data <- climateR::getMACA(
    AOI       = aoi,
    param     = common_params(),
    startDate = start_date,
    endDate   = end_date,
    model     = "BNU-ESM"
  )
  p("Tidying MACA data...")
  tidy_clim <-
    tidy_stack(
      c(climate_data),
      as_sf = as_sf
    ) %>%
    dplyr::rename(
      prcp  = tidyselect::contains("prcp"),
      rhmax = tidyselect::contains("rhmax"),
      rhmin = tidyselect::contains("rhmin"),
      shum  = tidyselect::contains("shum"),
      srad  = tidyselect::contains("srad"),
      tmin  = tidyselect::contains("tmin"),
      tmax  = tidyselect::contains("tmax")
    ) %>%
    dplyr::mutate(
      rhavg = (rhmax + rhmin) / 2,
      tavg  = (tmax + tmin) / 2,
      cbi_rhmax_tmax = chandler_bi(rhmax, tmax),
      cbi_rhmin_tmax = chandler_bi(rhmin, tmax),
      cbi_rhavg_tmax = chandler_bi(rhavg, tmax),
      cbi_rhmax_tmin = chandler_bi(rhmax, tmin),
      cbi_rhmin_tmin = chandler_bi(rhmin, tmin),
      cbi_rhavg_tmin = chandler_bi(rhavg, tmin),
      cbi_rhmax_tavg = chandler_bi(rhmax, tavg),
      cbi_rhmin_tavg = chandler_bi(rhmin, tavg),
      cbi_rhavg_tavg = chandler_bi(rhavg, tavg),
      burn_index = (
        cbi_rhmax_tmax + cbi_rhmin_tmax + cbi_rhavg_tmax +
          cbi_rhmax_tmin + cbi_rhmin_tmin + cbi_rhavg_tmin +
          cbi_rhmax_tavg + cbi_rhmin_tavg + cbi_rhavg_tavg
      ) / 9
    ) %>%
    dplyr::select(lat, lon, date, prcp, rhmax, rhmin, shum,
                  srad, tmin, tmax, burn_index)
  p("Tidied!")
  tidy_clim
}



